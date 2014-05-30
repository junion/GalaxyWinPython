/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/*
 * Hub functions are called in the following order:
 *	__GalIO_HubHandler negotiates the connection to the server
 *	GalIO_InHandler and GalIO_OutHandler manage the incoming/outgoing data
 *	incoming data is processed by a hub-specified handler function if
 *		provided
 * 
 * Server functions are called in the following order:
 *	GalIO_ServerInit calls GalIO_ListenerInit, sets up the structure and
 *		starts polling on the listen socket
 *	galaxy_server_poll handles the connection to the hub
 *	GalIO_ServerHandler negotiates the connection to the hub
 *	GalIO_ConnectionPoll polls an established connection
 *	GalIO_InHandler and GalIO_OutHandler manage the incoming/outgoing data
 *	incoming data is processed by a server-specified handler function if
 *		provided (in generic_server.c, GalSS_FrameHandler)
 *
 *	
 */

#include "galaxy/sysdep.h"
#include <stdio.h>
/*#ifndef WIN32
#include <netdb.h>
#else
#include <winsock2.h>*
#endif*/
#include "io_internal.h"
#include "binary_io.h"
#include "galaxy/program.h"

/* For destroying the server struct. In dispatch_function.c. */

extern
void _Gal_FreeDispatchFnPkg(Gal_DispatchFnPkg *pkg);

/* I need to know about the continuations. */

#include "../ServerStub/continuation.h"

static GalUtil_LocalMutex connection_mutex;
static GalUtil_LocalMutex server_tidx_mutex;
static GalUtil_LocalMutex server_property_mutex;

void _Gal_init_hub_server(void)
{
  GalUtil_InitLocalMutex(&connection_mutex);
  GalUtil_InitLocalMutex(&server_tidx_mutex);
  GalUtil_InitLocalMutex(&server_property_mutex);
}

extern void _gal_unlock_mutex(void *mutex);

static void __GalIO_LockConnectionMutex(int location)
{
#if 0
  GalUtil_Print(-1,"Locking mutex in thread %d at location %d\n",
		GalUtil_CurrentThreadID(), location);
#endif
  GalUtil_LockLocalMutex(&connection_mutex);
}

static void __GalIO_UnlockConnectionMutex(int location)
{
  GalUtil_UnlockLocalMutex(&connection_mutex);
#if 0
  GalUtil_Print(-1,"Unlocked mutex in thread %d at location %d\n",
		GalUtil_CurrentThreadID(), location);
#endif
}

#define IN_USE 1
#define DESTROYED 2
/* This is STRICTLY to be used when retrieving
   information that's defined in the header. I'll
   set up a server structure which is utterly
   unpopulated. The only initialization function
   which should do ANYTHING when this flag is set
   is GalSS_InitializeServerDefaults, GalIO_SetServerName,
   GalIO_SetServerDefaultPort. */
#define INITIALIZATION_RETRIEVAL 3

/* Connections go through several stages. */

/* A connection starts out UNVERIFIED. */
#define UNVERIFIED 0
/* Once the handshake is complete, the
   connection is PREVERIFIED. This needs to be copied
   over to hub_init.c. Argh. */
#define PREVERIFIED 1
/* After reinitialize is sent, the connection
   is in the SENT_FRAME state. */
#define SENT_FRAME 2
/* Once the reinitialize is completed, the
   connection is VERIFIED. */
#define VERIFIED 3

#define GALAXY_NO_ERROR 0
#define CONFIGURATION_ERROR 1
#define CONNECTION_ERROR 2
#define LISTENER_ERROR 3

static GalIO_CommStruct *NewCommStruct(int mutex_locked, int enqueue);
static int reset_listener(GalIO_ServerStruct *scomm, char *calling_fn);
static void galaxy_server_poll(Gal_TaskPkg *p);
static void galaxy_connection_poll(Gal_TaskPkg *p);
static void __GalIO_ICloseCommSocket(GalIO_CommStruct *gcomm);
static void __GalIO_ICloseServerListenSocket(GalIO_ServerStruct *scomm);
static GalIO_CommStruct *
__GalIO_MTContactHubFromServerLoc(GalIO_ServerLocation *loc,
				  GalIO_ServerStruct *scomm,
				  const char *session_id);
static void __GalIO_MTSetServerMaxConnections(GalIO_ServerStruct *scomm,
					      int max);

static Gal_Frame GalIO_ConnectionInitiationFrame();
static Gal_Frame GalIO_ServerInformationFrame(GalIO_ServerStruct *scomm,
					      const char *session_id);

static int GalIO_HubClientHandshakeHandler(GalIO_CommStruct *gcomm,
					   Gal_Frame *reply);
static int GalIO_ServerListenerHandshakeHandler(GalIO_ServerStruct *scomm,
						GalIO_CommStruct *gcomm,
						GalIO_CommStruct **new_conn);
static int GalIO_ServerClientHandshakeHandler(GalIO_ServerStruct *scomm,
					      GalIO_CommStruct *new_conn,
					      const char *session_id);

static Gal_Frame __GalIO_IDispatchViaHub(GalIO_CommStruct *gcomm, Gal_Frame frame, GalIO_MsgType *msg_type_ptr);
static int __GalIO_ICommWriteMessage(GalIO_CommStruct *gcomm, Nframe frame, GalIO_MsgType msg_type, int do_block);

int _GalIO_HubVerificationHandler(GalIO_CommStruct *gcomm, Gal_Frame init_frame);

static int __galio_apply_server_callback(void *data, void *caller_data);

int _GalIO_ServerIsInitializationPrototype(GalIO_ServerStruct *scomm);

/* This is how we create the connection queues. */

/* SAM 11/28/00: NOTE CAREFULLY that GalIO_OperateOnConnections()
   could deadlock in the threaded case if the comm_queue had
   its own mutex, if it detected that a connection died
   and tried to remove it from the queue. This is because
   _GalIO_QueueApply doesn't (well, frankly, can't afford to)
   unlock its mutex when it calls its member operation. This
   function is used by GalIO_ServerPoll(), so don't
   add a mutex to the comm_queue. */

GalIO_PointerQueue *_GalIO_CreateConnectionQueue()
{
  return _GalIO_NewPointerQueue(GAL_CONNECTION_PTYPE,
				1, 0, NULL);
}

static GalIO_ServerStruct *NewServerStruct(int max_connections)
{
  GalIO_ServerStruct *scomm;

  scomm = (GalIO_ServerStruct *)calloc(1, sizeof(GalIO_ServerStruct));
  scomm->status = IN_USE;
  scomm->default_port = 0;
  scomm->listen_sock = GAL_INVALID_SOCKET;
  scomm->listen_port = -1;
  scomm->server_listen_status = GAL_CONNECTION_LISTENER;
  scomm->max_connections = max_connections;
  scomm->num_connections = 0;
  scomm->comm_queue = _GalIO_CreateConnectionQueue();
  scomm->error = GALAXY_NO_ERROR;
  scomm->gcomm_prototype = NewCommStruct(0, 0);
  scomm->server_properties = Gal_MakeClauseFrame("server_properties");
  scomm->service_types = Gal_CreateListObject(NULL, 0, _gal_free_object, 1);
  /* Event registrations. */
  scomm->listener_startup_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
							     0, 1, free);
  scomm->listener_shutdown_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
							      0, 1, free);
  scomm->client_poll_startup_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
								0, 1, free);
  scomm->destroy_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
						    0, 1, free);
  scomm->connection_setup_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
							     0, 1, free);
  scomm->brokers = _GalIO_NewPointerQueue(GAL_BROKER_PTYPE,
					  0, 1, NULL);
					  
  return(scomm);
}

/* SAM 9/14/99: When I destroy the server, I need to destroy
   all the connections. */

static void __GalIO_RemoveCommStructFromServer(GalIO_CommStruct *gcomm)
{
  if (gcomm) {
    GalIO_ServerStruct *s_gcomm = gcomm->server;
    void *removed_element;

    if (s_gcomm) {
      __GalIO_LockConnectionMutex(1);
      removed_element = _GalIO_QueueDequeue(s_gcomm->comm_queue,
					    (void *) gcomm);
      if (removed_element)
	s_gcomm->num_connections--;
      __GalIO_UnlockConnectionMutex(3);
    }
  }
}

/* SAM 2/10/99: If __GalIO_ICloseCommSocket is called with mutexes
   unlocked, you don't need to worry about pushing the
   cleanup handler before you close the comm socket.
   Otherwise, you do. */

#if defined(GAL_WIN32_THREADS) || defined(GAL_PTHREADS)
static void unlock_conn_mutex(void *arg)
{
  __GalIO_UnlockConnectionMutex(4);
}
#endif

/* Global readers. There's a potential deadlock in the
   non-thread case, where so much of the network or
   process socket resources are used up that a "blocking"
   read can't read anything out of a different socket.
   So I need to make sure that global resources are
   freed in that case. The problem is that I need to track
   the global resources. They don't necessarily live on the
   server (and the blocking reader code doesn't really
   have access to a server anyway), because there may be
   more than one server that the programmer has created
   and is polling, and because the user may simply have
   opened a connection (or several connections) using
   GalIO_ClientConnect(). So I need to add all the
   connections, when they're created, to a global
   list of connections, and remove them when they're done. */

static GalIO_PointerQueue *__GalIO_ReaderQueue = (GalIO_PointerQueue *) NULL;

static void __GalIO_MTReaderEnqueue(GalIO_CommStruct *gcomm)
{
  if (!__GalIO_ReaderQueue) {
    __GalIO_ReaderQueue = _GalIO_CreateConnectionQueue();
  }
  _GalIO_QueueEnqueue(__GalIO_ReaderQueue, (void *) gcomm);
}

void _GalIO_ReaderEnqueue(GalIO_CommStruct *gcomm)
{
  __GalIO_LockConnectionMutex(0);
  __GalIO_MTReaderEnqueue(gcomm);
  __GalIO_UnlockConnectionMutex(0);
}

/* SAM 2/25/02: For cleaning up memory management. */

void _GalIO_FreeReaderQueue()
{
  if (__GalIO_ReaderQueue) {
    _GalIO_QueueDestroy(__GalIO_ReaderQueue);
    __GalIO_ReaderQueue = (GalIO_PointerQueue *) NULL;
  }
}  

static void __GalIO_MTReaderDequeue(GalIO_CommStruct *gcomm)
{
  if (__GalIO_ReaderQueue) {
    _GalIO_QueueDequeue(__GalIO_ReaderQueue, (void *) gcomm);
  }
}

void _GalIO_ReaderDequeue(GalIO_CommStruct *gcomm)
{
  __GalIO_LockConnectionMutex(0);
  __GalIO_MTReaderDequeue(gcomm);
  __GalIO_UnlockConnectionMutex(0);
}

static int __galio_flush_reader(void *arg, void *caller_data)
{
  GalIO_CommStruct *gcomm = (GalIO_CommStruct *) arg;
  int blocking = gcomm->read_blocking;

  /* NEVER BLOCK! */
  gcomm->read_blocking = 0;
  gcomm->queue->queue->read_blocking = 0;  
  _GalIO_SockObjectQueueProcessIn(gcomm->queue);
  gcomm->read_blocking = blocking;
  gcomm->queue->queue->read_blocking = blocking;
  /* Always continue. */
  return 1;
}

/* 4/18/02: The mutex needs to be protected with a cleanup push/pop.
   I'm going to do it before the trylock call. */

void _GalIO_FlushReaders()
{
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(unlock_conn_mutex, (void *) NULL);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(unlock_conn_mutex, (void *) NULL);
#endif
  switch (GalUtil_TryLockLocalMutex(&connection_mutex)) {
  case 0:
    /* Success */
    if (__GalIO_ReaderQueue) {
      _GalIO_QueueApply(__GalIO_ReaderQueue, __galio_flush_reader,
			(void *) NULL);
    }
    __GalIO_UnlockConnectionMutex(0);
    break;
  case GAL_MUTEX_ALREADY_LOCKED:
    /* Busy. If somebody's already working,
       there's no reason for you to work too. */
  default:
    /* Error */
    break;
  }
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
}

/* Broker management. This had better do the right
   thing with locking the mutex. The brokers queue has
   its own mutex, so we should be fine. */

void _GalIO_ServerAddOutgoingBroker(GalIO_ServerStruct *scomm,
				    GalIO_BrokerStruct *b)
{
  _GalIO_QueueEnqueue(scomm->brokers, (void *) b);
}

void _GalIO_ServerRemoveOutgoingBroker(GalIO_ServerStruct *scomm,
				       GalIO_BrokerStruct *b)
{
  _GalIO_QueueDequeue(scomm->brokers, (void *) b);
} 

static int __galio_apply_connection_callback(void *data, void *caller_data)
{
  GalIO_Callback *cb = (GalIO_Callback *) data;
  GalIO_ConnectionCallbackFn fn = (GalIO_ConnectionCallbackFn) cb->fn;
  GalIO_CommStruct *gcomm = (GalIO_CommStruct *) cb->callback_host;
  
  (*fn)(gcomm, cb->callback_data);
  /* Continue. */
  return 1;
}

/* Used by frame_util.c. The environments lock the underlying
   connection, so we get write errors instead of seg faults. */

void _GalIO_CommLock(GalIO_CommStruct *gcomm)
{
  GalUtil_LockLocalMutex(gcomm->ref_mutex);
  gcomm->reference_count += 1;
  GalUtil_UnlockLocalMutex(gcomm->ref_mutex);
}

static void __GalIO_FreeCommStruct(GalIO_CommStruct *gcomm)
{
  GalUtil_DestroyLocalMutex(gcomm->ref_mutex);
  gcomm->ref_mutex = (GalUtil_LocalMutex *) NULL;
  /* If this is a prototype, there will be no host. */
  if (gcomm->host)
    free(gcomm->host);
  free(gcomm);
}

void _GalIO_CommUnlock(GalIO_CommStruct *gcomm)
{
  int ref_count;
  
  GalUtil_LockLocalMutex(gcomm->ref_mutex);
  gcomm->reference_count -= 1;
  ref_count = gcomm->reference_count;
  GalUtil_UnlockLocalMutex(gcomm->ref_mutex);
  
  if ((ref_count == 0) &&
      (gcomm->status == DESTROYED)) {
    __GalIO_FreeCommStruct(gcomm);
  }
}

/* This function should be static, but it's needed in hub_init.c. */

/* SAM 10/30/00: I've added reference counts to the connection,
   too, just so that we get write errors instead of seg faults
   if the connection is closed. */

void __GalIO_IDestroyCommStruct(GalIO_CommStruct *gcomm,
				int is_prototype,
				int mutex_locked)
{
  if (gcomm)
  {
    if (!is_prototype) {
      __GalIO_ICloseCommSocket(gcomm);
      if (mutex_locked) {
	__GalIO_MTReaderDequeue(gcomm);
      } else {
	_GalIO_ReaderDequeue(gcomm);
      }
      _GalIO_QueueApply(gcomm->destroy_callbacks,
			__galio_apply_connection_callback,
			(void *) NULL);
      _GalSS_FreeContinuationQueue(gcomm->continuations);
      gcomm->continuations = (GalIO_PointerQueue *) NULL;
      if (gcomm->comm_data_free_fn && gcomm->comm_data) {
	(*gcomm->comm_data_free_fn)(gcomm->comm_data);
      }
    }
    _GalIO_DestroySockObjectQueue(gcomm->queue);
    gcomm->queue = NULL;
    
    _GalIO_QueueDestroy(gcomm->destroy_callbacks);
    gcomm->destroy_callbacks = (GalIO_PointerQueue *) NULL;
    _GalIO_QueueDestroy(gcomm->shutdown_callbacks);
    gcomm->shutdown_callbacks = (GalIO_PointerQueue *) NULL;
    _GalIO_QueueDestroy(gcomm->broker_in_callbacks);
    gcomm->broker_in_callbacks = (GalIO_PointerQueue *) NULL;
    _GalIO_QueueDestroy(gcomm->broker_out_callbacks);
    gcomm->broker_out_callbacks = (GalIO_PointerQueue *) NULL;
     _GalIO_QueueDestroy(gcomm->dispatch_fn_callbacks);
    gcomm->dispatch_fn_callbacks = (GalIO_PointerQueue *) NULL;
    /* SAM 5/10/02: DON'T DESTROY THE MUTEX UNTIL YOU FREE!
       This is because _GalIO_CommUnlock() locks the mutex
       when it decrements the reference count. */    
    if (is_prototype || (gcomm->done && (gcomm->reference_count == 0))) {
      __GalIO_FreeCommStruct(gcomm);
    } else {
      gcomm->status = DESTROYED;
    }
  }
}

void GalIO_DestroyServerStruct(GalIO_ServerStruct *scomm)
{
  if (scomm)
  {
    GalIO_CommStruct *gcomm;
    GalIO_BrokerStruct *b;
    
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPushCleanup(unlock_conn_mutex, (void *) NULL);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_push(unlock_conn_mutex, (void *) NULL);
#endif
    __GalIO_LockConnectionMutex(5);
    if (scomm->comm_queue) {
      gcomm = (GalIO_CommStruct *) _GalIO_QueuePop(scomm->comm_queue);
      while (gcomm) {
	GalIO_SetCommDone(gcomm);
	__GalIO_IDestroyCommStruct(gcomm, 0, 1);	
	gcomm = (GalIO_CommStruct *) _GalIO_QueuePop(scomm->comm_queue);
      }
      _GalIO_QueueDestroy(scomm->comm_queue);
      scomm->comm_queue = (GalIO_PointerQueue *) NULL;
    }
    __GalIO_ICloseServerListenSocket(scomm);
    Gal_FreeFrame(scomm->server_properties);
    scomm->server_properties = (Gal_Frame) NULL;
    Gal_FreeObject(scomm->service_types);
    scomm->service_types = (Gal_Object) NULL;
    /* First, apply all the destroy callbacks. */
    _GalIO_QueueApply(scomm->destroy_callbacks,
		      __galio_apply_server_callback,
		      (void *) NULL);
    if (scomm->server_data && scomm->server_data_free_fn) {
      (*scomm->server_data_free_fn)(scomm->server_data);
    }
    if (scomm->gcomm_prototype) {
      if (scomm->gcomm_prototype->fn_pkg)
	_Gal_FreeDispatchFnPkg(scomm->gcomm_prototype->fn_pkg);
      __GalIO_IDestroyCommStruct(scomm->gcomm_prototype, 1, 1);
      scomm->gcomm_prototype = (GalIO_CommStruct *) NULL;
    }
    /* If this server happens to have brokers associated with
       it, those brokers have to be destroyed too. They really
       need to be off the queue before they're destroyed,  because
       if they were on the queue, the destroyer would have to remove
       them, which would probably get reentrantly ugly. */
    if (scomm->brokers) {
      b = (GalIO_BrokerStruct *) _GalIO_QueuePop(scomm->brokers);
      while (b) {
	GalIO_BrokerDataDone(b);
	GalIO_DestroyBrokerStruct(b);
	b = (GalIO_BrokerStruct *) _GalIO_QueuePop(scomm->brokers);
      }
      _GalIO_QueueDestroy(scomm->brokers);
      scomm->brokers = (GalIO_PointerQueue *) NULL;
    }
    /* Get rid of all the queues. */
    _GalIO_QueueDestroy(scomm->listener_startup_callbacks);
    scomm->listener_startup_callbacks = (GalIO_PointerQueue *) NULL;
    _GalIO_QueueDestroy(scomm->listener_shutdown_callbacks);
    scomm->listener_shutdown_callbacks = (GalIO_PointerQueue *) NULL;
    _GalIO_QueueDestroy(scomm->destroy_callbacks);
    scomm->destroy_callbacks = (GalIO_PointerQueue *) NULL;
    _GalIO_QueueDestroy(scomm->client_poll_startup_callbacks);
    scomm->client_poll_startup_callbacks = (GalIO_PointerQueue *) NULL;
    _GalIO_QueueDestroy(scomm->connection_setup_callbacks);
    scomm->connection_setup_callbacks = (GalIO_PointerQueue *) NULL;
    if (scomm->done) {
      if (scomm->default_session_id) free(scomm->default_session_id);
      if (scomm->server_locations) {
	GalIO_FreeServerLocations(scomm->server_locations);
      }
      free(scomm);
    } else {
      scomm->status = DESTROYED;
    }

    __GalIO_UnlockConnectionMutex(6);
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_pop(0);
#endif
  }
}

/* This read handler is used for the default connection read
   function, i.e., connected to the Hub. Brokers will define
   a different one. -1 is error, 0 is nothing to do, 1 is something to do. */

static int __GalIO_ConnectionReadHandler(GalIO_CommStruct *gcomm)
{
  Gal_Frame frame = (Gal_Frame) NULL;
  int status = (*(gcomm->msg_reader))(gcomm, &frame, (GalIO_MsgType *) NULL,
				      NULL, (void *) NULL);
      
  /* frame was read, call data handler */
  
  if ((status == 1) && (gcomm->handler)) {
    (*(gcomm->handler))(gcomm, frame);
  }
  return status;
}

void _GalIO_CommFreeMessages(GalIO_CommStruct *gcomm, Gal_Frame incoming, Gal_Frame outgoing)
{
  (*(gcomm->msg_freer))(incoming, outgoing);
}

/* This handles the non-local server case. */

static void __GalIO_ConnectionFrameFreer(Gal_Frame incoming,
					 Gal_Frame outgoing)
{
  if (outgoing && outgoing != incoming)
    Gal_FreeFrame(outgoing);
  Gal_FreeFrame(incoming);
}

/* This handles the non-local server case. */

static int __GalIO_ConnectionFrameReader(GalIO_CommStruct *gcomm,
					 Gal_Frame *frame_ptr,
					 GalIO_MsgType *msg_type_ptr,
					 GalIO_MsgQueueTestFn test_fn,
					 void *client_data)
{
  int status = _galio_read_incoming_msg_from_sockqueue(gcomm->queue,
						       frame_ptr, 0);
  if ((status == 1) && msg_type_ptr) {
    *msg_type_ptr = GAL_MESSAGE_MSG_TYPE;
  } 
  return status;
}


static GalIO_CommStruct *NewCommStruct(int mutex_locked, int enqueue)
{
  GalIO_CommStruct *gcomm;

  gcomm = (GalIO_CommStruct *)calloc(1, sizeof(GalIO_CommStruct));
  gcomm->queue = _GalIO_CreateSockObjectQueue(GAL_INVALID_SOCKET, 32*1024, 32*1024);
  gcomm->status = IN_USE;
  gcomm->contact_port = -1;
  gcomm->sock = GAL_INVALID_SOCKET;
  gcomm->verified = UNVERIFIED;
  gcomm->error = GALAXY_NO_ERROR;
  gcomm->comm_data = (void *) NULL;
  gcomm->server = (GalIO_ServerStruct *) NULL;
  /* I need to customize behaviors so that they can
     be changed for (a) local (in Hub) vs. remove servers
     and (b) continuations vs. not */
  gcomm->msg_reader = __GalIO_ConnectionFrameReader;
  gcomm->msg_freer = __GalIO_ConnectionFrameFreer;
  gcomm->writer = __GalIO_ICommWriteMessage;
  gcomm->dispatcher = __GalIO_IDispatchViaHub;
  /* Default handler. */
  gcomm->read_handler = __GalIO_ConnectionReadHandler;
  /* Add it to the global list of readers. This may appear to
     be gratuitous, since this is pre-handshake, but I want to
     make sure that even during the handshake, the flushing works. */
  if (enqueue) {
    if (mutex_locked) {
      __GalIO_MTReaderEnqueue(gcomm);
    } else {
      _GalIO_ReaderEnqueue(gcomm);
    }
  }
  /* Callback queues. */  
  gcomm->shutdown_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
						     0, 1, free);
  gcomm->destroy_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
						    0, 1, free);
  gcomm->broker_in_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
						      0, 1, free);
  gcomm->broker_out_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
						       0, 1, free);
  gcomm->dispatch_fn_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
							0, 1, free);
  /* Initialize the reference counter mutex. */
  gcomm->ref_mutex = GalUtil_CreateLocalMutex();
  return(gcomm);
}
/* These are public, but they shouldn't be advertised in a header.
   DO NOT SET THESE TO ANYTHING THAT CALLS GalIO_DispatchViaHub or
   GalIO_CommWriteFrame. You'll get an infinite loop. */

void _GalIO_CommSetFrameWriter(GalIO_CommStruct *gcomm, GalIO_FrameWriter fn)
{
  gcomm->writer = fn;
}

void _GalIO_CommSetFrameDispatcher(GalIO_CommStruct *gcomm, GalIO_FrameDispatcher fn)
{
  gcomm->dispatcher = fn;
}

void _GalIO_CommSetFrameReader(GalIO_CommStruct *gcomm, GalIO_FrameReader fn)
{
  gcomm->msg_reader = fn;
}

void _GalIO_CommSetFrameFreer(GalIO_CommStruct *gcomm, void (*fn)(Gal_Frame, Gal_Frame))
{
  gcomm->msg_freer = fn;
}

int _GalIO_CommInvokeReadHandler(GalIO_CommStruct *gcomm)
{
  return (*(gcomm->read_handler))(gcomm);
}

/* SAM 7/13/00: This function is called when a client detaches on
   the Hub side as well, so there may not be a server. */

void GalIO_DestroyCommStruct(GalIO_CommStruct *gcomm)
{
  GalIO_ServerStruct *scomm = gcomm->server;
  int i;
  char *host = _gal_strdup(gcomm->host);
  unsigned short port = gcomm->contact_port;
  int destroy_server = 0;
  
  if (gcomm) {    
    __GalIO_RemoveCommStructFromServer(gcomm);
    if (gcomm->sock != GAL_INVALID_SOCKET) {
      GalUtil_PrintWithLocation(GAL_CONNECTION_VERBOSITY_LEVEL,
				__FUNCTION__,
				"Destroying connection to %s (socket %d)\n",
				gcomm->host, gcomm->sock);
    }
    __GalIO_IDestroyCommStruct(gcomm, 0, 0);
    /* If we're acting as a client, and the last
       Hub disconnects, then we're done. */
    __GalIO_LockConnectionMutex(7);
    if (scomm && GalIO_ServerIsClient(scomm)) {
      /* If we're a Hub client, we want to check a couple things.
	 First, we want to mark ourselves as not connected in
	 the set of locations. Next, if there are no more
	 connections, and the instructions are to disconnect,
	 then shutdown. */      
      for (i = 0; scomm->server_locations[i].host; i++) {
	if (Gal_StringEq(scomm->server_locations[i].host, host) &&
	    (port == scomm->server_locations[i].port)) {
	  scomm->server_locations[i].connected = GAL_HUB_CLIENT_DISCONNECTED;
	  break;
	}
      }
      if ((scomm->server_listen_status & GAL_HUB_CLIENT_DISCONNECT_MASK) == GAL_HUB_CLIENT_DISCONNECT_SHUTDOWN) {
	/* The server can contact multiple hubs, so we only exit if this
	   is the last connection. The connection is still in the
	   connection list at this point, maybe. */
	
	if (scomm->num_connections == 0) {
	  destroy_server = 1;
	}
      }
    }
    __GalIO_UnlockConnectionMutex(8);
    if (destroy_server) {
      GalIO_DestroyServerStruct(scomm);
    }
  }
  free(host);
}

/* accessor functions */

GAL_SOCKET
GalIO_GetServerListenSocket(GalIO_ServerStruct *scomm)
{
  if (scomm)
    return(scomm->listen_sock);
  else
    return(GAL_INVALID_SOCKET);
}

unsigned short
GalIO_GetServerListenPort(GalIO_ServerStruct *scomm)
{
  if (scomm)
    return(scomm->listen_port);
  else
    return(-1);
}

GAL_SOCKET
GalIO_GetCommSocket(GalIO_CommStruct *gcomm)
{
  if (gcomm)
    return(gcomm->sock);
  else
    return(GAL_INVALID_SOCKET);
}

static void
SetCommSocket(GalIO_CommStruct *gcomm, GAL_SOCKET sock)
{
  if (gcomm)
  {
    gcomm->sock = sock;
    _GalIO_SetSockObjectQueueSocket(gcomm->queue, sock);
  }
}

char *
GalIO_GetCommHost(GalIO_CommStruct *gcomm)
{
  if (gcomm)
    return(gcomm->host);
  else
    return(NULL);
}

void *
GalIO_GetCommServerData(GalIO_CommStruct *gcomm)
{
  if (gcomm)
    return(GalIO_GetServerData(gcomm->server));
  else
    return(NULL);
}

void *
GalIO_GetServerData(GalIO_ServerStruct *scomm)
{
  if (scomm)
    return(scomm->server_data);
  else
    return(NULL);
}

void
GalIO_SetServerData(GalIO_ServerStruct *scomm, void *data,
		    void (*free_fn)(void *))
{
  if (scomm && !_GalIO_ServerIsInitializationPrototype(scomm)) {
    if (scomm->server_data && scomm->server_data_free_fn) {
      (*scomm->server_data_free_fn)(scomm->server_data);
    }
    scomm->server_data = data;
    scomm->server_data_free_fn = free_fn;
  }
}

void *
GalIO_GetCommData(GalIO_CommStruct *gcomm)
{
  if (gcomm)
    return(gcomm->comm_data);
  else
    return(NULL);
}

void
GalIO_SetCommData(GalIO_CommStruct *gcomm, void *data,
		  void (*free_fn)(void *))
{
  if (gcomm) {
    if (gcomm->comm_data && gcomm->comm_data_free_fn) {
      (*gcomm->comm_data_free_fn)(gcomm->comm_data);
    }
    gcomm->comm_data = data;
    gcomm->comm_data_free_fn = free_fn;
  }  
}

void
GalIO_SetCommDone(GalIO_CommStruct *gcomm)
{
  if (gcomm)
    gcomm->done = 1;
}

void
GalIO_SetServerDone(GalIO_ServerStruct *scomm)
{
  if (scomm)
    scomm->done = 1;
}

char *GalIO_GetServerName(GalIO_ServerStruct *scomm)
{
  return scomm->server_name;
}

char *GalIO_GetCommServerName(GalIO_CommStruct *gcomm)
{
  if (gcomm->server)
    return gcomm->server->server_name;
  else return (char *) NULL;
}

int GalIO_GetServerMaxConnections(GalIO_ServerStruct *scomm)
{
  int i;
  __GalIO_LockConnectionMutex(9);
  i = scomm->max_connections;
  __GalIO_UnlockConnectionMutex(10);
  return i;
}

int GalIO_GetServerNumConnections(GalIO_ServerStruct *scomm)
{
  int num_connections = 0;

  __GalIO_LockConnectionMutex(11);
  num_connections = scomm->num_connections;
  __GalIO_UnlockConnectionMutex(12);
  return num_connections;
}

/* This function must be called with the mutex locked. */

static void __GalIO_MTSetServerMaxConnections(GalIO_ServerStruct *scomm, int max)
{
  if (!_GalIO_ServerIsInitializationPrototype(scomm)) {
    /* Don't do anything if the max is the same. */
    if (max == scomm->max_connections) {
      return;
    }

    /* Servers must accept at least one connection. */
    if (max < 1) {
      return;
    }
    /* Don't use this to disconnect people. */  
    if (max < scomm->num_connections) {
      return;
    } else {
      scomm->max_connections = max;
    }
  }
}

void GalIO_SetServerMaxConnections(GalIO_ServerStruct *scomm, int max)
{
  __GalIO_LockConnectionMutex(13);
  __GalIO_MTSetServerMaxConnections(scomm, max);
  __GalIO_UnlockConnectionMutex(14);
}

GalIO_CommStruct *GalIO_GetUniqueConnection(GalIO_ServerStruct *scomm)
{
  GalIO_CommStruct *comm = (GalIO_CommStruct *) NULL;
  int cant_do_it = 0;
  
  __GalIO_LockConnectionMutex(14);

  if (scomm->max_connections != 1) {
    cant_do_it = 1;
  } else if (scomm->num_connections == 1) {
    comm = (GalIO_CommStruct *) _GalIO_QueueNth(scomm->comm_queue, 0);
  }
  __GalIO_UnlockConnectionMutex(16);
  if (cant_do_it) {
    GalUtil_Warn("Max connections isn't 1, so you shouldn't use GalIO_GetUniqueConnection");
  }
  return comm;
}

typedef struct __galio_conn_applyer {
  void *caller_data;
  void (*op)(GalIO_CommStruct *, void *);
} __galio_conn_applyer;

static int __galio_do_connection(void *data, void *caller_data)
{
  __galio_conn_applyer *a = (__galio_conn_applyer *) caller_data;

  __GalIO_UnlockConnectionMutex(18);
  (*a->op)((GalIO_CommStruct *) data, a->caller_data);
  __GalIO_LockConnectionMutex(19);
  /* Always continue. */
  return 1;
}  

/* SAM 11/28/00: NOTE CAREFULLY that GalIO_OperateOnConnections()
   could deadlock in the threaded case if the comm_queue had
   its own mutex, if it detected that a connection died
   and tried to remove it from the queue. This is because
   _GalIO_QueueApply doesn't (well, frankly, can't afford to)
   unlock its mutex when it calls its member operation. This
   function is used by GalIO_ServerPoll(), so don't
   add a mutex to the comm_queue. */

void GalIO_OperateOnConnections(GalIO_ServerStruct *scomm,
				void *arg,
				void (*op)(GalIO_CommStruct *, void *))
{
  __galio_conn_applyer local_caller_data;
  
  __GalIO_LockConnectionMutex(17);
  local_caller_data.caller_data = arg;
  local_caller_data.op = op;
  _GalIO_QueueApply(scomm->comm_queue, __galio_do_connection,
		    &local_caller_data);
  __GalIO_UnlockConnectionMutex(20);
}

/* USE THIS FUNCTION WITH THE UTMOST CARE! */

GalIO_ServerStruct *GalIO_CommServer(GalIO_CommStruct *gcomm)
{
  return gcomm->server;
}

void GalIO_SetServerName(GalIO_ServerStruct *scomm, char *name)
{
  scomm->server_name = name;
}

unsigned short GalIO_GetServerDefaultPort(GalIO_ServerStruct *scomm)
{
  return scomm->default_port;
}

void GalIO_SetServerDefaultPort(GalIO_ServerStruct *scomm, unsigned short port)
{
  scomm->default_port = port;
}

int _GalIO_ServerIsInitializationPrototype(GalIO_ServerStruct *scomm)
{
  return (scomm->status == INITIALIZATION_RETRIEVAL);
}

/* This really ought to be in ServerStub, but I'd need
   to expose the GalIO_ServerStruct, etc. I'm not going there. */

GalIO_ServerStruct *_GalIO_CreateInitializationPrototype()
{
  GalIO_ServerStruct *s = (GalIO_ServerStruct *) calloc(1, sizeof(GalIO_ServerStruct));
  s->status = INITIALIZATION_RETRIEVAL;
  return s;
}

void GalIO_AddServerDispatchFunctionEntry(GalIO_ServerStruct *server,
					  const char *name, 
					  Gal_FrameDataFnPtr fn_with_data,
					  Gal_DispatchFnSignatureKeyEntry *in_key_array,
					  int allow_other_in_keys, 
					  int reply_provided,
					  Gal_DispatchFnSignatureKeyEntry *out_key_array,
					  int allow_other_out_keys)
{
  if (server && !_GalIO_ServerIsInitializationPrototype(server)) {
    server->gcomm_prototype->fn_pkg = Gal_AddDispatchFunctionEntry(server->gcomm_prototype->fn_pkg, name, fn_with_data, in_key_array, allow_other_in_keys, reply_provided, out_key_array, allow_other_out_keys);
  }
}

void GalIO_SetServerClientData(GalIO_ServerStruct *server,
			       const char *name, void *client_data)
{
  Gal_DispatchFnPkgSetClientData(server->gcomm_prototype->fn_pkg,
				 name, client_data);
}

void
_GalIO_SetServerDispatchFnAccess(GalIO_ServerStruct *server,
				 Gal_DispatchFunctionSelector s,
				 Gal_DispatchFnSignatureLister l,
				 Gal_DispatchFunctionInvoker i,
				 void *invocation_client_data)
{
  server->gcomm_prototype->fn_pkg = _Gal_DispatchFnPkgSetAccess(server->gcomm_prototype->fn_pkg, s, l, i, invocation_client_data);
}

void *GalIO_GetServerClientData(GalIO_ServerStruct *server, const char *name)
{
  return Gal_DispatchFnPkgGetClientData(server->gcomm_prototype->fn_pkg, name);
}

void GalIO_AddCommDispatchFunctionEntry(GalIO_CommStruct *gcomm,
					const char *name,
					Gal_FrameDataFnPtr fn_with_data,
					Gal_DispatchFnSignatureKeyEntry *in_key_array,
					int allow_other_in_keys, 
					int reply_provided,
					Gal_DispatchFnSignatureKeyEntry *out_key_array,
					int allow_other_out_keys)
{
  gcomm->fn_pkg = Gal_AddDispatchFunctionEntry(gcomm->fn_pkg, name, fn_with_data,
					       in_key_array, allow_other_in_keys,
					       reply_provided, out_key_array, allow_other_out_keys);
}

void GalIO_SetCommClientData(GalIO_CommStruct *gcomm,
			     const char *name, void *client_data)
{
  Gal_DispatchFnPkgSetClientData(gcomm->fn_pkg, name, client_data);
}

void _GalIO_SetCommDispatchFnAccess(GalIO_CommStruct *gcomm,
				    Gal_DispatchFunctionSelector s,
				    Gal_DispatchFnSignatureLister l,
				    Gal_DispatchFunctionInvoker i,
				    void *invocation_client_data)
{
  gcomm->fn_pkg = _Gal_DispatchFnPkgSetAccess(gcomm->fn_pkg, s, l, i,
					      invocation_client_data);
}

void *GalIO_GetCommClientData(GalIO_CommStruct *gcomm, const char *name)
{
  return Gal_DispatchFnPkgGetClientData(gcomm->fn_pkg, name);
}

Gal_DispatchFnPkg *GalIO_GetCommDispatchFnPkg(GalIO_CommStruct *gcomm)
{
  return gcomm->fn_pkg;
}

void GalIO_SetCommDispatchFnPkg(GalIO_CommStruct *gcomm, Gal_DispatchFnPkg *pkg)
{
  gcomm->fn_pkg = pkg;
}

/* SAM 7/2/02: This function is called in the scope of the generated
   _GalSS_InitializeDefaults, so I need to watch what happens
   with a call from a "fake" scomm. */

Gal_DispatchFnPkg *GalIO_GetServerDispatchFnPkg(GalIO_ServerStruct *scomm)
{
  if (scomm && !_GalIO_ServerIsInitializationPrototype(scomm)) {
    return scomm->gcomm_prototype->fn_pkg;
  } else {
    return (Gal_DispatchFnPkg *) NULL;
  }
}

void GalIO_SetServerDispatchFnPkg(GalIO_ServerStruct *scomm, Gal_DispatchFnPkg *pkg)
{
  if (scomm && !_GalIO_ServerIsInitializationPrototype(scomm)) {
    scomm->gcomm_prototype->fn_pkg = pkg;
  }
}

void GalIO_EnableDispatchFnValidation(GalIO_ServerStruct *scomm)
{
  if (scomm && !_GalIO_ServerIsInitializationPrototype(scomm)) {
    scomm->gcomm_prototype->validate = 1;
  }
}

int GalIO_CommValidating(GalIO_CommStruct *gcomm)
{
  return gcomm->validate;
}

/* socket functions */

/* This is cancellation-unsafe, so it would need to have
   a push/pop wrapped around it if it's mutexed. */

static void __GalIO_ICloseCommSocket(GalIO_CommStruct *gcomm)
{
  if (gcomm)
  {
    if (gcomm->sock != GAL_INVALID_SOCKET)
    {
      _GalIO_QueueApply(gcomm->shutdown_callbacks,
			__galio_apply_connection_callback,
			(void *) NULL);
      GAL_SOCKET_CLOSE(gcomm->sock);
    }
    gcomm->sock = GAL_INVALID_SOCKET;
    gcomm->verified = UNVERIFIED;
    gcomm->error = GALAXY_NO_ERROR;
    _GalIO_ClearSockObjectQueue(gcomm->queue);
  }
}

/* Thread note: GalIO_CloseCommSocket() must be called with
   the connection_mutex UNLOCKED. */

void GalIO_CloseCommSocket(GalIO_CommStruct *gcomm)
{
  __GalIO_ICloseCommSocket(gcomm);
}

/* This function should always be called with the mutex locked. */

static void __GalIO_ICloseServerListenSocket(GalIO_ServerStruct *scomm)
{
  if (scomm)
  {
    if (scomm->listen_sock != GAL_INVALID_SOCKET) {
      /* First, apply all the shutdown callbacks. */
      _GalIO_QueueApply(scomm->listener_shutdown_callbacks,
			__galio_apply_server_callback,
			(void *) NULL);
      shutdown(scomm->listen_sock, 2);
      GAL_SOCKET_CLOSE(scomm->listen_sock);
    }
    scomm->listen_sock = GAL_INVALID_SOCKET;
  }
}

void GalIO_CloseServerListenSocket(GalIO_ServerStruct *scomm)
{
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(unlock_conn_mutex, (void *) NULL);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(unlock_conn_mutex, (void *) NULL);
#endif
  __GalIO_LockConnectionMutex(21);

  __GalIO_ICloseServerListenSocket(scomm);

  __GalIO_UnlockConnectionMutex(22);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
}

static int reset_listener(GalIO_ServerStruct *scomm, char *calling_fn)
{
  GAL_SOCKET sock;
  unsigned short port;

  if ((calling_fn == NULL) || !calling_fn[0])
    calling_fn = "reset_listener";

  if (scomm)
  {
    GalIO_CloseServerListenSocket(scomm);

    if (scomm->default_port > 0)
    {
      port = scomm->default_port;
      while(((sock = GalUtil_SockCreateServer(port++,5)) == GAL_INVALID_SOCKET) && (port < 32768))
 	if (scomm->require_port)
	  break;

      if (sock != GAL_INVALID_SOCKET)
      {
	scomm->listen_port = port - 1;
	scomm->listen_sock = sock;
	scomm->error = GALAXY_NO_ERROR;
	GalUtil_Print(GAL_CONNECTION_VERBOSITY_LEVEL, "Opened listener on port %d\n", scomm->listen_port);
	/* Do all the listener startup callbacks. */
	/* First, apply all the shutdown callbacks. */
	_GalIO_QueueApply(scomm->listener_startup_callbacks,
			  __galio_apply_server_callback,
			  (void *) NULL);
	return(0);
      }
      else
      {
	GalUtil_WarnLevelWithLocation(GAL_CONNECTION_VERBOSITY_LEVEL, calling_fn, "Failed to open listener on port %d (pid %d)", scomm->default_port, _gal_getpid());
	scomm->error = LISTENER_ERROR;
      }
    }
    else
    {
      GalUtil_WarnLevelWithLocation(GAL_CONNECTION_VERBOSITY_LEVEL, calling_fn, "Cannot listen on port %d", scomm->default_port);
      scomm->error = CONFIGURATION_ERROR;
    }
  }
  return(-1);
}

int GalIO_ResetCommSockets(GalIO_ServerStruct *scomm)
{
  int status;

  if (scomm == NULL)
    return(-1);

  switch (scomm->error)
  {
  case LISTENER_ERROR:
    status = reset_listener(scomm, __FUNCTION__);
    return(status);
  case CONNECTION_ERROR:
    return(0);
  case CONFIGURATION_ERROR:
    return(-1);
  default:
    return(0);
  }
}

void GalIO_CommFlushOutQueue(GalIO_CommStruct *gcomm)
{
  if(gcomm)
    _GalIO_FlushSockObjectOutQueue(gcomm->queue);

}

/* msg must have a placeholder for host. */

void _GalIO_PrintMessageTraffic(GalIO_CommStruct *gcomm, char *msg,
				char *caller, Gal_Frame frame)
{
  /* How the message is printed out depends on what the
     verbosity level is. */
  if (GAL_VERBOSE >= GAL_TRAFFIC_SUMMARY_VERBOSITY_LEVEL) {
    GalUtil_PInfo1WithLocation(caller, msg, gcomm->host);
    if (frame)
      Gal_OutlineFrame(frame, GAL_PINFO1_LEVEL);
  } else if (GAL_VERBOSE >= GAL_WARNING_LEVEL) {
    GalUtil_Print(GAL_WARNING_LEVEL, msg, gcomm->host);
  }
}

/* returns 1 if frame was queued, 0 if frame was sent, -1 on error */

int GalIO_CommWriteFrame(GalIO_CommStruct *gcomm, Gal_Frame frame,
			 int do_block)
{
  _GalIO_PrintMessageTraffic(gcomm, "Sending new message to %s\n",
			     __FUNCTION__, frame);
  return GalIO_CommWriteMessage(gcomm, frame, GAL_MESSAGE_MSG_TYPE, do_block);
}

int GalIO_QueueProcessOut(GalIO_CommStruct *gcomm,  int do_block)
{
  int status = _GalIO_SockObjectQueueProcessOut(gcomm->queue);
  if (do_block) {
    while (status == 1) {
      GalUtil_MilliSleep(1);
      status = _GalIO_SockObjectQueueProcessOut(gcomm->queue);
    }
  }
  return status;      
}

static int __GalIO_ICommWriteMessage(GalIO_CommStruct *gcomm, Nframe frame, GalIO_MsgType msg_type, int do_block)
{
  int status;
  
  if (gcomm && gcomm->status == IN_USE)
  {
    status = _galio_write_msg_to_sockqueue(gcomm->queue, frame, msg_type);
    if (do_block) {
      while (status == 1) {
       GalUtil_MilliSleep(1);
       status = _GalIO_SockObjectQueueProcessOut(gcomm->queue);
      }
    }

    return(status);
  } else {
    GalUtil_WarnWithLocation(__FUNCTION__, "Can't write message because socket is not connected (pid %d)", _gal_getpid());
  }
  return(-1);
}

/* returns true if there's more stuff in the queue to be read.
   Note that this means stuff that's already been read from I/O but waiting in
   our internal queue to be processed. */
int GalIO_CommReadReady(GalIO_CommStruct *gcomm)
{
  return gcomm && _GalIO_SockObjectQueueInNonEmpty(gcomm->queue);
}

/* returns true if there's more stuff in the queue to be written.
   Note that this means stuff that's already been written to I/O but waiting in
   our internal queue to be processed. */
int GalIO_CommWriteReady(GalIO_CommStruct *gcomm)
{
  return gcomm && _GalIO_SockObjectQueueOutNonEmpty(gcomm->queue);
}

int GalIO_CommReadFrame(GalIO_CommStruct *gcomm, Nframe *frame, int do_block)
{
  return GalIO_CommReadMessage(gcomm, frame, (GalIO_MsgType *) NULL, do_block);
}

int GalIO_CommReadMessage(GalIO_CommStruct *gcomm, Nframe *frame, GalIO_MsgType *msg_type_ptr, int do_block)
{
  int status;
  GalIO_MsgType p = 0;

  /* Always make sure we have a pointer, but use the one
     passed in if possible. */
  
  *frame = NULL;

  if (gcomm == NULL)
    return(-1);

  /* Read a message, without restriction. */
  status = _galio_read_msg_from_sockqueue(gcomm->queue, frame, &p,
					  (GalIO_MsgQueueTestFn) NULL,
					  (void *) NULL,
					  do_block);

  if (msg_type_ptr) *msg_type_ptr = p;

  return(status);
}

/*
 *  GalIO_CommDispatchFrame sends a frame and waits for a reply.
 *  The reply frame is returned.
 *  For module-to-module subdialogues use the key GAL_ROUND_TRIP_FRAME_KEY.
 */

static Gal_Frame __GalIO_IDispatchViaHub(GalIO_CommStruct *gcomm, Gal_Frame frame, GalIO_MsgType *msg_type_ptr)
{
  if (gcomm) {
    return(GalIO_CommDispatchFrame(gcomm, frame, msg_type_ptr,
				   GAL_ROUND_TRIP_FRAME_KEY));
  } else {
    if (msg_type_ptr)
      *msg_type_ptr = GAL_ERROR_MSG_TYPE;
    return(NULL);
  }
}

/* I want to be able to modify the dispatchers in rare cases,
   such as the local server. */

Gal_Frame GalIO_DispatchViaHub(GalIO_CommStruct *gcomm, Gal_Frame frame, GalIO_MsgType *t)
{
  return (*gcomm->dispatcher)(gcomm, frame, t);
}

int GalIO_CommWriteMessage(GalIO_CommStruct *gcomm, Gal_Frame frame, GalIO_MsgType t, int do_block)
{
  return (*gcomm->writer)(gcomm, frame, t, do_block);
}

/* We need a mutex_protected source of server indices.
   We're going to start at 1 so we can use Gal_GetInt()
   to check matching, which returns 0 when the value
   is 0 or when there's no object at all. */

static int __GalIO_ServerTidx = 1;

static int __GalIO_GetServerTidx()
{
  int tidx;
  
  GalUtil_LockLocalMutex(&server_tidx_mutex);
  tidx = __GalIO_ServerTidx++;
  GalUtil_UnlockLocalMutex(&server_tidx_mutex);
  return tidx;
}

/* This is also used in the continuation stuff in ServerStub/frame_util.c. */

static Gal_Frame __GalIO_EnsureAdminInfo(Gal_Frame msg)
{
  Gal_Frame admin_info = Gal_GetFrame(msg, GAL_HUB_OPAQUE_DATA_FRAME_KEY);

  if (!admin_info) {
    admin_info = Gal_MakeFrame("admin_info", GAL_CLAUSE);
    Gal_SetProp(msg, GAL_HUB_OPAQUE_DATA_FRAME_KEY,
		Gal_FrameObject(admin_info));
  }
  return admin_info;
}

int _GalIO_PopulateDispatchMessage(Gal_Frame dispatch,
				   const char *reply_key)
{
  int server_tidx;
  Gal_Frame admin_info = __GalIO_EnsureAdminInfo(dispatch);
  
  Gal_SetProp(admin_info, reply_key, Gal_IntObject(1));
  /* Add a unique index for matching on return, if
     we have a reply_key. Otherwise, we just leave at as -1,
     which will mean don't check it. The idea is that this
     is used sometimes in the handshake code and it
     doesn't care about matching the replies. */
  server_tidx = __GalIO_GetServerTidx();
  Gal_SetProp(admin_info, GAL_SERVER_TOKEN_INDEX_FRAME_KEY,
	      Gal_IntObject(server_tidx));
  return server_tidx;
}

Gal_Frame GalIO_CommDispatchFrame(GalIO_CommStruct *gcomm,
				  Gal_Frame dispatch,
				  GalIO_MsgType *msg_type_ptr,
				  const char *reply_key)
{ 
  int res;
  Gal_Frame reply = NULL;
  GalIO_MsgType local_msg_type;
  int server_tidx = -1;

  if (dispatch)
  {
    if (reply_key) {
      server_tidx = _GalIO_PopulateDispatchMessage(dispatch, reply_key);
    }
    res = GalIO_CommWriteFrame(gcomm, dispatch, 1);
    if (res == -1)
    {
      if (msg_type_ptr) *msg_type_ptr = GAL_ERROR_MSG_TYPE;
      GalUtil_WarnWithLocation(__FUNCTION__, "Failed to send new message");
      reply = Gal_MakeFrame("system_error", GAL_CLAUSE);
      Gal_SetProp(reply, GAL_ERROR_NUMBER_FRAME_KEY, Gal_IntObject(GAL_TRANSMISSION_ERROR));
      return(reply);
    }

    /* Don't use GalIO_CommReadMessage here; make sure you're
       reading a REPLY. */
    
    res = _galio_read_reply_msg_from_sockqueue(gcomm->queue, &reply,
					       &local_msg_type,
					       server_tidx, 1);
    if (msg_type_ptr)
      *msg_type_ptr = local_msg_type;
    if (res >= 0) {
      /* We should be very careful to filter results which
	 aren't kosher. */
      switch (local_msg_type) {
      case GAL_REPLY_MSG_TYPE:
      case GAL_ERROR_MSG_TYPE:
	_GalIO_PrintMessageTraffic(gcomm,
				   "Received reply from %s\n",
				   __FUNCTION__, reply);
	return(reply);
      case GAL_DESTROY_MSG_TYPE:
	GalUtil_WarnWithLocation(__FUNCTION__, "Got incomprehensible destroy request as message reply; ignoring\n");
	return NULL;
      case GAL_POSTPONE_MSG_TYPE:
	GalUtil_WarnWithLocation(__FUNCTION__, "Got incomprehensible postpone request as message reply; ignoring\n");
	return NULL;
      default:
	GalUtil_WarnWithLocation(__FUNCTION__, "Got incomprehensible message as message reply; ignoring\n");
	return NULL;
      }
    }
    else
    {
      if (msg_type_ptr) *msg_type_ptr = GAL_ERROR_MSG_TYPE;
      GalUtil_WarnWithLocation(__FUNCTION__, "Failed to read reply frame");
      reply = Gal_MakeFrame("system_error", GAL_CLAUSE);
      Gal_SetProp(reply, GAL_ERROR_NUMBER_FRAME_KEY, Gal_IntObject(GAL_RECEPTION_ERROR));
      return(reply);
    }
  }
  else
  {
    if (msg_type_ptr) *msg_type_ptr = GAL_ERROR_MSG_TYPE;
    GalUtil_WarnWithLocation(__FUNCTION__, "Did not dispatch NULL frame");
    reply = Gal_MakeFrame("system_error", GAL_CLAUSE);
    Gal_SetProp(reply, GAL_ERROR_NUMBER_FRAME_KEY, Gal_IntObject(GAL_NO_FRAME_ERROR));
    return(reply);
  }
}

void _GalIO_ServerRequirePort(GalIO_ServerStruct *scomm, int require_port)
{
  scomm->require_port = require_port;
}

GalIO_ServerStruct *
GalIO_ListenerCreate(unsigned short port, int require_port, GalIO_FrameHandler foo_ptr, void *server_data, int poll_ms, int max_connections)
{
  GalIO_ServerStruct *scomm = NewServerStruct(max_connections);
  
  if (scomm)
  {
    scomm->default_port = port;
    scomm->require_port = require_port;
    scomm->gcomm_prototype->handler = foo_ptr;
    scomm->server_data = server_data;
    if (poll_ms > 0)
      scomm->poll_ms = poll_ms;
    else if (poll_ms == 0)
      scomm->poll_ms = GAL_DEFAULT_POLL_MS;
    else
      scomm->poll_ms = -1;
  }
  return scomm;
}

int GalIO_ServerUsesTimedTasks(GalIO_ServerStruct *server)
{
  if (server->poll_ms > 0)
    return 1;
  else return 0;
}

/* Server callbacks. There are several relevant events:
   
   (a) listener startup
   (b) listener shutdown
   (c) client poll startup
   (d) connection creation
   (e) destruction

   There will be a callback for each of them, and the callbacks
   will be registered with loop data, to help control how
   the callbacks are executed. The destroy callbacks are
   relatively important, because while the callback handlers
   return values indicating that the server was destroyed,
   you might want to have something done before the server
   was destroyed rather than after.

   Under what circumstances might the user want to add
   any events here? Do we want to give the user a "hook" to
   remove an event? Perhaps we want to set up the events so
   that there are flags, and have a single event registration
   function for each object type?
*/

/* This is the apply function for server callbacks. */

static int __galio_apply_server_callback(void *data, void *caller_data)
{
  GalIO_Callback *cb = (GalIO_Callback *) data;
  GalIO_ServerCallbackFn fn = (GalIO_ServerCallbackFn) cb->fn;
  GalIO_ServerStruct *scomm = (GalIO_ServerStruct *) cb->callback_host;
  
  (*fn)(scomm, cb->callback_data);
  /* Continue. */
  return 1;
}

GalIO_Callback *
GalIO_AddServerCallback(GalIO_ServerStruct *scomm,
			int callback_event,
			GalIO_ServerCallbackFn fn,
			void *callback_data)
{
  GalIO_Callback *s = (GalIO_Callback *) malloc(sizeof(GalIO_Callback));
  s->fn = (void *) fn;
  s->callback_data = callback_data;
  s->callback_type = callback_event;
  s->callback_host = (void *) scomm;
    
  switch (callback_event) {
  case GAL_SERVER_LISTENER_STARTUP_EVENT:
    _GalIO_QueueEnqueue(scomm->listener_startup_callbacks, (void *) s);
    break;
  case GAL_SERVER_LISTENER_SHUTDOWN_EVENT:
    _GalIO_QueueEnqueue(scomm->listener_shutdown_callbacks, (void *) s);
    break;
  case GAL_SERVER_CLIENT_POLL_STARTUP_EVENT:
    _GalIO_QueueEnqueue(scomm->client_poll_startup_callbacks, (void *) s);
    break;
  case GAL_SERVER_DESTRUCTION_EVENT:
    _GalIO_QueueEnqueue(scomm->destroy_callbacks, (void *) s);
    break;
  default:
    free(s);
    GalUtil_WarnWithLocation(__FUNCTION__, "Can't record server callback for unknown event type\n");
    return (GalIO_Callback *) NULL;
  }
  return s;
}

GalIO_Callback *
GalIO_AddServerConnectCallback(GalIO_ServerStruct *scomm,
			       GalIO_ServerConnectCallbackFn connect_callback,
			       void *callback_data)
{
  GalIO_Callback *s = (GalIO_Callback *) malloc(sizeof(GalIO_Callback));
  s->fn = (void *) connect_callback;
  s->callback_type = GAL_SERVER_CONNECTION_CREATION_EVENT;
  s->callback_data = callback_data;
  s->callback_host = (void *) scomm;

  _GalIO_QueueEnqueue(scomm->connection_setup_callbacks, (void *) s);
  return s;
}

void GalIO_RemoveServerCallback(GalIO_ServerStruct *scomm,
				GalIO_Callback *cb)
{  
  switch (cb->callback_type) {
  case GAL_SERVER_LISTENER_STARTUP_EVENT:
    _GalIO_QueueDequeue(scomm->listener_startup_callbacks, (void *) cb);
    break;
  case GAL_SERVER_LISTENER_SHUTDOWN_EVENT:
    _GalIO_QueueDequeue(scomm->listener_shutdown_callbacks, (void *) cb);
    break;
  case GAL_SERVER_CLIENT_POLL_STARTUP_EVENT:
    _GalIO_QueueDequeue(scomm->client_poll_startup_callbacks, (void *) cb);
    break;
  case GAL_SERVER_DESTRUCTION_EVENT:
    _GalIO_QueueDequeue(scomm->destroy_callbacks, (void *) cb);
    break;
  case GAL_SERVER_CONNECTION_CREATION_EVENT:
    _GalIO_QueueDequeue(scomm->connection_setup_callbacks, (void *) cb);
    break;
  default:
    GalUtil_WarnWithLocation(__FUNCTION__, "Can't remove connection callback for unknown event type\n");
    break;
  }
  free(cb);
}

GalIO_Callback *
GalIO_AddConnectionCallback(GalIO_CommStruct *gcomm,
			    int callback_event,
			    GalIO_ConnectionCallbackFn connect_callback,
			    void *callback_data)
{
  GalIO_Callback *s = (GalIO_Callback *) malloc(sizeof(GalIO_Callback));
  s->fn = (void *) connect_callback;
  s->callback_data = callback_data;
  s->callback_type = callback_event;
  s->callback_host = (void *) gcomm;
    
  switch (callback_event) {
  case GAL_CONNECTION_SHUTDOWN_EVENT:
    _GalIO_QueueEnqueue(gcomm->shutdown_callbacks, (void *) s);
    break;
  case GAL_CONNECTION_DESTRUCTION_EVENT:
    _GalIO_QueueEnqueue(gcomm->destroy_callbacks, (void *) s);
    break;
  default:
    free(s);
    GalUtil_WarnWithLocation(__FUNCTION__, "Can't record connection callback for unknown event type\n");
    return (GalIO_Callback *) NULL;
  }
  return s;
}

GalIO_Callback *
GalIO_AddConnectionBrokerCallback(GalIO_CommStruct *gcomm,
				  int callback_event,
				  GalIO_ConnectionBrokerCallbackFn connect_callback,
				  void *callback_data)
{
  GalIO_Callback *s = (GalIO_Callback *) malloc(sizeof(GalIO_Callback));
  s->fn = (void *) connect_callback;
  s->callback_data = callback_data;
  s->callback_type = callback_event;
  s->callback_host = (void *) gcomm;
    
  switch (callback_event) {
  case GAL_CONNECTION_BROKER_OUT_STARTUP_EVENT:
  case GAL_CONNECTION_BROKER_OUT_CREATION_EVENT:
    _GalIO_QueueEnqueue(gcomm->broker_out_callbacks, (void *) s);
    break;
  case GAL_CONNECTION_BROKER_IN_STARTUP_EVENT:
  case GAL_CONNECTION_BROKER_IN_CREATION_EVENT:
    _GalIO_QueueEnqueue(gcomm->broker_in_callbacks, (void *) s);
    break;
  default:
    free(s);
    GalUtil_WarnWithLocation(__FUNCTION__, "Can't record connection callback for unknown event type\n");
    return (GalIO_Callback *) NULL;
  }
  return s;
}

GalIO_Callback *
GalIO_AddConnectionDispatchFnCallback(GalIO_CommStruct *gcomm,
				      GalIO_ConnectionDispatchFnCallbackFn dispatch_callback,
				      void *callback_data)
{
  GalIO_Callback *s = (GalIO_Callback *) malloc(sizeof(GalIO_Callback));
  s->fn = (void *) dispatch_callback;
  s->callback_data = callback_data;
  s->callback_type = GAL_CONNECTION_DISPATCH_FN_EVENT;
  s->callback_host = (void *) gcomm;

  _GalIO_QueueEnqueue(gcomm->dispatch_fn_callbacks, (void *) s);
  return s;
}

void GalIO_RemoveConnectionCallback(GalIO_CommStruct *gcomm,
				    GalIO_Callback *cb)
{  
  switch (cb->callback_type) {
  case GAL_CONNECTION_BROKER_OUT_STARTUP_EVENT:
  case GAL_CONNECTION_BROKER_OUT_CREATION_EVENT:
    _GalIO_QueueDequeue(gcomm->broker_out_callbacks, (void *) cb);
    break;
  case GAL_CONNECTION_BROKER_IN_STARTUP_EVENT:
  case GAL_CONNECTION_BROKER_IN_CREATION_EVENT:
    _GalIO_QueueDequeue(gcomm->broker_in_callbacks, (void *) cb);
    break;
  case GAL_CONNECTION_SHUTDOWN_EVENT:
    _GalIO_QueueDequeue(gcomm->shutdown_callbacks, (void *) cb);
    break;
  case GAL_CONNECTION_DESTRUCTION_EVENT:
    _GalIO_QueueDequeue(gcomm->destroy_callbacks, (void *) cb);
    break;
  case GAL_CONNECTION_DISPATCH_FN_EVENT:
    _GalIO_QueueDequeue(gcomm->dispatch_fn_callbacks, (void *) cb);
  default:
    GalUtil_WarnWithLocation(__FUNCTION__, "Can't remove connection callback for unknown event type\n");
    break;
  }
  free(cb);
}

/* Backward compatibility. We set the disconnect callback in
   terms of the connection callback function above. */

typedef void (*__galio_simple_callback)(GalIO_CommStruct *);

static void __galio_disconnectcallback(GalIO_CommStruct *gcomm,
				       void *callback_data)
{
  __galio_simple_callback f = (__galio_simple_callback) callback_data;

  (*f)(gcomm);
}
			    
void GalIO_SetDisconnectCallback(GalIO_CommStruct *gcomm, void (*disconnect_callback)(GalIO_CommStruct *server_data))
{
  GalIO_AddConnectionCallback(gcomm,
			      GAL_CONNECTION_SHUTDOWN_EVENT,
			      __galio_disconnectcallback,
			      (void *) disconnect_callback);
}

void GalIO_SetServerDisconnectCallback(GalIO_ServerStruct *scomm, void (*disconnect_callback)(GalIO_CommStruct *server_data))
{
  GalIO_AddConnectionCallback(scomm->gcomm_prototype,
			      GAL_CONNECTION_SHUTDOWN_EVENT,
			      __galio_disconnectcallback,
			      (void *) disconnect_callback);
}

static GalIO_CommStruct *
__GalIO_ClientInitWithMutex(const char *host, unsigned short port,
			    GalIO_FrameHandler fnptr,
			    int poll_ms, int mutex_locked)
{
  GalIO_CommStruct *gcomm;

  if (!host)
  {
    GalUtil_WarnWithLocation(__FUNCTION__, "Can't set up client connection because hostname is NULL");
    return NULL;
  }

  gcomm = NewCommStruct(mutex_locked, 1);
  if (gcomm)
  {
    gcomm->host = _gal_strdup(host);
    gcomm->contact_port = port;
    gcomm->handler = fnptr;
    if (poll_ms > 0)
      gcomm->poll_ms = poll_ms;
    else if (poll_ms == 0)
      gcomm->poll_ms = GAL_DEFAULT_POLL_MS;
    else
      gcomm->poll_ms = -1;
  }
  return(gcomm);
}


GalIO_CommStruct *
GalIO_ClientInit(const char *host, unsigned short port, 
		 GalIO_FrameHandler fnptr, int poll_ms)
{
  return __GalIO_ClientInitWithMutex(host, port, 
				     fnptr, poll_ms, 0);
}

int
GalIO_OutHandler(GalIO_CommStruct *gcomm)
{
  int status;

  if (gcomm == NULL)
    return(-1);

  status = _GalIO_SockObjectQueueProcessOut(gcomm->queue);

  /* error, close the socket */
  if (status == -1)
  {
    GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't process outbound messages due to socket error");
    gcomm->error = CONNECTION_ERROR;
    return(-1);
  }

  /* there is still stuff in the queue */
  if (status == 1)
    return(0);

  /* the queue is empty */
  if (gcomm->done)
    return(1);
  else
    return(0);
}

int
GalIO_InHandler(GalIO_CommStruct *gcomm)
{
  int pre_status, status = 0;
  int read_blocking = gcomm->read_blocking;
  
  if (gcomm == NULL)
    return(-1);

  do {
    if (gcomm->done) {
      /* all the data has been received, close the socket */
      return(1);
    }
    
    /* Check for possible exit conditions. */
    Gal_MaybeEndTask(1, 1);

    /* Make sure that the read blocking is set to the
       original at this point, and later we'll need
       to set it to 0. */

    gcomm->read_blocking = read_blocking;
    gcomm->queue->queue->read_blocking = read_blocking;

    /* Process the sockqueue, in order to free the socket
       resources quickly. */

    /* SAM 10/19/01: Pay close attention. There's a problematic
       interaction between broker queues and this loop. If
       processing the object queue generates an error, you still
       want to call the handler if the queue is not empty.
       However, if the handler doesn't do anything (say,
       because the broker isn't active) and the connection
       dies before the broker is done, we must make sure that the
       original error is reported, since otherwise the broker
       will fall into a tight loop (it will always be readable,
       but never do anything, and never be done). I've folded
       the function _galio_process_sockqueue into this loop
       directly, to give me access to the original error.
       The function wasn't called anywhere else, so there's
       no problem.

       So just to be clear, we report the initial error result
       just in case the queue wasn't empty but the handler
       chose to do nothing (since we assume that it will
       always choose to do nothing; we can't count on the
       program to "eventually", e.g., activate the broker. */

    pre_status = _GalIO_SockObjectQueueProcessIn(gcomm->queue);
    
    if ((pre_status == -1) &&
	!_GalIO_SockObjectQueueInNonEmpty(gcomm->queue)) {
      /* If the result is an error and there's nothing in
	 the queue, barf. Otherwise, proceed. */
      gcomm->error = CONNECTION_ERROR;
      return(-1);
    }
    
    /* At this point, whether or not read blocking was
       enabled, we want to turn it OFF, since if we have
       read blocking on and we call the read handler
       below, it will block forever, because it's already
       read in the data. */

    gcomm->read_blocking = 0;
    gcomm->queue->queue->read_blocking = 0;

    /* -1 is error, 0 is nothing to do, 1 is something to do. */
    status = (*(gcomm->read_handler))(gcomm);

    if ((status == -1)
	|| ((pre_status == -1) && (status == 0))
	) {
      /* If the handler returns an error, or the
	 pre-read returned an error and the queue was non-empty
	 but the handler does nothing, barf. */
      gcomm->error = CONNECTION_ERROR;
      return(-1);
    } else if ((status == 0) && gcomm->queue->done) {
      /* If the queue is empty and also marked as done, then
	 set the connection to be done. */
      gcomm->done = 1;
    }
  } while (status > 0);

  return(0);
}

/*
 *  server and hub connection handlers
 */

int GalIO_ServerIsClient(GalIO_ServerStruct *scomm)
{
  return (GalIO_ServerListenStatus(scomm) & GAL_HUB_CLIENT);
}

int GalIO_ServerListensForConnections(GalIO_ServerStruct *s)
{
  return (s->server_listen_status & GAL_CONNECTION_LISTENER);
}

int GalIO_ServerListensForBrokers(GalIO_ServerStruct *s)
{
  return (s->server_listen_status & GAL_BROKER_LISTENER);
}

int GalIO_ServerIsListener(GalIO_ServerStruct *scomm)
{
  return (GalIO_ServerListensForConnections(scomm) ||
	  GalIO_ServerListensForBrokers(scomm));
}

/* Poll the server. If the server isn't timed task loop scheduled,
   poll the individual connections as well. */

/* -1 means error in setting up the server. 1 means a connection was found.
   0 means server is working, but nothing there. GalIO_ServerPoll()
   is intended to be used without the timed task loop. It will not
   reset the loops. Ditto with GalIO_ConnectionPoll. */

static void __galio_connection_poll(GalIO_CommStruct *gcomm, void *ignore)
{
  GalIO_ConnectionPoll(gcomm);
}

/* SAM 11/28/00: NOTE CAREFULLY that GalIO_OperateOnConnections()
   could deadlock in the threaded case if the comm_queue had
   its own mutex, if it detected that a connection died
   and tried to remove it from the queue. This is because
   _GalIO_QueueApply doesn't (well, frankly, can't afford to)
   unlock its mutex when it calls its member operation. */

int GalIO_ServerPoll(GalIO_ServerStruct *scomm)
{
  int status = 0;
  GalIO_CommStruct *new_conn = (GalIO_CommStruct *) NULL;

  switch(GalIO_ServerHandler(scomm, &new_conn))
  {
  case -1:
    switch(scomm->error)
    {
    case CONFIGURATION_ERROR:
      return(-1);
    case LISTENER_ERROR:
      reset_listener(scomm, __FUNCTION__);
      break;
    default:
      break;
    }
    status = -1;
    break;

  case 0:
    if (GalIO_ServerIsClient(scomm)) {
      GalIO_ServerCheckHubContacts(scomm);
    }
    /* no connection, continue listening */
    status = 0;
    break;

  case 1:
    status = 1;
    /* Found a new connection. */
    break;
  }
  /* If the timed tasks aren't enabled, the server poll does all
     the connection polls (because the connection inherits
     its polling policy from the server). We'll need to ignore
     the results. */
  GalIO_OperateOnConnections(scomm, (void *) NULL,
			     __galio_connection_poll);
  return(status);
}
  

/* poll an established connection */

/* GalIO_VerificationHandler():
   -1 means error was found, structure was removed. 1 means
   verification was successful. 0 means we haven't verified yet. */

/* SAM 9/20/00: The reinitialize frame no longer carries the
   signatures as a reply; in fact, a non-errorful reply is
   ignored. The signature stuff is now handled in the handshakes. */

/* Once the connection is established and verified, we
   change the read handler for the connection to enable
   the possibility of continuations. */

int GalIO_VerificationHandler(GalIO_CommStruct *gcomm)
{
  int has_reinitialize;
  Gal_Frame error_frame;
  char *op_name;
  
  if (gcomm->verified == VERIFIED) {
    return 1;
  } else {
  
    /* I try the in handler first. If the connection has
       not yet been verified, then the result may involve
       closing the connection for good. I can't use the normal
       in handler because I need to make sure that reinitialize
       is called first. */

    Gal_Frame frame;
    int status;
    Gal_DispatchFnInvocation invocation;

    /* Try reading the reinitialization message. */
    status = _galio_read_incoming_msg_from_sockqueue(gcomm->queue, &frame, 0);

    /* error reading from the queue, close the socket */
    switch (status) {
    case -1:
      GalUtil_WarnWithLocation(__FUNCTION__, "Encountered error reading from connection queue");
      GalIO_SetCommDone(gcomm);
      GalIO_DestroyCommStruct(gcomm);
      return(-1);
    case 0:
      /* nothing read from the queue */
      return 0;
    default:
      /* There will be a frame here. */
      
      /* If the server has a reinitialize message defined, but
	 this first message isn't reinitialize, then we should
	 send an error. If the server doesn't have a
	 reinitialize message, then we should send back
	 a pacifier and verify. Otherwise, just do the
	 dispatch function. */
      invocation.pkg = GalIO_GetCommDispatchFnPkg(gcomm);
      invocation.bare_op_name = "reinitialize";
      if (Gal_FindDispatchFunctionEntry(&invocation)) {
	has_reinitialize = 1;
      } else {
	has_reinitialize = 0;
      }
      
      op_name = Gal_SplitOperationName(Gal_FrameName(frame), (char **) NULL);

      if (has_reinitialize && strcmp(op_name, "reinitialize")) {
	/* If there's a reinitialize function, but that's not what
	   we got, barf. */
	error_frame = GalIO_CreateErrorFrame(GAL_CONN_REJECTION_ERROR,
					     "first message must be reinitialize");
	GalIO_CommWriteMessage(gcomm, error_frame, GAL_ERROR_MSG_TYPE, 1);
	GalIO_SetCommDone(gcomm);
	GalIO_DestroyCommStruct(gcomm);
	Gal_FreeFrame(error_frame);
	Gal_FreeFrame(frame);
	free(op_name);
	return(-1);
      } else if ((!has_reinitialize) && (!strcmp(op_name, "reinitialize"))) {
	/* There isn't a reinitialize function, but that's what we got.
	   Send a pacifier. */
	GalIO_CommWriteMessage(gcomm, frame, GAL_REPLY_MSG_TYPE, 1);
	gcomm->verified = VERIFIED;
	_GalSS_CommEnableContinuations(gcomm);
	Gal_FreeFrame(frame);
	free(op_name);
	return 1;
      } else {
	/* Normal case. Just process the dispatch function. This
	   will either call reinitialize when there is a reinitialize,
	   or it will call another dispatch function when that's what
	   there is. */
	free(op_name);
	gcomm->verified = VERIFIED;
	_GalSS_CommEnableContinuations(gcomm);
	if (gcomm->handler) {
	  /* It used to be that a reply of 0 meant that there
	     was no reply from the reinitialize function,
	     but now the reinitialize function is guaranteed
	     to have sent the return (perhaps by having it
	     forced). So we can change <= to < here. */
	  if ((*(gcomm->handler))(gcomm, frame) < 0) {
	    GalIO_SetCommDone(gcomm);
	    GalIO_DestroyCommStruct(gcomm);
	    return(-1);
	  }
	}
	return 1;
      }
    }
  }
}

/* GalIO_ConnectionPoll():
   -1 means an error was encountered and the connection has been destroyed.
   0 means we're in the midst of things.
   1 means we're done and the connection has been destroyed. */

int
GalIO_ConnectionPoll(GalIO_CommStruct *gcomm)
{
  int res = 0;
  
  switch (GalIO_VerificationHandler(gcomm)) {
  case -1:
    return -1;
  case 0:
    break;
  case 1:
    /* If we've gotten the verification, then keep going to the normal poll. */
    switch(GalIO_InHandler(gcomm)) {
    case -1:
      /* error, stop polling */
      /* done, stop polling */
      res = -1;
      break;
    case 1:
      /* done, stop polling */
      res = 1;
      break;
    }
    /* If reading failed, don't try to write, for God's sake... */
    if (res != 0)
      break;
    switch(GalIO_OutHandler(gcomm)) {
    case -1:
      /* error, stop polling */
      res = -1;
      break;
    case 1:
      /* done, stop polling */
      res = 1;
      break;
    }
  }
  if (res != 0) {
    if (gcomm->task_removal_callback) {
      GalIO_RemoveConnectionCallback(gcomm, gcomm->task_removal_callback);
    }
    GalIO_SetCommDone(gcomm);
    GalIO_DestroyCommStruct(gcomm);
  }
  return(res);
}

/* GalIO_ConnectionCallbackHandler():
   -1 means an error was encountered and the connection has been destroyed.
   0 means we're in the midst of things.
   1 means we're done and the connection has been destroyed. */

int GalIO_ConnectionCallbackHandler(GalIO_CommStruct *gcomm,
				    int read_blocking)
{
  gcomm->read_blocking = read_blocking;
  gcomm->queue->queue->read_blocking = read_blocking;

  return GalIO_ConnectionPoll(gcomm);
}

static void galaxy_connection_poll(Gal_TaskPkg *p)
{
  GalIO_CommStruct *gcomm = (GalIO_CommStruct *) Gal_TaskPkgData(p);
  
  switch (GalIO_ConnectionCallbackHandler(gcomm, Gal_TaskPkgBlocking(p))) {
  case 1:
  case -1:
    return;
  case 0:
    /* continue polling */
    if (gcomm->poll_ms > 0) {
      /* SAM 10/27/00: BAD idea to have connections do blocking
	 reads, because in the threaded case,
	 it might conflict with brokers also
	 trying to read, e.g., dispatch replies. Also, unless
	 I have separate in and out mutexes in the socket queue,
	 the brokers will block on WRITE. Ugh. */
      Gal_ReAddTaskWithSocketIO(p, gcomm, gcomm->poll_ms,
				0, &(gcomm->sock),
				(GAL_SOCKET *) NULL, NULL);
    }
    return;
  default:
    return;
  }
}

/*  GalIO_ServerHandler listens for connections on the socket initialized by GalIO_ServerInit.
 *  It returns 1 if connected and verified, 0 if not, -1 if error.
 */

static void *__galio_copy_callback(void *data)
{
  GalIO_Callback *cb = (GalIO_Callback *) data;
  GalIO_Callback *s = (GalIO_Callback *) malloc(sizeof(GalIO_Callback));
  
  s->fn = cb->fn;
  s->callback_data = cb->callback_data;
  s->callback_type = cb->callback_type;
  return (void *) s;
}  

static int __galio_apply_server_connection_callback(void *data, void *caller_data)
{
  GalIO_Callback *cb = (GalIO_Callback *) data;
  GalIO_ServerConnectCallbackFn fn = (GalIO_ServerConnectCallbackFn) cb->fn;
  GalIO_ServerStruct *scomm = (GalIO_ServerStruct *) cb->callback_host;
  GalIO_CommStruct *gcomm = (GalIO_CommStruct *) caller_data;
  
  (*fn)(scomm, gcomm, cb->callback_data);
  /* Continue. */
  return 1;
}

/* __GalIO_InsertConnectionIntoServer MUST BE CALLED WITH THE MUTEX LOCKED! */

static void __GalIO_InsertConnectionIntoServer(GalIO_ServerStruct *scomm,
					       GalIO_CommStruct *new_conn)
{
  new_conn->server = scomm;
  _GalIO_QueueEnqueue(scomm->comm_queue, (void *) new_conn);
  scomm->num_connections++;
  /* New connection must inherit stuff from parent: polling, handler. */
  new_conn->poll_ms = scomm->poll_ms;
  new_conn->handler = scomm->gcomm_prototype->handler;
  /* Copy the disconnect callbacks. */
  _GalIO_QueueImport(new_conn->destroy_callbacks,
		     scomm->gcomm_prototype->destroy_callbacks,
		     __galio_copy_callback);
  /* Call the connection callbacks. If the server hasn't
     been started yet, these callbacks may not yet exist. */
  _GalIO_QueueApply(scomm->connection_setup_callbacks,
		    __galio_apply_server_connection_callback,
		    (void *) new_conn);
  new_conn->fn_pkg = scomm->gcomm_prototype->fn_pkg;
  new_conn->validate = scomm->gcomm_prototype->validate;
}

/* This function should be static, but I need it in hub_init.c. */

int __GalIO_ServerContactHandler(GalIO_ServerStruct *scomm,
				 GalIO_CommStruct **new_conn_ptr)
{
  int status;
  GAL_SOCKET sock;
  GalIO_CommStruct *new_conn = (GalIO_CommStruct *) NULL;
  char host[MAXHOSTNAMELEN+1];
  
  if (scomm->listen_sock == GAL_INVALID_SOCKET) {
    GalUtil_WarnWithLocation(__FUNCTION__, "Can't listen because server socket is not open! (pid %d)", _gal_getpid());
    scomm->error = LISTENER_ERROR;
    return(-1);
  }

  /* if we are not yet connected */
  /* check for connections */

  /* if not connected, check for connections. Don't let interrupted
     system calls faze you. */

  if (scomm->read_blocking) {
    status = GalUtil_SockServerBlockingCheckForConnection(scomm->listen_sock, host, &sock);
  } else {
    status = GalUtil_SockServerCheckForConnection(scomm->listen_sock, host, &sock);
  }

  switch (status) {
  case 0:
    /* no attempted connections */    
    if (new_conn_ptr)
      *new_conn_ptr = (GalIO_CommStruct *) NULL;
    return(0);
  case -1:
    /* socket error */
    GalUtil_WarnWithLocation(__FUNCTION__, "Encountered error while checking for incoming server connections");
    scomm->error = LISTENER_ERROR;
    if (new_conn_ptr)
      *new_conn_ptr = (GalIO_CommStruct *) NULL;
    return(-1);
  default:
    /* connected to client */
    GalUtil_PrintWithLocation(GAL_CONNECTION_VERBOSITY_LEVEL,
			      __FUNCTION__,
			      "Accepted connection from %s (socket %d)\n",
			      host, sock);
    GalUtil_SockBlockOff(sock);
    /* Build a comm object. */
    new_conn = NewCommStruct(0, 1);
    new_conn->host = _gal_strdup(host);
    SetCommSocket(new_conn, sock);
    new_conn->verified = UNVERIFIED;
    *new_conn_ptr = new_conn;
    return 1;
  }
}

/* SAM 07/11/00: Make sure that when the server is acting as
   a client, it doesn't fail because there's no socket.
   This function ONLY handles the listener case. See GalIO_ServerPoll()
   and GalIO_ServerCheckHubContacts(). */

/* 4/18/02: the mutexed read handler can do a read from the socket,
   so it needs to be protected with a push/pop. */

int
GalIO_ServerHandler(GalIO_ServerStruct *scomm, GalIO_CommStruct **new_conn_ptr)
{
  int status;
  GalIO_CommStruct *new_conn = (GalIO_CommStruct *) NULL;
  int sub_status;
  
  if (scomm == NULL)
    return(-1);

  if (!GalIO_ServerIsListener(scomm)) {
    return(0);
  }

  status = __GalIO_ServerContactHandler(scomm, &new_conn);
  switch (status) {
  case 0:
  case -1:
    /* Both errors. Just return. */
    return status;
  default:
    /* Got a connection. See if there's room. */

    /* We'd better grab the mutex and hold it until we build
     a new connection object. */

 #ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPushCleanup(unlock_conn_mutex, (void *) NULL);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_push(unlock_conn_mutex, (void *) NULL);
#endif   
    __GalIO_LockConnectionMutex(27);

    /* The handshake handler needs to return the connection type,
       so if I get a broker, I don't insert it into the connection. */
    if (GalIO_ServerListenerHandshakeHandler(scomm, new_conn, new_conn_ptr) != -1) {
      sub_status = 1;
    } else {
      sub_status = 0;
    }

    __GalIO_UnlockConnectionMutex(29);
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_pop(0);
#endif

    if (sub_status == 1) {
      return 1;
    } else {
      /* Handshake failed. Destroy the connection. */
      __GalIO_IDestroyCommStruct(new_conn, 0, 0);
      if (new_conn_ptr)
	*new_conn_ptr = (GalIO_CommStruct *) NULL;
      return 0;
    }
  }
}

int _GalIO_ClientContactHandler(GalIO_CommStruct *gcomm, int silent)
{
  GAL_SOCKET sock;

  if (gcomm == NULL) {
    return(-1);
  }

  if (gcomm->sock == GAL_INVALID_SOCKET)
  {
    if (gcomm->contact_port > 0)
    {
      /* if not connected, connect to server */
      sock = GalUtil_SockCreateClient(gcomm->host, gcomm->contact_port);

      /* failed to connect */
      if (sock == GAL_INVALID_SOCKET)
      {
	if (!silent)
	  GalUtil_WarnLevelWithLocation(GAL_CONNECTION_VERBOSITY_LEVEL, __FUNCTION__, "Failed to connect to '%s' on port %d", gcomm->host, gcomm->contact_port);
	gcomm->error = CONNECTION_ERROR;
	return(-1);
      }

      /* connected to client */
      SetCommSocket(gcomm, sock);
      gcomm->verified = UNVERIFIED;
    }
    else
    {
      if (!silent)
	GalUtil_WarnLevelWithLocation(GAL_CONNECTION_VERBOSITY_LEVEL, __FUNCTION__, "Cannot contact host '%s' on invalid port %d", gcomm->host, gcomm->contact_port);
      gcomm->error = CONFIGURATION_ERROR;
      return(-1);
    }
  }
  return 0;
}

/*  __GalIO_HubHandler connects to a server on the host and port
 *  initialized by GalIO_ClientInit.
 *  If welcome is present, send a frame and get one back to confirm
 *  that the connection is okay.
 *  GalIO_HubHandler returns 1 if connected and verified, 0 if not,
 *  -1 if error.
 */

/* SAM 9/19/00: The reply for the signatures will now be obtained
   from GalIO_HubClientHandshakeHandler, rather than
   _GalIO_HubVerificationHandler. The response from reinitialize
   is meaningless. */

/* This function is called repeatedly until the verification
   fails or succeeds. So this function 
   needs to be robust in the face of multiple calls. */

static int __GalIO_HubHandler(GalIO_CommStruct *gcomm, int silent,
			      Gal_Frame welcome_frame, Gal_Frame *reply)
{
  if (_GalIO_ClientContactHandler(gcomm, silent) == -1) {
    if (reply)
      *reply = (Gal_Frame) NULL;
    return -1;
  }
  if (GalIO_HubClientHandshakeHandler(gcomm, reply) == -1) {
    return -1;
  }
  return _GalIO_HubVerificationHandler(gcomm, welcome_frame);
}

/* This function should be static, but it's needed over in hub_init.c. */

void __GalIO_MarkSuccessfulHandshake(GalIO_CommStruct *gcomm)
{
  gcomm->verified = PREVERIFIED;
}

int __GalIO_HandshakeComplete(GalIO_CommStruct *gcomm)
{
  return (gcomm->verified != UNVERIFIED);
}

int _GalIO_HubVerificationHandler(GalIO_CommStruct *gcomm, Gal_Frame init_frame)
{
  Gal_Frame frame;
  GalIO_MsgType t;
  int status;

  switch (gcomm->verified)
  {
  case VERIFIED:
    return(1);
  case UNVERIFIED:
    /* Should never be here. */
    return -1;
  case PREVERIFIED:
    if (init_frame == NULL)
    {
      gcomm->verified = VERIFIED;
      return(1);
    }
    /* connected, send verification frame */
    status = _galio_write_msg_to_sockqueue(gcomm->queue, init_frame,
					   GAL_MESSAGE_MSG_TYPE);

    /* error writing to the queue, close the socket */
    if (status == -1)
    {
      GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't write to connection queue due to socket error");
      gcomm->error = CONNECTION_ERROR;
      return(-1);
    }
    gcomm->verified = SENT_FRAME;

    /* if the frame was queued, break, if the frame was sent, fall through */
    if (status == 1) {
      return(0);
    }

  case SENT_FRAME:
    /* verify the reply frame */
    status = _galio_read_reply_msg_from_sockqueue(gcomm->queue, &frame, &t,
						  -1, 0);

    /* error reading from the queue, close the socket */
    if (status == -1)
    {
      GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't read from connection queue due to socket error while reading initialization reply from %s:%d",
		   gcomm->host, gcomm->contact_port);
      gcomm->error = CONNECTION_ERROR;
      return(-1);
    }

    /* nothing read from the queue */
    if (status == 0) {
      return(0);
    }

    if (frame && (t != GAL_ERROR_MSG_TYPE)) 
    {
      GalUtil_Debug1("%s: server initialized\n", __FUNCTION__);
      gcomm->verified = VERIFIED;
      Gal_FreeFrame(frame);
      return(1);
    }
    else
    {
      GalUtil_WarnWithLocation(__FUNCTION__, "Failed to initialize server\n");
      gcomm->error = CONNECTION_ERROR;
      if (frame) Gal_FreeFrame(frame);
      return(-1);
    }
  default:
    GalUtil_WarnWithLocation(__FUNCTION__, "Encountered unknown verification state %d during initialization", gcomm->verified);
    gcomm->error = CONFIGURATION_ERROR;
    return(-1);
  }
}

/*
 *  server initialization and polling routines
 */


/* SAM 8/15/00: I need to encapsulate all the relevant behavior
   BESIDES the task startup as a function, to access from the
   Python bindings. This will also be valuable for C implementations
   with their own main loops. */

/* GalIO_ServerCallbackHandler()
   returns 1 if there's a new connection, 0 if not, -1 if error and
   the listener has been shut down, 
   -2 if error and server has been destroyed. */

int GalIO_ServerCallbackHandler(GalIO_ServerStruct *scomm, int read_blocking,
				GalIO_CommStruct **new_conn_ptr)
{
  GalIO_CommStruct *new_conn = (GalIO_CommStruct *) NULL;
  int res;
  
  scomm->read_blocking = read_blocking;
  res = GalIO_ServerHandler(scomm, &new_conn);
  
  switch (res) {
  case -1:
    /* error, try going back to listening */
    if (scomm->error == LISTENER_ERROR)
      reset_listener(scomm, __FUNCTION__);
    switch(scomm->error) {
    case CONFIGURATION_ERROR:
      if (scomm->server_task_removal_callback) {
	GalIO_RemoveServerCallback(scomm, scomm->server_task_removal_callback);
      }
      if (scomm->client_task_removal_callback) {
	GalIO_RemoveServerCallback(scomm, scomm->client_task_removal_callback);
      }
      GalIO_DestroyServerStruct(scomm);
      return -2;
    case LISTENER_ERROR:
      /* Already tried resetting, don't bother. */
      break;
    default:
      break;
    }
    break;
  case 0:
    /* no connection, continue listening */
    break;
  case 1:
    break;
  }
  if (new_conn_ptr)
    *new_conn_ptr = new_conn;
  return res;
}

/* SAM 2/8/00: I have disabled blocking reads in galaxy_server_poll
   because accept() isn't a Posix thread cancellation point and if you
   try to exit the loop, you'll wait until the next client contacts
   the server. */

static void
galaxy_server_poll(Gal_TaskPkg *p)
{
  GalIO_ServerStruct *scomm = (GalIO_ServerStruct *) Gal_TaskPkgData(p);

  GalIO_ServerCallbackHandler(scomm, Gal_TaskPkgBlocking(p),
			      (GalIO_CommStruct **) NULL);
  
  /* continue polling */
  /* Be sure that if the listen sock has been shut down, as it
     might be in the reconnection case, that you don't repoll. */
  if ((scomm->poll_ms > 0) && (scomm->listen_sock != GAL_INVALID_SOCKET)) {
    Gal_ReAddTaskWithSocketIO(p, (void *) scomm, scomm->poll_ms, 0,
			      &(scomm->listen_sock), (GAL_SOCKET *) NULL, NULL);
  }
}

/* Used in hub_init.c for listener-in-Hub */

GalIO_ServerStruct *
GalIO_ServerInit(unsigned short port, int require_port, GalIO_FrameHandler fnptr, void *server_data, int poll_ms, int max_connections)
{
  return GalIO_ServerStart(GalIO_ServerCreate(port, require_port, fnptr, server_data, poll_ms, max_connections));
}

/* Used in generic-server-toplevel.c */

GalIO_ServerStruct *
GalIO_ServerCreate(unsigned short port, int require_port, GalIO_FrameHandler fnptr, void *server_data, int poll_ms, int max_connections)
{
  return GalIO_ListenerCreate(port, require_port, fnptr, server_data, poll_ms, max_connections);
}

/* client_poll_flags allows the programmer to control the poll
   behavior for GalIO_ContactHub. -1 means inherit the poll properties
   of the parent, otherwise, use these poll properties. In addition,
   if the parent or current poll properties are anything BESIDES
   what amounts to once-only
   (GAL_HUB_CLIENT_CONNECT_FAILURE_NOOP | GAL_HUB_CLIENT_DISCONNECT_NOOP),
   make sure the client poll is started. */

GalIO_CommStruct *GalIO_ContactHub(const char *host, unsigned short port,
				   GalIO_ServerStruct *scomm,
				   const char *session_id,
				   int client_poll_flags)
{
  /* This function adds a server location to the server and
     then calls GalIO_ContactHubFromServerLoc. */
  GalIO_ServerLocation *loc = GalIO_AddServerLocation(scomm, host, port,
						      client_poll_flags);
  GalIO_CommStruct *new_conn;
  int candidate_flags;
  
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(unlock_conn_mutex, (void *) NULL);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(unlock_conn_mutex, (void *) NULL);
#endif
  __GalIO_LockConnectionMutex(30);

  new_conn = __GalIO_MTContactHubFromServerLoc(loc, scomm, session_id);

  /* Use the flags passed in to test for whether to set up the poll,
     or if we're supposed to inherit from the parent, get the parent flags. */
  
  candidate_flags = client_poll_flags;
  if (client_poll_flags == -1) {
    candidate_flags = scomm->server_listen_status;
  }
  if ((candidate_flags & (GAL_HUB_CLIENT_CONNECT_FAILURE_MASK | 
			  GAL_HUB_CLIENT_DISCONNECT_MASK)) !=
      (GAL_HUB_CLIENT_CONNECT_FAILURE_NOOP |
       GAL_HUB_CLIENT_DISCONNECT_NOOP)) {
    /* That is, if you mask the candidate flags with the connect and
       disconnect positions, and if that isn't equal to "don't
       do anything before or after", make sure the
       server is listening for clients. */
    GalIO_StartClient(scomm);
  }

  __GalIO_UnlockConnectionMutex(31);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
  
  return new_conn;
}

/* This function should be static, but I need it in hub_init.c. */

/* 4/18/02: Must be push/pop protected for thread cancellation. */

void __GalIO_ServerInsertConnection(GalIO_ServerStruct *scomm,
				    GalIO_CommStruct *gcomm,
				    int mutex_locked)
{
  if (mutex_locked) {
    __GalIO_InsertConnectionIntoServer(scomm, gcomm);
  } else {
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPushCleanup(unlock_conn_mutex, (void *) NULL);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_push(unlock_conn_mutex, (void *) NULL);
#endif    
    __GalIO_LockConnectionMutex(32);

    __GalIO_InsertConnectionIntoServer(scomm, gcomm);

    __GalIO_UnlockConnectionMutex(33);
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_pop(0);
#endif
  }
}

/* This function should be called with the MUTEX LOCKED.
   GalIO_ClientInit is thread-safe, and _GalIO_ClientContactHandler
   and GalIO_ClientHandshakeHandler are both thread-safe as long
   as multiple threads aren't trying to use the same connection.
   They also don't lock the mutex.

   I need this to be called with the mutex locked because of
   how the GalIO_CheckHubContacts works.

   1/8/02: But GalIO_ClientInit WAS locking the mutex
   somewhere in there, so it couldn't be called with the
   mutex locked. 
*/

static GalIO_CommStruct *
__GalIO_MTContactHubFromServerLoc(GalIO_ServerLocation *loc,
				  GalIO_ServerStruct *scomm,
				  const char *session_id)
{
  GalIO_CommStruct *new_gcomm;

  /* Polling will be taken care of when the connection
     is inserted into the server. Frame handler will be written again,
     but I need to pass it to shut up the ClientInit function. */
  new_gcomm = __GalIO_ClientInitWithMutex(loc->host, loc->port,
					  scomm->gcomm_prototype->handler,
					  -1, 1);
  if (new_gcomm) {
    if (_GalIO_ClientContactHandler(new_gcomm, 0) == -1) {
      GalIO_SetCommDone(new_gcomm);
      __GalIO_IDestroyCommStruct(new_gcomm, 0, 1);
      return (GalIO_CommStruct *) NULL;
    }
    /* SAM 7/6/00: In order to have a listener for multiple
       connections on the Hub side, we need to send the name
       of the server, very early in the game, before the
       reinitialize function is called. This sort of bites,
       because I don't want to rewrite the handshake protocols
       one side at a time. */
    if (GalIO_ServerClientHandshakeHandler(scomm, new_gcomm, session_id) == -1) {
      __GalIO_IDestroyCommStruct(new_gcomm, 0, 1);
      return (GalIO_CommStruct *) NULL;
    }
    /* If the contact is made, then we want to set up the connection
       and poll. We also want to make sure that when the disconnection
       happens, the server will exit. See GalIO_DestroyCommStruct.
    */
    __GalIO_ServerInsertConnection(scomm, new_gcomm, 1);
    
    loc->connected = GAL_HUB_CLIENT_CONNECTED;
  }
  return new_gcomm;
}

/* SAM 2/8/00: I have disabled blocking reads in galaxy_server_poll
   because accept() isn't a Posix thread cancellation point and if you
   try to exit the loop, you'll wait until the next client contacts
   the server. */

/* I need to go through the host:port list twice in the case
   where it acts as a client, since I need to set the max_connections
   first. */

/* There are two things to do when we connect if we're not
   a listening server. The first is
   to poll, which will amount to setting up a timed task to
   establish the connection. The second is to do the
   connection immediately and fail if it doesn't get established.
   I can do both by setting up a timed task. */

static void galaxy_contact_hub(Gal_TaskPkg *p)
{	
  /* If this server is actually a client, I need to contact the
     Hub and then start the connection poll, without the server poll.
     Furthermore, we need to do this for perhaps multiple
     connections to multiple hubs. */
  GalIO_ServerStruct *scomm = (GalIO_ServerStruct *) Gal_TaskPkgData(p);

  GalIO_ServerCheckHubContacts(scomm);

  /* This task just keeps going. If nothing's connected, and
     the server shuts down on disconnect, then we should probably
     exit. This is handled in GalIO_DestroyCommStruct. */
  
  /* continue polling, but not as frequently as the listen task. */  
  Gal_ReAddTask(p, (void *) scomm, (scomm->poll_ms * 10), 0, NULL);
}

/* I really ought to be locking the mutex here, but this
   function makes a lot of high-level calls, and unlocking the
   mutex would defeat the purpose of having it locked
   at the beginning of the loop. So I've made sure that
   all the functions inside can be called with the mutex locked. */

/* 4/18/02: Must have a push/pop to protect things like
   the printouts in the case of thread cancellation. */

void GalIO_ServerCheckHubContacts(GalIO_ServerStruct *scomm)
{
  int i;
  int destroy_server = 0;
  int listen_status;
  /* If there are no connections, don't do anything
     at the moment; someone may add some later. */

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(unlock_conn_mutex, (void *) NULL);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(unlock_conn_mutex, (void *) NULL);
#endif
  __GalIO_LockConnectionMutex(35);
  
  if (scomm->server_locations) {
    for (i = 0; scomm->server_locations[i].host; i++) {
      if (scomm->server_locations[i].connected != GAL_HUB_CLIENT_CONNECTED) {
	/* If we're not connected, and the server has never
	   contacted the Hub, or the server
	   polls for reconnection and it's never been connected,
	   or the server is disconnected and polls for reconnection,
	   try to connect. */
	if (scomm->server_locations[i].poll_flags == -1) {
	  listen_status = scomm->server_listen_status;
	} else {
	  listen_status = scomm->server_locations[i].poll_flags;
	}
	if ((scomm->server_locations[i].connected ==
	     GAL_HUB_CLIENT_NEVER_CONNECTED) &&
	    ((listen_status & GAL_HUB_CLIENT_CONNECT_FAILURE_MASK) ==
	     GAL_HUB_CLIENT_CONNECT_FAILURE_SHUTDOWN)) {
	  /* If we've attempted to connect and failed, and
	     the connection failure is supposed to mean a shutdown,
	     then shut down. */
	  destroy_server = 1;
	  break;
	}
	if ((scomm->server_locations[i].connected ==
	     GAL_HUB_CLIENT_NEVER_CONTACTED) ||
	    ((scomm->server_locations[i].connected ==
	      GAL_HUB_CLIENT_NEVER_CONNECTED) &&
	     ((listen_status & GAL_HUB_CLIENT_CONNECT_FAILURE_MASK) ==
	      GAL_HUB_CLIENT_CONNECT_FAILURE_RETRY)) ||
	    ((scomm->server_locations[i].connected ==
	      GAL_HUB_CLIENT_DISCONNECTED) &&
	     ((listen_status & GAL_HUB_CLIENT_DISCONNECT_MASK) ==
	      GAL_HUB_CLIENT_DISCONNECT_RETRY))) {
	  if (!__GalIO_MTContactHubFromServerLoc(&(scomm->server_locations[i]),
						 scomm, (char *) NULL)) {
	    GalUtil_WarnLevelWithLocation(GAL_CONNECTION_VERBOSITY_LEVEL, __FUNCTION__, "Couldn't contact hub at host %s and port %d", scomm->server_locations[i].host, (int) scomm->server_locations[i].port);
	    /* Mark it as contacted but not connected. */
	    scomm->server_locations[i].connected = GAL_HUB_CLIENT_NEVER_CONNECTED;
	  } else {
	    GalUtil_PrintWithLocation(GAL_CONNECTION_VERBOSITY_LEVEL, __FUNCTION__, "Connected to hub at host %s and port %d\n", scomm->server_locations[i].host, (int) scomm->server_locations[i].port);
	  }
	}
      }
    }
  }
  
  __GalIO_UnlockConnectionMutex(37);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
  
  if (destroy_server) {
    GalIO_DestroyServerStruct(scomm);
  }
}

/* Added a function to start up the listener if it's
   not already started. This will be used by the brokering.
   This is NOT the same as the now removed
   GalIO_ListenerStart, which I personally
   had no use for. -1 if failed, 1 if already started, 0 if
   just started up. Don't forget to start the poll! Now
   handled in the callbacks.

   This forces the server to be a listener. */

int GalIO_StartListener(GalIO_ServerStruct *scomm, int additional_flags)
{
  int status;
  
  if (!scomm)
    return -1;
  if (scomm->listen_sock != GAL_INVALID_SOCKET) {
    scomm->server_listen_status = scomm->server_listen_status | additional_flags;
    return 1;
  }

  status = reset_listener(scomm, __FUNCTION__);
  
  if (status == 0) {
    scomm->server_listen_status = scomm->server_listen_status | additional_flags;
  }
  return status;
}

/* This function forces the server to be a client.
   -1 if failed, 1 if already started, 0 if
   just started up. Don't forget to start the poll! */

int GalIO_StartClient(GalIO_ServerStruct *scomm)
{
  if (!scomm)
    return -1;
  if (GalIO_ServerIsClient(scomm)) {
    return 1;
  }
  scomm->server_listen_status = scomm->server_listen_status | GAL_HUB_CLIENT;

  /* Run the client startup callbacks. */
  _GalIO_QueueApply(scomm->client_poll_startup_callbacks,
		    __galio_apply_server_callback,
		    (void *) NULL);
  return 0;
}

/* SAM 9/28/00: The server can be a listener and a client simultaneously.
   We set up two tasks. This is mostly because later we might want to
   add a separate listener task to a client when brokers start up.

   If it's a client, then make sure that the task is added if
   appropriate. If it's a listener, try to start it up. If
   it can't start (and it's not a client), die; otherwise, if
   it did start up, start the task if necessary. */

static void __galio_server_task_shutdown(GalIO_ServerStruct *scomm,
			   void *callback_data)
{
  Gal_TaskPkg *p = (Gal_TaskPkg *) callback_data;

  Gal_RemoveTask(p);
}

static void __galio_connection_task_shutdown(GalIO_CommStruct *gcomm,
					     void *callback_data)
{
  Gal_TaskPkg *p = (Gal_TaskPkg *) callback_data;

  Gal_RemoveTask(p);
}

static void __galio_setup_server_client_poll(GalIO_ServerStruct *scomm,
					     void *callback_data)
{
  Gal_TaskPkg *p;
  GalIO_Callback *c;
  int success = 0;
  
  p = Gal_AddTask(galaxy_contact_hub,
		  (void *) scomm, -1, 0, NULL);
  if (p) {
    c = GalIO_AddServerCallback(scomm,
				GAL_SERVER_DESTRUCTION_EVENT,
				__galio_server_task_shutdown,
				(void *) p);
    /* This is the particular shutdown callback which
       removes the timed task. If in the threaded case,
       I let it run, it will shut down the thread
       WHILE THE DESTROY IS UNDERWAY. So we will
       remove this before we call the destroy function. */
    scomm->client_task_removal_callback = c;
    
    /* Make sure everything is set up, then start the task. */
    if (Gal_StartTask(p, (scomm->poll_ms * 10)) == 1) {
      GalIO_RemoveServerCallback(scomm, c);
      scomm->client_task_removal_callback = (GalIO_Callback *) NULL;
    } else {
      success = 1;
    }
  }

  if (!success) {
    GalUtil_Warn("Failed to set up requested poll for Hub contact");
  }
}

void __galio_setup_server_listener(GalIO_ServerStruct *scomm,
				   void *caller_data)
{
  Gal_TaskPkg *p;
  GalIO_Callback *c;
  int success = 0;
  
  p = Gal_AddTaskWithSocketIO(galaxy_server_poll, (void *) scomm,
			      -1, 0, &(scomm->listen_sock),
			      (GAL_SOCKET *) NULL, NULL);
  if (p) {
    c = GalIO_AddServerCallback(scomm,
				GAL_SERVER_LISTENER_SHUTDOWN_EVENT,
				__galio_server_task_shutdown,
				(void *) p);
    /* This is the particular shutdown callback which
       removes the timed task. If in the threaded case,
       I let it run, it will shut down the thread
       WHILE THE DESTROY IS UNDERWAY. So we will
       remove this before we call the destroy function. */
    scomm->server_task_removal_callback = c;
    /* Make sure everything is set up, then start the task. */

    if (Gal_StartTask(p, scomm->poll_ms) == 1) {
      GalIO_RemoveServerCallback(scomm, c);
      scomm->server_task_removal_callback = (GalIO_Callback *) NULL;
    } else {
      success = 1;
    }
  }

  if (!success) {
    GalUtil_Warn("Failed to set up requested poll for server listener");
  }
}

void __galio_create_connection_poll(GalIO_ServerStruct *scomm,
				    GalIO_CommStruct *new_gcomm,
				    void *caller_data)
{
  Gal_TaskPkg *p;
  GalIO_Callback *c;  

  if (new_gcomm->poll_ms > 0) {
    int success = 0;
    
    /* No blocking reads for connections. See galaxy_connection_poll. */
    p = Gal_AddTaskWithSocketIO(galaxy_connection_poll,
				(void *) new_gcomm,
				-1, 0,
				&(new_gcomm->sock),
				(GAL_SOCKET *) NULL,
				NULL);
    if (p) {
      c = GalIO_AddConnectionCallback(new_gcomm,
				      GAL_CONNECTION_SHUTDOWN_EVENT,
				      __galio_connection_task_shutdown,
				      (void *) p);
      GalIO_AddConnectionBrokerCallback(new_gcomm,
					GAL_CONNECTION_BROKER_OUT_STARTUP_EVENT,
					_galio_create_broker_out_poll,
					(void *) NULL);
      GalIO_AddConnectionBrokerCallback(new_gcomm,
					GAL_CONNECTION_BROKER_IN_STARTUP_EVENT,
					_galio_create_broker_in_poll,
					(void *) NULL);
      /* This is the particular shutdown callback which
	 removes the timed task. If in the threaded case,
	 I let it run, it will shut down the thread
	 WHILE THE DESTROY IS UNDERWAY. So we will
	 remove this before we call the destroy function. */
      new_gcomm->task_removal_callback = c;

      /* Make sure everything is set up, then start the task. */
      if (Gal_StartTask(p, new_gcomm->poll_ms) == 1) {
	GalIO_RemoveConnectionCallback(new_gcomm, c);
	new_gcomm->task_removal_callback = (GalIO_Callback *) NULL;
      } else {
	success = 1;
      }
    }
    if (!success) {
      GalUtil_Warn("Failed to set up requested poll for connection");
    }
  }
}

static void __galio_terminate_scomm(GalIO_ServerStruct *scomm,
				    void *caller_data)
{
  Gal_EndTasks(1);
}

/* Called in generic-server-toplevel.c */

#ifdef __INSURE__
#include <signal.h>
static void __Gal_Exit(int sig)
{
  printf("Requesting timed task loop exit.\n"); fflush(stdout);
  Gal_EndTasks(1);
}
#endif

GalIO_ServerStruct *
GalIO_ServerStart(GalIO_ServerStruct *scomm)
{
  int is_client = 0;

#ifdef __INSURE__  
  Gal_AddSignalHandler(SIGINT, __Gal_Exit);
#endif

  if (scomm) {
    /* Set up the callbacks for the Hub loop. This only happens
       if the Communicator main loop is being used. */
    if (scomm->poll_ms > 0) {
      GalIO_AddServerCallback(scomm,
			      GAL_SERVER_CLIENT_POLL_STARTUP_EVENT,
			      __galio_setup_server_client_poll,
			      (void *) NULL);
      GalIO_AddServerCallback(scomm,
			      GAL_SERVER_LISTENER_STARTUP_EVENT,
			      __galio_setup_server_listener,
			      (void *) NULL);
      GalIO_AddServerCallback(scomm,
			      GAL_SERVER_DESTRUCTION_EVENT,
			      __galio_terminate_scomm,
			      (void *) NULL);
      GalIO_AddServerConnectCallback(scomm,
				     __galio_create_connection_poll,
				     (void *) NULL);      
    }
    if (GalIO_ServerIsClient(scomm)) {
      is_client = 1;
      /* Run the client startup callbacks. */
      _GalIO_QueueApply(scomm->client_poll_startup_callbacks,
			__galio_apply_server_callback,
			(void *) NULL);
    }
    if (GalIO_ServerIsListener(scomm)) {
      /* This will take care of running the listener startup callbacks. */
      int status = reset_listener(scomm, __FUNCTION__);
      
      if (status == -1) {
	if (!is_client) {
	  /* If this isn't also a client, destroy the server
	     and fail. */
	  scomm->done = 1;	  
	  GalIO_DestroyServerStruct(scomm);
	  return(NULL);
	}
      } 
    }
    return scomm;
  } else {
    return NULL;
  }
}

/* Irrespective of the timed task loop. */

GalIO_CommStruct *GalIO_AcceptUniqueConnection(GalIO_ServerStruct *scomm)
{
  /* This will not reset a poll for the server, and it will
     do a blocking read. If it doesn't accept a connection,
     well, there's none to be had. */
  GalIO_CommStruct *new_gcomm;
  
  GalIO_ServerCallbackHandler(scomm, 1, &new_gcomm);
  return new_gcomm;
}		       

/* Support for when the server is actually contacting the Hub instead of
   the other way around. */

void GalIO_SetServerListenStatus(GalIO_ServerStruct *scomm,
				 int server_listen_status,
				 const char *client_pair_string,
				 const char *session_id)
{
  scomm->server_listen_status = server_listen_status;
  if (client_pair_string) {
    scomm->server_locations = GalIO_DigestServerLocations(client_pair_string);
  }
  if (session_id)
    scomm->default_session_id = _gal_strdup(session_id);
}

GalIO_ServerLocation *GalIO_GetServerLocations(GalIO_ServerStruct *scomm)
{
  return scomm->server_locations;
}

int GalIO_NumServerLocations(GalIO_ServerLocation *locs)
{
  int i;

  if (!locs) return 0;
  
  for (i = 0; locs[i].host; i++);
  return i;
}

GalIO_ServerLocation *GalIO_AddServerLocation(GalIO_ServerStruct *scomm,
					      const char *host,
					      unsigned short port,
					      int client_poll_flags)
{
  int i;

  __GalIO_LockConnectionMutex(38);
  if (!scomm->server_locations) {
    scomm->server_locations = (GalIO_ServerLocation *) calloc(2, sizeof(GalIO_ServerLocation));
    i = 0;
    scomm->server_locations[i].host = _gal_strdup(host);
    scomm->server_locations[i].port = port;
    scomm->server_locations[i].connected = GAL_HUB_CLIENT_NEVER_CONTACTED;
    scomm->server_locations[i].poll_flags = client_poll_flags;
  } else {
    i = GalIO_NumServerLocations(scomm->server_locations);
    scomm->server_locations = (GalIO_ServerLocation *) realloc(scomm->server_locations, (i + 2) * sizeof(GalIO_ServerLocation));    
    scomm->server_locations[i].host = _gal_strdup(host);
    scomm->server_locations[i].port = port;
    scomm->server_locations[i].connected = GAL_HUB_CLIENT_NEVER_CONTACTED;
    scomm->server_locations[i].poll_flags = client_poll_flags;
    scomm->server_locations[i+1].host = (char *) NULL;
    scomm->server_locations[i+1].port = 0;
    scomm->server_locations[i+1].connected = GAL_HUB_CLIENT_NEVER_CONTACTED;
    scomm->server_locations[i+1].poll_flags = -1;
  }
  __GalIO_UnlockConnectionMutex(39);
  return &(scomm->server_locations[i]);
}

char *GalIO_NthHostAndPort(GalIO_ServerLocation *locs, int i,
			   unsigned short *port)
{
  int j;

  if (locs) {
    for (j = 0; locs[j].host; j++) {
      if (i == j) {
	if (port) {
	  *port = locs[j].port;
	  return locs[j].host;
	}
      }
    }
  }
  if (port) {
    *port = (unsigned short) 0;
  }
  return (char *) NULL;
}
  
GalIO_ServerLocation *GalIO_DigestServerLocations(const char *client_pair_string)
{
  GalIO_ServerLocation *locs;  

  /* I need to go through the client_pair_string twice,
     so I need to copy, because strtok_r() still alters the string. */
  /* I need to make sure that client_pair_string is present.
     One could imagine setting server_listen_status and not adding
     any connections immediately. */
  char *client_pair_copy;
  char *tok; 
  char *lasts;
  char *host;
  int port;
  int i = 0;
  int found = 0;
  int num_connections = 0;

  if (!client_pair_string)
    return (GalIO_ServerLocation *) NULL;
  
  client_pair_copy = _gal_strdup(client_pair_string);
  tok = _gal_strtok_r(client_pair_copy, " \t", &lasts);
    
  while (tok) {
    num_connections++;
    tok = _gal_strtok_r((char *) NULL, " \t", &lasts);
  }
  free(client_pair_copy);
  locs = (GalIO_ServerLocation *) calloc(1 + num_connections, sizeof(GalIO_ServerLocation));
        
  /* Start again. */
  client_pair_copy = _gal_strdup(client_pair_string);
  tok = _gal_strtok_r(client_pair_copy, " \t", &lasts);

  /* Each hub location which is badly described will be skipped. */
  while (tok) {
    host = Gal_SplitLocation(tok, &port);
    if ((port == -1) || !(host)) {
      GalUtil_WarnWithLocation(__FUNCTION__, "Incomplete Hub location %s. Skipping\n", tok);
    } else {
      locs[i].host = host;
      locs[i].port = (unsigned short) port;
      locs[i].connected = GAL_HUB_CLIENT_NEVER_CONTACTED;
      locs[i].poll_flags = -1;
      found = 1;
      i++;
    }
    tok = _gal_strtok_r((char *) NULL, " \t", &lasts);
  }
  free(client_pair_copy);    
  if (!found) {
    free(locs);
    locs = (GalIO_ServerLocation *) NULL;
  }
  return locs;
}

void GalIO_FreeServerLocations(GalIO_ServerLocation *loc)
{
  int i = 0;

  for (i = 0; loc[i].host; i++) {
    free(loc[i].host);
  }	
  free(loc);
}

int GalIO_ServerListenStatus(GalIO_ServerStruct *scomm)
{
  return scomm->server_listen_status;
}

char *GalIO_ServerSessionID(GalIO_ServerStruct *scomm)
{
  return scomm->default_session_id;
}

/*
 *  hub initialization and polling routines
 */

/* SAM 10/14/99: A higher-level function to be used by functions that
   want to contact servers directly. Hubs do this, but dispatch
   servers will also use it. */

static int dummy_frame_handler(GalIO_CommStruct *gcomm, Gal_Frame frame)
{
  return 0;
}

GalIO_CommStruct *GalIO_ClientConnect(const char *name,
				      const char *host, unsigned short port,
				      int silent,
				      Gal_Frame welcome_frame,
				      Gal_Frame *reply_frame)
{
  GalIO_CommStruct *gcomm;
  Gal_Frame *captured_reply = reply_frame;
  int status;
  Gal_Frame admin_info;

  if (welcome_frame) {
    Gal_SetFrameName(welcome_frame, "reinitialize");
    /* Make sure it's in the data. */
    admin_info = __GalIO_EnsureAdminInfo(welcome_frame);
    if (!Gal_GetObject(admin_info, GAL_ROUND_TRIP_FRAME_KEY))
      Gal_SetProp(admin_info, GAL_ROUND_TRIP_FRAME_KEY, Gal_IntObject(1));
  }
      
  gcomm = GalIO_ClientInit(host, port, dummy_frame_handler, -1);
  while ((status = __GalIO_HubHandler(gcomm, silent,
				      welcome_frame, captured_reply)) == 0) {
    /* The first reply I get, that's the one I keep. */
    if (captured_reply && (&captured_reply)) {
      /* Once we've snagged something, stop trying to collect. */
      captured_reply = (Gal_Frame *) NULL;
    }
    /* If the status is 0, that means that it found nothing. Better
       sleep a little bit. */
    GalUtil_MilliSleep(1);
  }
  if (status == -1)
  {
    GalUtil_WarnLevelWithLocation(GAL_CONNECTION_VERBOSITY_LEVEL, __FUNCTION__, "Error connecting to server %s at %s:%d", name, host, port);
    GalIO_SetCommDone(gcomm);
    GalIO_DestroyCommStruct(gcomm);
    return (GalIO_CommStruct *) NULL;
  }
  return gcomm;
}

/* Not sure where else to put this. */

Gal_Frame GalIO_CreateErrorFrame(int errnum, const char *error)
{
  Gal_Frame f = Gal_MakeFrame("system_error", GAL_CLAUSE);
  
  Gal_SetProp(f, GAL_ERROR_NUMBER_FRAME_KEY, Gal_IntObject(errnum));
  Gal_SetProp(f, GAL_ERROR_DESCRIPTION_FRAME_KEY, Gal_StringObject(error));
  return f;
}

/* Returns -1 if no error, error number otherwise.
   Sets err_desc if provided. */

int GalIO_GetError(Gal_Frame f, char **err_desc)
{
  Gal_Object err_num;
  
  if (!(err_num = Gal_GetObject(f, GAL_ERROR_NUMBER_FRAME_KEY))) {
    if (err_desc)
      *err_desc = (char *) NULL;
    return -1;
  }
  if (err_desc)
    *err_desc = Gal_GetString(f, GAL_ERROR_DESCRIPTION_FRAME_KEY);
  return Gal_IntValue(err_num);
}    

char *
GalIO_MsgTypeToName(GalIO_MsgType mt) {
  switch(mt) {
  case GAL_OBJECT_MSG_TYPE:
    return "GAL_OBJECT_MSG_TYPE";
  case GAL_MESSAGE_MSG_TYPE:
    return "GAL_MESSAGE_MSG_TYPE";
  case GAL_REPLY_MSG_TYPE:
    return "GAL_REPLY_MSG_TYPE";
  case GAL_DESTROY_MSG_TYPE:
    return "GAL_DESTROY_MSG_TYPE";
  case GAL_BROKER_START_MSG_TYPE:
    return "GAL_BROKER_START_MSG_TYPE";
   case GAL_BROKER_END_MSG_TYPE:
    return "GAL_BROKER_END_MSG_TYPE";
   case GAL_ERROR_MSG_TYPE:
    return "GAL_ERROR_MSG_TYPE";
   case GAL_DISCONNECT_MSG_TYPE:
    return "GAL_DISCONNECT_MSG_TYPE";
  default:
    return (char *) NULL;
  }
}

/* Server properties. */

/* Add to the response to the handshake: 
b: {c <servername> 
      :extra_service_types <list>
      :session_id <session_id>
      :properties <frame>
      :signatures <signatures> }

      (see GalIO_ServerInformationFrame());
*/

/* This is not thread safe. The server property mutex
   should be set when we update from elsewhere. */

Gal_Frame GalIO_ServerProperties(GalIO_ServerStruct *server)
{
  return server->server_properties;
}

/* Delete before set. */

void GalIO_ServerModifyProperties(GalIO_ServerStruct *server,
				  Gal_Frame new_properties,
				  char **delete_properties)
{
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			    (void *) &server_property_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex,
		       (void *) &server_property_mutex);
#endif
  GalUtil_LockLocalMutex(&server_property_mutex);
  
  Gal_UpdateFrameProperties(server->server_properties,
			    new_properties, delete_properties);
  
  GalUtil_UnlockLocalMutex(&server_property_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
}

void GalIO_AddServiceType(GalIO_ServerStruct *server, const char *stype)
{
  Gal_ListObjectAdd(server->service_types, Gal_StringObject(stype));
}

/* 
 * Hub-server handshake
 */

/* SAM 9/19/00: This has been completely revised for 3.0. There
   are four cases we need to worry about: server as listener,
   server as client, Hub as listener, Hub as client. We'll assign
   the various functions involved in the connectivity as follows:

   Step one: connection management. There's one for clients,
   one for listeners.
   
   _GalIO_ClientContactHandler: Hub as client, server as client.
   Responsible for establishing the client side of the connection.

   __GalIO_ServerContactHandler: Hub as listener, server as listener.
   Responsible for establishing the server side of the connection.

   Step two: handshake management. There's one each for the four
   possibilities. The connection isn't inserted into the
   server list until the handshakes are completed.

Protocol:

a: "I am a connection"
{c handshake :conn_type 1 } (flag is GAL_CONNECTION_LISTENER)

(see GalIO_ConnectionInitiationFrame())

b: {c <servername> 
      :session_id <session_id>
      :signatures <signatures> }

(see GalIO_ServerInformationFrame())

c: {c system_error
      :errno 6 (flag is GAL_CONN_REJECTION_ERROR)
      :err_description "unsupported connection type" }
   {c system_error
      :errno 6 (flag is GAL_CONN_REJECTION_ERROR)
      :err_description "max connections exceeded" }

(see GalIO_ServerListenerHandshakeHandler())

d: {c system_error
      :errno 6 (flag is GAL_CONN_REJECTION_ERROR)
      :err_description "Hub listener isn't listening for named server"
      }
   {c system_error
      :errno 6 (flag is GAL_CONN_REJECTION_ERROR)
      :err_description "Server already has general-purpose connection"
      }

(see GalHUB_HubListenerHandshakeHandler())

e: "I am a broker"
   {c handshake 
      :conn_type 2 (flag is GAL_BROKER_LISTENER)
      :call_id <callid> }

f: {c system_error
      :errno 6 (flag is GAL_CONN_REJECTION_ERROR
      :err_description "no match for broker" }
   {c system_error
      :errno 6 (flag is GAL_CONN_REJECTION_ERROR
      :err_description "broker expired" }

(see GalIO_BrokerInitiationFrame())

Hub client: see GalIO_HubClientHandshakeHandler(). Sends (a), fields
(b) as reply or (c) as error.

Server listener: see GalIO_ServerListenerHandshakeHandler(). Fields
(a) or (e). For regular connections, sends (b) as reply or (c) as
error. For brokers, sends (f) as error or pacifier as reply; see
_GalIO_BrokerListenerHandshakeHandler().

Server client: see GalIO_ServerClientHandshakeHandler(). Sends (b),
fields (d) as error or pacifier as reply.

Hub listener: see GalHUB_HubListenerHandshakeHandler(). Fields (b),
sends (d) as error or pacifier as reply.

Broker client: see GalIO_BrokerClientHandshakeHandler(). Sends (e),
fields (f) as error or pacifier as reply.

The server can also send an error back from verification if the server
has reinitialize defined and the first thing it gets isn't
reinitialize: 

{c system_error
   :errno 6 (flag is GAL_CONN_REJECTION_ERROR
   :err_description "first message must be reinitialize" }

   Step three: initialization management. There's one for Hub,
   one for servers. 

   _GalIO_HubVerificationHandler: Hub as client, Hub as listener.
   Responsible for managing the Hub side of the "reinitialize"
   processing.

   GalIO_VerificationHandler: server as listener, server as client.
   Responsible for managing the server side of the "reinitialize"
   processing.

*/

static Gal_Frame GalIO_ConnectionInitiationFrame()
{
  Gal_Frame f = Gal_MakeFrame("handshake", GAL_CLAUSE);

  Gal_SetProp(f, GAL_CONNECTION_TYPE_FRAME_KEY,
	      Gal_IntObject(GAL_CONNECTION_LISTENER));
  Gal_SetProp(f, GAL_PROTOCOL_VERSION_FRAME_KEY,
	      Gal_IntObject(GAL_TRANSPORT_PROTOCOL_VERSION));
  return f;
}  

static Gal_Frame GalIO_ServerInformationFrame(GalIO_ServerStruct *scomm,
					      const char *session_id)
{
  Gal_Frame f = Gal_MakeFrame(GalIO_GetServerName(scomm), GAL_CLAUSE);
  Gal_DispatchFnPkg *pkg = GalIO_GetServerDispatchFnPkg(scomm);

  if (!session_id) {
    session_id = GalIO_ServerSessionID(scomm);
  }
    
  if (session_id) {
    Gal_SetProp(f, GAL_SESSION_ID_FRAME_KEY, Gal_StringObject(session_id));
  }

  Gal_SetProp(f, GAL_SIGNATURES_FRAME_KEY,
	      Gal_CopyObject(Gal_EncodeDispatchFnPkgSigs(pkg)));
  Gal_SetProp(f, GAL_SERVER_PROPERTIES_FRAME_KEY,
	      Gal_FrameObject(Gal_CopyFrame(scomm->server_properties)));
  Gal_SetProp(f, GAL_SERVICE_TYPE_FRAME_KEY,
	      Gal_CopyObject(scomm->service_types));
  Gal_SetProp(f, GAL_PROTOCOL_VERSION_FRAME_KEY,
	      Gal_IntObject(GAL_TRANSPORT_PROTOCOL_VERSION));
  return f;
}

/* GalIO_HubClientHandshakeHandler: Hub as client.
   Sends GalIO_ConnectionInitiationFrame(), fields
   GalIO_ServerInformationFrame() or error. */

static int GalIO_HubClientHandshakeHandler(GalIO_CommStruct *gcomm,
					   Gal_Frame *reply)
{
  Gal_Frame f;
  Gal_Frame reply_f;
  GalIO_MsgType t;

  /* Don't do the handshake if it's already done. I
     make sure in GalIO_HubHandler() not to
     overwrite a value I've already collected. */
  
  if (__GalIO_HandshakeComplete(gcomm)) {
    if (reply) *reply = (Gal_Frame) NULL;
    return 0;
  }

  f = GalIO_ConnectionInitiationFrame();
  reply_f = GalIO_CommDispatchFrame(gcomm, f, &t, (char *) NULL);
  Gal_FreeFrame(f);

  if (reply) {
    *reply = reply_f;
  } else if (reply_f) {
    Gal_FreeFrame(reply_f);
  }
    
  switch (t) {
  case GAL_ERROR_MSG_TYPE:
    return -1;
  case GAL_REPLY_MSG_TYPE:
    /* The response will contain the server information. The
       caller ultimately decodes the signature info in the Hub
       code. */        
    __GalIO_MarkSuccessfulHandshake(gcomm);
    return 0;
  default:
    return -1;
  }
}

/* GalIO_ServerListenerHandshakeHandler: server as listener.
   Fields GalIO_ConnectionInitiationFrame(), sends
   GalIO_ServerInformationFrame() or error. */

/* This handshake handler is the one that needs to know
   what to do with broker connections. This is called with the
   mutex locked. */

static int GalIO_ServerListenerHandshakeHandler(GalIO_ServerStruct *scomm,
						GalIO_CommStruct *gcomm,
						GalIO_CommStruct **new_conn_ptr)
{
  Gal_Frame input = (Gal_Frame) NULL;
  Gal_Frame reply;
  GalIO_MsgType reply_type = GAL_REPLY_MSG_TYPE;
  int status = 0;
  int conn_type;

  /* Don't do the handshake if it's already done. */
  if (__GalIO_HandshakeComplete(gcomm)) {
    return 0;
  }  
  
  if (GalIO_CommReadFrame(gcomm, &input, 1) != 1) {
    /* The read failed, for some reason. */
    return -1;
  }

  /* Now, we check to see if the the frame is the right type. */
  conn_type = Gal_GetInt(input, GAL_CONNECTION_TYPE_FRAME_KEY);

  if (!(scomm->server_listen_status & conn_type)) {
    /* Connection type is unsupported. */
    switch (conn_type) {
    case GAL_BROKER_LISTENER:
      /* Steve and I have agreed that this should be the same
	 error as when a broker isn't found. */
      reply = GalIO_CreateErrorFrame(GAL_CONN_REJECTION_ERROR,
				   "no match for broker");
      break;
    default:
      reply = GalIO_CreateErrorFrame(GAL_CONN_REJECTION_ERROR,
				     "unsupported connection type");
      break;
    }
    reply_type = GAL_ERROR_MSG_TYPE;
    status = -1;
    GalIO_CommWriteMessage(gcomm, reply, reply_type, 1);
    Gal_FreeFrame(reply);
  } else {
    switch (conn_type) {
    case GAL_CONNECTION_LISTENER:
      if (scomm->num_connections >= scomm->max_connections) {
	reply = GalIO_CreateErrorFrame(GAL_CONN_REJECTION_ERROR,
				       "max connections exceeded");
	reply_type = GAL_ERROR_MSG_TYPE;
	status = -1;
      } else {
	reply = GalIO_ServerInformationFrame(scomm, (char *) NULL);	
	/* Handshake was successful. Store the connection. */
	__GalIO_InsertConnectionIntoServer(scomm, gcomm);
	if (new_conn_ptr)
	  *new_conn_ptr = gcomm;
      }
      GalIO_CommWriteMessage(gcomm, reply, reply_type, 1);
      Gal_FreeFrame(reply);
      break;
    case GAL_BROKER_LISTENER:
      status = _GalIO_BrokerListenerHandshakeHandler(scomm, gcomm, input);
      break;
    default:
      break;
    }
  }
  
  if (status == 0) {
    __GalIO_MarkSuccessfulHandshake(gcomm);
  }

  /* Don't free this until here, since we want to make sure
     that the broker can use it if it wants. */
  Gal_FreeFrame(input);
  
  return status;  
}

/* GalIO_ServerClientHandshakeHandler: server as client: Sends 
   GalIO_ServerInformationFrame(), fields
   error or pacifier. */

static int GalIO_ServerClientHandshakeHandler(GalIO_ServerStruct *scomm,
					      GalIO_CommStruct *new_conn,
					      const char *session_id)
{
  Gal_Frame f;
  Gal_Frame reply_f;
  GalIO_MsgType t;

  /* Don't do the handshake if it's already done. */
  if (__GalIO_HandshakeComplete(new_conn)) {
    return 0;
  }  

  f = GalIO_ServerInformationFrame(scomm, session_id);
  reply_f = GalIO_CommDispatchFrame(new_conn, f, &t, (char *) NULL);
  Gal_FreeFrame(f);

  if (t == GAL_REPLY_MSG_TYPE) {
    /* It's fine. */
    Gal_FreeFrame(reply_f);    
    __GalIO_MarkSuccessfulHandshake(new_conn);
    return 0;
  } else if (reply_f) {
    /* Otherwise, print out the error. */
    switch (Gal_GetInt(reply_f, GAL_ERROR_NUMBER_FRAME_KEY)) {
    case GAL_TRANSMISSION_ERROR:
      GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't send handshake frame while establishing connection");
      break;
    case GAL_RECEPTION_ERROR:
      GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't read handshake response while establishing connection");
      break;
    case GAL_NO_FRAME_ERROR:
      GalUtil_WarnWithLocation(__FUNCTION__, "No handshake response frame provided while establishing connection");
      break;
    default:
      if (Gal_GetObject(reply_f, GAL_ERROR_DESCRIPTION_FRAME_KEY)) {
	GalUtil_Warn(Gal_GetString(reply_f, GAL_ERROR_DESCRIPTION_FRAME_KEY));
      } else {
	GalUtil_WarnWithLocation(__FUNCTION__, "Encountered unknown handshake error %d while establishing connection\n",
		 Gal_GetInt(reply_f, GAL_ERROR_NUMBER_FRAME_KEY));
      }
    }
    Gal_FreeFrame(reply_f);
    return -1;
  } else {
    /* Major problems reading. */
    GalUtil_WarnWithLocation(__FUNCTION__, "Failed to complete initial handshake while establishing connection");
    return -1;
  }
}
