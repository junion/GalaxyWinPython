/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"

/*#ifndef WIN32
#include <netdb.h>
#else
#include <winsock2.h>
#endif
*/
#include "io_internal.h"
#include "binary_io.h"

#define IN_USE 1
#define TIMED_OUT 2
#define DESTROYED 3

/* Default number of seconds till broker expiration. */

#define GAL_DEFAULT_BROKER_EXPIRATION 10

static int match_broker_call_id(char *ref_id, Nframe in_frame);
static GalIO_BrokerStruct *NewBrokerStruct(void);
static int send_eot(GalIO_BrokerStruct *b);
static void poll_broker_data_out(Gal_TaskPkg *b);
static void poll_broker_data_in(Gal_TaskPkg *b);

/* sender-receiver verification function */
static int match_broker_call_id(char *ref_id, Nframe in_frame)
{
  char *in_id;

  if (ref_id && in_frame)
  {
    in_id = Gal_GetString(in_frame, GAL_BROKER_CALL_ID_FRAME_KEY);
    if (in_id && (strcmp(ref_id, in_id) == 0))
      return(1);
  }
  return(0);
}

static int BrokerIDNumber = 0;
static GalUtil_LocalMutex broker_id_mutex;

void _Gal_init_broker()
{
  GalUtil_InitLocalMutex(&broker_id_mutex);
}

static int __GalIO_GetBrokerIDNumber()
{
  int broker_num;

  GalUtil_LockLocalMutex(&broker_id_mutex);
  broker_num = BrokerIDNumber++;
  GalUtil_UnlockLocalMutex(&broker_id_mutex);
  return broker_num;
}

void GalIO_FrameSetBrokerCallID(Gal_Frame f, const char *call_id)
{
  Gal_SetProp(f, GAL_BROKER_CALL_ID_FRAME_KEY,
	      Gal_StringObject(call_id));
}

/* create a new struct */
static GalIO_BrokerStruct *NewBrokerStruct(void)
{
  GalIO_BrokerStruct *b;

  b = (GalIO_BrokerStruct *)calloc(1, sizeof(GalIO_BrokerStruct));
  b->status = IN_USE;
  b->queued = 0;
  b->destroy_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
						0, 1, free);
  b->data_done_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
						  0, 1, free);
  b->abort_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
					      0, 1, free);
  b->connect_callbacks = _GalIO_NewPointerQueue(GAL_CALLBACK_PTYPE,
						0, 1, free);
  return(b);
}

static __GalIO_OutgoingBroker *NewOutgoingBroker(GalIO_ServerStruct *scomm,
						 int timeout_seconds)
{
  __GalIO_OutgoingBroker *b = (__GalIO_OutgoingBroker *) calloc(1, sizeof(__GalIO_OutgoingBroker));
  char call_id_buf[1024];
  
  /* Set the server backpointer. */
  b->server = scomm;
  /* Set the call ID. It's very important that the call ID
     be unique, because now brokers can time out. So we use
     the IP address, the pid, and an integer. */
  sprintf(call_id_buf, "%s:%d:%d", GalIO_IPAddress(),
	  (int) _gal_getpid(), __GalIO_GetBrokerIDNumber());
  b->call_id = _gal_strdup(call_id_buf);
  /* Make sure the broker has a queue. */
  b->queue = _GalIO_CreateSockObjectQueue(GAL_INVALID_SOCKET,
					  128*1024, 128*1024);
  /* Make sure the broker has a queue of outgoing connections. */
  b->comm_queue = _GalIO_CreateConnectionQueue();
  /* Set up the expiration. This will no longer be handled
     in a separate timeout, but rather in poll_broker_data_out.
     Otherwise, it's up to the nonpolling loop to handle.
     GalIO_ConnectionPoll() should be able to rise to the
     occasion, perhaps, except that it's called by the
     Galaxy connection poll as well. We'll think about this.

     The reason I'm not making this into its own task is that
     I need to make sure something sensible happens when there
     isn't any timed task loop. */

  /* Even if the timeout is 0, set up the time. */
  if (timeout_seconds == 0) {
    timeout_seconds = GAL_DEFAULT_BROKER_EXPIRATION;
  }

  /* If the expiration time is < 0, never expire. */
  if (timeout_seconds > 0) {
    b->expiration_time = (struct timeval *) malloc(sizeof(struct timeval));
    /* Get the current time, and increment. */
    _gal_gettimeofday(b->expiration_time);
    b->expiration_time->tv_sec += timeout_seconds;
  }

  return b;
}

static __GalIO_IncomingBroker
*NewIncomingBroker(GalIO_CommStruct *gcomm, Gal_Frame frame,
		   GalIO_BrokerDataHandler fnptr)
{
  __GalIO_IncomingBroker *b = (__GalIO_IncomingBroker *) calloc(1, sizeof(__GalIO_IncomingBroker));

  b->gcomm = gcomm;
  b->frame = Gal_CopyFrame(frame);
  b->handler = fnptr;
  b->active = 0;
  return b;
}

static void __GalIO_DestroyIncomingBrokerStruct(__GalIO_IncomingBroker *b)
{
  if (b) {
    if (b->gcomm) {
      GalIO_SetCommDone(b->gcomm);
      GalIO_DestroyCommStruct(b->gcomm);
      b->gcomm = (GalIO_CommStruct *) NULL;
    }    
    if (b->frame) {
      Gal_FreeFrame(b->frame);
      b->frame = (Gal_Frame) NULL;
    }
    if (b->host_gcomm) {
      _GalIO_CommUnlock(b->host_gcomm);
      b->host_gcomm = (GalIO_CommStruct *) NULL;
    }
  }
}

static void __GalIO_DestroyOutgoingBrokerStruct(__GalIO_OutgoingBroker *b,
						GalIO_BrokerStruct *top_b)
{
  GalIO_CommStruct *gcomm;
  
  if (b) {
    /* A single connection for incoming brokers, perhaps
       a queue for outgoing. */
    gcomm = (GalIO_CommStruct *) _GalIO_QueuePop(b->comm_queue);
    while (gcomm) {
      GalIO_SetCommDone(gcomm);
      GalIO_DestroyCommStruct(gcomm);
      gcomm = (GalIO_CommStruct *) _GalIO_QueuePop(b->comm_queue);
    }
    _GalIO_QueueDestroy(b->comm_queue);
    b->comm_queue = (GalIO_PointerQueue *) NULL;
    _GalIO_DestroySockObjectQueue(b->queue);
    b->queue = NULL;
    /* If this object has a server, then
       it's an outbound broker. Force dequeue. */
    if (b->server) {      
      _GalIO_ServerRemoveOutgoingBroker(b->server, top_b);      
    }
    if (b->expiration_time) {
      free(b->expiration_time);
      b->expiration_time = (struct timeval *) NULL;
    }
    if (b->call_id) {
      free(b->call_id);
      b->call_id = (char *) NULL;
    }
  }
}

static int __galio_apply_broker_callback(void *data, void *caller_data)
{
  GalIO_Callback *cb = (GalIO_Callback *) data;
  GalIO_BrokerCallbackFn fn = (GalIO_BrokerCallbackFn) cb->fn;
  GalIO_BrokerStruct *b = (GalIO_BrokerStruct *) cb->callback_host;
  
  (*fn)(b, cb->callback_data);
  /* Continue. */
  return 1;
}

/* destroy struct (if done sending/receiving data) */
void GalIO_DestroyBrokerStruct(GalIO_BrokerStruct *b)
{
  if (b)
  {
    _GalIO_QueueApply(b->destroy_callbacks,
		      __galio_apply_broker_callback,
		      (void *) NULL);
    /* SAM 2/28/02: Check the free_fn first. If it's there,
       and the data is still there but it's a freed element,
       you're in deep shit. Otherwise, you'll never touch
       the possibly freed element. */
    if (b->caller_data_free_fn && b->caller_data)
      (*b->caller_data_free_fn)(b->caller_data);
    b->caller_data = (void *) NULL;
    __GalIO_DestroyIncomingBrokerStruct(b->in_b);
    __GalIO_DestroyOutgoingBrokerStruct(b->out_b, b);

    _GalIO_QueueDestroy(b->destroy_callbacks);
    b->destroy_callbacks = (GalIO_PointerQueue *) NULL;
    _GalIO_QueueDestroy(b->abort_callbacks);
    b->abort_callbacks = (GalIO_PointerQueue *) NULL;
    _GalIO_QueueDestroy(b->data_done_callbacks);
    b->data_done_callbacks = (GalIO_PointerQueue *) NULL;
    _GalIO_QueueDestroy(b->connect_callbacks);
    b->connect_callbacks = (GalIO_PointerQueue *) NULL;
    if (b->done && (!b->queued)) {
      if (b->out_b) free(b->out_b);
      if (b->in_b) free(b->in_b);
      free(b);
    } else
      b->status = DESTROYED;
  }
}

/* accessor functions (the structure is hidden as a void * externally) */
GAL_SOCKET GalIO_GetBrokerSocket(GalIO_BrokerStruct *b)
{
  if (b && b->in_b && b->in_b->gcomm)
    return(GalIO_GetCommSocket(b->in_b->gcomm));
  else
    return(GAL_INVALID_SOCKET);
}

GAL_SOCKET GalIO_GetBrokerListenSocket(GalIO_BrokerStruct *b)
{
  if (b && b->out_b && b->out_b->server)
    return(GalIO_GetServerListenSocket(b->out_b->server));
  else
    return(GAL_INVALID_SOCKET);
}

/* SAM 10/3/00: There's only a frame for incoming brokers now. */

Gal_Frame GalIO_GetBrokerFrame(GalIO_BrokerStruct *b)
{
  if (b && b->in_b) {
    return b->in_b->frame;
  } else {
    return (Gal_Frame) NULL;
  }    
}

void *GalIO_GetBrokerData(GalIO_BrokerStruct *b)
{
  if (b)
    return(b->caller_data);
  else
    return(NULL);
}

/* Retained for backward compatibility. */

void *GalIO_GetBrokerCallerData(GalIO_BrokerStruct *b)
{
  if (b)
    return(b->caller_data);
  else
    return(NULL);
}


/* The caller data was only intended to be used with broker
   callbacks on the incoming side, but we can also use it to set
   the caller_data to use with the finalizer which is called
   when the broker is destroyed. It returns the old data,
   in case someone needs to free it or something. */

void GalIO_SetBrokerData(GalIO_BrokerStruct *b, void *caller_data,
			 void (*free_fn)(void *))
{
  if (b) {
    if (b->caller_data && b->caller_data_free_fn) {
      (*b->caller_data_free_fn)(b->caller_data);
    }
    b->caller_data = caller_data;
    b->caller_data_free_fn = free_fn;
  }
}

/* Only makes sense for incoming brokers. */

void GalIO_SetBrokerActive(GalIO_BrokerStruct *b)
{
  if (b && b->in_b)
    b->in_b->active = 1;
}

/* SAM 10/24/00: Finalizers are now destroy callbacks. But the
   data with which the callback is called is the data that
   the element currently has as caller data. */

static void __galio_apply_finalizer(GalIO_BrokerStruct *b,
				    void *caller_data)
{
  GalIO_BrokerDataFinalizer finalizer = (GalIO_BrokerDataFinalizer) caller_data;

  (*finalizer)(b, b->caller_data);
  b->caller_data = (void *) NULL;
}

/* I'm keeping around a special slot so that the finalizer
   can be removed. */

void 
GalIO_BrokerSetFinalizer(GalIO_BrokerStruct *b, 
			 GalIO_BrokerDataFinalizer finalizer)
{
  if (b) {
    if (b->finalizer_callback)
      GalIO_RemoveBrokerCallback(b, b->finalizer_callback);
    b->finalizer_callback = GalIO_AddBrokerCallback(b,
						    GAL_BROKER_DESTRUCTION_EVENT,
						    __galio_apply_finalizer,
						    (void *) finalizer);
  }
}

/* write functions */

static int __galio_broker_queue_append(void *gcomm, void *obj)
{
  _GalIO_SockObjectQueueAppendOut(((GalIO_CommStruct *) gcomm)->queue,
				  (GalIO_SockObjectStruct *) obj);
  /* Always continue. */
  return 1;
}

static int __GalIO_BrokerWriteSockObject(GalIO_BrokerStruct *b,
					 GalIO_SockObjectStruct *obj)
{
  if ((!b) || (!b->out_b) || (!obj)) {
    return -1;
  }
  /* This will basically always succeed. */
  _GalIO_SockObjectQueueAppendOut(b->out_b->queue, obj);
  _GalIO_QueueApply(b->out_b->comm_queue, __galio_broker_queue_append,
		    (void *) obj);
  return 1;
}

int GalIO_BrokerWriteObject(GalIO_BrokerStruct *b, Gal_Object o)
{
  if (b && b->out_b && (b->status != DESTROYED)) {
    return __GalIO_BrokerWriteSockObject(b, _galio_create_sock_object(o));
  } else {
    return -1;
  }
}

/* Specialized functions. */

int GalIO_BrokerWriteFrame(GalIO_BrokerStruct *b, Nframe frame)
{
  if (b && b->out_b && (b->status != DESTROYED))
    return(__GalIO_BrokerWriteSockObject(b, _galio_create_msg_sock_object(frame, GAL_OBJECT_MSG_TYPE)));
  return -1;
}

int GalIO_BrokerWriteString(GalIO_BrokerStruct *b, char *str)
{
  if (b && b->out_b && (b->status != DESTROYED))
    return(__GalIO_BrokerWriteSockObject(b, _galio_create_typed_sock_object(GAL_STRING, (void *) str, 1)));
  return -1;
}

int GalIO_BrokerWriteInt(GalIO_BrokerStruct *b, int i)
{
  if (b && b->out_b && (b->status != DESTROYED))
    return(__GalIO_BrokerWriteSockObject(b, _galio_create_typed_sock_object(GAL_INT, (void *) i, 1)));
  return -1;
}

int GalIO_BrokerWriteFloat(GalIO_BrokerStruct *b, float f)
{
  if (b && b->out_b && (b->status != DESTROYED)) {
    float *fptr = (float *) calloc(1, sizeof(float));
    int res;
    
    *fptr = f;
    res = __GalIO_BrokerWriteSockObject(b, _galio_create_typed_sock_object(GAL_FLOAT, (void *) fptr, 1));
    free(fptr);
    return res;
  }  
  return -1;
}

int GalIO_BrokerWriteList(GalIO_BrokerStruct *b, Gal_Object *elts, int n_elts)
{
  if (b && b->out_b && (b->status != DESTROYED))
    return(__GalIO_BrokerWriteSockObject(b, _galio_create_typed_sock_object(GAL_LIST, (void *) elts, n_elts)));
  return -1;
}

int GalIO_BrokerWriteBinary(GalIO_BrokerStruct *b, void *data, int n_bytes)
{
  if (b && b->out_b && (b->status != DESTROYED))
    return(__GalIO_BrokerWriteSockObject(b, _galio_create_typed_sock_object(GAL_BINARY, data, n_bytes)));
  return -1;
}

int GalIO_BrokerWriteInt16(GalIO_BrokerStruct *b, void *data, int n_ints)
{
  if (b && b->out_b && (b->status != DESTROYED))
    return(__GalIO_BrokerWriteSockObject(b, _galio_create_typed_sock_object(GAL_INT_16, data, n_ints)));
  return -1;
}

int GalIO_BrokerWriteInt32(GalIO_BrokerStruct *b, void *data, int n_ints)
{
  if (b && b->out_b && (b->status != DESTROYED))
    return(__GalIO_BrokerWriteSockObject(b, _galio_create_typed_sock_object(GAL_INT_32, data, n_ints)));
  return -1;
}

int GalIO_BrokerWriteInt64(GalIO_BrokerStruct *b, void *data, int n_ints)
{
  if (b && b->out_b && (b->status != DESTROYED))
    return(__GalIO_BrokerWriteSockObject(b, _galio_create_typed_sock_object(GAL_INT_64, data, n_ints)));
  return -1;
}

int GalIO_BrokerWriteFloat32(GalIO_BrokerStruct *b, void *data, int n_floats)
{
  if (b && b->out_b && (b->status != DESTROYED))
    return(__GalIO_BrokerWriteSockObject(b, _galio_create_typed_sock_object(GAL_FLOAT_32, data, n_floats)));
  return -1;
}

int GalIO_BrokerWriteFloat64(GalIO_BrokerStruct *b, void *data, int n_floats)
{
  if (b && b->out_b && (b->status != DESTROYED))
    return(__GalIO_BrokerWriteSockObject(b, _galio_create_typed_sock_object(GAL_FLOAT_64, data, n_floats)));
  return -1;
}

static int send_eot(GalIO_BrokerStruct *b)
{
  if (b && b->out_b && (b->status != DESTROYED))
    return(__GalIO_BrokerWriteSockObject(b, _GalIO_CreateEOTSockObject()));
  return -1;
}

/* mark when done with broker struct */

static void __GalIO_BrokerDataSuccess(GalIO_BrokerStruct *b)
{
  if (!b->done) {
    /* This should never be reached if there
       was an error; the failure should have been marked
       and done should have been set. */
    _GalIO_QueueApply(b->data_done_callbacks,
		      __galio_apply_broker_callback,
		      (void *) NULL);
  }
  b->done = 1;
}

static void __GalIO_BrokerDataFailure(GalIO_BrokerStruct *b)
{
  /* Read error, mark as done and run abort callbacks.
     See __GalI_BrokerDataSuccess. */
  if (!b->done) {
    _GalIO_QueueApply(b->abort_callbacks,
		      __galio_apply_broker_callback,
		      (void *) NULL);
  }
  b->done = 1;
}

void GalIO_BrokerDataDone(GalIO_BrokerStruct *b)
{
  if (b)
  {
    if (b->status != DESTROYED) {
      __GalIO_BrokerDataSuccess(b);
    }
    if (b->status == DESTROYED) {      
      if (b->out_b) free(b->out_b);
      if (b->in_b) free(b->in_b);
      free(b);
    }
  }
}

int GalIO_BrokerIsDone(GalIO_BrokerStruct *b)
{
  return b->done;
}

/* mark when done sending data. Make sure you can call this
   more than once and it won't do any harm. */

void GalIO_BrokerDataOutDone(GalIO_BrokerStruct *b)
{
  if (!b->done)
    send_eot(b);
  GalIO_BrokerDataDone(b);
}
    
/*
 *  broker outgoing data
 */

/* SAM 8/15/00: I need to encapsulate all the relevant behavior
   BESIDES the task startup as a function, to access from the
   Python bindings. This will also be valuable for C implementations
   with their own main loops. */

/* GalIO_BrokerDataOutCallbackHandler() 
   returns 1 if the broker is done and has been destroyed,
   0 if not done */

int GalIO_BrokerDataOutCallbackHandler(GalIO_BrokerStruct *b)
{
  int res;

  res = GalIO_BrokerDataOutHandler(b);

  switch (res) {
  case 1:
    /* done, destroy broker struct */
    GalUtil_PrintWithLocation(GAL_WARNING_LEVEL, __FUNCTION__, "Done sending data from out broker\n");
    if (b->task_removal_callback) {
      GalIO_RemoveBrokerCallback(b, b->task_removal_callback);
    }
    GalIO_DestroyBrokerStruct(b);
    break;
  case 0:
    /* Continue. */
    break;
  default:
    GalUtil_WarnWithLocation(__FUNCTION__, "Unexpected value %d returned from out broker handler", res);
    break;
  }
  return res;
}

/* and now the internal timed task polling function. */

static void
poll_broker_data_out(Gal_TaskPkg *p)
{
  GalIO_BrokerStruct *b = (GalIO_BrokerStruct *) Gal_TaskPkgData(p);
  int res;

  res = GalIO_BrokerDataOutCallbackHandler(b);

  switch (res) {
  case 1:
    /* done (broker destroyed), stop polling */
    return;
  case 0:
    /* continue polling */
    if (b->poll_ms > 0) {
      Gal_ReAddTask(p, (void *) b, b->poll_ms, 0, NULL);
    }
    return;
  case -1:
    /* error, broker destroyed */
    return;
  default:
    return;
  }
}

/* Probing broker properties. */

unsigned short GalIO_GetBrokerListenPort(GalIO_BrokerStruct *b)
{
  if (b && b->out_b && b->out_b->server) {
    return GalIO_GetServerListenPort(b->out_b->server);
  } else {
    return 0;
  }
}

char *GalIO_GetBrokerCallID(GalIO_BrokerStruct *b)
{
  if (b && b->out_b) {
    return b->out_b->call_id;
  } else {
    return (char *) NULL;
  }
}

void GalIO_BrokerPopulateFrame(GalIO_BrokerStruct *b,
			       Gal_Frame f,
			       const char *host_key,
			       const char *port_key)
{  
  if (b && b->out_b && f) {
    Gal_SetProp(f, host_key, Gal_StringObject(GalIO_IPAddress()));
    Gal_SetProp(f, port_key,
		Gal_IntObject(GalIO_GetBrokerListenPort(b)));
    GalIO_FrameSetBrokerCallID(f, b->out_b->call_id);
  }
}

GalIO_Callback *GalIO_AddBrokerCallback(GalIO_BrokerStruct *b,
					int callback_event,
					GalIO_BrokerCallbackFn fn,
					void *callback_data)
{
  GalIO_Callback *s = (GalIO_Callback *) malloc(sizeof(GalIO_Callback));
  s->fn = (void *) fn;
  s->callback_data = callback_data;
  s->callback_type = callback_event;
  s->callback_host = (void *) b;
    
  switch (callback_event) {
  case GAL_BROKER_DATA_DONE_EVENT:
    _GalIO_QueueEnqueue(b->data_done_callbacks, (void *) s);
    break;
  case GAL_BROKER_ABORT_EVENT:
    _GalIO_QueueEnqueue(b->abort_callbacks, (void *) s);
    break;
  case GAL_BROKER_DESTRUCTION_EVENT:
    _GalIO_QueueEnqueue(b->destroy_callbacks, (void *) s);
    break;
  case GAL_BROKER_CONNECTION_EVENT:
    _GalIO_QueueEnqueue(b->connect_callbacks, (void *) s);
    break;
  default:
    free(s);
    GalUtil_WarnWithLocation(__FUNCTION__, "Can't record broker callback for unknown event type\n");
    return (GalIO_Callback *) NULL;
  }
  return s;
}

void GalIO_RemoveBrokerCallback(GalIO_BrokerStruct *b,
				GalIO_Callback *cb)
{
  switch (cb->callback_type) {
  case GAL_BROKER_DATA_DONE_EVENT:
    _GalIO_QueueDequeue(b->data_done_callbacks, (void *) cb);
    break;
  case GAL_BROKER_ABORT_EVENT:
    _GalIO_QueueDequeue(b->abort_callbacks, (void *) cb);
    break;
  case GAL_BROKER_DESTRUCTION_EVENT:
    _GalIO_QueueDequeue(b->destroy_callbacks, (void *) cb);
    break;
  case GAL_BROKER_CONNECTION_EVENT:
    _GalIO_QueueDequeue(b->connect_callbacks, (void *) cb);
    break;
  default:
    GalUtil_WarnWithLocation(__FUNCTION__, "Can't remove broker callback for unknown event type\n");
    break;
  }
  free(cb);
}

/* SAM 9/28/00: Trying to set up brokers to use the
   listener their connection is associated with. The poll
   is attached to the connection in hub_server.c. */

static void __galio_broker_task_shutdown(GalIO_BrokerStruct *b,
					 void *callback_data)
{
  Gal_TaskPkg *p = (Gal_TaskPkg *) callback_data;

  Gal_RemoveTask(p);
}

void _galio_create_broker_out_poll(GalIO_CommStruct *gcomm,
				   GalIO_BrokerStruct *b,
				   void *caller_data)
{
  Gal_TaskPkg *p;
  GalIO_Callback *c;

  if (b->poll_ms > 0) {
    int success = 0;
    
    p = Gal_AddTask(poll_broker_data_out, (void *) b, -1, 0, NULL);
    if (p) {
      c = GalIO_AddBrokerCallback(b, GAL_BROKER_DESTRUCTION_EVENT,
				  __galio_broker_task_shutdown,
				  (void *) p);

      /* This is the particular shutdown callback which
	 removes the timed task. If in the threaded case,
	 I let it run, it will shut down the thread
	 WHILE THE DESTROY IS UNDERWAY. So we will
	 remove this before we call the destroy function. */
      b->task_removal_callback = c;
    
      if (Gal_StartTask(p, b->poll_ms) == 1) {
	/* If there's no task, then we need to remove
	   a bunch of stuff. */
	GalIO_RemoveBrokerCallback(b, c);
	b->task_removal_callback = (GalIO_Callback *) NULL;
      } else {
	success = 1;
      }
    }
    if (!success) {
      GalUtil_Warn("Failed to set up requested poll for out broker");
    }
  }
}

/* The same queue contains both creation and startup callbacks. */

static int __galio_apply_broker_creation_callback(void *data,
						  void *caller_data)
{
  GalIO_Callback *cb = (GalIO_Callback *) data;

  if ((cb->callback_type == GAL_CONNECTION_BROKER_OUT_CREATION_EVENT) ||
      (cb->callback_type == GAL_CONNECTION_BROKER_IN_CREATION_EVENT)) {
    GalIO_ConnectionBrokerCallbackFn fn = (GalIO_ConnectionBrokerCallbackFn) cb->fn;
    GalIO_CommStruct *gcomm = (GalIO_CommStruct *) cb->callback_host;
    GalIO_BrokerStruct *b = (GalIO_BrokerStruct *) caller_data;
  
    (*fn)(gcomm, b, cb->callback_data);
  }
  /* Continue. */    
  return 1;
}

static int __galio_apply_broker_startup_callback(void *data,
						 void *caller_data)
{
  GalIO_Callback *cb = (GalIO_Callback *) data;

  if ((cb->callback_type == GAL_CONNECTION_BROKER_OUT_STARTUP_EVENT) ||
      (cb->callback_type == GAL_CONNECTION_BROKER_IN_STARTUP_EVENT)) {
    GalIO_ConnectionBrokerCallbackFn fn = (GalIO_ConnectionBrokerCallbackFn) cb->fn;
    GalIO_CommStruct *gcomm = (GalIO_CommStruct *) cb->callback_host;
    GalIO_BrokerStruct *b = (GalIO_BrokerStruct *) caller_data;
  
    (*fn)(gcomm, b, cb->callback_data);
  }
  /* Continue. */    
  return 1;
}

void GalIO_CommStartBroker(GalIO_CommStruct *gcomm,
			   GalIO_BrokerStruct *b, int poll_ms)
{
  if (b->poll_ms == -1) {
    if (poll_ms > 0)
      b->poll_ms = poll_ms;
    else if (poll_ms == 0)
      b->poll_ms = GAL_DEFAULT_POLL_MS;
    else
      b->poll_ms = -1;
  
    /* Run the startup callbacks. */
    if (b->poll_ms > -1) {
      if (b->out_b) {
	/* It's an outgoing broker. */
	if (gcomm)
	  _GalIO_QueueApply(gcomm->broker_out_callbacks,
			    __galio_apply_broker_startup_callback,
			    (void *) b);
      }
      if (b->in_b) {
	/* It's an incoming broker. */
	if (gcomm) {
	  _GalIO_QueueApply(gcomm->broker_in_callbacks,
			    __galio_apply_broker_startup_callback,
			    (void *) b);
	} else {
	  _galio_create_broker_in_poll((GalIO_CommStruct *) NULL,
				       b, (void *) NULL);
	}
      }
    }
  }
}

GalIO_BrokerStruct *
GalIO_BrokerDataOutInit(GalIO_CommStruct *gcomm,
			int poll_ms, int timeout_seconds)
{
  GalIO_BrokerStruct *b;
  GalIO_ServerStruct *scomm;

  /* First, let's make sure we can turn on the listener. */

  /* There has to be a connection... */
  if (!gcomm) {
    return (GalIO_BrokerStruct *) NULL;
  }

  /* And that connection has to have a server... */
  scomm = gcomm->server;
  if (!scomm) {
    return (GalIO_BrokerStruct *) NULL;
  }

  if (GalIO_StartListener(scomm, GAL_BROKER_LISTENER) == -1) {
    return (GalIO_BrokerStruct *) NULL;
  }

  b = NewBrokerStruct();
  if (b) {
    b->out_b = NewOutgoingBroker(scomm, timeout_seconds);
    /* Add the broker to the broker list. */
    _GalIO_ServerAddOutgoingBroker(scomm, b);
    
    /* Run the broker creation callbacks. */
    _GalIO_QueueApply(gcomm->broker_out_callbacks,
		      __galio_apply_broker_creation_callback,
		      (void *) b);
    b->caller_data = NULL;

    b->poll_ms = -1;
    
    GalIO_CommStartBroker(gcomm, b, poll_ms);
    
    return(b);
  } else {
    GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't create out broker");
    return NULL;
  }
}

/* This function is called from GalIO_ServerListenerHandshakeHandler()
   in hub_server.c. */

typedef struct __galio_broker_searcher {
  GalIO_BrokerStruct *b;
  Gal_Frame input;
} __galio_broker_searcher;

/* The fn return 1 (continue), 0 (halt), -1 (dequeue). */

static int __galio_match_broker(void *arg, void *caller_data)
{
  __galio_broker_searcher *bs = (__galio_broker_searcher *) caller_data;
  Gal_Frame input = bs->input;
  GalIO_BrokerStruct *broker = (GalIO_BrokerStruct *) arg;

  if (match_broker_call_id(broker->out_b->call_id, input)) {
    bs->b = broker;
    return 0;
  } else {
    return 1;
  }
}

int _GalIO_BrokerListenerHandshakeHandler(GalIO_ServerStruct *scomm,
					  GalIO_CommStruct *gcomm,
					  Gal_Frame input)
{
  __galio_broker_searcher bs;
  GalIO_BrokerStruct *out_broker;
  Gal_Frame reply;

  bs.input = input;
  bs.b = (GalIO_BrokerStruct *) NULL;

  _GalIO_QueueApply(scomm->brokers, __galio_match_broker, (void *) &bs);
  out_broker = bs.b;
  
  /* At this point, either broker_host is NULL and
     out_broker is NULL, or I found a broker host and
     an actual broker. */
  if (!out_broker) {
    GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't find matching out broker for incoming request %s", Gal_GetString(input, GAL_BROKER_CALL_ID_FRAME_KEY));
    reply = GalIO_CreateErrorFrame(GAL_CONN_REJECTION_ERROR,
				   "no match for broker");
    GalIO_CommWriteMessage(gcomm, reply, GAL_ERROR_MSG_TYPE, 1);
    Gal_FreeFrame(reply);
    return -1;
  } else if (out_broker->status != IN_USE) {
    /* Next, check for expiration. */
    GalUtil_WarnWithLocation(__FUNCTION__, "Found matching out broker for incoming request %s, but broker is expired", Gal_GetString(input, GAL_BROKER_CALL_ID_FRAME_KEY));
    reply = GalIO_CreateErrorFrame(GAL_CONN_REJECTION_ERROR,
				   "broker expired");    
    GalIO_CommWriteMessage(gcomm, reply, GAL_ERROR_MSG_TYPE, 1);
    Gal_FreeFrame(reply);
    return -1;
  } else {
    /* If we actually matched the reference frame, we install
       the connection in the broker and we keep going. */
    GalUtil_PInfo1WithLocation(__FUNCTION__, "Found matching out broker for incoming request %s\n", Gal_GetString(input, GAL_BROKER_CALL_ID_FRAME_KEY));
    _GalIO_QueueEnqueue(out_broker->out_b->comm_queue, (void *) gcomm);

    /* For God's sake, send back the reply before you turn
       the connection into a shadow and enqueue the outgoing material. */
    Gal_SetProp(input, GAL_PROTOCOL_VERSION_FRAME_KEY,
		Gal_IntObject(GAL_TRANSPORT_PROTOCOL_VERSION));
       
    GalIO_CommWriteMessage(gcomm, input, GAL_REPLY_MSG_TYPE, 1);

    /* The queue should now be empty. I will clear it, so
       that any memory which is in there is freed before I
       turn off memory management. After that, I populate the
       outbound side with the stuff to be sent. */
    _GalIO_FlushSockObjectOutQueue(gcomm->queue);
    _GalIO_FlushSockObjectInQueue(gcomm->queue);
    
    /* Make sure the outbound connection queue is a "shadow",
       so I can enqueue data to the broker queue and
       also to the others and be sure it won't be freed
       except when I free the broker queue. */
    gcomm->queue->manage_memory = 0;

    /* Add all the stuff in the broker queue to the
       new queue. */
    _GalIO_SockObjectQueueImport(gcomm->queue, out_broker->out_b->queue);
    /* Now, call the connect callbacks. */
    _GalIO_QueueApply(out_broker->connect_callbacks,
		      __galio_apply_broker_callback,
		      (void *) NULL);
    return 0;
  }
}

/* The server listener takes care of the acceptances, the
   handshake handler takes care of the handshake, the broker
   write functions take care of enqueueing the data. All this function
   needs to do is look through all the connections and
   call OutHandler on them. */

/* Mapping function for out handler. -1 means remove the item
   from the queue, 1 means continue. */

/* SAM 11/10/00: It appears that the only way to tell if a connection
   has been shutdown is to try to write to it or read from it;
   polling is no good. So if there's nothing left to write, you'll
   never know if this connection has died, unless you try to
   read from it. Since I really do need to know when the connection
   has died, and since this is strictly an outbound connection, I
   should try to read from it to see if I get nothing. */

static int __galio_poll_write_sock_for_shutdown(GAL_SOCKET sock)
{
  fd_set read_fdset;
  struct timeval tv;
  int interrupted = 0;
    
  tv.tv_sec = 0;
  tv.tv_usec = 0;
  FD_ZERO(&read_fdset);
  FD_SET(sock, &read_fdset);
  return GalUtil_SockSelect(sock + 1, &read_fdset, NULL, NULL,
			    &tv, &interrupted);
  /* Status will be -1 if there's an error (which means something's
     wrong) or 1 if it's readable (which means it's disconnected). */
}

static int __galio_handle_out_broker_connections(void *arg, void *caller_data)
{
  GalIO_CommStruct *gcomm = (GalIO_CommStruct *) arg;
  int res;

  if (_GalIO_SockObjectQueueOutNonEmpty(gcomm->queue)) {
    res = GalIO_OutHandler(gcomm);
  } else {
    /* Check to see if the connection is readable. If it is,
       it must be because the other end disconnected, since the
       other end is only reading. */
    res = __galio_poll_write_sock_for_shutdown(gcomm->sock);
  }

  
  switch (res) {
  case -1:
  case 1:
    /* error or done, dequeue. */
    GalIO_SetCommDone(gcomm);
    GalIO_DestroyCommStruct(gcomm);
    return -1;
  default:
    return 1;
  }
}

/* GalIO_BrokerDataOutHandler().
   I use the OutHandler()
   for each connection. This should always succeed, returning 1
   if the broker is done and the timer has expired, 0 otherwise.
   Actually, this should return 1 when it's ready to be destroyed.
   That means that either it's already marked for destruction, or
   the broker has expired and there are no more connections. */
  
int
GalIO_BrokerDataOutHandler(GalIO_BrokerStruct *b)
{
  struct timeval now;

  if (!b->out_b) {
    return 0;
  }
  _GalIO_QueueApply(b->out_b->comm_queue,
		    __galio_handle_out_broker_connections,
		    (void *) NULL);

  /* Once it's expired, you can't write anything to it, either. */
  if (b->status == IN_USE) {
    /* If we haven't expired yet, check the time. */
    _gal_gettimeofday(&now);

    /* If the current seconds are after the expiration time,
       or they're equal and the usecs are after, then we've
       expired. If there's no expiration time, never expire. */    
    if (b->out_b->expiration_time &&
	((now.tv_sec > b->out_b->expiration_time->tv_sec) ||
	 ((now.tv_sec == b->out_b->expiration_time->tv_sec) &&
	  (now.tv_usec > b->out_b->expiration_time->tv_usec)))) {
      /* Current time is past expiration time. Don't let anyone
	 else connect. So don't change the status to DESTROYED yet.
	 In fact, you can't really use the done flag either. */
      b->status = TIMED_OUT;
    }
  }
  if (b->done &&
      ((b->status == DESTROYED) ||
       (b->status == TIMED_OUT &&
	(!_GalIO_QueueNonEmpty(b->out_b->comm_queue))))) {
    return 1;
  } else {
    return 0;
  }
}

void GalIO_ForceBrokerExpiration(GalIO_BrokerStruct *b)
{
  if (b->out_b && (b->status == IN_USE)) {
    /* Don't even check the time, just mark it as timed out. */
    b->status = TIMED_OUT;
  }
}

/*
 *  retrieve incoming data
 */

/* SAM 8/15/00: I need to encapsulate all the relevant behavior
   BESIDES the task startup as a function, to access from the
   Python bindings. This will also be valuable for C implementations
   with their own main loops. */

/* GalIO_BrokerInCallbackHandler() 
   returns 1 if the broker is done and has been destroyed,
   0 if not done, -1 if error was encountered and the broker
   has been destroyed. */

int GalIO_BrokerDataInCallbackHandler(GalIO_BrokerStruct *b,
				      int read_blocking)
{
  int res;

  if (!b->in_b) {
    return 0;
  }
  
  b->in_b->read_blocking = read_blocking;
  b->in_b->gcomm->queue->queue->read_blocking = read_blocking;

  res = GalIO_BrokerDataInHandler(b);

  switch (res) {
  case 1:
    /* done, destroy broker */
    GalUtil_PrintWithLocation(GAL_WARNING_LEVEL, __FUNCTION__, "In broker done receiving data\n");
    if (b->task_removal_callback) {
      GalIO_RemoveBrokerCallback(b, b->task_removal_callback);
    }
    GalIO_DestroyBrokerStruct(b);
    break;
  case 0:
    break;
  case -1:
    /* error, destroy broker */
    if (b->task_removal_callback) {
      GalIO_RemoveBrokerCallback(b, b->task_removal_callback);
    }
    GalIO_BrokerDataDone(b);
    GalIO_DestroyBrokerStruct(b);
    break;
  default:
    break;
  }
  return res;
}

/* And here's the internal timed task poll function. */

static void
poll_broker_data_in(Gal_TaskPkg *p)
{
  int res;
  GalIO_BrokerStruct *b = (GalIO_BrokerStruct *) Gal_TaskPkgData(p);

  res = GalIO_BrokerDataInCallbackHandler(b, Gal_TaskPkgBlocking(p));

  switch (res) {
  case 1:
    /* done (broker destroyed), stop polling */
    return;
  case 0:
    /* not done, continue polling */
    if (b->poll_ms > 0) {
      Gal_ReAddTaskWithSocketIO(p, (void *) b, b->poll_ms, 1,
				&(b->in_b->gcomm->sock), (GAL_SOCKET *) NULL, NULL);
    } else {
      Gal_ReAddTaskWithSocketIO(p, (void *) b, b->poll_ms, 0,
				&(b->in_b->gcomm->sock), (GAL_SOCKET *) NULL, NULL);
    }
    return;
  case -1:
    /* error (broker destroyed), stop polling */
  default:    
    return;
  }
}

/* See the handshake discussion at the end of hub_server.c. */

static Gal_Frame GalIO_BrokerInitiationFrame(Gal_Frame call_id_container)
{
  Gal_Frame f = Gal_MakeFrame("handshake", GAL_CLAUSE);
  
  Gal_SetProp(f, GAL_CONNECTION_TYPE_FRAME_KEY,
	      Gal_IntObject(GAL_BROKER_LISTENER));
  Gal_SetProp(f, GAL_BROKER_CALL_ID_FRAME_KEY,
	      Gal_CopyObject(Gal_GetObject(call_id_container,
					   GAL_BROKER_CALL_ID_FRAME_KEY)));
  Gal_SetProp(f, GAL_PROTOCOL_VERSION_FRAME_KEY,
	      Gal_IntObject(GAL_TRANSPORT_PROTOCOL_VERSION));
  return f;
}

static int GalIO_BrokerClientHandshakeHandler(GalIO_CommStruct *gcomm,
					      Gal_Frame call_id_container)
{
  Gal_Frame f = GalIO_BrokerInitiationFrame(call_id_container);
  Gal_Frame reply_f;
  GalIO_MsgType t;
  
  reply_f = GalIO_CommDispatchFrame(gcomm, f, &t, (char *) NULL);
  Gal_FreeFrame(f);
  Gal_FreeFrame(reply_f);

  if (t == GAL_ERROR_MSG_TYPE) {
    return -1;
  }
  return 0;
}

/* This reader is used by connections which brokers are
   holding. -1 is error, 0 is nothing to do, 1 is something to do. */

static int __GalIO_BrokerReadHandler(GalIO_CommStruct *gcomm)
{
  GalIO_BrokerStruct *b = (GalIO_BrokerStruct *) GalIO_GetCommData(gcomm);
  int status = 0;
  void *object;
  Gal_ObjectType object_type;
  int object_count;

  if (!b->in_b) {
    return 0;
  }
  
  if (b->in_b->active) {
    /* read object and call data handler */
    status = _galio_read_object_from_sockqueue(gcomm->queue, &object, &object_type, &object_count, (GalIO_MsgType *) NULL);
    if (status == 1) {
      (*(b->in_b->handler))(b, object, object_type, object_count);
    } else if ((status == 0) && gcomm->queue->done) {
      /* If the queue is empty and also marked as done, then
	 set the broker to be done, and run the data done callbacks. */
      __GalIO_BrokerDataSuccess(b);
    } else if (status == -1) {
      __GalIO_BrokerDataFailure(b);
    }
  } else if (gcomm->queue->done) {
    status = 0;
  } else if (!b->in_b->active) {
    /* Don't wait forever for a non-active handler to become active.
       This problem arises if you're using broker queues, and
       data has been read into the broker in GalIO_InHandler.
       The broker has nothing to do, since it's not active.
       If it were to report that it has something to do, then
       it would loop forever in GalIO_InHandler if the sender
       were to die without sending the done message.
     */
    status = 0;
  } else if (_GalIO_SockObjectQueueInNonEmpty(gcomm->queue)) {
    status = 1;
  }
  return status;
}

extern void __GalIO_IDestroyCommStruct(GalIO_CommStruct *gcomm,
				       int mutex_locked);
extern int _GalIO_ClientContactHandler(GalIO_CommStruct *gcomm, int silent);

void _galio_create_broker_in_poll(GalIO_CommStruct *gcomm,
				  GalIO_BrokerStruct *b,
				  void *caller_data)
{
  Gal_TaskPkg *p;
  GalIO_Callback *c;

  if (b->poll_ms > 0) {
    int success = 0;
    
    p = Gal_AddTask(poll_broker_data_in, (void *) b, -1, 0, NULL);
    if (p) {
      c = GalIO_AddBrokerCallback(b, GAL_BROKER_DESTRUCTION_EVENT,
				  __galio_broker_task_shutdown,
				  (void *) p);
    
      /* This is the particular shutdown callback which
	 removes the timed task. If in the threaded case,
	 I let it run, it will shut down the thread
	 WHILE THE DESTROY IS UNDERWAY. So we will
	 remove this before we call the destroy function. */
      b->task_removal_callback = c;

      /* Make sure everything is set up, then start the task. */
      if (Gal_StartTask(p, b->poll_ms) == 1) {
	GalIO_RemoveBrokerCallback(b, c);
	b->task_removal_callback = (GalIO_Callback *) NULL;
      } else {
	success = 1;
      }
    }
    if (!success) {
      GalUtil_Warn("Failed to set up requested poll for in broker");
    }
  }
}

/* SAM 10/24/00: Backward compatibility. I am cleaning up the callback
   and data model, but it's not the sort of thing that everyone
   will care about. I will recommend that folks upgrade, but I
   can't really insist. */

GalIO_BrokerStruct *
GalIO_CommBrokerDataInInit(GalIO_CommStruct *host_gcomm,
			   const char *host, unsigned short port,
			   Gal_Frame frame,
			   GalIO_BrokerDataHandler fnptr,
			   int poll_ms,
			   void *caller_data, void (*caller_data_free_fn)(void *))
{
  GalIO_BrokerStruct *b;
  GalIO_CommStruct *gcomm;

  if (!fnptr)
    return NULL;

  if (!host)
    return NULL;

  /* First thing we do is the handshake. If it fails, then we don't
     even bother. */

  gcomm = GalIO_ClientInit(host, port, (GalIO_FrameHandler) NULL, -1);

  /* The handshake for the broker has two steps, just the
     connection and the handshake. This is the client side. */
  if (_GalIO_ClientContactHandler(gcomm, 1) == -1) {
    __GalIO_IDestroyCommStruct(gcomm, 0);
    return (GalIO_BrokerStruct *) NULL;
  }
  if (GalIO_BrokerClientHandshakeHandler(gcomm, frame) == -1) {    
    __GalIO_IDestroyCommStruct(gcomm, 0);
    return (GalIO_BrokerStruct *) NULL;
  }

  b = NewBrokerStruct();
  if (b) {    
    gcomm->read_handler = __GalIO_BrokerReadHandler;
    GalIO_SetCommData(gcomm, (void *) b, NULL);
    b->in_b = NewIncomingBroker(gcomm, frame, fnptr);
    b->caller_data = caller_data;
    b->caller_data_free_fn = caller_data_free_fn;
    
    /* Anyone who embeds broker calls should either pass in
       the connection object (which would host the callbacks that
       are set) or do it all by hand after the broker is set up.
       The default main loop does the "right thing" here, no
       matter which option is chosen. See __galio_create_connection_poll. */
    /* Run the broker creation callbacks. */
    if (host_gcomm) {
      _GalIO_QueueApply(host_gcomm->broker_in_callbacks,
			__galio_apply_broker_creation_callback,
			(void *) b);
      b->in_b->host_gcomm = host_gcomm;
      _GalIO_CommLock(host_gcomm);
    }
    
    b->poll_ms = -1;
    
    GalIO_CommStartBroker(host_gcomm, b, poll_ms);

  } else {
    __GalIO_IDestroyCommStruct(gcomm, 0);
  }
  
  return(b);
}

GalIO_BrokerStruct *
GalIO_BrokerDataInInit(const char *host, unsigned short port, Gal_Frame frame,
		       GalIO_BrokerDataHandler fnptr, void *refptr, int poll_ms)
{
  return GalIO_CommBrokerDataInInit((GalIO_CommStruct *) NULL,
				    host, port, frame, fnptr, poll_ms,
				    refptr, NULL);
}


/* contact a server on a host and port to get data */
/* returns 1 if done, 0 if not done, -1 if error */

int
GalIO_BrokerDataInHandler(GalIO_BrokerStruct *b)
{
  int res;
  
  if (!b->in_b) {
    return 1;
  }
  res = GalIO_InHandler(b->in_b->gcomm);
  /* The in handler first reads in everything it
     can read, and then processes the result. If
     the connection dies BEFORE it reads anything, then
     the abort callbacks will never be called, because
     it's the connection handler called by GalIO_InHandler which
     handles the abort callbacks. So we need to do some
     cleanup here. */
  if (res < 0) {
    __GalIO_BrokerDataFailure(b);
  }
  return res;
}

/* returns true if there's more stuff in the queue to be read.
   Note that this means stuff that's already been read from I/O but waiting in
   our internal queue to be processed. And like sockets, you want it to
   return 1 when it's done. */

int GalIO_BrokerReadReady(GalIO_BrokerStruct *b)
{
  return (b && (b->status == IN_USE) && b->in_b && b->in_b->gcomm &&
	  (_GalIO_SockObjectQueueInNonEmpty(b->in_b->gcomm->queue) ||
	   b->in_b->gcomm->queue->done));
}

/* returns true if there's more stuff in the queue to be written.
   Note that this means stuff that's already been written to I/O but waiting in
   our internal queue to be processed. */

/* SAM 11/10/00: It appears that the only way to tell if a connection
   has been shutdown is to try to write to it or read from it;
   see __galio_poll_write_sock_for_shutdown above. Since this
   function is called by the bindings to check to see if the
   handler should be called, it had better check for shutdown
   as well. */

static int __galio_poll_outbound_broker(void *arg, void *caller_data)
{
  GalIO_CommStruct *gcomm = (GalIO_CommStruct *) arg;
  int *found_ptr = (int *) caller_data;

  if (_GalIO_SockObjectQueueOutNonEmpty(gcomm->queue)) {
    *found_ptr = 1;
    /* Stop. */
    return 0;
  } else {
    /* Check to see if the connection is readable. If it is,
       it must be because the other end disconnected, since the
       other end is only reading. */
    if (__galio_poll_write_sock_for_shutdown(gcomm->sock)) {
      /* Zero means not readable, which means no disconnect. */
      *found_ptr = 1;
      /* Stop. */
      return 0;
    }
  }
  return 1;
}

int GalIO_BrokerWriteReady(GalIO_BrokerStruct *b)
{
  int found = 0;
  
  if (b && (b->status != DESTROYED) && b->out_b) {
    if (_GalIO_SockObjectQueueOutNonEmpty(b->out_b->queue)) {
      return 1;
    } else {
      _GalIO_QueueApply(b->out_b->comm_queue,
			__galio_poll_outbound_broker,
			(void *) &found);
      if (found)
	return 1;
      else
	return 0;
    }
  } else {
    return 0;
  }
}


/*
 *   BrokerStruct queue
 */

/* append a broker struct to a queue and return the queue */

GalIO_BrokerStruct *GalIO_BrokerStructQueueAppend(GalIO_BrokerStruct *b, GalIO_BrokerStruct *bqueue)
{
  GalIO_BrokerStruct *qptr;

  if (b)
  {
    b->queued = 1;

    if (bqueue == NULL)
    {
      b->prev = NULL;
      b->next = NULL;
      return b;
    }

    qptr = bqueue;
    while (qptr)
    {
      /* no entry should appear more than once in the queue */
      if (qptr == b)
      {
	GalUtil_WarnWithLocation(__FUNCTION__, "Broker %x already in queue\n", b);
	break;
      }
      /* append new entry at the end of the queue */
      if (qptr->next == NULL)
      {
	b->prev = qptr;
	b->next = NULL;
	qptr->next = b;
	break;
      }
      /* check next entry in the queue */
      qptr = qptr->next;
    }
  }
  return bqueue;
}

/* remove a broker struct from the head of a queue and return the queue */

GalIO_BrokerStruct *GalIO_BrokerStructQueuePop(GalIO_BrokerStruct *bqueue)
{
  GalIO_BrokerStruct *qptr, *next_qptr;

  qptr = bqueue;

  if (!qptr)
    return qptr;

  qptr->queued = 0;
  next_qptr = qptr->next;
  if (next_qptr)
    next_qptr->prev = (GalIO_BrokerStruct *) NULL;
  GalIO_DestroyBrokerStruct(qptr);

  return next_qptr;
}

/*  dequeue a broker struct and return the queue */

GalIO_BrokerStruct *GalIO_BrokerStructDequeue(GalIO_BrokerStruct *b, GalIO_BrokerStruct *bqueue)
{
  GalIO_BrokerStruct *qptr;
  GalIO_BrokerStruct *new_head = bqueue;

  if (b && bqueue && b->queued)
  {
    qptr = bqueue;

    while (qptr)
    {
      if (qptr == b)
      {
	GalIO_BrokerStruct *next, *prev;

	b->queued = 0;
	prev = b->prev;
	next = b->next;

	if (prev)
	  prev->next = next;
	else
	  new_head = next;

	if (next)
	  next->prev = prev;

	break;
      }
      qptr = qptr->next;
    }
  }
  return new_head;
}
