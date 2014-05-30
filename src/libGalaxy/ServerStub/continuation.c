/*
  This file (c) Copyright 1998 - 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* This file is special, in that it needs to see the innards of
   the connection object, because it's defining a special
   reader, etc. There will be an associated header file which
   hub_server will be able to look at. */

#include <stdio.h>
#include "galaxy/sysdep.h"

#include "continuation.h"

/* 
 * Continuations
 */

/* Continuations are "inspired" by the real notion of continuation,
   but since C can't do that, we can do something similar:
   send a new message, lock the
   environment, introduce a notion of a "postponed return", send
   it back to the Hub so that the Hub will be able to send
   the server new stuff, and when the Hub program ends and the
   reply message comes in, the continuation function is executed
   with the environment, the message reply, and a "state" which
   was saved with the continuation. At that point, the postponement
   flag is cleared and execution continues normally. */
    
extern int _GalIO_PopulateDispatchMessage(Gal_Frame dispatch,
					  char *reply_key);

GalIO_PointerQueue *_GalSS_CreateContinuationQueue()
{
  /* Create a continuation queue and give it its own mutex. */
  return _GalIO_NewPointerQueue(GAL_CONTINUATION_PTYPE,
				1, 1, NULL);
}

static void __GalSS_FreeContinuation(GalSS_Continuation *c)
{
  if (c->state && c->state_free_fn) {
    (*c->state_free_fn)(c->state);
  }
  free(c);
}

void _GalSS_FreeContinuationQueue(GalIO_PointerQueue *queue)
{
  GalSS_Continuation *c;
  
  if (queue) {
    c = (GalSS_Continuation *) _GalIO_QueuePop(queue);
    while (c) {
      GalSS_EnvUnlock(c->env);
      __GalSS_FreeContinuation(c);
      c = (GalSS_Continuation *) _GalIO_QueuePop(queue);
    }
  }
  _GalIO_QueueDestroy(queue);
}

int GalSS_EnvDispatchFrameWithContinuation(GalSS_Environment *env,
					   Gal_Frame frame,
					   GalSS_ContinuationFn fn,
					   void *continuation_state,
					   void (*continuation_state_free_fn)(void *))
{
  return GalSS_EnvDispatchFrameToProviderWithContinuation(env, frame,
							  (char *) NULL, fn,
							  continuation_state,
							  continuation_state_free_fn);
}

int GalSS_EnvDispatchFrameToProviderWithContinuation(GalSS_Environment *env,
						     Gal_Frame frame,
						     const char *provider,
						     GalSS_ContinuationFn fn,
						     void *continuation_state,
						     void (*continuation_state_free_fn)(void *))
{
  GalSS_Continuation *c;
  int server_tidx;
  GalIO_PointerQueue *c_queue;
  int status;
  
  /* If a return has already been satisfied, do nothing. */
  if (env->return_satisfied) {
    return -1;
  }

  /* If this connection hasn't been set up for continuations,
     do nothing. */
  c_queue = env->gcomm->continuations;

  if (!c_queue) {
    return -1;
  }
  
  /* Add the session info. Do this first, so that populating
     the dispatch message doesn't get overwritten. */
  _GalSS_EnvAdministerNewMessage(env, frame, provider);
    
  /* Set up the message to do a round trip. */
  server_tidx = _GalIO_PopulateDispatchMessage(frame, GAL_ROUND_TRIP_FRAME_KEY);
  
  /* The message is now ready to be sent. */
  status = GalIO_CommWriteFrame(env->gcomm, frame, 1);

  /* If the write didn't fail, store the continuation. */
  if (status != -1) {
    c = (GalSS_Continuation *) calloc(1, sizeof(GalSS_Continuation));
    /* Populate the continuation */
    /* SAM 11/21/00: Steve observes that if we allow session IDs
       to be updated, we'd better save away copies of the environment objects
       so that subsequent session ID updates don't affect the
       saved environment. GalSS_EnvCopy locks the environment too. */
    c->env = GalSS_EnvCopy(env);
    c->fn = fn;
    c->state = continuation_state;
    c->server_tidx = server_tidx;
    c->state_free_fn = continuation_state_free_fn;

    /* Postpone the result. */
    GalSS_EnvPostponeReply(env);

    /* Store the continuation. */
    _GalIO_QueueEnqueue(c_queue, (void *) c);
  }
  return status;
}
    
/* This is the function which handles firing the
   continuation. */

static int _GalSS_ContinuationHandler(GalSS_Continuation *c, Gal_Frame frame,
				      GalIO_MsgType msg_type)
{
  Gal_Frame result;
  GalSS_Environment *env = c->env;
  
  /* The environment was locked when it was stored, so there's
     no need to lock it now. It will, however, be unlocked
     when the handler exits. */

  /* The return_satisfied field will not be set, because
     it was set in the continuation establishment function
     only to make sure the real result wasn't set, and then
     the flag was unset in GalSS_FrameHandler. */

  /* Make sure you filter the unkosher response types,
     just like in GalIO_CommDispatchFrame(). */

  switch (msg_type) {
  case GAL_REPLY_MSG_TYPE:
  case GAL_ERROR_MSG_TYPE:
    GalUtil_PInfo1WithLocation(__FUNCTION__, "Continuation frame:\n");
    Gal_OutlineFrame(frame, GAL_PINFO1_LEVEL);
    break;
  case GAL_DESTROY_MSG_TYPE:
    GalUtil_Warn("Got incomprehensible destroy request as message reply; ignoring\n");
    return (int) NULL;
  case GAL_POSTPONE_MSG_TYPE:
    GalUtil_Warn("Got incomprehensible postpone request as message reply; ignoring\n");
    return (int) NULL;
  default:
    GalUtil_Warn("Got incomprehensible message as message reply; ignoring\n");
    return (int) NULL;
  }
  
  result = (*c->fn)(frame, msg_type, env, c->state);

  /* Free the continuation. */
  __GalSS_FreeContinuation(c);

  result = _GalSS_UpdateNormalReply(env, result, frame);

  /* Once we have the result, the response should be
     handled just as if a normal dispatch server had been
     fired. */

  return _GalSS_FrameReturnHandler(env, frame, result);
}

/* I'm setting up some interdependencies between io/ and ServerStub/
   that I'd hoped to avoid. In particular, the connections have
   to host the continuations, and the connection read handler needs
   to be aware of the possibility of continuations. This is all
   a little ugly. */

typedef struct __galss_continuation_searcher {
  GalIO_PointerQueue *c_queue;
  GalSS_Continuation *c;
} __galss_continuation_searcher;

static int __galss_find_continuation(void *arg, void *caller_data)
{
  int server_tidx = (int) caller_data;
  GalSS_Continuation *c = (GalSS_Continuation *) arg;

  return (server_tidx == c->server_tidx);
}

static int __GalSS_MsgIsNewOrContinuation(GalIO_MsgType msg_type,
					  void *decoded_data,
					  int decoded_size,
					  Gal_ObjectType decoded_type,
					  void *client_data)
{
  __galss_continuation_searcher *gcs = (__galss_continuation_searcher *) client_data;
  Gal_Object server_tidx_obj;
  int server_tidx;
  Gal_Frame f;
  Gal_Frame admin_info;
  
  if (decoded_type != GAL_FRAME) {
    return 0;
  }
  
  if (msg_type == GAL_MESSAGE_MSG_TYPE) {
    return 1;
  }
  
  /* Everything else is a reply. Try to find it on the continuations. */
  if (!gcs->c_queue) {
    return 0;
  }

  f = (Gal_Frame) decoded_data;
  admin_info = Gal_GetFrame(f, GAL_HUB_OPAQUE_DATA_FRAME_KEY);
  if (!admin_info) {
    return 0;
  }
  server_tidx_obj = Gal_GetObject(admin_info,
				  GAL_SERVER_TOKEN_INDEX_FRAME_KEY);
  /* There needs to be a server tidx for the continuation to match. */
  if (!server_tidx_obj) {
    return 0;
  }
  server_tidx = Gal_IntValue(server_tidx_obj);
  gcs->c = (GalSS_Continuation *) _GalIO_QueueDequeueIf(gcs->c_queue, __galss_find_continuation, (void *) server_tidx);

  if (gcs->c) {
    return 1;
  } else {
    return 0;
  }
}

/* This read handler is used for connections hosting dispatch
   functions, i.e., on the server side. 
   -1 is error, 0 is nothing to do, 1 is something to do. */

extern int _galio_read_msg_from_sockqueue(GalIO_SockObjectQueueStruct *queue,
					  Gal_Frame *frame,
					  GalIO_MsgType *msg_type_ptr,
					  GalIO_MsgQueueTestFn test_fn,
					  void *client_data,
					  int blocking);

static int __GalSS_EnvConnectionFrameReader(GalIO_CommStruct *gcomm,
					    Gal_Frame *frame_ptr,
					    GalIO_MsgType *msg_type_ptr,
					    GalIO_MsgQueueTestFn test_fn,
					    void *client_data)
{
  return _galio_read_msg_from_sockqueue(gcomm->queue, frame_ptr,
					msg_type_ptr, test_fn,
					client_data, 0);
}

static int __GalSS_EnvConnectionReadHandler(GalIO_CommStruct *gcomm)
{
  Gal_Frame frame = (Gal_Frame) NULL;
  __galss_continuation_searcher gcs;
  int status;
  GalIO_MsgType msg_type;

  /* Populate the continuation searcher. */
  gcs.c_queue = gcomm->continuations;
  gcs.c = (GalSS_Continuation *) NULL;

  /* Search for a message. If it's new, fire the normal
     handler. Otherwise, fire the continuation handler. */
  
  status =  (*(gcomm->msg_reader))(gcomm, &frame, &msg_type,
				   __GalSS_MsgIsNewOrContinuation,
				   (void *) &gcs);
  /* frame was read, call data handler */
  
  if (status == 1) {
    if ((msg_type == GAL_MESSAGE_MSG_TYPE) && (gcomm->handler)) {
      (*(gcomm->handler))(gcomm, frame);
    } else if (gcs.c) {
      _GalSS_ContinuationHandler(gcs.c, frame, msg_type);
    }
  }    
  return status;
}

/* This is a utility function which sets up continuations
   on a connection. */

void _GalSS_CommEnableContinuations(GalIO_CommStruct *gcomm)
{
  gcomm->msg_reader = __GalSS_EnvConnectionFrameReader;
  gcomm->read_handler = __GalSS_EnvConnectionReadHandler;
  gcomm->continuations =  _GalSS_CreateContinuationQueue();
}

/* 
 * "Simple" continuations
 */

/* These are cases where we can set up callbacks already,
   such as brokers or timed tasks, where the presence of an
   environment would be valuable. This will also be important
   to mirror in the MITRE utilities which set up the stdin poll. */

/* Let's start with the broker in handler. This function differs
   from the normal data in handler in that we explicitly
   add support for an environment object (already in the
   structures). */

static void __galss_broker_unlock_env(GalIO_BrokerStruct *b,
				      void *caller_data)
{
  if (b && b->in_b && b->in_b->host_env)
    GalSS_EnvUnlock(b->in_b->host_env);
}

void GalSS_BrokerSetEnvironment(GalIO_BrokerStruct *b, GalSS_Environment *env)
{
  /* If there's already a host_env, don't add the destruction
     callback. */
  if (b && b->in_b) {
    if (b->in_b->host_env) {
      GalSS_EnvUnlock(b->in_b->host_env);
      GalSS_EnvLock(env);
      b->in_b->host_env = env;
    } else {
      GalSS_EnvLock(env);
      b->in_b->host_env = env;
      GalIO_AddBrokerCallback(b, GAL_BROKER_DESTRUCTION_EVENT,
			      __galss_broker_unlock_env,
			      (void *) NULL);
    }
  }
}

GalSS_Environment *GalSS_BrokerGetEnvironment(GalIO_BrokerStruct *b)
{
  if (b && b->in_b) {
    return b->in_b->host_env;
  }
  return (GalSS_Environment *) NULL;
}

GalIO_BrokerStruct *
GalSS_EnvBrokerDataInInit(GalSS_Environment *env,
			  const char *host, unsigned short port,
			  Gal_Frame frame, GalIO_BrokerDataHandler fnptr,
			  int poll_ms,
			  void *refptr, void (*free_fn)(void *))
{
  GalIO_BrokerStruct *b = (GalIO_BrokerStruct *) NULL;
  /* SAM 11/21/00: Steve observes that if we allow session IDs
     to be updated, we'd better save away copies of the environment objects
     so that subsequent session ID updates don't affect the
     saved environment. */
  GalSS_Environment *new_env = GalSS_EnvCopyNoLock(env);
    
  b = GalIO_CommBrokerDataInInit(GalSS_EnvComm(new_env), host, port, frame,
				 fnptr, -1, refptr, free_fn);  
  if (b) {
    GalSS_BrokerSetEnvironment(b, new_env);
    GalIO_CommStartBroker(GalSS_EnvComm(new_env), b, poll_ms);
  }
  return b;
}

void GalSS_EnvStartBroker(GalSS_Environment *env,
			  GalIO_BrokerStruct *b, int poll_ms)
{
  GalIO_CommStartBroker(GalSS_EnvComm(env), b, poll_ms);
}

/* Next, let's define the timed task callback. Same
   idea; add an environment to the timed task. */

void GalSS_TaskSetEnvironment(Gal_TaskPkg *p, GalSS_Environment *env)
{
  /* If there's already a host_env, don't add the destruction
     callback. */
  if (p) {
    if (p->host_env) {
      GalSS_EnvUnlock(p->host_env);
      GalSS_EnvLock(env);
      p->host_env = env;
    } else {
      GalSS_EnvLock(env);
      p->host_env = env;
    }
  }
}

GalSS_Environment *GalSS_TaskGetEnvironment(Gal_TaskPkg *p)
{
  if (p) {
    return p->host_env;
  }
  return (GalSS_Environment *) NULL;
}

/* We should set up the timed task callback declaration so
   that it does something similar to what's above. We'll have
   a little structure which embodies the task information. */

Gal_TaskPkg *GalSS_EnvAddTask(GalSS_Environment *env,
			      void (*task)(Gal_TaskPkg *),
			      void *caller_data,
			      long num_millisecs, int read_blocking_available,
			      GAL_SOCKET *read_socket,
			      GAL_SOCKET *write_socket,
			      GAL_SOCKET *err_socket,
			      FILE *read_file, FILE *write_file,
			      FILE* err_file,
			      Gal_TaskConditionFn condition,
			      void (*cleanup_fn)(void *))
{
  Gal_TaskPkg *p;
  /* SAM 11/21/00: Steve observes that if we allow session IDs
     to be updated, we'd better save away copies of the environment objects
     so that subsequent session ID updates don't affect the
     saved environment. */
  GalSS_Environment *new_env = GalSS_EnvCopyNoLock(env);
  
  p = Gal_AddTaskExtended(task,
			  caller_data, -1,
			  read_blocking_available,
			  read_socket, write_socket, err_socket,
			  read_file, write_file, err_file,
			  condition,
			  cleanup_fn);
  if (p) {
    GalSS_TaskSetEnvironment(p, new_env);
    if (Gal_StartTask(p, num_millisecs) == 1) {
      p = (Gal_TaskPkg *) NULL;
    }
  }
  return p;
}

/* Finally, we have continuations which allow us to access
   environments and messages as they're executed. I'd put this in
   hub_server.c, but I'm not that brave yet. */

typedef struct __galss_call_pkg {
  GalSS_Environment *env;
  Gal_Frame f;
} __galss_call_pkg;

static int __galss_apply_dispatch_fn_callback(void *data, void *caller_data)
{
  GalIO_Callback *cb = (GalIO_Callback *) data;
  GalIO_ConnectionDispatchFnCallbackFn fn = (GalIO_ConnectionDispatchFnCallbackFn) cb->fn;
  __galss_call_pkg *cp = (__galss_call_pkg *) caller_data;
  
  (*fn)(cp->env, cp->f, cb->callback_data);
  /* Continue. */
  return 1;
}

void _GalSS_ApplyDispatchFnCallbacks(GalSS_Environment *env,
				     Gal_Frame f)
{
  GalIO_CommStruct *gcomm = env->gcomm;
  GalIO_PointerQueue *q = gcomm->dispatch_fn_callbacks;
  __galss_call_pkg cp;

  cp.env = env;
  cp.f = f;
  
  _GalIO_QueueApply(q, __galss_apply_dispatch_fn_callback, (void *) &cp);
}

/* Here, we'll try to set up enough information on a
   connection so that environments in some other location can be
   managed. */

static void __galss_update_env_loc(GalSS_Environment *env,
				   Gal_Frame f, void *caller_data)
{
  GalSS_Environment **env_loc = (GalSS_Environment **) caller_data;
  GalSS_Environment *old_env = *env_loc;
  
  if (old_env) {
    GalSS_EnvUnlock(old_env);
  }
  GalSS_EnvLock(env);
  *env_loc = env;
}

static void __galss_free_env_loc(GalIO_CommStruct *gcomm,
				 void *caller_data)
{
  GalSS_Environment **env_loc = (GalSS_Environment **) caller_data;
  GalSS_Environment *old_env = *env_loc;
  
  if (old_env) {
    GalSS_EnvUnlock(old_env);
  }
  *env_loc = (GalSS_Environment *) NULL;
}

void GalSS_EnvMaintainInLocation(GalIO_CommStruct *gcomm,
				 const char *initial_session_id,
				 GalSS_Environment **env_loc)
{
  GalIO_AddConnectionDispatchFnCallback(gcomm,
					__galss_update_env_loc,
					(void *) env_loc);
  *env_loc = GalSS_EnvCreate(gcomm);
  GalSS_EnvLock(*env_loc);
  GalSS_EnvUpdateSessionID(*env_loc, initial_session_id);
  GalIO_AddConnectionCallback(gcomm, GAL_CONNECTION_SHUTDOWN_EVENT,
			      __galss_free_env_loc,
			      (void *) env_loc);
}
  
