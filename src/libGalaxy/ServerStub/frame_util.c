/*
  This file (c) Copyright 1998 - 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"

#include "generic-server-internal.h"
#include "galaxy/program.h"

/* Adding function maps. We add function declarations one by one - importing
   from a table is pretty impossible given the parallel arrays I'm using
   in dispatch_function.c. SAM 10/2/99. */

void GalSS_AddDispatchFunction(GalIO_ServerStruct *i, const char *name,
			       Gal_FrameDataFnPtr fn,
			       Gal_DispatchFnSignatureKeyEntry *in_key_array,
			       int allow_other_in_keys, 
			       int reply_provided,
			       Gal_DispatchFnSignatureKeyEntry *out_key_array,
			       int allow_other_out_keys)
{
  GalIO_AddServerDispatchFunctionEntry(i, name, fn,
				       in_key_array, allow_other_in_keys,
				       reply_provided, out_key_array,
				       allow_other_out_keys);
}

void GalSS_InitializeServerDefaults(GalIO_ServerStruct *server, char *name, unsigned short port)
{
  if (name) {
    GalIO_SetServerName(server, name);
  }
  if (port) {
    GalIO_SetServerDefaultPort(server, port);
  }
}

static char *Exclusions[] = {GAL_HUB_OPAQUE_DATA_FRAME_KEY,
			     GAL_SERVER_TOKEN_INDEX_FRAME_KEY,
			     GAL_SESSION_ID_FRAME_KEY,
			     NULL};

/* SAM 9/20/00: We've finally removed the special case handling
   for the reinitialize function. All the special case handling
   is now in GalIO_VerificationHandler, and it mostly involves
   making sure that reinitialize gets called if it's present,
   during the verification phase. The signatures are now
   passed around during the handshake. */

/* One consequence of this is that we can no longer really
   shut off module-to-module subdialogues during verification.
   Sigh. */

/* This function assumes that the connection object was stored
   in the environment object already. */


static void __GalSS_EnvAdministerReply(GalSS_Environment *env,
				       Gal_Frame res,
				       int update_frame_name);

/* This function is responsible for populating the environment
   object which it is passed. I may move that responsibility elsewhere. */

/* SAM 12/11/00: The frame now contains a frame in the key
   GAL_HUB_OPAQUE_DATA_FRAME_KEY (:hub_opaque_data) with the
   following elements:
   Hub-to-server and back:
   GAL_SESSION_ID_FRAME_KEY (:session_id) 
   GAL_TOKEN_INDEX_FRAME_KEY (:tidx)
   GAL_OPAQUE_SCRIPT_MODULE_HUB_FRAME_KEY (:hub_program_info)
   Hub-to-server, Server-to-Hub:
   GAL_ROUND_TRIP_FRAME_KEY (:reply_requested)
   GAL_TOKEN_TIMESTAMP_FRAME_KEY (:timestamp)
   GAL_PROVIDER_ID_FRAME_KEY (:provider_id)
   (originating provider from Hub to server, target provider
   from server to Hub)
   (on return or new message) Server-to-Hub:
   GAL_RESET_SESSION_FRAME_KEY (:reset_session - calls builtin.new_session)
   GAL_LOCK_MASK_FRAME_KEY (:lock_mask - encodes session lock flags)
   GAL_LOCK_VALUE_FRAME_KEY (:lock_value)
   (new message) Server-to-Hub:
   GAL_SERVER_TOKEN_INDEX_FRAME_KEY (:server_token_index)
*/

Gal_Frame GalSS_CallServerFunction(Gal_Frame frame,
				   GalSS_Environment *env,
				   int optional)
{ 
  Gal_Frame res = (Gal_Frame) NULL;
  char *bare_op_name;
  GalIO_CommStruct *gcomm = env->gcomm;
  Gal_Frame admin_info = (Gal_Frame) NULL;
  Gal_Object admin_info_obj = Gal_RemProp(frame,
					  GAL_HUB_OPAQUE_DATA_FRAME_KEY);
  /* This one REALLY only has local scope. */
  Gal_DispatchFnInvocation invocation;
  int found;

  if (admin_info_obj) {
    admin_info = Gal_FrameValue(admin_info_obj);
    Gal_FreeWrapper(admin_info_obj);
  }
  
  if (admin_info) {
    env->hub_data = admin_info;
  } else {
    env->hub_data = (Gal_Frame) NULL;
  }

  /* Next, we get the op name. */
  
  env->op_name = _gal_strdup(Gal_FrameName(frame));

  /* Now, we need to make sure we have the true op_name, so we
     need to throw out the server. The function copies, so we
     don't have to worry about the frame being freed. But I
     want to return the same frame name, so I must store that first. */
  
  bare_op_name = Gal_SplitOperationName(env->op_name, (char **) NULL);

  /* Next, we do the cacheing for the incoming elements. */

  if (admin_info) {
    if (Gal_GetObject(admin_info, GAL_SESSION_ID_FRAME_KEY)) {
      GalSS_EnvUpdateSessionID(env,
			       Gal_GetString(admin_info,
					     GAL_SESSION_ID_FRAME_KEY));
      /* Seed the frame itself, just in case. This is for
	 backward compatibility. If there already is one,
	 the user passed it in by hand, and we shouldn't overwrite. */
      if (!Gal_GetObject(frame, GAL_SESSION_ID_FRAME_KEY)) {
	Gal_SetProp(frame, GAL_SESSION_ID_FRAME_KEY,
		    Gal_CopyObject(Gal_GetObject(admin_info,
						 GAL_SESSION_ID_FRAME_KEY)));
	env->session_id_promoted = 1;
      }
    }
    env->return_required = Gal_GetInt(admin_info, GAL_ROUND_TRIP_FRAME_KEY);
  }

  /* Next, we find the function entry. */

  invocation.pkg = GalIO_GetCommDispatchFnPkg(gcomm);
  invocation.env = env;
  invocation.bare_op_name = bare_op_name;
  invocation.sig = (Gal_DispatchFnSignature *) NULL;
  invocation.call_client_data = (void *) NULL;
  invocation.frame = frame;
  
  found = Gal_FindDispatchFunctionEntry(&invocation);
  
  /* If we've found a dispatch function, we call it. */
  
  if (found) {
    Gal_DispatchFnSignature *sig = invocation.sig;
    
    if (GalIO_CommValidating(gcomm) && sig) {
      /* If validation has been requested, do it. */
      Gal_ValidateDispatchFnInput(bare_op_name, frame, sig, Exclusions);
    }

    /* Do the dispatch function callbacks, if they exist. */
    
    _GalSS_ApplyDispatchFnCallbacks(env, frame);
    
    res = _Gal_InvokeDispatchFn(&invocation);
    
    if (GalIO_CommValidating(gcomm) && sig) {
      /* If validation has been requested, do it. */
      Gal_ValidateDispatchFnOutput(bare_op_name, res, sig, Exclusions);
    }

    res = _GalSS_UpdateNormalReply(env, res, frame);
    free(bare_op_name);
    return res;
  } else {
    if (!optional) {
      char errstr[256];
      char *server_name = GalIO_GetCommServerName(gcomm);
      char buf[256];
      
      /* server_name may not exist, if there's no server
	 (this happens with the Builtin server in the Hub).
	 So we need to do something different then. */

      if (!server_name) {
	char *gcomm_host = GalIO_GetCommHost(gcomm);
	if (!gcomm_host) {
	  server_name = "<unknown>";
	} else {
	  sprintf(buf, "@ ");
	  strncpy(buf + 2, gcomm_host, 250);
	  server_name = buf;
	}
      }
      
      GalUtil_Warn("Function %s does not exist!!!\n", bare_op_name); 
      GalUtil_Warn("You need to add it to the operations for server %s\n",
		   server_name);
      sprintf(errstr, "Function %s does not exist", bare_op_name);
      /* This will force the return. */
      GalSS_EnvErrorOfType(env, GAL_NO_OPNAME_ERROR, errstr);
      res = (Gal_Frame) NULL;
    } else {
      res = frame;
    }
    /* Make sure I free everything I may have allocated. */
    free(bare_op_name);    
    return(res);
  }
}

/* Administering the reply. Note the description of the
   incoming frame before GalSS_CallServerFunction. We will
   currently allow a few things to update the incoming state
   before returning it: :session_id in the return frame
   (will be deprecated), :hub_new_session (--> :reset_session),
   any of the four lock keys:
   :hub_serve_any_session
   :hub_serve_this_session_only
   :hub_get_session_lock
   :hub_release_session_lock
   . This stuff should all be replaced by GalSS_EnvSetSession
   as time goes by. */

static void __GalSS_UpdateSessionInfo(GalSS_Environment *env,
				      Gal_Frame admin_info,
				      Gal_Frame msg,
				      int is_new_msg);

static void __GalSS_EnvAdministerReply(GalSS_Environment *env,
				       Gal_Frame res,
				       int update_frame_name)
{
  /* Sam 12/1/99: Make sure the administrative info is reset,
     even if the user tries to do something awful. */

  char *frame_name = (char *) NULL;
  Gal_Frame admin_info;
  
  if (res) {
    if (env->hub_data != (Gal_Frame) NULL) {
      admin_info = Gal_CopyFrame(env->hub_data);
    } else {
      admin_info = Gal_MakeFrame("admin_info", GAL_CLAUSE);
    }
    
    /* Turns out that we need this for the logger. */
    /* The name of the frame should be the same as passed in, whenever
       possible. */

    /* For some reason, the name was returning NULL sometimes. */
    frame_name = Gal_FrameName(res);
    if (update_frame_name && ((!frame_name) || strcmp(frame_name, env->op_name))) {
      /* As long as the frame name hasn't been changed to destroy,
	 keep it the same. */
      Gal_SetFrameName(res, env->op_name);
    }
    /* Update the session information. */
    __GalSS_UpdateSessionInfo(env, admin_info, res, 0);
    Gal_SetProp(res, GAL_HUB_OPAQUE_DATA_FRAME_KEY,
		Gal_FrameObject(admin_info));
  }
}

static void __GalSS_UpdateSessionInfo(GalSS_Environment *env,
				      Gal_Frame admin_info,
				      Gal_Frame msg,
				      int is_new_msg)
{
  int mask = -1;
  int value = -1;
  char *session_id = env->session_id;
  char *frame_session_id = Gal_GetString(msg, GAL_SESSION_ID_FRAME_KEY);

  /* Currently, I've copied :session_id into the toplevel
     msg for backward compatibility. Updating this :session_id
     in 2.1 didn't do anything if there was a session ID passed
     in, and if there wasn't, it only did something if
     it was a dispatch server or :hub_new_session was
     present or something. In this release, it is
     only here for reference. It is NOT consulted on the
     way out. The only way to update session IDs for returns
     is by calling GalSS_EnvUpdateSessionID or GalSS_EnvSetSession.
     This session ID may be used for new messages, though.
     But even in that case, the environment isn't updated.
     And even this is for backward compatibility. */
  if (frame_session_id && is_new_msg)
    session_id = frame_session_id;  
  
  if (session_id)
    Gal_SetProp(admin_info, GAL_SESSION_ID_FRAME_KEY,
		Gal_StringObject(session_id));

  /* If we added the session ID to the frame, remove
     it when you reply. */
  if (frame_session_id && (!is_new_msg) && env->session_id_promoted)
    Gal_DelProp(msg, GAL_SESSION_ID_FRAME_KEY);

  /* Do NOT remove the session ID from the frame passed in.
     It may be that the caller will expect it to be there. */
  
  /* THESE WILL BE DEPRECATED IN A FUTURE RELEASE! */
  /* Now, check for all the appropriate keys. */
  /* First, the session reset key. */
  if (Gal_GetObject(msg, GAL_HUB_NEW_SESSION_FRAME_KEY))
    Gal_SetProp(admin_info, GAL_RESET_SESSION_FRAME_KEY,
		Gal_IntObject(1));

  /* Next, the session lock keys. */
  GalSS_SessionDecodeLocks(msg, &mask, &value);
  if (mask > 0) {
    Gal_SetProp(admin_info, GAL_LOCK_MASK_FRAME_KEY,
		Gal_IntObject(mask));
    Gal_SetProp(admin_info, GAL_LOCK_VALUE_FRAME_KEY,
		Gal_IntObject(value));
  }

  /* Possibly set the timestamp with an inherited value. */
  if(env->inherited_token_timestamp != -1) {
    double* token_timestamp_ptr = (double*) malloc(sizeof(double)); 
    if(!token_timestamp_ptr) {
      GalUtil_Error("Could not allocate memory for token timestamp.");
    }

    *token_timestamp_ptr = env->inherited_token_timestamp;
    Gal_SetProp(admin_info, GAL_TOKEN_TIMESTAMP_FRAME_KEY,
		Gal_CreateFloat64Object((void*)token_timestamp_ptr, 1, 1));
  }
}

int GalSS_SessionDecodeLocks(Gal_Frame fr, int *mask_ptr, int *value_ptr)
{
  int mask = 0;
  int value = 0;

  if (!fr)
    return 0;

  if (Gal_GetInt(fr, GAL_HUB_SERVE_ANY_SESSION_FRAME_KEY)) {
    mask = mask | GAL_SERVER_READS_ONLY_FROM_SESSION;
  }
  if (Gal_GetInt(fr, GAL_HUB_SERVE_THIS_SESSION_ONLY_FRAME_KEY)) {
    mask = mask | GAL_SERVER_READS_ONLY_FROM_SESSION;
    value = value | GAL_SERVER_READS_ONLY_FROM_SESSION;
  }
  if (Gal_GetInt(fr, GAL_HUB_GET_SESSION_LOCK_FRAME_KEY)) {
    mask = mask | GAL_SESSION_WRITES_ONLY_TO_SERVER;
    value = value | GAL_SESSION_WRITES_ONLY_TO_SERVER;
  }
  if (Gal_GetInt(fr, GAL_HUB_RELEASE_SESSION_LOCK_FRAME_KEY)) {
    mask = mask | GAL_SESSION_WRITES_ONLY_TO_SERVER;
  }
  if (Gal_GetInt(fr, GAL_HUB_SESSION_LOCK_PERMANENT_FRAME_KEY)) {
    value = value | GAL_PERMANENT_LOCK;
  }
  *mask_ptr = mask;
  *value_ptr = value;
  return 1;
}

void _GalSS_EnvAdministerNewMessage(GalSS_Environment *env,
				    Gal_Frame msg, const char *provider_id)
{
  Gal_Frame admin_info = Gal_MakeFrame("admin_info", GAL_CLAUSE);

  __GalSS_UpdateSessionInfo(env, admin_info, msg, 1);
  if (provider_id) {
    Gal_SetProp(admin_info, GAL_PROVIDER_ID_FRAME_KEY,
		Gal_StringObject(provider_id));
  }
  Gal_SetProp(msg, GAL_HUB_OPAQUE_DATA_FRAME_KEY,
	      Gal_FrameObject(admin_info));
}

Gal_Frame _GalSS_UpdateNormalReply(GalSS_Environment *env,
				   Gal_Frame res, Gal_Frame frame)
{
  /* Now that we've figured out what the right return value is,
     we need to update the value with the appropriate administrative
     info. */
  
  /* If a reply is forced, use the input. */
  if (env->return_required && !(env->return_satisfied) && !res) {
    res = frame;
  }

  /* Careful, we should rename the frame when it's a normal
     reply, but not otherwise. */
  __GalSS_EnvAdministerReply(env, res, 1);
  return res;  
}

/*
 *    ENVIRONMENTS
 */

/* SAM 10/3/00: Instead of eliminating environments, we're going
   to make them simpler to use, and exploit them to maintain
   session information and so on. */

/*
   Environments contain:
   (a) a connection, which is the pipe through which messages
   are sent and read;
   (b) call information (tidx, hub_program_info, incoming frame name)
   (c) session information (just session ID right now)
   (d) a flag indicating whether the return has been sent
   (e) a flag indicating whether a return is required
   (f) a reference count
   
   The rules for environments are as follows:
   (a) you can only return from an environment once (GalSS_EnvError,
   GalSS_EnvDestroyToken, return from a dispatch function, or
   GalSS_Reply). All subsequent return attempts will be ignored,
   unlike the previous semantics of GalSS_EnvForceReply, where
   subsequent return were interpreted as new messages
   (b) every new message produced contains the session information,
   so that continuations of threads of execution is are transparent
   (c) when you save away an environment, you must "lock" it, which
   increments the reference count; when the reference count reaches 0,
   the environment is freed
*/

GalSS_Environment *GalSS_EnvCreate(GalIO_CommStruct *gcomm)
{
  GalSS_Environment *env = (GalSS_Environment *) calloc(1, sizeof(GalSS_Environment));
  env->gcomm = gcomm;
  env->reference_count = 0;
  env->inherited_token_timestamp = -1;
  return env;
}

void GalSS_EnvUpdateSessionID(GalSS_Environment *env, const char *session_id)
{
  if (env->session_id)
    free(env->session_id);
  /* SAM 4/27/01: If this function is being called as part of a
     continuation dispatch called from reinitialize(), it's quite
     possible that there will be no session ID. On Linux, at least,
     this will seg fault. */
  if (session_id)
    env->session_id = _gal_strdup(session_id);
}

char *GalSS_EnvGetSessionID(GalSS_Environment *env)
{
  return env->session_id;
}

int GalSS_EnvReturnRequired(GalSS_Environment *env)
{
  return env->return_required;
}

/* Reference count starts at 0. When it returns to 0, the
   environment is freed. */

/* SAM 10/30/00: Whenever I lock an environment, I'd better
   lock the underlying connection too, so that if I try
   to write through a closed connection I get a socket error,
   not a seg fault :-). */

extern void _GalIO_CommLock(GalIO_CommStruct *gcomm);
extern void _GalIO_CommUnlock(GalIO_CommStruct *gcomm);

void GalSS_EnvLock(GalSS_Environment *env)
{
  env->reference_count += 1;
  _GalIO_CommLock(env->gcomm);
}

void GalSS_EnvUnlock(GalSS_Environment *env)
{
  env->reference_count -= 1;
  _GalIO_CommUnlock(env->gcomm);
  if (env->reference_count == 0) {
    /* Free the damn thing. */
    if (env->hub_data)
      Gal_FreeFrame(env->hub_data);
    if (env->session_id)
      free(env->session_id);
    if (env->op_name)
      free(env->op_name);
    free(env);
  }
}

/* SAM 11/21/00: Steve observes that if we allow session IDs
   to be updated, we'd better save away copies of the environment objects
   so that subsequent session ID updates don't affect the
   saved environment. We need to save enough information to
   provide a return. And lock the environment.

   This is the preferred way to save away an environment in a threaded
   context. */

static GalSS_Environment *__GalSS_EnvCopy(GalSS_Environment *old_env,
					  int do_lock)
{
  GalSS_Environment *env = GalSS_EnvCreate(old_env->gcomm);

  if (do_lock)
     GalSS_EnvLock(env);
  GalSS_EnvUpdateSessionID(env, old_env->session_id);
  if (old_env->hub_data)
    env->hub_data = Gal_CopyFrame(old_env->hub_data);
  if (old_env->op_name)
    env->op_name = _gal_strdup(old_env->op_name);
  env->return_required = old_env->return_required;
  env->return_satisfied = old_env->return_satisfied;
  env->return_postponed = old_env->return_postponed;
  env->session_id_promoted = old_env->session_id_promoted;
  return env;
}

GalSS_Environment *GalSS_EnvCopy(GalSS_Environment *old_env)
{
  return __GalSS_EnvCopy(old_env, 1);
}

GalSS_Environment *GalSS_EnvCopyNoLock(GalSS_Environment *old_env)
{
  return __GalSS_EnvCopy(old_env, 0);
}

int GalSS_EnvWriteFrame(GalSS_Environment *env, Gal_Frame frame, int do_block)
{
  return GalSS_EnvWriteFrameToProvider(env, frame, (char *) NULL, do_block);  
}

int GalSS_EnvWriteFrameToProvider(GalSS_Environment *env, Gal_Frame frame,
				  const char *provider_id, int do_block)
{
  if (frame && env) {    
    _GalSS_EnvAdministerNewMessage(env, frame, provider_id);
    return GalIO_CommWriteFrame(env->gcomm, frame, do_block);
  } else {
    return -1;
  }
}

Gal_Frame
GalSS_EnvDispatchFrameToProvider(GalSS_Environment *env, Gal_Frame frame,
				 const char *provider_id, GalIO_MsgType *t)
{
  if (frame && env) {
    _GalSS_EnvAdministerNewMessage(env, frame, provider_id);
    return GalIO_DispatchViaHub(env->gcomm, frame, t);
  } else if (env) {
    /* The frame dispatcher will format the error. */
    return GalIO_DispatchViaHub(env->gcomm, frame, t);
  } else {
    /* The frame dispatcher will format the error. */
    return GalIO_DispatchViaHub((GalIO_CommStruct *) NULL, frame, t);
  }
}

Gal_Frame GalSS_EnvDispatchFrame(GalSS_Environment *env, Gal_Frame frame,
				 GalIO_MsgType *t)
{
  return GalSS_EnvDispatchFrameToProvider(env, frame, (char *) NULL, t);
}

/* Errors. From the application side, of course. */

int GalSS_EnvErrorOfType(GalSS_Environment *env, int type,
			 const char *description)
{
  Gal_Frame f;
  int status;

  /* Only write a reply if one hasn't already been written yet.
     See the rules above. */

  if (env && !env->return_satisfied) {
    f = GalIO_CreateErrorFrame(type, description);
    __GalSS_EnvAdministerReply(env, f, 0);
    status = GalIO_CommWriteMessage(env->gcomm, f, GAL_ERROR_MSG_TYPE, 1);
    /* Make sure that everyone knows the return
	has already been satisfied. */
    env->return_satisfied = 1;
    Gal_FreeFrame(f);
    return status;
  } else {
    return -1;
  }
}

int GalSS_EnvError(GalSS_Environment *env, const char *description)
{
  return GalSS_EnvErrorOfType(env, GAL_APPLICATION_ERROR, description);
}

int GalSS_EnvDestroyToken(GalSS_Environment *env)
{
  Gal_Frame f;
  int status;

  /* Only write a reply if one hasn't already been written yet.
     See the rules above. */

  if (env && !env->return_satisfied) {
     f = Gal_MakeFrame("destroy", GAL_CLAUSE);
     __GalSS_EnvAdministerReply(env, f, 0);
     status = GalIO_CommWriteMessage(env->gcomm, f, GAL_DESTROY_MSG_TYPE, 1);
     /* Make sure that everyone knows the return
	has already been satisfied. */
     env->return_satisfied = 1;
     Gal_FreeFrame(f);
     return status;
  } else {
    return -1;
  }
}

/* Force a normal reply, by the same rules as above. */

int GalSS_EnvReply(GalSS_Environment *env, Gal_Frame f)
{
  int status;

  /* Only write a reply if one hasn't already been written yet.
     See the rules above. */

  if (env && !env->return_satisfied) {
     __GalSS_EnvAdministerReply(env, f, 1);
     status = GalIO_CommWriteMessage(env->gcomm, f, GAL_REPLY_MSG_TYPE, 1);
     /* Make sure that everyone knows the return
	has already been satisfied. */
     env->return_satisfied = 1;
     return status;
  } else {
    return -1;
  }
}

/* This function sends a postponement message back to the
   Hub and marks the environment as postponed, which tells
   the dispatch function caller not to send a reply THIS time. */

int GalSS_EnvPostponeReply(GalSS_Environment *env)
{
  Gal_Frame f;
  int status;
  
  if (env && !env->return_satisfied) {
    f = Gal_MakeFrame("postpone", GAL_CLAUSE);
    __GalSS_EnvAdministerReply(env, f, 0);
    status = GalIO_CommWriteMessage(env->gcomm, f, GAL_POSTPONE_MSG_TYPE, 1);
    env->return_postponed = 1;
    env->return_satisfied = 1;
    Gal_FreeFrame(f);
    return status;
  } else {
    return -1;
  }  
}

/*
 *    PROPERTIES
 */

/* keys is a null-terminated list. */

static Gal_Frame __GalSS_EnvGetProperties(GalSS_Environment *env,
					  char *namespace, char **keys)
{
  Gal_Frame f = Gal_MakeClauseFrame("builtin.get_properties");
  int i, j;
  Gal_Object *objs;
  Gal_Frame reply_f;
  GalIO_MsgType msg_type;

  if (!keys) {
    Gal_FreeFrame(f);
    return (Gal_Frame) NULL;
  }  

  Gal_SetProp(f, ":namespace", Gal_StringObject(namespace));
  for (i = 0; keys[i]; i++);
  objs = (Gal_Object *) calloc(i, sizeof(Gal_Object));
  for (j = 0; j < i; j++) {
    objs[j] = Gal_StringObject(keys[j]);
  }
  Gal_SetProp(f, ":properties", Gal_CreateListObject(objs, i, _gal_free_object, 1));  
  reply_f = GalSS_EnvDispatchFrame(env, f, &msg_type);
  Gal_FreeFrame(f);
  switch (msg_type) {
  case GAL_REPLY_MSG_TYPE:
    return reply_f;
  default:
    Gal_FreeFrame(reply_f);
    return (Gal_Frame) NULL;
  }
}  

/* Delete always happens before set. */

static void __GalSS_EnvModifyProperties(GalSS_Environment *env,
					char *namespace,
					Gal_Frame properties_to_set,
					char **properties_to_delete)
{  
  Gal_Frame f = Gal_MakeClauseFrame("builtin.modify_properties");

  Gal_SetProp(f, ":namespace", Gal_StringObject(namespace));
  if (properties_to_set) {
    Gal_SetProp(f, ":properties_to_set", Gal_FrameObject(properties_to_set));
  }
  if (properties_to_delete) {
    int i, j;
    Gal_Object *objs;
    
    for (i = 0; properties_to_delete[i]; i++);
    objs = (Gal_Object *) calloc(i, sizeof(Gal_Object));
    for (j = 0; j < i; j++) {
      objs[j] = Gal_StringObject(properties_to_delete[j]);
    }
    Gal_SetProp(f, ":properties_to_delete",
		Gal_CreateListObject(objs, i, _gal_free_object, 1));
  }
  GalSS_EnvWriteFrame(env, f, 1);
  Gal_FreeFrame(f);
}

/* Msg to Hub:
   {c builtin.get_properties
      :namespace "session"
      :properties <list> }
*/

Gal_Frame GalSS_EnvGetSessionProperties(GalSS_Environment *env, char **keys)
{
  return __GalSS_EnvGetProperties(env, "session", keys);
}

/* Msg to Hub:
   {c builtin.modify_properties
      :namespace "session"
      :properties_to_set <frame> }

*/

void GalSS_EnvSetSessionProperties(GalSS_Environment *env, Gal_Frame properties)
{
  __GalSS_EnvModifyProperties(env, "session", properties, (char **) NULL);
}

/* GalSS_EnvSetSession() flushes locks and replaces. */

/* Msg to Hub:
   {c builtin.set_session
      :session_id <string>
      :previous_session_id <string>
      :lock_info <int> } */

void GalSS_EnvSetSession(GalSS_Environment *env, const char *session_name, int lock_info)
{
  Gal_Frame f = Gal_MakeClauseFrame("builtin.set_session");
  char *old_session_id = env->session_id;
  
  Gal_SetProp(f, ":session_id", Gal_StringObject(session_name));
  if (old_session_id)
    Gal_SetProp(f, ":previous_session_id", Gal_StringObject(old_session_id));
  if (lock_info > -1)
    Gal_SetProp(f, ":lock_info", Gal_IntObject(lock_info));
  GalSS_EnvUpdateSessionID(env, session_name);
  GalSS_EnvWriteFrame(env, f, 1);
  Gal_FreeFrame(f);
}

/* Msg to Hub:
   {c builtin.modify_properties
      :namespace "session"
      :properties_to_delete <list> }
*/

void GalSS_EnvDeleteSessionProperties(GalSS_Environment *env, char **keys)
{
  __GalSS_EnvModifyProperties(env, "session", (Gal_Frame) NULL, keys);
}

void GalSS_EnvModifySessionProperties(GalSS_Environment *env,
				      Gal_Frame properties_to_set,
				      char **properties_to_delete)
{
  __GalSS_EnvModifyProperties(env, "session", properties_to_set, properties_to_delete);
}

/* Server properties. */

/* Msg to Hub:
   {c builtin.get_properties
      :namespace "server"
      :properties <list> }
*/

Gal_Frame GalSS_EnvGetServerProperties(GalSS_Environment *env, char **keys)
{
  return __GalSS_EnvGetProperties(env, "server", keys);
}

/* The property frame is freed after setting. */

/* Msg to Hub:
   {c builtin.modify_properties
      :namespace "server"
      :properties_to_set <frame> }

*/

/* And make sure to update locally. */

void GalSS_EnvModifyServerProperties(GalSS_Environment *env,
				     Gal_Frame properties_to_set,
				     char **properties_to_delete)
{
  /* Do this first, because the properties frame is FREED
     after the modify call. */
  if (GalIO_CommServer(GalSS_EnvComm(env))) {
    GalIO_ServerModifyProperties(GalIO_CommServer(GalSS_EnvComm(env)),
				 properties_to_set, properties_to_delete);
  }
  __GalSS_EnvModifyProperties(env, "server", properties_to_set, properties_to_delete);
}

void GalSS_EnvSetServerProperties(GalSS_Environment *env, Gal_Frame properties)
{
  
  GalSS_EnvModifyServerProperties(env, properties, (char **) NULL);
}

/* Msg to Hub:
   {c builtin.modify_properties
      :namespace "server"
      :properties_to_delet <list> }
*/

void GalSS_EnvDeleteServerProperties(GalSS_Environment *env, char **keys)
{
  GalSS_EnvModifyServerProperties(env, (Gal_Frame) NULL, keys);
}

/* Gets the token timestamp from the specified environment.
   If there is no timestamp, this function returns -1. */
double GalSS_EnvGetTokenTimestamp(GalSS_Environment *env)
{
  if(env->hub_data) {
    Gal_Object token_timestamp_object = Gal_GetObject(env->hub_data, GAL_TOKEN_TIMESTAMP_FRAME_KEY);
    if (token_timestamp_object) {
      int size;
      double *token_timestamp_ptr = (double*) Gal_Float64Value(token_timestamp_object, &size);
      return *token_timestamp_ptr;
    } 
  } 
  return -1;
}

/* Tells the specified environment to use the current timestamp for
   new messages sent from the environment. */
void GalSS_EnvInheritTokenTimestamp(GalSS_Environment *env)
{
  env->inherited_token_timestamp = GalSS_EnvGetTokenTimestamp(env);
}

/* Providers. */

char *GalSS_EnvGetOriginatingProvider(GalSS_Environment *env)
{
  if (env->hub_data) {
    return Gal_GetString(env->hub_data, GAL_PROVIDER_ID_FRAME_KEY);
  } else {
    return (char *) NULL;
  }
}
