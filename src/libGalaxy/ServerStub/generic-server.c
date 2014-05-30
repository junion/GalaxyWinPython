/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"
#include "generic-server-internal.h"

/* 
 *       THESE ARE THE NEW FUNCTIONS WHICH OPERATE ON ENVIRONMENTS.
 *       These should be used in dispatch functions whenever possible.
 *       SAM 10/3/99. See also GalSS_EnvWriteFrame and 
 *       GalSS_EnvDispatchFrame in frame_util.c.
 */

void *GalSS_EnvGetCommData(GalSS_Environment *env)
{
  return GalIO_GetCommData(env->gcomm);
}

void GalSS_EnvSetCommData(GalSS_Environment *env, void *data, void (*free_fn)(void *))
{
  GalIO_SetCommData(env->gcomm, data, free_fn);
}

GalIO_CommStruct *GalSS_EnvComm(GalSS_Environment *env)
{
  return env->gcomm;
}

void *GalSS_EnvGetClientData(GalSS_Environment *env, const char *name)
{
  return GalIO_GetCommClientData(env->gcomm, name);
}

extern
void _GalIO_PrintMessageTraffic(GalIO_CommStruct *gcomm, char *msg,
				char *caller, Gal_Frame frame);

int GalSS_FrameHandler(GalIO_CommStruct *gcomm, Gal_Frame frame)
{
  Gal_Frame result;
  GalSS_Environment *env;

  /* Allocate the environment. I could make this live on the
     stack for efficiency purposes, but I'm too lazy... */
  env = GalSS_EnvCreate(gcomm);
  
  /* "Lock" the environment (bump the reference counter). */
  GalSS_EnvLock(env);

  _GalIO_PrintMessageTraffic(gcomm, "Received new message from %s\n",
			     __FUNCTION__, frame);
  
  result = GalSS_CallServerFunction(frame, env, 0);

  return _GalSS_FrameReturnHandler(env, frame, result);
}

/* See also the continuation stuff in frame_util.c. */

extern void _GalIO_CommFreeMessages(GalIO_CommStruct *gcomm,
				    Gal_Frame incoming, Gal_Frame outgoing);

int _GalSS_FrameReturnHandler(GalSS_Environment *env, Gal_Frame frame,
			      Gal_Frame result)
{
  int status;
  
  if (result) {    
    /* Only write the result if the environment hasn't yet
       been satisfied. */
    if (!env->return_satisfied) {

      _GalIO_PrintMessageTraffic(env->gcomm,
				 "Returning result frame to %s\n",
				 __FUNCTION__, result);
      status = GalIO_CommWriteMessage(env->gcomm, result, GAL_REPLY_MSG_TYPE, 1);
      if (status == -1) {
	GalIO_CloseCommSocket(env->gcomm);
      } else {
	status = 1;
      }
    } else {
      /* Didn't write anything. */
      _GalIO_PrintMessageTraffic(env->gcomm,
				 "Reply already provided to %s\n",
				 __FUNCTION__, (Gal_Frame) NULL);
      status = 0;
    }
  } else {
    _GalIO_PrintMessageTraffic(env->gcomm,
			       "No result frame for %s\n",
			       __FUNCTION__, (Gal_Frame) NULL);
    status = 0;
  }
  /* If the return is postponed, then the only reason that
     return_satisfied was true is because the return was
     postponed. Subsequent execution should be permitted to
     provide the real return. */
  if (env->return_postponed) {
    env->return_postponed = 0;
    env->return_satisfied = 0;
  }
  _GalIO_CommFreeMessages(env->gcomm, frame, result);
  /* "Unlock" the environment, which decrements the
     reference counter and frees at 0. */
  GalSS_EnvUnlock(env);
  return(status);
}

/* 
 *  for Emacs...
 *  Local Variables:
 *  mode: c
 *  fill-column: 110
 *  comment-column: 80
 *  c-indent-level: 2
 *  c-continued-statement-offset: 2
 *  c-brace-offset: -2
 *  c-argdecl-indent: 2
 *  c-label-offset: -2
 *  End:
 */
