/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"
#include <limits.h>
#include <signal.h>

Gal_Frame twice(Gal_Frame frame, void *server_data)
{
  int i = Gal_GetInt(frame, ":int");
  char *program_name = Gal_GetString(frame, ":program");
  Gal_Frame new_f;
  
  if (!program_name)
    program_name = "main";
  
  if (i == 0) {
    /* We'll loop forever. */
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "i is 0");
    return (Gal_Frame) NULL;
  } else if ((INT_MAX / i) < 2) {
    /* If we're about to overflow... */
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "double would overflow MAXINT");
    return (Gal_Frame) NULL;
  } else {
    new_f = Gal_MakeFrame(program_name, GAL_CLAUSE);
    Gal_SetProp(new_f, ":int", Gal_IntObject(2 * i));
    Gal_SetProp(new_f, ":program", Gal_StringObject(program_name));
    GalSS_EnvWriteFrame((GalSS_Environment *) server_data, new_f, 0);
    Gal_FreeFrame(new_f);
    return (Gal_Frame) NULL;
  }
}

/* I have two versions of the complex twice function:
   one without continuations and one with continuations. */

static Gal_Frame __continue_complex_twice(Gal_Frame res_f, GalIO_MsgType t,
					  GalSS_Environment *env, void *caller_data)
{
  /* When this function is called directly from a dispatch function,
     frame will be freed by the dispatch function invocation. When
     called from a continuation, it will be freed by the
     continuation free function. */
  Gal_Frame frame = (Gal_Frame) caller_data;
  Gal_Frame ret_val;
  Gal_Frame twice_frame;
  char *prog_name = (char *) NULL;
  
  if (!res_f) {
    GalUtil_Warn("Didn't hear back from multiply");
    return (Gal_Frame) NULL;
  }

  switch (t) {
  case GAL_REPLY_MSG_TYPE:
    prog_name = Gal_GetString(frame, ":program");
    if (!prog_name) prog_name = "main";
    twice_frame = Gal_MakeFrame("twice", GAL_CLAUSE);
    Gal_SetProp(twice_frame, ":program", Gal_StringObject(prog_name));
    Gal_SetProp(twice_frame, ":int", Gal_CopyObject(Gal_GetObject(res_f, ":int")));
    ret_val = twice(twice_frame, (void *) env);
    Gal_FreeFrame(twice_frame);
    return ret_val;
  case GAL_ERROR_MSG_TYPE:
    return (Gal_Frame) NULL;
  default:
    return (Gal_Frame) NULL;
  }
}

Gal_Frame complex_twice(Gal_Frame frame, void *server_data)
{
  Gal_Frame new_f = Gal_MakeFrame("multiply", GAL_CLAUSE);
  Gal_Frame res_f;
  GalIO_MsgType t;
  Gal_Frame ret_val;
  
  Gal_SetProp(new_f, ":int", Gal_IntObject(Gal_GetInt(frame, ":int")));  
  res_f = GalSS_EnvDispatchFrame((GalSS_Environment *) server_data, new_f, &t);
  Gal_FreeFrame(new_f);

  ret_val =  __continue_complex_twice(res_f, t, (GalSS_Environment *) server_data,
				      (void *) frame);

  /* If we don't free res_f, no one will. */  
  Gal_FreeFrame(res_f);
  return ret_val;
}

static void __continuation_free_frame(void *state)
{
  Gal_FreeFrame((Gal_Frame) state);
}

Gal_Frame continuation_complex_twice(Gal_Frame frame, void *server_data)
{
  Gal_Frame new_f = Gal_MakeFrame("multiply", GAL_CLAUSE);
  
  Gal_SetProp(new_f, ":int", Gal_IntObject(Gal_GetInt(frame, ":int")));  
  GalSS_EnvDispatchFrameWithContinuation((GalSS_Environment *) server_data,
					 new_f, __continue_complex_twice,
					 (void *) Gal_CopyFrame(frame),
					 __continuation_free_frame);
  Gal_FreeFrame(new_f);
  return (Gal_Frame) NULL;
}

/* This is included for use in demonstrating continuation_complex_double.pgm. */

Gal_Frame echo_frame(Gal_Frame frame, void *server_data)
{
  return frame;
}

Gal_Frame reinitialize(Gal_Frame frame, void *server_data)
{
  char *program_name = Gal_GetString(frame, ":program");
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  int i = (int) GalIO_GetCommServerData(GalSS_EnvComm(env));
  Gal_Frame new_f;

  if (!program_name)
    program_name = "main";

  new_f = Gal_MakeFrame(program_name, GAL_CLAUSE);

  /* New message. */
  Gal_SetProp(new_f, ":int", Gal_IntObject(i));
  Gal_SetProp(new_f, ":program", Gal_StringObject(program_name));
  GalSS_EnvWriteFrame(env, new_f, 0);
  Gal_FreeFrame(new_f);
  return (Gal_Frame) NULL;
}

static char *oas[] = {
  "-increment i", "set the initial value", "1",
  NULL
};

static void exit_gracefully(int signum)
{
  GalUtil_Warn("Exiting.\n");
#ifdef __INSURE__
  GalUtil_PInfo1("Calling Insure cleanup\n");
  _Insure_cleanup();
#endif
  exit(0);
}  

void *_GalSS_init_server(GalIO_ServerStruct *s, int argc, char **argv)
{
  int i, increment = 1;
  
  if (!GalUtil_OACheckUsage(argc, argv, oas, &i))
    exit(1);
  GalUtil_OAExtract(argc, argv, oas, "-increment", GAL_OA_INT, &increment);
  signal(SIGINT, exit_gracefully);
  GalIO_SetServerData(s, (void *) increment, NULL);
  return (void *) NULL;
}
