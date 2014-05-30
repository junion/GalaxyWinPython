/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "Dialogue_server.h"
#include "galaxy/server_functions.h"

#include "component_engine.h"

static void __DialogueError(GalSS_Environment *env, char *err_string)
{
  Gal_Frame reply = Gal_MakeFrame("FromDialogue", GAL_CLAUSE);
  Gal_Frame output_frame = Gal_MakeFrame("error", GAL_CLAUSE);

  Gal_SetProp(output_frame, ":description", Gal_StringObject(err_string));
  Gal_SetProp(reply, ":output_frame", Gal_FrameObject(output_frame));
  GalSS_EnvWriteFrame(env, reply, 0);
  Gal_FreeFrame(reply);
}

static void __DoDialogueSteps(void *step_data, int step_type,
                              GalSS_Environment *env)
{
  ParseTree *p = (ParseTree *) NULL;
  GalIO_MsgType reply_type;  
  DialogueAction *a = NextDialogueStep(step_data, step_type);
  Gal_Frame msg_frame;
  Gal_Frame response_frame;
  Gal_Frame output_frame;
  Gal_Frame reply;
  char *err_string = (char *) NULL;

  printf("Current dialogue step is %s\nNext dialogue step is %s\n",
         DialogueStepName(step_type), DialogueStepName(a->step_type));
  fflush(stdout);
    
  /* There may be no next dialogue step. */
  if (!a)
    return;

  switch (a->step_type) {
  case CONSULT_BACKEND:
    msg_frame = Gal_MakeFrame("DBQuery", GAL_CLAUSE);
    
    Gal_SetProp(msg_frame, ":sql_query",
                Gal_StringObject((char *) a->step_data));
    free((char *) a->step_data);
    response_frame = GalSS_EnvDispatchFrame(env, msg_frame, &reply_type);
    Gal_FreeFrame(msg_frame);
    if (reply_type == GAL_ERROR_MSG_TYPE) {
      err_string = "error consulting backend";
    } else {
      DBResult *db = HubFrameToDBResult(response_frame);
      if (!db) {
        err_string = "can't convert backend response to DB result";
      } else {
        __DoDialogueSteps((void *) db, BACKEND_RESPONSE, env);
        FreeDBResult(db);       
      }
    }
    Gal_FreeFrame(response_frame);
    break;
  case PRESENT_OUTPUT:
    p = (ParseTree *) a->step_data;
    break;
  }  
  FreeDialogueAction(a);

  /* Reply construction. */
  
  if (p) {
    output_frame = ParseTreeToFrame(p);
    FreeParseTree(p);
    reply = Gal_MakeFrame("FromDialogue", GAL_CLAUSE);  
    Gal_SetProp(reply, ":output_frame", Gal_FrameObject(output_frame));
    GalSS_EnvWriteFrame(env, reply, 0);
    Gal_FreeFrame(reply);
  } else if (err_string) {
    __DialogueError(env, err_string);
  }
  return;
}

Gal_Frame DoGreeting(Gal_Frame frame, void *server_data)
{
  Gal_Frame greeting = Gal_ReadFrameFromString("{c FromDialogue :output_frame {c greeting } :is_greeting 1 }");  
  
  GalSS_EnvWriteFrame((GalSS_Environment *) server_data, greeting, 0);
  Gal_FreeFrame(greeting);
  return (Gal_Frame) NULL;
}

Gal_Frame DoDialogue(Gal_Frame frame, void *server_data)
{
  Gal_Frame dialogue_input;
  ParseTree *p;
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  
  /* Deconstruction and type checking */
  dialogue_input = Gal_GetFrame(frame, ":frame");
  if (!dialogue_input) {
    __DialogueError(env, "no dialogue input frame");
    return (Gal_Frame) NULL;
  }
  p = FrameToParseTree(dialogue_input);

  if (!p) {
    __DialogueError(env, "can't convert dialogue input frame");
    return (Gal_Frame) NULL;
  }

  /* Core processing */
  __DoDialogueSteps((void *) p, USER_INPUT, env);
  return (Gal_Frame) NULL;
}

/* Command line argument information */

static char *oas[] = {
  "-dialogue_data file",  "dialogue data", (char *) NULL,
  NULL
};

/* Server initialization function */

void *_GalSS_init_server(GalIO_ServerStruct *server, int argc, char **argv)
{
  char *data_file = (char *) NULL;
  
  if (GalUtil_OACheckUsage(argc, argv, oas, NULL) == 0)	
    exit(1);
  
  if (GalUtil_OAExtract(argc, argv, oas, "-dialogue_data",
			GAL_OA_STRING, &data_file) == 0) {
    GalUtil_Warn("No -dialogue_data argument. Exiting.");
    exit(1);
  }
  InitializeDialogue(data_file);
  free(data_file);
  return (void *) NULL;
}
