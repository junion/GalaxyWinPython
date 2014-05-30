/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"
#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "UI_server.h"
#include "galaxy/server_functions.h"

#include "component_engine.h"

typedef struct __UIPkg {
  UIDevice *device;
  GalSS_Environment *env;
} UIPkg;

/*
  This is an example of what a text in/text out server would
  look like. The user can hit
  <return> to send the next utterance, or when there
  are no more utterances, <return> will disconnect. 

  This server is restricted to being a Hub client. When the
  user asks for a connection, a connection will be made
  to the Hub.
  
  This example will not yet run on Windows NT, because it
  relies on the MITRE stdin poll, which does not run on NT
  at the moment.
*/

/* When the fake audio server sends a new "audio input", it creates
   a real broker connection and populates it with random data.
   Nonetheless, this illustrates the actual behavior of a
   typical audio server when it's sending audio data to the Hub. */

void __SendText(UIDevice *a, char *input)
{
  UIPkg *p = (UIPkg *) a->client_data;
  Gal_Frame output_f;

  if (input) {
    output_f = Gal_MakeFrame("FromUI", GAL_CLAUSE);

    Gal_SetProp(output_f, ":input_string", Gal_StringObject(input));

    printf("[You said `%s'.]\n", input);
    fflush(stdout);
    /* Send the input. */
    GalSS_EnvWriteFrame(p->env, output_f, 0);
    /* Free the frame. */
    Gal_FreeFrame(output_f);
  } else {
    /* The device shut down. End the session and exit. */
    /* We ran out of inputs. Disconnect. */
    printf("UI device no longer available. Disconnecting.\n");
    fflush(stdout);

    /* First, end the session. */
    output_f = Gal_MakeFrame("Builtin.end_session", GAL_CLAUSE);
    GalSS_EnvWriteFrame(p->env, output_f, 0);
    Gal_FreeFrame(output_f);
      
    /* Next, shut down the connection. This ought
       to reset the poll. */      
    GalIO_SetCommDone(GalSS_EnvComm(p->env));
    /* DON'T DESTROY THE CONNECTION. In batch mode,
       we'll be inside a poll for that connection, and
       all sorts of bad things will happen. It's enough
       to set it to be done; that will force it to be
       destroyed when the current poll is finished. */
  }
}

Gal_Frame Print(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  GalIO_CommStruct *gcomm = GalSS_EnvComm(env);
  UIPkg *p = (UIPkg *) GalIO_GetCommServerData(gcomm);
  
  printf("[System said `%s'.]\n", Gal_GetString(f, ":output_string"));
  fflush(stdout);
  UIPresent(p->device, Gal_GetString(f, ":output_string"));
  if (Gal_GetInt(f, ":enable_input")) {
    EnableUIInput(p->device);
  }
  return (Gal_Frame) NULL;
}

Gal_Frame EnableInput(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  GalIO_CommStruct *gcomm = GalSS_EnvComm(env);
  UIPkg *p = (UIPkg *) GalIO_GetCommServerData(gcomm);

  EnableUIInput(p->device);
  return (Gal_Frame) NULL;
}

/* The reinitialize
   message is called, where we manage some of the connectedness
   information. */

Gal_Frame reinitialize(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  Gal_Frame session_f;
  
  /* Create the session. */
  session_f = Gal_MakeFrame("OpenTextSession", GAL_CLAUSE);
  GalSS_EnvWriteFrame(env, session_f, 0);
  Gal_FreeFrame(session_f);
  
  return (Gal_Frame) NULL;
}

/* We need our own main() because this is a Hub client exclusively. */


/* Command line argument information */

static char *oas[] = {
  "-ui_data file",  "UI data", (char *) NULL,
  "-batch num_times", "run the UI in batch mode num_times", NULL,
  NULL
};

/* Server initialization function */

void *_GalSS_init_server(GalIO_ServerStruct *server, int argc, char **argv)
{
  char *data_file = (char *) NULL;
  UIDevice *a;
  UIPkg *p;
  int num_batch = 0;
  
  if (GalUtil_OACheckUsage(argc, argv, oas, NULL) == 0) 
    exit(1);
  
  if (GalUtil_OAExtract(argc, argv, oas, "-ui_data",
                        GAL_OA_STRING, &data_file) == 0) {
    GalUtil_Warn("No -ui_data argument. Exiting.");
    exit(1);
  }
  GalUtil_OAExtract(argc, argv, oas, "-batch",
                    GAL_OA_INT, &num_batch);
  a = InitializeUI(data_file, __SendText, num_batch);
  free(data_file);
  if (!a) {
    GalUtil_Warn("Can't initialize UI device. Exiting.");
    exit(1);
  }
  p = (UIPkg *) calloc(1, sizeof(UIPkg));
  p->device = a;
  a->client_data = (void *) p;
  return (void *) p;
}

/* We need our own main() because this is a Hub client exclusively. */

int main(int argc, char **argv)
{
  GalIO_ServerStruct *server;
  GalSS_Environment *env;
  GalIO_CommStruct *gcomm;
  UIPkg *p;
  char *contact_info = (char *) NULL;
  char *session_id = (char *) NULL;

  server = GalSS_CmdlineSetupServerForHubContact(argc, argv, &contact_info,
                                                 &session_id,
                                                 0, -1, GAL_LOOP_TT);
  
  if (!server) {
    GalUtil_Warn("Failed to set up server!");
    exit(1);
  } else if (!GalIO_ServerStart(server)) {
    GalUtil_Warn("Server startup failed");
    exit(1);
  } else {
    int iterations = 1;
    int i = 0;
    
    p = (UIPkg *) GalIO_GetServerData(server);
    if (p->device->batch)
      iterations = p->device->batch;

    for (i = 0; i < iterations; i++) {
      ReinitializeUI(p->device);
      /* Now, set up the connection. */
      env = GalSS_SetupEnvironment((char *) NULL, 0,
                                   (char *) NULL,
                                   contact_info, session_id,
                                   1, server);
      if (!env) {
        GalUtil_Warn("Couldn't create environment, exiting.");
        GalIO_SetServerDone(server);
        GalIO_DestroyServerStruct(server);
        exit(1);
      }
      gcomm = GalSS_EnvComm(env);
      /* This environment will be freed when the
         connection is destroyed. */
      GalSS_EnvMaintainInLocation(gcomm, GalSS_EnvGetSessionID(env),
                                  &(p->env));
      GalSS_EnvUnlock(env);      
      GalSS_RunServer(server);
    }
    ShutdownUIInput(p->device);
    GalIO_SetServerDone(server);
    GalIO_DestroyServerStruct(server);
    exit(0);
  }
}
