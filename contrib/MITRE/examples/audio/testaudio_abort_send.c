/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <signal.h>

#include "galaxy/sysdep.h"

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "testaudio_send_server.h"
#include "galaxy/server_functions.h"

#include "testaudio.h"

static int BrokerMethod = BROKER_ORIGINAL_ENV;

enum {BROKER_KILL, EXIT_KILL, KILL_KILL, BUS_KILL};

int KillStyle = BROKER_KILL;
int KillAfter = 1;

/* As soon as a connection comes up, we want to
   mark the broker as done WITHOUT WRITING A DONE MESSAGE.
   Then, have another task checking to see if the broker
   is done, and destroy it. The reason we can't destroy
   the broker from the callback is because the callback queue
   that's currently being run will be destroyed.

   SAM 10/21/01: We have to check a number of conditions:
   not just killing the broker cleanly, but also killing
   the server itself. */

void __mark_broker_for_destruction(GalIO_BrokerStruct *b,
				   void *external_data)
{
  /* Sometimes I might want to wait until multiple connections are
     made before I trigger the broker death. */
  int conns = (int) GalIO_GetBrokerData(b);
  
  conns++;
  /* If we haven't reached the number of connections
     to kill after, return. Otherwise, record the modified
     number of connections. */
  if (conns < KillAfter) return;
  GalIO_SetBrokerData(b, (void *) conns, NULL);
  
  /* Next, flush the output, so that all the
     messages in the outbound queue are guaranteed to be
     sent. */  
  GalIO_BrokerDataOutHandler(b);

  /* On Windows, we need to sleep for a bit, because the
     Hub can lose data if the server exits too quickly. */

  GalUtil_MilliSleep(1000);

  /* Now, figure out how to kill.*/
  switch (KillStyle) {
  case BROKER_KILL:
    /* Mark the broker as done, not OUT done. */
    GalIO_BrokerDataDone(b);
    break;
  case KILL_KILL:
    /* Unceremoniously exit. */
#ifdef WIN32
	  /* On Windows, there's no such thing as kill() or SIGKILL). */
    exit(0);
#else
    kill(_gal_getpid(), SIGKILL);
#endif
    break;
  case EXIT_KILL:
    /* Exit cleanly. */
    exit(0);
    break;
  case BUS_KILL:
    /* Generate a bad seg fault. */
    strcpy((char *) 0, (char *) 1);
    break;      
  }
}

void __destroy_broker(Gal_TaskPkg *p)
{
  GalIO_BrokerStruct *b = (GalIO_BrokerStruct *) Gal_TaskPkgData(p);

  if (GalIO_BrokerIsDone(b)) {
    GalIO_DestroyBrokerStruct(b);
  } else {
    Gal_ReAddTask(p, (void *) b, 100, 0, NULL);
  }
}

static Gal_Frame prepare_audio_frame(GalSS_Environment *env)
{
  Gal_Frame f = Gal_MakeFrame("main", GAL_CLAUSE);
  GalIO_BrokerStruct *b = (GalIO_BrokerStruct *) NULL;
  Gal_Object proxy = (Gal_Object) NULL;
  
  /* Now that we have the audio, we write the binary data
     through the broker. */

  switch (BrokerMethod) {
  case BROKER_ORIGINAL_ENV:    
  case BROKER_ORIGINAL_GCOMM:
    b = GalIO_BrokerDataOutInit(GalSS_EnvComm(env), 0, -1);
    if (b && (GalIO_GetBrokerListenPort(b) > 0)) {
      GalIO_BrokerPopulateFrame(b, f, ":binary_host", ":binary_port");
      GalIO_BrokerWriteString(b, "sample string");
    }
    break;
  case BROKER_PROXY_OBJ:
  case BROKER_PROXY_STREAM:
  case BROKER_PROXY_ORIGINAL:
    proxy = GalSS_ObjProxifyObjectType(env, (Gal_ObjectType) -1, 0, -1);
    if (proxy) {
      Gal_SetProp(f, ":binary_proxy", Gal_CopyObject(proxy));
      GalSS_ObjProxyWrite(proxy, Gal_StringObject("sample string"), 1);
      b = Gal_ProxyObjectBroker(proxy);
    }
    break;
  }

  if (b) {
    GalIO_AddBrokerCallback(b, GAL_BROKER_CONNECTION_EVENT,
			    __mark_broker_for_destruction,
			    (void *) NULL);
    /* Track the number of connections established. */
    GalIO_SetBrokerData(b, (void *) 0, NULL);
    /* Add the destroy callback. */
    Gal_AddTask(__destroy_broker, (void *) b,
		100, 0, NULL);    
    GalSS_EnvStartBroker(env, b, 10);
  }
  return f;
}

Gal_Frame notify(Gal_Frame f, void *server_data)
{
  GalUtil_Print(-1, "Audio send: %s\n", Gal_GetString(f, ":notification"));
  return (Gal_Frame) NULL;
}
static char *oas[] = {
  "-kill_style style", "set the kill style (broker|exit|kill|bus)", "broker",
  "-kill_after n", "how many conns to accept before killing", "1",
  NULL
};

Gal_Frame reinitialize(Gal_Frame f, void *server_data)
{
  Gal_Frame fr;
  char *broker_method = Gal_GetString(f, ":broker_method");

  if ((!broker_method) || !strcmp(broker_method, "original_env")) {
    BrokerMethod = BROKER_ORIGINAL_ENV;
  } else if (!strcmp(broker_method, "original_comm")) {
    BrokerMethod = BROKER_ORIGINAL_GCOMM;
  } else if (!strcmp(broker_method, "proxy_obj")) {
    BrokerMethod = BROKER_PROXY_OBJ;
  } else if (!strcmp(broker_method, "proxy_stream")) {
    BrokerMethod = BROKER_PROXY_STREAM;
  } else if (!strcmp(broker_method, "proxy_original")) {
    BrokerMethod = BROKER_PROXY_ORIGINAL;
  }
  
  /* Send a brokering message. */
  fr = prepare_audio_frame((GalSS_Environment *) server_data);
  GalSS_EnvWriteFrame((GalSS_Environment *) server_data, fr, 0);
  Gal_FreeFrame(fr);
  return (Gal_Frame) NULL;
}

void *_GalSS_init_server(GalIO_ServerStruct *s, int argc, char **argv)
{
  int i;
  char *style = (char *) NULL;
  
  if (!GalUtil_OACheckUsage(argc, argv, oas, &i))
    exit(1);
  GalUtil_OAExtract(argc, argv, oas, "-kill_style", GAL_OA_STRING, &style);
  if (Gal_StringEq(style, "kill")) {
    KillStyle = KILL_KILL;
  } else if (Gal_StringEq(style, "broker")) {
    KillStyle = BROKER_KILL;
  } else if (Gal_StringEq(style, "exit")) {
    KillStyle = EXIT_KILL;
  } else if (Gal_StringEq(style, "bus")) {
    KillStyle = BUS_KILL;
  } else {
    GalUtil_Warn("Unknown kill style, using broker");
    KillStyle = BROKER_KILL;
  }
  if (style) free(style);
  GalUtil_OAExtract(argc, argv, oas, "-kill_after", GAL_OA_INT, &KillAfter);
  return (void *) NULL;
}
