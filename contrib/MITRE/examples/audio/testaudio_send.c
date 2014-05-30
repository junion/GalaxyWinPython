/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdlib.h>
#include <stdio.h>

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "testaudio_send_server.h"
#include "galaxy/server_functions.h"

#include "testaudio.h"

typedef struct __OutData {
  FILE *fp;
  GalIO_BrokerStruct *b;
  Gal_Object proxy;
} OutData;  

static int BrokerMethod = BROKER_ORIGINAL_ENV;

/* The outbound side of the broker test only cares if
   the method to send is either original or not. There are
   a few ways to do it, but in this case I'm just going to
   illustrate proxifying an object here. */

static void __write_data(Gal_TaskPkg *p)
{
  OutData *o = (OutData *) Gal_TaskPkgData(p);
  FILE *fp = o->fp;
  char *buf = (char *) malloc(BLOCKSIZE * sizeof(char));
  int count = fread(buf, sizeof(char), BLOCKSIZE, fp);

  if (count) {
    switch (BrokerMethod) {
    case BROKER_ORIGINAL_ENV:    
    case BROKER_ORIGINAL_GCOMM:
      GalIO_BrokerWriteBinary(o->b, buf, count);
      break;
    case BROKER_PROXY_OBJ:
    case BROKER_PROXY_STREAM:
    case BROKER_PROXY_ORIGINAL:
      GalSS_ObjProxyArrayAdd(o->proxy, buf, count);
      break;
    }
  }
  free(buf);
  if (count == BLOCKSIZE) {
    /* Not done yet. */
    Gal_ReAddTask(p, (void *) o, 10, 0, NULL);
  } else {
    switch (BrokerMethod) {
    case BROKER_ORIGINAL_ENV:    
    case BROKER_ORIGINAL_GCOMM:
      GalIO_BrokerDataOutDone(o->b);
      break;
    case BROKER_PROXY_OBJ:
    case BROKER_PROXY_STREAM:
    case BROKER_PROXY_ORIGINAL:
      GalSS_ObjProxyDone(o->proxy);
      Gal_FreeObject(o->proxy);
      break;
    }
    fclose(fp);
    free(o);
  }
}

static Gal_Frame prepare_audio_frame(GalSS_Environment *env,
				     char *filename, int use_stream)
{
  Gal_Frame f = Gal_MakeFrame("main", GAL_CLAUSE);
  FILE *fp;
  size_t count = 0;
  int total = 0;
  char *buf = (char *) NULL;
  Gal_Object proxy;
  GalIO_BrokerStruct *b;
  OutData *o = (OutData *) NULL;
  
  if (!filename) {
    fprintf(stderr, "No filename provided\n");
    exit(1);
  }
  
  fp = fopen(filename, "rb");
  if (!fp) {
    fprintf(stderr, "Couldn't open %s\n", filename);
    exit(1);
  }

  /* If we're the sending direction, we read binary data from the file until
     we find EOF, and then we build a binary data structure. Once we do that,
     we transmit the data over a socket by transmitting the size
     of the memory buffer, and then transmitting the contents of the
     memory buffer.

     If we're streaming, we do it bit by bit; otherwise, we do it all
     now. */

  if (use_stream) {
    o = (OutData *) calloc(1, sizeof(OutData));
    o->fp = fp;
  } else {
    buf = (char *) malloc(BLOCKSIZE * sizeof(char));
    count = fread(buf, sizeof(char), BLOCKSIZE, fp);
    total = count;
    while (count == BLOCKSIZE) {
      buf = (char *) realloc(buf, total + BLOCKSIZE);
      count = fread(buf + total, sizeof(char), BLOCKSIZE, fp);    
      total += count;
    }
    fclose(fp);
  }

  switch (BrokerMethod) {
  case BROKER_ORIGINAL_ENV:
  case BROKER_ORIGINAL_GCOMM:
    b = GalIO_BrokerDataOutInit(GalSS_EnvComm(env), 0, 10);
    if (b && (GalIO_GetBrokerListenPort(b) > 0)) {
      GalIO_BrokerPopulateFrame(b, f, ":binary_host", ":binary_port");
      if (use_stream) {
	o->b = b;
	Gal_AddTask(__write_data, (void *) o, 10, 0, NULL);
      } else {
	GalIO_BrokerWriteBinary(b, buf, total);
	GalIO_BrokerDataOutDone(b);
      }
    }
    break;
  case BROKER_PROXY_OBJ:
  case BROKER_PROXY_STREAM:
  case BROKER_PROXY_ORIGINAL:
    /* Now that we have the audio, we write the binary data
       through the broker. */
    proxy = GalSS_ObjProxifyObjectType(env, GAL_BINARY, 0, 10);
    if (proxy) {
      if (use_stream) {
	o->proxy = proxy;
	/* The frame will be freed on write, so we have to
	   copy it to make sure the proxy survives, since
	   we plan on writing to it. */
	Gal_SetProp(f, ":binary_proxy", Gal_CopyObject(proxy));
	Gal_AddTask(__write_data, (void *) o, 10, 0, NULL);
      } else {
	GalSS_ObjProxyArrayAdd(proxy, buf, total);
	GalSS_ObjProxyDone(proxy);
	Gal_SetProp(f, ":binary_proxy", proxy);
      }
    }
    break;
  }
  free(buf);
  return f;
}

Gal_Frame notify(Gal_Frame f, void *server_data)
{
  GalUtil_Print(-1, "Audio send: %s\n", Gal_GetString(f, ":notification"));
  return (Gal_Frame) NULL;
}

Gal_Frame reinitialize(Gal_Frame f, void *server_data)
{
  char *audiofile = Gal_GetString(f, ":audiofile");
  Gal_Frame fr;
  char *broker_method = Gal_GetString(f, ":broker_method");
  int use_stream = Gal_GetInt(f, ":use_stream");

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
  
  if (audiofile) {
    /* Send a brokering message. */
    fr = prepare_audio_frame((GalSS_Environment *) server_data,
			     audiofile, use_stream);
    GalSS_EnvWriteFrame((GalSS_Environment *) server_data, fr, 0);
    Gal_FreeFrame(fr);
  }
  return (Gal_Frame) NULL;
}
