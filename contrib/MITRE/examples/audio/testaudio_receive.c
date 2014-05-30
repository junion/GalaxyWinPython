/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>

#include "galaxy/sysdep.h"

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "testaudio_receive_server.h"
#include "galaxy/server_functions.h"

#include "testaudio.h"

/* Incoming. */

/* This is the audio handler for the original method and the
   backward compatibility with broker proxies. */

static void audio_handler(GalIO_BrokerStruct *broker_struct,
                          void *data, Gal_ObjectType data_type,
                          int n_samples)
{
  DataHandler *d = (DataHandler *) GalIO_GetBrokerData(broker_struct);
  
  switch (data_type) {
  case GAL_BINARY:
    if (d->data_buf)
      d->data_buf = (char *) realloc(d->data_buf, n_samples + d->size);
    else
      d->data_buf = (char *) malloc(n_samples + d->size);
    _gal_bcopy(data, d->data_buf + d->size, n_samples);
    d->size += n_samples;
    free(data);
    break;
  default:
    GalUtil_Warn("Unknown data type %s\n", Gal_ObjectTypeString(data_type));
  }
}

void __FreeDataHandler(void *data)
{
  DataHandler *d = (DataHandler *) data;
  free(d->data_buf);
  free(d);
}

void __gcomm_notify(GalIO_CommStruct *gcomm, char *msg)
{
  Gal_Frame f = Gal_MakeFrame("notify", GAL_CLAUSE);

  Gal_SetProp(f, ":notification", Gal_StringObject(msg));
  GalIO_CommWriteFrame(gcomm, f, 0);
  Gal_FreeFrame(f);
}

void __env_notify(GalSS_Environment *env, char *msg)
{
  Gal_Frame f = Gal_MakeFrame("notify", GAL_CLAUSE);

  Gal_SetProp(f, ":notification", Gal_StringObject(msg));
  GalSS_EnvWriteFrame(env, f, 0);
  Gal_FreeFrame(f);
}

void __play_audio(void *data, int data_size)
{
  FILE *fp = fopen("/dev/audio", "w");
  
  if (!fp) {
    GalUtil_Warn("Couldn't open /dev/audio");
  } else {
    fwrite(data, sizeof(char), data_size, fp);
    fflush(fp);
    fclose(fp);
  }
}

/* This is the done callback for the dispreferred method
   with the bare connection. */

void __gcomm_report_done(GalIO_BrokerStruct *b, void *data)
{
  DataHandler *d = (DataHandler *) GalIO_GetBrokerData(b);

  __play_audio(d->data_buf, d->size);
  __gcomm_notify(d->gcomm, "Audio received.");
}

/* This is the abort callback for the dispreferred method
   with the bare connection. */

void __gcomm_report_abort(GalIO_BrokerStruct *b, void *data)
{
  DataHandler *d = (DataHandler *) GalIO_GetBrokerData(b);

  __gcomm_notify(d->gcomm, "Audio aborted.");
}

/* This is the done callback for the methods using
   the environment: the original one, and the one for
   backward compatibility with the broker proxies. */

void __env_report_done(GalIO_BrokerStruct *b, void *data)
{
  DataHandler *d = (DataHandler *) GalIO_GetBrokerData(b);
  GalSS_Environment *env = GalSS_BrokerGetEnvironment(b);

  __play_audio(d->data_buf, d->size);
  __env_notify(env, "Audio received.");
}

/* This is the abort callback for the methods using
   the environment: the original one, and the one for
   backward compatibility with the broker proxies. */

void __env_report_abort(GalIO_BrokerStruct *b, void *data)
{
  GalSS_Environment *env = GalSS_BrokerGetEnvironment(b);
  
  __env_notify(env, "Audio aborted.");
}

/* This is the data handler for the proxy. In the way
   we've set up, it's called once, at the end. Remember, we
   own the object passed in, so we have to free it. */

void proxy_audio_handler(GalSS_Environment *env, Gal_ObjectType proxy_type,
		          Gal_Object elt, void *caller_data)
{
  if (proxy_type == GAL_BINARY) {
    int size;
    void *data;

    data = Gal_BinaryValue(elt, &size);
    __play_audio(data, size);
  }
  Gal_FreeObject(elt);
}

/* This is the done handler for the proxy. */

void __proxy_report_done(GalSS_Environment *env, Gal_ObjectType proxy_type,
			 void *caller_data)
{
  __env_notify(env, "Audio received.");
}

/* This is the abort handler for the proxy. */

void __proxy_report_abort(GalSS_Environment *env, Gal_ObjectType proxy_type,
			  void *caller_data)
{
  __env_notify(env, "Audio aborted.");
}

/* There are several ways of unbrokering data. We
   illustrate a number of them here.

   (a) Use GalSS_ObjUnproxifyObject(), with broker
       proxies. This will block until the data is retrieved.
   (b) Use GalSS_ObjUnproxify(), with broker proxies.
       This will set up a callback to handle the data.
   (c) Use the original low-level brokering method. It is
       not optimized for typed data streams, like the one
       we can use here. We will test this both with
       environments (the preferred method) and bare connections
       (the dispreferred method).
   (d) Use GalSS_EnvBrokerProxyObjInInit(), which translates
       an incoming proxy into the old unbrokering method.
*/

static int BrokerMethod = BROKER_ORIGINAL_ENV;

Gal_Frame reinitialize(Gal_Frame f, void *server_data)
{
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
  return (Gal_Frame) NULL;
}  

Gal_Frame receive_audio(Gal_Frame f, void *server_data)
{
  DataHandler *d = (DataHandler *) NULL;
  GalIO_BrokerStruct *b = (GalIO_BrokerStruct *) NULL;
  char *host;
  int port;
  Gal_Object proxy;
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  
  switch (BrokerMethod) {
  case BROKER_ORIGINAL_ENV:
  case BROKER_ORIGINAL_GCOMM:
  case BROKER_PROXY_ORIGINAL:
    /* We need a cache for the data. The proxies have the
       capacity to take care of this for us. */
    d = (DataHandler *) malloc(sizeof(DataHandler));
    d->data_buf = (char *) NULL;
    d->size = 0;
  }

  /* If we're using the original method with bare
     connections, we also need to save away the connection
     object. This is one of the reasons the environment
     method is the preferred original method. */
  if (BrokerMethod == BROKER_ORIGINAL_GCOMM) {
    d->gcomm = GalSS_EnvComm((GalSS_Environment *) server_data);
  }

  switch (BrokerMethod) {
  case BROKER_ORIGINAL_ENV:
  case BROKER_ORIGINAL_GCOMM:
    /* In both these cases, we expect a host, port and call ID
       arriving separately from the Hub. There's also no type
       information associated with the stream. */
    host = Gal_GetString(f, ":binary_host");
    port = Gal_GetInt(f, ":binary_port");
    
    if (host && port) {
      /* If this is the environment method, we use one function;
	 we use another for the bare connection. */
      switch (BrokerMethod) {
      case BROKER_ORIGINAL_ENV:
	b = GalSS_EnvBrokerDataInInit(env, host, (unsigned short) port,
				      f, audio_handler,
				      -1, d, __FreeDataHandler);
	if (b) {
	  GalIO_AddBrokerCallback(b, GAL_BROKER_ABORT_EVENT,
				  __env_report_abort, (void *) NULL);
	  GalIO_AddBrokerCallback(b, GAL_BROKER_DATA_DONE_EVENT,
				  __env_report_done, (void *) NULL);
	  GalIO_SetBrokerActive(b);

	  GalSS_EnvStartBroker(env, b, 0);
	}
	break;
      case BROKER_ORIGINAL_GCOMM:
	b = GalIO_CommBrokerDataInInit(d->gcomm, host, (unsigned short) port,
				       f, audio_handler,
				       -1, d, __FreeDataHandler);
	if (b) {
	  GalIO_AddBrokerCallback(b, GAL_BROKER_ABORT_EVENT,
				  __gcomm_report_abort, (void *) NULL);
	  GalIO_AddBrokerCallback(b, GAL_BROKER_DATA_DONE_EVENT,
				  __gcomm_report_done, (void *) NULL);
	  GalIO_SetBrokerActive(b);

	  GalSS_EnvStartBroker(env, b, 0);
	}
	break;
      }
    }
    if (!b)
      free(d);
    break;
    
  case BROKER_PROXY_OBJ:
    /* We get the proxy object, and if it's a proxy, we
       synchronously retrieve the data. Note that the behavior
       which would be in the callbacks in other methods
       follows immediately here. The object which is
       returned is cached in the proxy object, which retains
       "ownership" of it. Therefore, it will be freed when
       the dispatch function exits. */
    proxy = Gal_GetObject(f, ":binary_proxy");
    
    if (Gal_Proxyp(proxy)) {
      Gal_Object bdata = GalSS_ObjUnproxifyObject(env, proxy);
      
      if (Gal_Binaryp(bdata)) {
	int size;
	void *data;

	data = Gal_BinaryValue(bdata, &size);
	__play_audio(data, size);
	__env_notify(env, "Audio received.");
      } else {
	/* Assume something failed violently. */
	__env_notify(env, "Audio aborted.");
      }
    }
    break;
    
  case BROKER_PROXY_STREAM:
    /* We get the proxy object, and proceed asynchronously. The
       object passed into the callback is owned by the callback.
       We use a delayed callback, which will cache all the
       data for us and call us when it's done. So the work
       that's handled in the DATA_DONE callback is actually
       handled in the data handler here. Just to  have a done
       handler, we'll send the notification from there. */
    proxy = Gal_GetObject(f, ":binary_proxy");
    
    if (Gal_Proxyp(proxy)) {
      GalSS_ObjUnproxify(env, proxy, proxy_audio_handler,
			 __proxy_report_done, __proxy_report_abort,
			 0, 0, NULL, NULL);
    }
    break;
    
  case BROKER_PROXY_ORIGINAL:
    /* For the backward compatibility with the original brokering,
       we use a minor variation on the broker environment method. */
    proxy = Gal_GetObject(f, ":binary_proxy");
    if (proxy) {
      b = GalSS_EnvBrokerProxyObjInInit(env, proxy, audio_handler,
				     -1, d, __FreeDataHandler);
      if (b) {
	GalIO_AddBrokerCallback(b, GAL_BROKER_ABORT_EVENT,
				__env_report_abort, (void *) NULL);
	GalIO_AddBrokerCallback(b, GAL_BROKER_DATA_DONE_EVENT,
				  __env_report_done, (void *) NULL);
	GalIO_SetBrokerActive(b);
	
	GalSS_EnvStartBroker(env, b, 0);
      }
    }
    if (!b)
      free(d);
    break;
  }
  return (Gal_Frame) NULL;
}
