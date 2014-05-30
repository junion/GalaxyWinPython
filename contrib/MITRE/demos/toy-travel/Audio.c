/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"
#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "Audio_server.h"
#include "galaxy/server_functions.h"

#include "component_engine.h"

typedef struct __AudioPkg {
  AudioDevice *device;
  GalSS_BrokerProxy *out_proxy;
  GalSS_BrokerProxy *in_proxy;
  GalSS_Environment *env;
  int enable_input_pending;
} AudioPkg;

/*
  This is an example of what an "audio" server would look like.
  Instead of listening on an audio channel or a telephone
  connection, it polls stdin for instructions from the user.
  The user can hit
  <return> to send the next utterance, or when there
  are no more utterances, <return> will disconnect and exit.

  This server is restricted to being a Hub client. When the
  user asks for a connection, a connection will be made
  to the Hub.
  
  This example will not yet run on Windows NT, because it
  relies on the MITRE stdin poll, which does not run on NT
  at the moment.
*/

static void __AudioPoll(AudioDevice *a, void *data, int num_samples)
{
  AudioPkg *p = (AudioPkg *) a->client_data;
  Gal_Frame output_f;

  printf("[Previous device state is %s.]\n[Current device state is %s.]\n",
	 AudioDeviceStateName(a->previous_device_state),
	 AudioDeviceStateName(a->current_device_state));
  fflush(stdout);
  
  switch (a->current_device_state) {
  case AUDIO_IDLE:
    switch (a->previous_device_state) {
    case AUDIO_RECORDING:
      /* We have data and this is the last chunk. */
      if (!p->out_proxy) {
	GalUtil_Warn("No out proxy to send audio");
      } else {
	GalSS_ProxyArrayAdd(p->out_proxy, data, num_samples);
	printf("[Audio data from user (%d samples).]\n", num_samples);
	fflush(stdout);
	GalSS_ProxyDone(p->out_proxy);
	GalSS_FreeBrokerProxy(p->out_proxy);
	p->out_proxy = (GalSS_BrokerProxy *) NULL;
	printf("[Audio data from user done.]\n");
	fflush(stdout);
      }
      break;
    }
    break;
  case AUDIO_RECORDING:
    switch (a->previous_device_state) {
    case AUDIO_IDLE:
      /* We have data and this is the first chunk. */
      if (p->out_proxy) {
	GalUtil_Warn("Out proxy already exists");
      } else {
	output_f = Gal_MakeFrame("FromAudio", GAL_CLAUSE);	
	/* Set up the outgoing broker connection. */
	p->out_proxy = GalSS_ProxifyObjectType(p->env, GAL_INT_16, 0, 10);
	
	if (p->out_proxy) {
	  /* Send the first chunk. */
	  GalSS_ProxyArrayAdd(p->out_proxy, data, num_samples);
	  printf("[Audio data from user (%d samples).]\n", num_samples);
	  fflush(stdout);
	  /* Notify the Hub that data is coming. */
	  Gal_SetProp(output_f, ":sample_rate",
		      Gal_IntObject(a->sample_rate));
	  Gal_SetProp(output_f, ":encoding_format",
		      Gal_StringObject(a->encoding_format));
	  Gal_SetProp(output_f, ":proxy",
		      Gal_CreateProxyObject(p->out_proxy, 0));
	  GalSS_EnvWriteFrame(p->env, output_f, 0);	  
	}
	Gal_FreeFrame(output_f);
      }
      break;
    case AUDIO_RECORDING:
      /* We have data and this is an intermediate chunk. */
      if (!p->out_proxy) {
	GalUtil_Warn("No out proxy to send audio");
      } else {
	GalSS_ProxyArrayAdd(p->out_proxy, data, num_samples);
	printf("[Audio data from user (%d samples).]\n", num_samples);
	fflush(stdout);
      }
      break;
    }
    break;
  case AUDIO_PLAYING:
    break;
  case AUDIO_UNAVAILABLE:
    /* The device shut down. End the session and exit. */
    /* We ran out of inputs. Disconnect. */
    printf("Audio no longer available. Disconnecting.\n");
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
    break;
  }
  if (data)
    free(data);
}

/* Here's the dispatch function which "plays" the audio, and
   its supporting tools. */

/* The finalizer is called when the broker connection is
   done. */

static void __AudioOutputCallback(GalSS_Environment *env,
				  Gal_ObjectType o_type,
				  Gal_Object elt, void *caller_data)
{
  AudioPkg *p = (AudioPkg *) caller_data;
  int n_samples = 0;
  void *data = Gal_Int16Value(elt, &n_samples);
  
  printf("[Audio data to user (%d samples)]\n", n_samples);
  fflush(stdout);
  PlayAudio(p->device, data, n_samples);
  Gal_FreeObject(elt);
}


static void __AudioOutputFinalizer(GalSS_Environment *env,
				  Gal_ObjectType o_type,
				  void *caller_data)
{
  AudioPkg *p = (AudioPkg *) caller_data;
  
  /* The current prompt will always be the connected one
     in this case. */
  printf("[Audio data to user is finalized (%d samples).]\n",
	 p->device->samples_read);
  fflush(stdout);
  PlayingIsDone(p->device);
  GalSS_FreeBrokerProxy(p->in_proxy);
  p->in_proxy = (GalSS_BrokerProxy *) NULL;

  if (p->enable_input_pending) {
    EnableAudioInput(p->device);
    p->enable_input_pending = 0;
  }
}

Gal_Frame EnableInput(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  GalIO_CommStruct *gcomm = GalSS_EnvComm(env);
  AudioPkg *p = (AudioPkg *) GalIO_GetCommServerData(gcomm);

  EnableAudioInput(p->device);
  return (Gal_Frame) NULL;
}

Gal_Frame Play(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  GalIO_CommStruct *gcomm = GalSS_EnvComm(env);
  AudioPkg *p = (AudioPkg *) GalIO_GetCommServerData(gcomm);
  GalSS_BrokerProxy *proxy = Gal_GetProxy(f, ":proxy");
  int sample_rate = Gal_GetInt(f, ":sample_rate");
  char *encoding_format = Gal_GetString(f, ":encoding_format");
  int enable_input = Gal_GetInt(f, ":enable_input");

  if (proxy && (sample_rate > 0)) {
    if (p->in_proxy) {
      GalUtil_Warn("In proxy already exists");
    } else {
      EnableAudioOutput(p->device, encoding_format, sample_rate);

      p->in_proxy = GalSS_CopyBrokerProxy(proxy);
      if (enable_input)
	p->enable_input_pending = 1;
      GalSS_Unproxify(env, p->in_proxy, __AudioOutputCallback,
                      __AudioOutputFinalizer, NULL, 1, 0, (void *) p, NULL);
    }
  }
  return (Gal_Frame) NULL;
}

/* When the Audio server asks for a connection, the Hub connects,
   and the normal flow of control resumes. The reinitialize
   message is called, where we manage some of the connectedness
   information. */

Gal_Frame reinitialize(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  GalIO_CommStruct *gcomm = GalSS_EnvComm(env);
  AudioPkg *p = (AudioPkg *) GalIO_GetCommServerData(gcomm);
  Gal_Frame session_f;
  
  GalSS_EnvMaintainInLocation(gcomm, GalSS_EnvGetSessionID(env), &(p->env));

  /* Create the session. */
  session_f = Gal_MakeFrame("OpenAudioSession", GAL_CLAUSE);
  GalSS_EnvWriteFrame(env, session_f, 0);
  Gal_FreeFrame(session_f);

  /* Don't enable the audio device yet - only after the greeting. */
  
  return (Gal_Frame) NULL;
}

/* Command line argument information */

static char *oas[] = {
  "-audio_data file",  "audio data", (char *) NULL,
  "-batch num_times", "run the UI in batch mode num_times", NULL,
  NULL
};

/* Server initialization function */

void *_GalSS_init_server(GalIO_ServerStruct *server, int argc, char **argv)
{
  char *data_file = (char *) NULL;
  AudioDevice *a;
  AudioPkg *p;
  int num_batch = 0;
  
  if (GalUtil_OACheckUsage(argc, argv, oas, NULL) == 0)	
    exit(1);
  
  if (GalUtil_OAExtract(argc, argv, oas, "-audio_data",
			GAL_OA_STRING, &data_file) == 0) {
    GalUtil_Warn("No -audio_data argument. Exiting.");
    exit(1);
  }
  GalUtil_OAExtract(argc, argv, oas, "-batch",
		    GAL_OA_INT, &num_batch);
  a = InitializeAudio(data_file, __AudioPoll, num_batch);
  free(data_file);
  if (!a) {
    GalUtil_Warn("Can't initialize audio device. Exiting.");
    exit(1);
  }
  p = (AudioPkg *) calloc(1, sizeof(AudioPkg));
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
  AudioPkg *p;
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
    
    p = (AudioPkg *) GalIO_GetServerData(server);
    if (p->device->batch)
      iterations = p->device->batch;

    for (i = 0; i < iterations; i++) {
      ReinitializeAudio(p->device);
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
    ShutdownAudioInput(p->device);
    GalIO_SetServerDone(server);
    GalIO_DestroyServerStruct(server);
    exit(0);
  }
}
