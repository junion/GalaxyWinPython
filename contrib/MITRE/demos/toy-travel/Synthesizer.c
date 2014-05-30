/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "Synthesizer_server.h"
#include "galaxy/server_functions.h"

#include "component_engine.h"

/* When the fake synthesizer server sends a new "audio output", it creates
   a real broker connection and populates it with random data.
   Nonetheless, this illustrates the actual behavior of a
   typical synthesizer when it's sending audio data to the Hub. */

typedef struct LocalSynthesisRecord {
  GalSS_BrokerProxy *bp;
  SynthesisRecord *s;
} LocalSynthesisRecord;

void __PollSynthesizer(Gal_TaskPkg *pkg)
{
  LocalSynthesisRecord *lsr = (LocalSynthesisRecord *) Gal_TaskPkgData(pkg);
  int num_samples;
  void *data = PollSynthesis(lsr->s, &num_samples);

  if (data) {
    GalSS_ProxyArrayAdd(lsr->bp, data, num_samples);
    free(data);
  }
  if (SynthesisIsDone(lsr->s)) {
    FreeSynthesis(lsr->s);
    GalSS_ProxyDone(lsr->bp);
    GalSS_FreeBrokerProxy(lsr->bp);
    free(lsr);
  } else {
    Gal_ReAddTask(pkg, (void *) lsr, 100, 0, NULL);
  }
}

Gal_Frame Synthesize(Gal_Frame f, void *server_data)
{
  char *output_string;
  Gal_Frame output_f;
  GalSS_BrokerProxy *bp;
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  SynthesisRecord *s;
  LocalSynthesisRecord *lsr;

  /* Deconstruction and type checking */
  output_string = Gal_GetString(f, ":output_string");
  if (!output_string) {
    GalSS_EnvError(env, "no output string");
    return (Gal_Frame) NULL;
  }

  s = InitializeSynthesis(output_string);

  if (!s) {
    GalSS_EnvError(env, "can't initialize synthesis");
    return (Gal_Frame) NULL;
  }

  /* Set up outbound broker. */
  output_f = Gal_MakeFrame("FromSynthesizer", GAL_CLAUSE);
  bp = GalSS_ProxifyObjectType(env, GAL_INT_16, 0, 10);
	
  if (bp) {
    Gal_SetProp(output_f, ":proxy", Gal_CreateProxyObject(bp, 0));
    /* Notify the Hub that data is coming. */
    Gal_SetProp(output_f, ":sample_rate",
		Gal_IntObject(s->sample_rate));
    Gal_SetProp(output_f, ":encoding_format",
		Gal_StringObject(s->encoding_format));
    GalSS_EnvWriteFrame(env, output_f, 0);
    Gal_FreeFrame(output_f);
    
    /* Set up a poll to get audio */
    lsr = (LocalSynthesisRecord *) calloc(1, sizeof(LocalSynthesisRecord));
    lsr->bp = bp;
    lsr->s = s;
    Gal_AddTask(__PollSynthesizer, (void *) lsr, 100, 0, NULL);
  } else {
    GalSS_EnvError(env, "can't set up synthesis broker");
    FreeSynthesis(s);
  }
  return (Gal_Frame) NULL;
}

/* Command line argument information */

static char *oas[] = {
  "-synthesizer_data file",  "synthesizer data", (char *) NULL,
  NULL
};

/* Server initialization function */

void *_GalSS_init_server(GalIO_ServerStruct *server, int argc, char **argv)
{
  char *data_file = (char *) NULL;
  
  if (GalUtil_OACheckUsage(argc, argv, oas, NULL) == 0)	
    exit(1);
  
  if (GalUtil_OAExtract(argc, argv, oas, "-synthesizer_data",
			GAL_OA_STRING, &data_file) == 0) {
    GalUtil_Warn("No -synthesizer_data argument. Exiting.");
    exit(1);
  }
  InitializeSynthesizer(data_file);
  free(data_file);
  return (void *) NULL;
}
