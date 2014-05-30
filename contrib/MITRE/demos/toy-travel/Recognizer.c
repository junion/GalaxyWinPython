/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"
#define SERVER_FUNCTIONS_INCLUDE "Recognizer_server.h"
#include "galaxy/server_functions.h"

#include "component_engine.h"

typedef struct LocalRecognizerRecord {
  GalSS_BrokerProxy *bp;
  RecognizerRecord *c;
} LocalRecognizerRecord;

static void __SRCallback(GalSS_Environment *env,
			 Gal_ObjectType otype,
			 Gal_Object elt, void *caller_data)
{
  LocalRecognizerRecord *lrr = (LocalRecognizerRecord *) caller_data;
  int n_samples = 0;
  void *data = Gal_Int16Value(elt, &n_samples);
  
  GalUtil_PInfo2("SR callback: got %s data (%d samples)\n",
		 Gal_ObjectTypeString(otype),
		 n_samples);
  IncrementalRecognize(lrr->c, data, n_samples);
  Gal_FreeObject(elt);
}

/* The finalizer is called when the broker connection is
   done. When the recognizer finalizes, it should send a frame
   to the Hub. We can retrieve the Hub information from
   the data slot in the callback, which will be a RecognizerRecord. */

static void __SRFinalizer(GalSS_Environment *env,
			  Gal_ObjectType otype,
			  void *caller_data)
{
  LocalRecognizerRecord *lrr = (LocalRecognizerRecord *) caller_data;
  char *sr_result = FinishRecognition(lrr->c);
  Gal_Frame msg_result;

  if (!sr_result) {
    msg_result = Gal_ReadFrameFromString("{c FromRecognizer :recognizer_error 1 }");
  } else {
    msg_result = Gal_MakeFrame("FromRecognizer", GAL_CLAUSE);
    Gal_SetProp(msg_result, ":input_string", Gal_StringObject(sr_result));
  }
  FreeRecognizerRecord(lrr->c);
  GalSS_FreeBrokerProxy(lrr->bp);
  free(lrr);
  GalSS_EnvWriteFrame(env, msg_result, 0);
  Gal_FreeFrame(msg_result);
}

Gal_Frame Recognize(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  GalSS_BrokerProxy *bp = Gal_GetProxy(f, ":proxy");
  int sample_rate = Gal_GetInt(f, ":sample_rate");
  char *encoding_format = Gal_GetString(f, ":encoding_format");
  
  if (bp && (sample_rate > 0)) {
    RecognizerRecord *c = InitializeRecognition(encoding_format,
						sample_rate);
    LocalRecognizerRecord *lrr = (LocalRecognizerRecord *) calloc(1, sizeof(LocalRecognizerRecord));

    lrr->c = c;
    lrr->bp = GalSS_CopyBrokerProxy(bp);
    GalSS_Unproxify(env, lrr->bp, __SRCallback, __SRFinalizer,
		    NULL, 1, 0, (void *) lrr, NULL);
  }
  return (Gal_Frame) NULL;
}

/* Command line argument information */

static char *oas[] = {
  "-recognizer_data file",  "recognizer data", (char *) NULL,
  NULL
};

/* Server initialization function */

void *_GalSS_init_server(GalIO_ServerStruct *server, int argc, char **argv)
{
  char *data_file = (char *) NULL;
  
  if (GalUtil_OACheckUsage(argc, argv, oas, NULL) == 0)	
    exit(1);
  
  if (GalUtil_OAExtract(argc, argv, oas, "-recognizer_data",
			GAL_OA_STRING, &data_file) == 0) {
    GalUtil_Warn("No -recognizer_data argument. Exiting.");
    exit(1);
  }
  InitializeRecognizer(data_file);
  free(data_file);
  return (void *) NULL;
}
