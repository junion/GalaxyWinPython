/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef __MITRE_GALAXY_H__
#define __MITRE_GALAXY_H__

#ifdef __cplusplus
extern "C" {
#endif  

#include <stdarg.h>
#include "galaxy/galaxy_all.h"
/* From stdin_utility.c */

struct __MGal_StdinPoll;

typedef Gal_Frame (*MGal_StdinFrameCreator)(char *str, struct __MGal_StdinPoll *pollb);

typedef struct __MGal_StdinPoll {
  char *prompt;
  MGal_StdinFrameCreator fn;
  GalIO_CommStruct *gcomm;
  GalSS_Environment *env;
  Gal_StringBuffer *input_buf;
  int ms;
  /* This is if the task is active. */
  int active;
  /* This is if the prompt has been fired. */
  int prompt_fired;
  void *poll_data;
  Gal_TaskPkg *task;
#ifdef WIN32
  /* On Windows, we'll be using threads for the stdin read. */
  GalUtil_ThreadID thread_id;
  GalUtil_LocalMutex mutex;
  Gal_StringBuffer *stdin_pipe;
  HANDLE termination_event;
#endif
} MGal_StdinPoll;  

void MGal_ActivateStdinPoll(MGal_StdinPoll *poll_struct);
MGal_StdinPoll *MGalIO_CreateStdinPoll(char *prompt, GalIO_CommStruct *comm,
				       MGal_StdinFrameCreator fn, int ms,
				       int activate);
void MGal_FreeStdinPoll(MGal_StdinPoll *poll_struct);
int MGal_PollStdin(MGal_StdinPoll *poll_struct);
void MGal_SetStdinPollData(MGal_StdinPoll *poll_struct, void *data);
void *MGal_GetStdinPollData(MGal_StdinPoll *poll_struct);  
void MGal_SetStdinPollPrompt(MGal_StdinPoll *poll_struct, char *prompt);

MGal_StdinPoll *MGalSS_EnvCreateStdinPoll(char *prompt,
					  GalSS_Environment *env,
					  MGal_StdinFrameCreator fn,
					  int ms, int activate);
/* From frame_utility.c */

Gal_Frame MGal_CreateFullFrame(char *name, int type, int num_pairs, ...);

/* From broker_utility.c */

typedef void (*MGal_BrokerDTHandler)(GalIO_BrokerStruct *broker_struct,
				     void *data, int n_samples);  

typedef struct __MGal_BrokerDTEntry {
  Gal_ObjectType data_type;
  void *val;
  MGal_BrokerDTHandler handler;
  struct __MGal_BrokerDTEntry *next;
} MGal_BrokerDTEntry;  

GalIO_BrokerStruct *MGal_AddOutgoingBrokering(GalIO_CommStruct *gcomm,
					      Gal_Frame fr, int poll_ms,
					      int timeout_ms);
void MGal_AddBrokerDTHandler(Gal_ObjectType dt, void *val,
			     MGal_BrokerDTHandler h);
GalIO_BrokerStruct *MGal_AddIncomingBrokering(Gal_Frame fr,
					      int poll_ms,
					      void *caller_data,
					      int activate);

/* From binary_utility.c */

typedef void *(*MGal_BinaryDataEncoder)(void *obj, int *size);
typedef void *(*MGal_BinaryDataDecoder)(void *data, int size);

typedef struct __MGal_BinaryDataType {
  int data_type;
  MGal_BinaryDataEncoder encoder;
  MGal_BinaryDataDecoder decoder;
  struct __MGal_BinaryDataType *next;
} MGal_BinaryDataType;

void MGal_AddBinaryDataType(int data_type,
			    MGal_BinaryDataEncoder encoder,
			    MGal_BinaryDataDecoder decoder);
Gal_Object MGal_OpaqueObject(void *obj, int data_type);
void *MGal_GetOpaque(Gal_Frame fr, char *key, int *data_type);
void *MGal_GetOpaqueWarn(Gal_Frame fr, char *key, int data_type);

/* From mgutil_init.c */

void MGal_InitializeStatics(void);

#ifdef __cplusplus
}
#endif

#endif /* __MITRE_GALAXY_H__ */
