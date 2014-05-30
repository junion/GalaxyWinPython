/*
  This file (c) Copyright 1998 - 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _H_CONTINUATION_INTERNAL
#define _H_CONTINUATION_INTERNAL

#include "../io/io_internal.h"
#include "generic-server-internal.h"

typedef struct __galss_continuation {
  GalSS_Environment *env;
  GalSS_ContinuationFn fn;
  void *state;
  void (*state_free_fn)(void *);
  int server_tidx;
} GalSS_Continuation;

/* Used by hub_server.c. */

void _GalSS_FreeContinuationQueue(GalIO_PointerQueue *queue);
void _GalSS_CommEnableContinuations(GalIO_CommStruct *gcomm);
#endif
