/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#define BLOCKSIZE 1024

/* I'm going to read a file and send it along. */

/* char *GlobalBuf; */

/* #define AUDIO_START "start"
   #define AUDIO_END "end" */

typedef struct __DataHandler {
  GalIO_CommStruct *gcomm;
  char *data_buf;
  int size;
} DataHandler;

enum {BROKER_ORIGINAL_ENV,
      BROKER_ORIGINAL_GCOMM,
      BROKER_PROXY_OBJ,
      BROKER_PROXY_STREAM,
      BROKER_PROXY_ORIGINAL};
