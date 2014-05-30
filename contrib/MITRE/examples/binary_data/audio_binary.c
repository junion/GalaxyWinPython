/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "MITRE_galaxy.h"
#define SERVER_FUNCTIONS_INCLUDE "audio_binary_server.h"
#include "galaxy/server_functions.h"

#define BLOCKSIZE 1024

/* I'm going to read a file and send it along. */

/* char *GlobalBuf; */

#define AUDIO_START "start"
#define AUDIO_END "end"

static Gal_Frame prepare_audio_frame(char *filename)
{
  Gal_Frame f = Gal_MakeFrame("main", GAL_CLAUSE);
  FILE *fp;
  size_t count;
  int total;
  char *buf;
  
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
     memory buffer. */

  buf = (char *) malloc(BLOCKSIZE * sizeof(char));
  count = fread(buf, sizeof(char), BLOCKSIZE, fp);
  total = count;
  while (count == BLOCKSIZE) {
    buf = (char *) realloc(buf, total + BLOCKSIZE);
    count = fread(buf + total, sizeof(char), BLOCKSIZE, fp);    
    total += count;
  }
  fclose(fp);

  /* Now that we have the audio, we add a binary element. */
  /* GlobalBuf = buf; */
  Gal_SetProp(f, ":audio_data", Gal_BinaryObject((void *) buf, total));
  free(buf);
  return f;
}

/* Incoming */

Gal_Frame receive_audio(Gal_Frame f, void *server_data)
{
  int size = 0;
  FILE *fp;
  void *buf = Gal_GetBinary(f, ":audio_data", &size);
  
  fp = fopen("/dev/audio", "w");
  if (!fp) {
    GalUtil_Warn("Couldn't open /dev/audio");
  } else {
    fwrite((char *) buf, sizeof(char), size, fp);
    fflush(fp);
    fclose(fp);
  }
  return (Gal_Frame) NULL;
}

Gal_Frame reinitialize(Gal_Frame f, void *server_data)
{
  char *audiofile = Gal_GetString(f, ":audiofile");
  Gal_Frame fr;

  if (audiofile) {
    /* Send a brokering message. */
    fr = prepare_audio_frame(audiofile);
    GalSS_EnvWriteFrame((GalSS_Environment *) server_data, fr, 0);
    Gal_FreeFrame(fr);
  }
  return (Gal_Frame) NULL;
}
