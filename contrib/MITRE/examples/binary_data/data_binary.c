/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "galaxy/util.h"

#define SERVER_FUNCTIONS_INCLUDE "data_binary_server.h"
#include "galaxy/server_functions.h"

typedef struct __MTestStruct {
  int first;
  int last;
  char *msg;
} MTestStruct;

static void *encode_test_struct(MTestStruct *s, int *len)
{
  char *encode_buf;

  encode_buf = (char *) malloc(sizeof(char) * (strlen(s->msg) + 64));
  sprintf(encode_buf, "%d %d %s", s->first, s->last, s->msg);
  *len = strlen(encode_buf);
  return (void *) encode_buf;
}

static MTestStruct *decode_test_struct(void *data, int len)
{
  MTestStruct *s = (MTestStruct *) malloc(sizeof(MTestStruct));
  char *char_data = (char *) calloc(len + 1, sizeof(char));
  int where;

  strncpy(char_data, (char *) data, len);
  
  sscanf(char_data, "%d %d %n", &(s->first), &(s->last), &where);
  s->msg = (char *) malloc(sizeof(char) * (1 + (len - where)));
  strncpy(s->msg, char_data + where, len - where);
  s->msg[len - where] = '\0';
  free(char_data);
  return s;
}

Gal_Frame receive_binary(Gal_Frame f, void *server_data)
{
  int size = 0;
  void *buf = Gal_GetBinary(f, ":binary_data", &size);
  MTestStruct *s = decode_test_struct(buf, size);
  
  /* GalUtil_PInfo1("Decoded buf is `%s'\n", buf); */
  GalUtil_PInfo1("First is %d, last is %d, msg is `%s'\n",
		 s->first, s->last, s->msg);
  Gal_SetProp(f, ":test_first", Gal_IntObject(s->first));
  Gal_SetProp(f, ":test_last", Gal_IntObject(s->last));
  Gal_SetProp(f, ":test_msg", Gal_StringObject(s->msg));
  free(s->msg);
  free(s);
  return f;
}  

Gal_Frame reinitialize(Gal_Frame f, void *server_data)
{
  Gal_Frame fr;
  int len;
  void *data;
  MTestStruct *s = (MTestStruct *) malloc(sizeof(MTestStruct));

  s->first = Gal_GetInt(f, ":test_first");
  s->last = Gal_GetInt(f, ":test_last");
  s->msg = Gal_GetString(f, ":test_msg");
  
  /* Create a new message. */
  fr = Gal_MakeFrame("main", GAL_CLAUSE);
  data = encode_test_struct(s, &len);
  free(s);
  Gal_SetProp(fr, ":binary_data", Gal_BinaryObject(data, len));
  free(data);
  GalSS_EnvWriteFrame((GalSS_Environment *) server_data, fr, 0);
  Gal_FreeFrame(fr);
  return (Gal_Frame) NULL;
}
