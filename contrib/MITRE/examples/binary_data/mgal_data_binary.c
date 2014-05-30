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

#define SERVER_FUNCTIONS_INCLUDE "mgal_data_binary_server.h"
#include "galaxy/server_functions.h"
#include "MITRE_galaxy.h"

enum {TEST1_DT, TEST2_DT};

typedef struct __MTestStruct1 {
  int first;
  int last;
  char *msg;
} MTestStruct1;

typedef struct __MTestStruct2 {
  int key;
  char *val;
} MTestStruct2;

static void *encode_test_struct1(void *data, int *len)
{
  char *encode_buf;
  MTestStruct1 *s = (MTestStruct1 *) data;

  encode_buf = (char *) malloc(sizeof(char) * (strlen(s->msg) + 64));
  sprintf(encode_buf, "%d %d %s", s->first, s->last, s->msg);
  *len = strlen(encode_buf);
  return (void *) encode_buf;
}

static void *decode_test_struct1(void *data, int len)
{
  MTestStruct1 *s = (MTestStruct1 *) malloc(sizeof(MTestStruct1));
  int where;
  char *s_data = (char *) data;
  
  sscanf(s_data, "%d %d %n", &(s->first), &(s->last), &where);
  s->msg = (char *) malloc(sizeof(char) * (1 + (len - where)));
  strncpy(s->msg, s_data + where, len - where);
  s->msg[len - where] = '\0';
  return (void *) s;
}

static void *encode_test_struct2(void *data, int *len)
{
  char *encode_buf;
  MTestStruct2 *s = (MTestStruct2 *) data;

  encode_buf = (char *) malloc(sizeof(char) * (strlen(s->val) + 64));
  sprintf(encode_buf, "%d %s", s->key, s->val);
  printf("Encoded buf is `%s'\n", encode_buf); fflush(stdout);
  *len = strlen(encode_buf);
  return (void *) encode_buf;
}

static void *decode_test_struct2(void *data, int len)
{
  MTestStruct2 *s = (MTestStruct2 *) malloc(sizeof(MTestStruct2));
  int where;
  char *s_data = (char *) data;

  /* printf("Buf to decode is `%s', len %d\n", (char *) data, len); fflush(stdout); */
  sscanf(s_data, "%d %n", &(s->key), &where);
  s->val = (char *) malloc(sizeof(char) * (1 + (len - where)));
  strncpy(s->val, s_data + where, len - where);
  s->val[len - where] = '\0';
  return (void *) s;
}

Gal_Frame receive_binary(Gal_Frame f, void *server_data)
{
  int dt;
  void *obj = MGal_GetOpaque(f, ":binary_data", &dt);
  MTestStruct1 *s1;
  MTestStruct2 *s2;

  if (obj) {
    switch (dt) {
    case TEST1_DT:
      s1 = (MTestStruct1 *) obj;

      GalUtil_PInfo1("First is %d, last is %d, msg is `%s'\n",
		     s1->first, s1->last, s1->msg);
      Gal_SetProp(f, ":test_first", Gal_IntObject(s1->first));
      Gal_SetProp(f, ":test_last", Gal_IntObject(s1->last));
      Gal_SetProp(f, ":test_msg", Gal_StringObject(s1->msg));
      break;
    case TEST2_DT:
      s2 = (MTestStruct2 *) obj;

      GalUtil_PInfo1("Key is %d, val is `%s'\n",
		     s2->key, s2->val);
      Gal_SetProp(f, ":test_first", Gal_IntObject(s2->key));
      Gal_SetProp(f, ":test_msg", Gal_StringObject(s2->val));
      break;
    }
  }  
  return f;
}  

static int HowMany = 1;

Gal_Frame reinitialize(Gal_Frame f, void *server_data)
{
  Gal_Frame fr = Gal_MakeFrame("main", GAL_CLAUSE);
  MTestStruct1 *s1;
  MTestStruct2 *s2;

  switch (HowMany % 2) {
  case 0:
    s1 = (MTestStruct1 *) malloc(sizeof(MTestStruct1));

    s1->first = Gal_GetInt(f, ":test_first");
    s1->last = Gal_GetInt(f, ":test_last");
    s1->msg = Gal_GetString(f, ":test_msg");
    Gal_SetProp(fr, ":binary_data", MGal_OpaqueObject((void *) s1, TEST1_DT));
    break;
  case 1:
    s2 = (MTestStruct2 *) malloc(sizeof(MTestStruct2));

    s2->key = Gal_GetInt(f, ":test_first");
    s2->val = Gal_GetString(f, ":test_msg");
    Gal_SetProp(fr, ":binary_data", MGal_OpaqueObject((void *) s2, TEST2_DT));
    break;
  }

  /* Send the new message */
  GalSS_EnvWriteFrame((GalSS_Environment *) server_data, fr, 0);
  HowMany++;
  Gal_FreeFrame(fr);
  return (Gal_Frame) NULL;  
}

void *_GalSS_init_server(GalIO_ServerStruct *s, int argc, char **argv)
{
  MGal_AddBinaryDataType(TEST1_DT, encode_test_struct1, decode_test_struct1);
  MGal_AddBinaryDataType(TEST2_DT, encode_test_struct2, decode_test_struct2);
  return (void *) NULL;
}

