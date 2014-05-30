/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdlib.h>
#include "galaxy/galaxy_all.h"
#include "MITRE_galaxy.h"

/* In this file, I present a packaging of binary data. */

static MGal_BinaryDataType *Binary_DT_Table = (MGal_BinaryDataType *) NULL;

void MGal_AddBinaryDataType(int data_type,
			    MGal_BinaryDataEncoder encoder,
			    MGal_BinaryDataDecoder decoder)
{
  MGal_BinaryDataType *new_entry = (MGal_BinaryDataType *) malloc(sizeof(MGal_BinaryDataType));

  new_entry->data_type = data_type;
  new_entry->encoder = encoder;
  new_entry->decoder = decoder;
  new_entry->next = Binary_DT_Table;

  Binary_DT_Table = new_entry;
}

/* This function is meant to be parallel to Gal_IntObject, etc. */

Gal_Object MGal_OpaqueObject(void *obj, int data_type)
{
  MGal_BinaryDataType *dt_entry = Binary_DT_Table;
  int size = 0;
  Gal_Object temp;
  void *encoding = (void *) NULL;
  
  while (dt_entry && (dt_entry->data_type != data_type)) {
    dt_entry = dt_entry->next;
  }
  if (!dt_entry) {
    return (Gal_Object) NULL;
  }
  if (!dt_entry->encoder) {
    return (Gal_Object) NULL;
  }
  encoding = (*(dt_entry->encoder))(obj, &size);
  temp = Gal_BinaryObject(encoding, size);
  return Gal_FrameObject(MGal_CreateFullFrame("__opaque__",
					      GAL_CLAUSE, 2,
					      ":type", Gal_IntObject(data_type),
					      ":data", temp));
}

/* The object ought to be a frame, to start with. */

static void *__MGal_GetOpaqueValue(Gal_Frame fr, char *key,
				   int type_check, int *type)
{
  Gal_Frame f = Gal_GetFrame(fr, key);  
  MGal_BinaryDataType *dt_entry = Binary_DT_Table;
  Gal_Object type_obj, data_obj;
  int data_type;
  void *bdata;
  int bsize = 0;
  
  if (!f) {
    return (void *) NULL;
  }
  if (!Gal_FrameNameEq(f, "__opaque__")) {
    return (void *) NULL;
  }
  if ((!(type_obj = Gal_GetObject(f, ":type"))) ||
      (!(data_obj = Gal_GetObject(f, ":data")))) {
    return (void *) NULL;
  }
  data_type = Gal_IntValue(type_obj);  
  /* If the type should be checked but doesn't match, barf */
  if (type_check && ((*type) != data_type)) {
    return (void *) NULL;
  } else {
    *type = data_type;
  }
  /* Find the type entry */
  while (dt_entry && (dt_entry->data_type != data_type)) {
    dt_entry = dt_entry->next;
  }
  if (!dt_entry) {
    return (void *) NULL;
  }
  if (!dt_entry->decoder) {
    return (void *) NULL;
  }
  bdata = Gal_BinaryValue(data_obj, &bsize);
  return (*(dt_entry->decoder))(bdata, bsize);
}

void *MGal_GetOpaqueWarn(Gal_Frame fr, char *key, int data_type)
{
  return __MGal_GetOpaqueValue(fr, key, 1, &data_type);
}

void *MGal_GetOpaque(Gal_Frame fr, char *key, int *data_type)
{
  return __MGal_GetOpaqueValue(fr, key, 0, data_type);
}
