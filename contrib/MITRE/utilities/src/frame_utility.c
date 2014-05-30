/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdarg.h>
#include "galaxy/galaxy_all.h"

Gal_Frame __MGal_VACreateFullFrame(char *name, int type,
				   int num_pairs, va_list args)
{
  Gal_Frame fr;
  Gal_Object t;
  char *key;
  int i;

  fr = Gal_MakeFrame(name, type);
  for (i = 0; i < num_pairs; i++) {
    key = va_arg(args, char *);
    t = va_arg(args, Gal_Object);
    Gal_SetProp(fr, key, t);
  }
  return fr;
}

Gal_Frame MGal_CreateFullFrame(char *name, int type, int num_pairs, ...)
{
  va_list args;
  Gal_Frame fr;
  
  va_start(args, num_pairs);
  fr = __MGal_VACreateFullFrame(name, type, num_pairs, args);
  va_end(args);
  return fr;
}

