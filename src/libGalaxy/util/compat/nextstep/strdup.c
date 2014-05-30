/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* 9/3/99: Extracted from sysdep.c by Sam Bayer (MITRE) following
   a suggestion by Lee Heatherington. */

/* This file provided by MIT to handle the absence of strdup() in NextStep. */

#include "GC_config.h"
#include "galaxy/util.h"

#include <stdlib.h>
#include <libc.h>
#include <math.h>

char *_gal_strdup(char *str) {
  int len;
  char *str2;
  
  len = strlen(str);
  str2 = (char *) calloc(len+1, sizeof(char));
  bcopy(str, str2, (len+1)*sizeof(char));
  return(str2);
}
