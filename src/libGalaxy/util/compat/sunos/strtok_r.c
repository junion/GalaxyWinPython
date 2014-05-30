/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* 9/3/99: Extracted from sysdep.c by Sam Bayer (MITRE) following
   a suggestion by Lee Heatherington. */

/* This file provided by MIT to handle the absence of strtok_r() in SunOS. */

#include "GC_config.h"
#include "galaxy/util.h"
   
#include <stdio.h>
#include <errno.h>
#include <string.h>

char *_gal_strtok_r(char *s1, const char *s2, char **lasts)
{
  *lasts = NULL;
  return(strtok(s1, s2));
}
