/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* 9/3/99: Extracted from sysdep.c by Sam Bayer (MITRE) following
   a suggestion by Lee Heatherington. */

/* This file provided by MIT to handle the absence of bcopy()
   in Win32. */

#include "GC_config.h"
#include "galaxy/util.h"

#include <string.h>

void _gal_bcopy(const void *s1, void *s2, size_t n)
{
  memmove(s2, s1, n);
}
