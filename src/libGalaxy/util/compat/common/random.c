/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* 9/3/99: Extracted from sysdep.c by Sam Bayer (MITRE) following
   a suggestion by Lee Heatherington. */

/* This file provided by MIT to handle the absence of random() in 
   various OSs. */

#include "GC_config.h"
#include "galaxy/util.h"

#include <math.h>
#include <stdlib.h>

long _gal_random()
{
  return (long)rand();
}
