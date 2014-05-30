/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* 9/3/99: Extracted from sysdep.c by Sam Bayer (MITRE) following
   a suggestion by Lee Heatherington. */

/* This file provided by MIT to handle the absence of strerror() in SunOS.
   This version is also thread_safe, so I'll be borrowing it for
   Linux as well. */

#include "GC_config.h"
#include "galaxy/util.h" 
  
#include <stdio.h>
#include <errno.h>
#include <string.h>

extern int sys_nerr;
extern char *sys_errlist[];

char *_gal_strerror(int errnum)
{
  if ((errnum > 0) && (errnum < sys_nerr))
    return(sys_errlist[errnum]);
  else
    return("");
}
