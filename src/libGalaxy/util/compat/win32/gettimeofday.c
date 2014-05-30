/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* 9/3/99: Extracted from sysdep.c by Sam Bayer (MITRE) following
   a suggestion by Lee Heatherington. */

/* This file provided by MIT to handle the absence of gettimeofday()
   in Win32. */

#include "galaxy/sysdep.h"
#include "galaxy/util.h"

#include <winsock2.h>

int _gal_gettimeofday(struct timeval *tp)
{
  struct _timeb w32tv;

  _ftime(&w32tv);
  tp->tv_sec = w32tv.time;
  tp->tv_usec = w32tv.millitm * 1000; /* convert from milli to micro seconds */
  return 0;
}
