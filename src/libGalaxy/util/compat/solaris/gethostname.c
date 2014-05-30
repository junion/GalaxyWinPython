/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* 9/3/99: Extracted from sysdep.c by Sam Bayer (MITRE) following
   a suggestion by Lee Heatherington. */

/* This file provided by MIT to handle the behavior of gethostname()
   in Solaris. */

#include "GC_config.h"
#include "galaxy/util.h"

#include <unistd.h>
#include <time.h>
#include <sys/systeminfo.h>
   
/* solaris does have gethostname() in its BSD compatibility package,
   but they recommend the use of sysinfo instead */
int _gal_gethostname(char *name, int namelen)
{
  long status;

  status = sysinfo(SI_HOSTNAME, name, namelen);
  return status > 0 && status <= namelen;
}
