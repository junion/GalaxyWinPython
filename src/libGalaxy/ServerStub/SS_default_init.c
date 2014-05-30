/*
  (c) Copyright 1998 - 2000 M.I.T.

  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "generic-server-internal.h"

void _GalSS_InitializeDefaults(GalIO_ServerStruct *s)
{
  /* This function probably ought to do SOMEthing. If
     the name of the server is NULL, strange things happen.
     So I should also print out a warning. One situation
     in which I found that this function was called was
     when one user attempted to define this function in
     a C++ file, and (of course) it was mangled. */
  GalUtil_Warn("No call to _GalSS_InitializeDefaults found!\n");
  GalSS_InitializeServerDefaults(s, "<unknown>", -1);
}
