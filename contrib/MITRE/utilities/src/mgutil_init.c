/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"

extern void _MGal_init_broker_utility();

void MGal_InitializeStatics(void)
{
  /* _MGal_init_broker_utility(); */
}

void _GalSS_configure_MITREUtilities(GalIO_ServerStruct *s)
{
  MGal_InitializeStatics();
}
