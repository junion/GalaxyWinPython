/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "gal_internal.h"

#ifdef WIN32
#include "galaxy/gthread.h"
#endif

static int statics_initialized = 0;

/* SAM 12/06/00: Turns out that there are a number of globals which
   need to be initialized. Not just mutexes, but also the verbosity
   functions. I'll add this to the docs. */

void Gal_InitializeStatics(void)
{
  /* Some servers (MIT telephony) initialize multiple instances of themselves, 
   * so make sure we only initialize statics the first time
   */
  if (statics_initialized)
    return;

  GalUtil_VerboseUseBW();
  _Gal_init_error_tags();
  _Gal_init_hub_server();
  _Gal_init_broker();
  _Gal_init_ip_util();
  _Gal_init_nfio();
  _Gal_init_nframe();
  _Gal_init_program_tags();
  _Gal_init_signal();
  _Gal_init_stream_util();
  _Gal_init_sym();
  _Gal_init_timed_tasks();
  _Gal_init_tobj();
  _Gal_init_vlist();

#ifdef WIN32
  GalUtil_InitMainWin32Thread();
#endif

  statics_initialized = 1;
}
