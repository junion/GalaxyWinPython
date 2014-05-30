/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <galaxy/galaxy_all.h>

#include "simple_mainloop.h"

static void _simple_mainloop_timer_callback(void *client_data)
{
  GalSS_ELRDoCallback((GalSS_ELR *) client_data, GALSS_ELR_TIMER);
}

void *_simple_mainloop_set_timer(GalSS_ELR *elr, int ms)
{
  return (void *) SM_AddTimerCallback((Looper *) GalSS_ELRGetLoopData(elr),
				      ms, _simple_mainloop_timer_callback,
				      (void *) elr);
}

void _simple_mainloop_unset_timer(GalSS_ELR *elr, void *tag)
{
  SM_RemoveTimerCallback((TimerCallback *) tag);
}

static void _simple_mainloop_fd_callback(void *client_data)
{
  GalSS_ELRDoCallback((GalSS_ELR *) client_data, GALSS_ELR_FD);
}

void *_simple_mainloop_set_fd(GalSS_ELR *elr, GAL_SOCKET fd)
{
  SM_AddFDCallback((Looper *) GalSS_ELRGetLoopData(elr),
		   fd, _simple_mainloop_fd_callback,
		   (void *) elr);
  return (void *) fd;
}

void _simple_mainloop_unset_fd(GalSS_ELR *elr, void *tag)
{
  SM_RemoveFDCallback((Looper *) GalSS_ELRGetLoopData(elr),
		      (GAL_SOCKET) tag);		      
}

/* This function doesn't have to do something for every case.
   It's just here in case it's needed. */

void _simple_mainloop_behavior_fn(GalSS_ELR *elr, int event)
{
  switch (event) {
  case GAL_SERVER_LISTENER_SHUTDOWN_EVENT:
    GalUtil_Warn("The server has failed.\n");
    SM_RemoveAllFDCallbacks((Looper *) GalSS_ELRGetLoopData(elr));
    SM_RemoveAllTimerCallbacks((Looper *) GalSS_ELRGetLoopData(elr));
    break;
  case GAL_SERVER_DESTRUCTION_EVENT:
    GalUtil_Warn("The server has been destroyed.\n");
    SM_LooperExit((Looper *) GalSS_ELRGetLoopData(elr));
    break;
  }
}

int main(int argc, char **argv)
{
  Looper *l = SM_NewLooper();
  GalSS_ELR *elr = GalSS_ELRSetupServer((GalSS_ServerArgs *) NULL,
					argc, argv,
					_simple_mainloop_set_timer,
					_simple_mainloop_unset_timer,
					_simple_mainloop_set_fd,
					_simple_mainloop_unset_fd,
					_simple_mainloop_behavior_fn,
					(void *) l, NULL, 1);

  if (!elr) {
    exit(1);
  }
  SM_Mainloop(l);
  exit(0);
}
