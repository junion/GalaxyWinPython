/*
  This file (c) Copyright 1998 - 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* MITRE added this file to the GalaxyCommunicator core to support
   signal handling in both threaded and non-threaded applications.
   The user should add a signal using Gal_AddSignalHandler(), and
   the "right thing" will happen. */

#include <stdlib.h>
#include <signal.h>
#include "galaxy/galaxy_all.h"

typedef void (*__sig_handler)(int);

static GalUtil_LocalMutex signal_mutex;

void _Gal_init_signal(void)
{
  GalUtil_InitLocalMutex(&signal_mutex);
}

#ifdef GAL_PTHREADS
static __sig_handler *__Gal_SignalArray = (__sig_handler *) NULL;
static int __Gal_NumSignals = 0;



/* This is the signal handler. We catch all signals, and
   if one doesn't have a value, then we simply report that
   a signal was received. */

/* SAM 4/18/02: If we're being real paranoid, there's
   a printout in the scope of the signal handler mutex lock,
   which makes it not cancellation-safe. So I'll add a push/pop. */

extern void _gal_unlock_mutex(void *mutex);

static void *__Gal_SignalHandler(void *arg)
{
  sigset_t set;
  int sig;
  int res;

  /* Catch all signals. */
  sigfillset(&set);

  /* Loop forever. */
  while (1) {
    res = sigwait(&set, &sig);
    
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) &signal_mutex);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_push(_gal_unlock_mutex, (void *) &signal_mutex);
#endif
    GalUtil_LockLocalMutex(&signal_mutex);
    /* If the signal is specified to be ignored, ignore it.
       If the signal is specified to be the default, report it
       as unhandled. If the signal is specified as a signal
       handler, then run the signal handler. */
    if ((sig < __Gal_NumSignals) && (__Gal_SignalArray[sig] != NULL)) {
      if (__Gal_SignalArray[sig] == SIG_DFL) {
	GalUtil_Warn("Received unhandled signal number %d\n", sig);
      } else if (__Gal_SignalArray[sig] != SIG_IGN) {
	/* Invoke the signal handler. */
	(*__Gal_SignalArray[sig])(sig);
      }
    } else {
      GalUtil_Warn("Received unhandled signal number %d\n", sig);
    }
    /* The handler may never return, in which case the
       mutex will never be released. Let's not worry about
       that case, since most of the time it means that the
       whole process exited. */
    GalUtil_UnlockLocalMutex(&signal_mutex);
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_pop(0);
#endif
  }
  return (void *) 0;
}
#endif /* GAL_PTHREADS */

#ifdef GAL_THREADS
static void __Gal_Exit(int sig)
{
  /* SAM 2/22/02: This MUST NOT BE GalUtil_Fatal(), which
     raises SIGQUIT, which is bound to __Gal_Exit. */
  GalUtil_Warn("Received signal %d; exiting\n", sig);
  exit(1);
}
#endif /* GAL_THREADS */

/* THIS MUST BE CALLED IN THE INITIAL THREAD.
   This function is a noop in the nonreentrant case.
   I *always* set up a thread to handle the signals, because
   even if we don't use threads for the timed task loop, the
   application developer might use signals. */

static int signals_initialized = 0;

int Gal_SignalsInitialized()
{
  int initialized = signals_initialized;
#ifdef GAL_PTHREADS
  GalUtil_LockLocalMutex(&signal_mutex);
  initialized = (__Gal_NumSignals != 0);
  GalUtil_UnlockLocalMutex(&signal_mutex);
#endif /* GAL_PTHREADS */
  return initialized;
}

void Gal_InitializeSignals()
{
#ifdef GAL_PTHREADS
  sigset_t set;
  /*pthread_attr_t sig_attr;*/
  GalUtil_ThreadID sig_thread;
  
  sigfillset(&set);
  /* Disable all signals in the current thread */
  pthread_sigmask(SIG_SETMASK, &set, NULL);

  /* Initialize the signal handler DB. */
  GalUtil_LockLocalMutex(&signal_mutex);
  __Gal_NumSignals = 64;
  __Gal_SignalArray = (__sig_handler *) calloc(64, sizeof(__sig_handler));
  GalUtil_UnlockLocalMutex(&signal_mutex);
  
  /* Create a thread to handle the signals. */
  /*
  pthread_attr_init(&sig_attr);
  pthread_attr_setdetachstate(&sig_attr, PTHREAD_CREATE_DETACHED);
  pthread_create(&sig_thread, &sig_attr, __Gal_SignalHandler, NULL);
  */
  GalUtil_ThreadCreate(&sig_thread, __Gal_SignalHandler, NULL);

  /* Initialize a few default signal handlers, like SIGQUIT and
     SIGINT. We can add more later. */
  Gal_AddSignalHandler(SIGHUP, __Gal_Exit);
  Gal_AddSignalHandler(SIGINT, __Gal_Exit);
  Gal_AddSignalHandler(SIGQUIT, __Gal_Exit);
  Gal_AddSignalHandler(SIGTERM, __Gal_Exit);
#endif /* GAL_PTHREADS */

#ifdef GAL_WIN32_THREADS
  Gal_AddSignalHandler(SIGINT, __Gal_Exit);
  Gal_AddSignalHandler(SIGTERM, __Gal_Exit);
#endif /* GAL_WIN32_THREADS */

#ifndef WIN32
  {
    /* Ignore broken pipes */
    struct sigaction sa;
    memset(&sa,0,sizeof(struct sigaction));
    sa.sa_handler = SIG_IGN;
    sa.sa_flags = SA_RESTART;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGPIPE, &sa, NULL);
  }
#endif
  signals_initialized = 1;
}

/* This function installs a signal handler. It has the semantics
   of signal, not sigset; so the signal handler needs to reset
   itself if it doesn't exit. */

void Gal_AddSignalHandler(int sig, __sig_handler handler)
{
#ifdef GAL_PTHREADS
  int max_sigs, i;

  GalUtil_LockLocalMutex(&signal_mutex);
  max_sigs = __Gal_NumSignals;
  /* Be sure to realloc. */
  if (sig > max_sigs) {
    max_sigs = max_sigs * 2;
  }
  if (max_sigs > __Gal_NumSignals) {
    __Gal_SignalArray = (__sig_handler *) realloc(__Gal_SignalArray, (max_sigs * sizeof(__sig_handler)));
    /* zero out the new cells. */
    for (i = __Gal_NumSignals; i < max_sigs; i++) {
      __Gal_SignalArray[i] = (__sig_handler) NULL;
    }
  }
  __Gal_NumSignals = max_sigs;
  __Gal_SignalArray[sig] = handler;
  GalUtil_UnlockLocalMutex(&signal_mutex);
#else
  signal(sig, handler);
#endif /* GAL_PTHREADS */
}

