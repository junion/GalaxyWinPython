/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"

#include <sys/types.h>
#ifdef WIN32
#include <time.h>
#else
#include <sys/time.h>
#endif

#include "galaxy/galaxy.h"

/* SAM 9/15/99: I tried experimenting with a model that avoided
   mutexing the timed task list itself for a while, but that didn't
   work at all. I thought about encapsulating the timed task data,
   but the only options were (a) changing the timed task API, which
   is probably unacceptable to MIT, (b) using thread-specific data, which
   would require the timed task loop to be running in whatever thread
   added tasks, or (c) allowing only one thread to run the loop. The
   last solution seemed the best, but when I tried to set it up to
   that the timed task list itself wasn't mutexed, I ended up in
   a situation which was getting increasingly complicated and
   had more and more convoluted semantics. So I'm going to take the
   performance hit and just do the fine-grained mutex. */

/* private */
#define MAX_TIMED_ENTRIES 1000

#define TIMED_TASK_DEBUG 0

typedef struct {
  void (*task)(void *caller_data);
  void *caller_data;
} Gal_BareTimedTask;

static void __Gal_SetTaskPkgMillisecs(Gal_TaskPkg *p,
				      long num_millisecs);

static Gal_BareTimedTask *__Gal_CreateBareTimedTask(void (*task)(void *caller_data), void *caller_data)
{
  Gal_BareTimedTask *bt = (Gal_BareTimedTask *) malloc(sizeof(Gal_BareTimedTask));
  bt->task = task;
  bt->caller_data = caller_data;
  return bt;
}

static void __Gal_RunBareTimedTask(Gal_TaskPkg *p)
{
  Gal_BareTimedTask *t = (Gal_BareTimedTask *) Gal_TaskPkgData(p);
  (*t->task)(t->caller_data);
}

static void __Gal_FreeBareTimedTask(void *caller_data)
{
  free(caller_data);
}

static Gal_TaskPkg *timed[MAX_TIMED_ENTRIES];
static fd_set read_fdset;
static fd_set write_fdset;
static fd_set err_fdset;
static int maxfd = -1;
static int timed_num = 0;

static Gal_TaskPkg *NewTaskPkg(void (*task)(Gal_TaskPkg *),
			       void *caller_data,
			       long num_millisecs,
			       int read_blocking,
			       void (*cleanup_fn)(void *),
			       GAL_SOCKET *read_socket,
			       GAL_SOCKET *write_socket,
			       GAL_SOCKET *err_socket,
			       FILE *read_file,
			       FILE *write_file,
			       FILE *err_file,
			       Gal_TaskConditionFn condition);

/* The timed task system can be in one of a number of states. Here's the
   expected functionality:

   Gal_TimedTasksLoop() starts up the timed task loop, even in a
   threaded context.
   Gal_TimedTasksLoopExit() does a deferred shutdown of the timed task
   loop, even in a threaded context.
   Gal_EnableTimedTaskThreads() instructs the system to use threads 
   for Gal_AddTask, Gal_ReAddTask and Gal_RemoveTask.
   Gal_TimedTaskLoopThreadWaiter() waits until all the threads spawned
   by Gal_AddTask have been terminated.
   Gal_EndTasks() does either a deferred or immediate shutdown of the
   timed task loop, if it's running, and the thread loop, if it's running,
   and causes both Gal_TimedTaskLoop and Gal_TimedTaskLoopThreadWaiter
   to return. */

enum {TT_IDLE, TT_RUNNING, TT_DEFERRED_EXIT, TT_IMMEDIATE_EXIT};

static int tt_loop_status = TT_IDLE;
static int tt_thread_status = TT_IDLE;

static GalUtil_LocalMutex timed_task_mutex;

/* SAM 2/7/00: GAL_THREADS here should be GAL_PTHREADS, because
   we can't do the timed task stuff right yet on Windows. */
/* SCW 10/2/00: Windows supports threads now. */

static void Gal_init_idle_tasks();

void _Gal_init_timed_tasks(void)
{
  GalUtil_InitLocalMutex(&timed_task_mutex);
  Gal_init_idle_tasks();
}

static GalUtil_ThreadID timer_thread;

/* This function is mutexed by a (remote) caller. */

/* SAM 7/10/00: The maxfd settings in here are something of a hack.
   It's used in Unix to collect the max file descriptor to pass
   to select. However, in WIN32, the maxfd argument to select()
   is ignored, and sockets are not integers (so they're not
   file descriptors), and select doesn't work with file descriptors.
   I need to support these differences in a clear way in this code. */

static void compute_fdset(void)
{
  int i;
  unsigned int local_maxfd=0;
  
  maxfd = -1;
  FD_ZERO(&read_fdset);
  FD_ZERO(&write_fdset);
  
  for(i=0;i<timed_num;i++) {
    if(timed[i]->read_socket && *(timed[i]->read_socket) != GAL_INVALID_SOCKET) {
      FD_SET(*(timed[i]->read_socket), &read_fdset);
#ifdef WIN32
      local_maxfd = 1;
#else
      if (local_maxfd < *(timed[i]->read_socket))
	local_maxfd = *(timed[i]->read_socket);
#endif
    }
	
    if(timed[i]->write_socket && *(timed[i]->write_socket) != GAL_INVALID_SOCKET) {
      FD_SET(*(timed[i]->write_socket), &write_fdset);
#ifdef WIN32
      local_maxfd = 1;
#else
      if (local_maxfd < *(timed[i]->write_socket))
	local_maxfd = *(timed[i]->write_socket);
#endif
    }

    if(timed[i]->err_socket && *(timed[i]->err_socket) != GAL_INVALID_SOCKET) {
      FD_SET(*(timed[i]->err_socket), &err_fdset);
#ifdef WIN32
      local_maxfd = 1;
#else
      if (local_maxfd < *(timed[i]->err_socket))
	local_maxfd = *(timed[i]->err_socket);
#endif
    }


#ifndef WIN32
    /*  The following is a hack and should only work under Unix!!! - SPC */
    if (timed[i]->read_file) {
      unsigned int fd = _gal_fileno(timed[i]->read_file);
      FD_SET(fd, &read_fdset);
      if (local_maxfd < fd)
	local_maxfd = fd;
    }

    if (timed[i]->write_file) {
      unsigned int fd = _gal_fileno(timed[i]->write_file);
      FD_SET(fd, &write_fdset);
      if (local_maxfd < fd)
	local_maxfd = fd;
    }

    if (timed[i]->err_file) {
      unsigned int fd = _gal_fileno(timed[i]->err_file);
      FD_SET(fd, &err_fdset);
      if (local_maxfd < fd)
	local_maxfd = fd;
    }
#endif
  }
  /* At this point, if any sockets have been set, maxfd will be
     the right value in Unix and 1 in Windows. */
  if (local_maxfd > 0)
    maxfd = local_maxfd;
}


/* canonify so that tv_usec is always < 1000000 */

static void canonify_time(struct timeval *t)
{
  long overflow;

  if(t->tv_usec < 0) {
    long secs_to_sub = -t->tv_usec / 1000000 + 1;
    t->tv_usec += secs_to_sub * 1000000;
    t->tv_sec -= secs_to_sub;
  }
  overflow = t->tv_usec / 1000000;
  t->tv_sec += overflow;
  t->tv_usec %= 1000000;
}

static void time_sub(struct timeval *a, struct timeval *b, struct timeval *result)
{
  result->tv_sec = a->tv_sec - b->tv_sec;
  result->tv_usec = a->tv_usec - b->tv_usec;
  canonify_time(result);
}

static int gt_time(struct timeval *a, struct timeval *b)
{
  canonify_time(a);
  canonify_time(b);
  if(a->tv_sec > b->tv_sec)
    return 1;
  if(a->tv_sec == b->tv_sec && a->tv_usec > b->tv_usec)
    return 1;
  return 0;
}

/* Make sure this doesn't happen while the task is executing! */

extern void GalSS_EnvUnlock(GalSS_Environment *env);

static void run_task_pkg_cleanup(Gal_TaskPkg *p)
{
  if (!p->executing) {
    GalUtil_PInfo2("Running task package cleanup\n");
    /* Check the cleanup fn first. If it's there,
       and the caller data has already been freed,
       there's a big problem. Otherwise, if the
       caller data has already been freed, the bad
       pointer won't be touched. */
    if (p->cleanup_fn && p->caller_data)
      (*p->cleanup_fn)(p->caller_data);
    if (p->host_env)
      GalSS_EnvUnlock(p->host_env);
    free(p);
  }
}

#if TIMED_TASK_DEBUG
static void print_timed_tasks(char *status)
{
  int i;
  GalUtil_Print(-1,"Num timed tasks is %d (%s)", timed_num, status);
  for (i=0;i<timed_num;i++)
    GalUtil_Print(-1,", 0x%x", (int) timed[i]);
  GalUtil_Print(-1,"\n");
}
#endif

extern void _gal_unlock_mutex(void *mutex);

/* 4/18/02: This mutex needs to be protected because of the printing. */

static int __Gal_MaybeCleanupTimedTask(Gal_TaskPkg *pkg)
{
  if (tt_thread_status == TT_DEFERRED_EXIT ||
      tt_thread_status == TT_IMMEDIATE_EXIT) {
    run_task_pkg_cleanup(pkg);
    return 1;
  } else {
    return 0;
  }
}

static int __Gal_ScheduleTimedTask(Gal_TaskPkg *pkg, int num_millisecs)
{  
  int i;
  int retval = 0;

  /* I'll call this every time I try to schedule the task. */
  if (__Gal_MaybeCleanupTimedTask(pkg) == 1) {
    return 1;
  }

  __Gal_SetTaskPkgMillisecs(pkg, num_millisecs);

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) &timed_task_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) &timed_task_mutex);
#endif
  GalUtil_LockLocalMutex(&timed_task_mutex);
  
  if(timed_num >= MAX_TIMED_ENTRIES) {
    run_task_pkg_cleanup(pkg);
    retval = 1;
  } else {
    for (i=0;i<timed_num;i++) {
      if (gt_time(&timed[i]->tv, &(pkg->tv))) {
	/* SunOS doesn't have memmove, so do this in very portable manner */
	int j;
	for(j=timed_num+1;j>=i+1;j--) {
	  timed[j] = timed[j-1];
	}
	timed[i] = pkg;
	break;
      }
    }
    if (i == timed_num) {
      /* add at end */
      timed[i] = pkg;
    }
    timed_num++;
#if TIMED_TASK_DEBUG
    print_timed_tasks("added");
#endif
    if (pkg->write_socket != NULL || pkg->read_socket != NULL)
      compute_fdset();
  }
  
  GalUtil_UnlockLocalMutex(&timed_task_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
  
  return retval;
}

static int Gal_AddTimedTaskWithIO(void *task, void *caller_data, long num_millisecs,
				  GAL_SOCKET *read_socket,
				  GAL_SOCKET *write_socket,
				  FILE *read_file, FILE *write_file)
{
  Gal_TaskPkg *new_p;

  /* Build a new task pkg. */

  new_p = NewTaskPkg(__Gal_RunBareTimedTask,
		     __Gal_CreateBareTimedTask(task, caller_data),
		     -1, 0,
		     __Gal_FreeBareTimedTask,
		     read_socket, write_socket, 0,
		     read_file, write_file, 0, NULL);
  return __Gal_ScheduleTimedTask(new_p, num_millisecs);
}

/* Add task to be called num_millisecs
   from now to our timed tasks task table.

   Tasks only executed if timed_function_loop() is running.

   returns 1 if table is full, 0 if success */

int Gal_AddTimedTask(void *task, void *caller_data, long num_millisecs)
{
  return Gal_AddTimedTaskWithIO(task,caller_data,num_millisecs,0,0,0,0);
}

int Gal_AddTimedTaskWithFileIO(void *task, void *caller_data, long num_millisecs, FILE *read_file, FILE *write_file)
{
#ifdef WIN32
  GalUtil_Warn("Can't poll file IO under Windows");
  return 1;
#else
  return Gal_AddTimedTaskWithIO(task,caller_data,num_millisecs, (GAL_SOCKET *)0, (GAL_SOCKET *)0, read_file, write_file);
#endif
}

int Gal_AddTimedTaskWithSocketIO(void *task, void *caller_data, long num_millisecs, GAL_SOCKET *read_socket, GAL_SOCKET *write_socket)
{
  return Gal_AddTimedTaskWithIO(task,caller_data,num_millisecs, read_socket, write_socket, (FILE *)NULL, (FILE *)NULL);
}

/* This function is mutexed by its caller. It must release the
   mutex before it executes a task. Otherwise, it just hangs on.
   This function return 1 if there are timed tasks pending,
   0 if there are none, -1 if the loop is over.
*/

static void run_timed_task(Gal_TaskPkg *pkg)
{
  pkg->done = 1;
  pkg->executing = 1;
  (*pkg->task)(pkg);
  pkg->executing = 0;
  pkg->run_reasons = 0;
  /* If the task hasn't been reset, destroy it. */
  if (pkg->done) {
    run_task_pkg_cleanup(pkg);
  }
}

static int __Gal_ITimedTasksLoopHandler(struct timeval *tv)
{
  Gal_TaskPkg *p;
  int run_reasons;
  
  tv->tv_sec = 0;
  tv->tv_usec = 0;

  /* first check for i/o */
  /* SAM 7/10/00: See compute_fdset() for how maxfd is computed
     differently in WIN32 and in Unix. */
  if (maxfd >= 0) {    
    int j;
    /* now find who needs to be called */
    for(j=0;j<timed_num;j++) {
      run_reasons = 0;
      if (timed[j]->read_socket &&
	  (*(timed[j]->read_socket) != GAL_INVALID_SOCKET)
	  && FD_ISSET(*(timed[j]->read_socket), &read_fdset)) {
	run_reasons = run_reasons | GAL_SOCKET_READABLE;
      }
      if (timed[j]->write_socket &&
	  (*(timed[j]->write_socket) != GAL_INVALID_SOCKET)
	  && FD_ISSET(*(timed[j]->write_socket), &write_fdset)) {
	run_reasons = run_reasons | GAL_SOCKET_WRITABLE;
      }
      if (timed[j]->err_socket &&
	  (*(timed[j]->err_socket) != GAL_INVALID_SOCKET)
	  && FD_ISSET(*(timed[j]->err_socket), &err_fdset)) {
	run_reasons = run_reasons | GAL_SOCKET_ERR_READABLE;
      }
#ifndef WIN32
      if (timed[j]->read_file &&
	  FD_ISSET(_gal_fileno(timed[j]->read_file), &read_fdset)) {
	run_reasons = run_reasons | GAL_FILE_READABLE;
      }
      if (timed[j]->write_file &&
	  FD_ISSET(_gal_fileno(timed[j]->write_file), &write_fdset)) {
	run_reasons = run_reasons | GAL_FILE_WRITABLE;
      }
      if (timed[j]->err_file &&
	  FD_ISSET(_gal_fileno(timed[j]->err_file), &err_fdset)) {
	run_reasons = run_reasons | GAL_FILE_ERR_READABLE;
      }
#endif
      /* As long as I'm checking everything, I should check
	 the conditions. But only if there's no run reason yet.
	 These conditions had better be quick to check... */
      if ((!run_reasons) && timed[j]->condition &&
	  (*timed[j]->condition)(timed[j]->caller_data)) {
	run_reasons = run_reasons | GAL_CONDITION_SATISFIED;
      }
	  
      if (run_reasons != 0) {
	int k;

	/* do the task */
	p = timed[j];

	for(k=j;k<timed_num-1;k++) {
	  timed[k] = timed[k+1];
	}		
	timed_num--;
#if TIMED_TASK_DEBUG
	print_timed_tasks("select fired");
#endif
	GalUtil_UnlockLocalMutex(&timed_task_mutex);
	/* The task may readd the task, otherwise it will die right here. */	
	p->run_reasons = run_reasons;
	run_timed_task(p);
	GalUtil_LockLocalMutex(&timed_task_mutex);
	switch (tt_loop_status) {
	case TT_IMMEDIATE_EXIT:
	  return -1;
	default:
	  break;
	}
      }
    }
  }

  while(timed_num) {
    struct timeval now;
    struct timeval diff;
    int i;

    _gal_gettimeofday(&now);

    time_sub(&timed[0]->tv, &now, &diff);

    // GalUtil_Print(-1,"countdown = %d secs, %d usec.\n", diff.tv_sec, diff.tv_usec); 

    if(diff.tv_sec > 0 || (diff.tv_sec == 0 && diff.tv_usec >= 0)) {
      *tv = diff;
      return 1;
    }	

    /* it's time to do next task */
    p = timed[0];

    for(i=0;i<timed_num-1;i++) {
      timed[i] = timed[i+1];
    }		
    timed_num--;
#if TIMED_TASK_DEBUG
    print_timed_tasks("timer fired");
#endif
    GalUtil_UnlockLocalMutex(&timed_task_mutex);
    p->run_reasons = GAL_TIMER_EXPIRED;
    run_timed_task(p);
    GalUtil_LockLocalMutex(&timed_task_mutex);
    switch (tt_loop_status) {
    case TT_IMMEDIATE_EXIT:
      return -1;
    default:
      break;
    }
  }
  /* Once we've run out of elements, see if it's
     still in normal running status. If it's not, then
     signal that it's time to end. */
  if (tt_loop_status == TT_RUNNING) 
    return 0;
  else return -1;
}

/* SAM 9/15/99: This function should not be called 
   in a multithreaded context. */

int Gal_TimedTasksLoopHandler(struct timeval *tv)
{
#ifdef GAL_THREADS
  GalUtil_Warn("Can't call Gal_TimedTasksLoopHandler() in multithreaded context\n");
  return 0;
#else
  return __Gal_ITimedTasksLoopHandler(tv);
#endif /* GAL_THREADS */

}

/* Call this loop to process requests.  Call Gal_TimedTasksLoopExit to exit. */

/* SAM 9/15/99: In a multithreaded context, we're going to permit
   the timed task loop to run in one and only one thread. */

/* In the multithreaded case, the loop will basically hold
   the mutex except when it's executing tasks. This seems somewhat
   ridiculous, but I can't think of another way to do it. */

/* 4/18/02: Furthermore, the mutex must be push/pop protected
   in case the task hits a cancellation point. */

void Gal_TimedTasksLoop(void)
{
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) &timed_task_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) &timed_task_mutex);
#endif  
  GalUtil_LockLocalMutex(&timed_task_mutex);
  
  if (tt_loop_status == TT_RUNNING) {
    GalUtil_Warn("Timed task loop is already running in thread %d\n",
		 timer_thread);
  } else {
    tt_loop_status = TT_RUNNING;
    timer_thread = GalUtil_CurrentThreadID();
    
    /* Still holding the mutex! */
  
    compute_fdset();

    while (tt_loop_status == TT_RUNNING) {
      struct timeval tv;
      int sock_status=0;
      switch (__Gal_ITimedTasksLoopHandler(&tv)) {
      case 1:
	/* should probably only be computed here and not in __Gal_ITimed...
	   SPC. Firing tasks ends up perhaps readding tasks which
	   have already been
	   fired, so we need to make sure that they don't get fired a
	   second time. See AddTimedTaskWithIO.
	*/
	compute_fdset(); 
#if TIMED_TASK_DEBUG
	GalUtil_Print(-1,"Gal_TimedTasksLoop: tv.tv_sec = %d  tv.tv_usec = %d maxfd = %d, num_tasks = %d\n", 
		      tv.tv_sec, tv.tv_usec, maxfd, timed_num);
#endif
	/* SAM 7/10/00: See compute_fdset() for how maxfd is computed
	   under Windows and WIN32. */
	if (maxfd > -1) {
	  int interrupted = 0;	
#if TIMED_TASK_DEBUG
	  struct timeval select_tv;
	  _gal_gettimeofday(&select_tv);
	  GalUtil_Print(-1,"Time before select: %d %d\n", select_tv.tv_sec, select_tv.tv_usec);
#endif
	  sock_status = GalUtil_SockSelect(maxfd + 1, &read_fdset, &write_fdset, (fd_set *)0, &tv, &interrupted);
#if TIMED_TASK_DEBUG
	  _gal_gettimeofday(&select_tv);
	  GalUtil_Print(-1,"Time after select: %d %d\n", select_tv.tv_sec, select_tv.tv_usec);
#endif
	  /* Under WIN32, select() can't be used for sleeping. If all of the fd_sets are 
	     empty, select() returns WSAEINVAL. This should never happen, because at least
	     the server should be initialized. */
	  if (sock_status == GAL_SOCKET_ERROR) {
	    if (interrupted) {
	      /* Interrupted system call - probably a signal
		 which was caught. Be sure to check the idle
		 tasks before continuing, just in case something
		 serious should be dealt with. */
	      Gal_RunIdleFunctions();
	    } else {
	      GalUtil_Error("Error during select: %d", GAL_SOCKET_ERRNO);
	    }
	  }

	} else {
	  Gal_RunIdleFunctions();
#if TIMED_TASK_DEBUG
	  GalUtil_Print(-1,"Sleeping for %d milliseconds till next expiration\n",
			tv.tv_usec / 1000);
#endif
	  GalUtil_MilliSleep(tv.tv_usec / 1000);
	}

	break;
      case 0:
	Gal_RunIdleFunctions();
	/* Release the mutex to sleep. */
	GalUtil_UnlockLocalMutex(&timed_task_mutex);
#if TIMED_TASK_DEBUG
	GalUtil_Print(-1,"Default loop sleep for 1 second\n");
#endif
	_gal_sleep(1);
	GalUtil_LockLocalMutex(&timed_task_mutex);
	break;
      case -1:
      default:
	/* it's time to exit the loop. */
	break;
      }
    }
    /* On exit, we want to make sure to free up the loop. */
    tt_loop_status = TT_IDLE;
  }
  
  GalUtil_UnlockLocalMutex(&timed_task_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
}

/* SAM 9/15/99: At the moment, only the thread that started the
   loop can exit it. */

/* 4/18/02: must be push/pop protected for cancellation in printf. */

void Gal_TimedTasksLoopExit(void)
{
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) &timed_task_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) &timed_task_mutex);
#endif
  GalUtil_LockLocalMutex(&timed_task_mutex);
  
  if (tt_loop_status == TT_RUNNING &&
      !GalUtil_EqualThreadID(GalUtil_CurrentThreadID(), timer_thread)) {
    GalUtil_Warn("Can't exit timed task loop because it was started by another thread (%d)\n", timer_thread);
  } else {
    /* Be sure that the mutex is locked when you mark the
       loop for exiting. */
    tt_loop_status = TT_DEFERRED_EXIT;
  }
  
  GalUtil_UnlockLocalMutex(&timed_task_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
}

/* This can only be used to remove bare tasks. See also Gal_RemoveTask(). */

/* 4/18/02: Must be protected for push/pop for thread cancellation. */

int Gal_RemoveTimedTask(void *task, void *caller_data)
{
  int j;
  int retval = 0;

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) &timed_task_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) &timed_task_mutex);
#endif
  GalUtil_LockLocalMutex(&timed_task_mutex);
  
  for (j=0;j<timed_num;j++) {
    if ((timed[j]->task == __Gal_RunBareTimedTask) &&
	(((Gal_BareTimedTask *) timed[j]->caller_data)->task == task) && 
	(!caller_data ||
	 (((Gal_BareTimedTask *) timed[j]->caller_data)->caller_data == caller_data))) {
      int i;

      run_task_pkg_cleanup(timed[j]);
      for(i=j;i<timed_num-1;i++) {
	timed[i] = timed[i+1];
      }
      timed_num--;
#if TIMED_TASK_DEBUG
      print_timed_tasks("simple removed");
#endif
      compute_fdset();
      retval = 1;
      break;
    }
  }
  
  GalUtil_UnlockLocalMutex(&timed_task_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
  
  return retval;
}

/* SAM 9/27/99: We can run the timed task loop as a thread
   dispatch instead. This is, among other things, how I'll test the
   thread-safety of the library. In that circumstance, the function
   GalIO_AddTask() will either set up a timed task, if threads
   aren't running, or set up the thread. This should be compatible
   with running the timed task loop.

   It is not possible to disable timed task threads once they're
   enabled, except by using the thread waiter in conjunction with
   Gal_TimedTasksLoopExit.
*/

void Gal_EnableTimedTaskThreads()
{
#ifdef GAL_THREADS
  GalUtil_LockLocalMutex(&timed_task_mutex);
  if (tt_thread_status == TT_IDLE) {
    /* Don't enable threads if an exit was requested. */
    tt_thread_status = TT_RUNNING;
  }
  GalUtil_UnlockLocalMutex(&timed_task_mutex);
#else
  GalUtil_Warn("Timed task threads not available");
#endif /* GAL_THREADS */
}

/* This won't exactly do the right thing, because
   someone might enable the instant after the value
   is captured, but it's not clear that I can ever get
   out of that problem... */

int Gal_TimedTaskThreadsEnabled()
{
  int res = 0;
  GalUtil_LockLocalMutex(&timed_task_mutex);
  res = (tt_thread_status == TT_RUNNING);
  GalUtil_UnlockLocalMutex(&timed_task_mutex);
  return res;
}

/* SAM 2/7/00: In order to make the "same" thing happen in timed
   tasks and in threads, we have to set it up so that the caller
   can wait for all the tasks to die. My solution for this is for
   run_task_thread to increment a counter when it starts up a thread,
   and for each thread to decrement the counter when it exits.
   It will check for loop_exit after
   each task is done, and if it's set, it will terminate. When
   the thread counter reaches 0 and loop_exit is set, then the
   executor will signal to this condition variable to wake up,
   and the wait function will return.

   The RIGHT way to do this would be track all the threads, because
   this way I can't cancel a thread that's sleeping, only when it
   wakes up. So a long-sleeping task could cause the thread waiter
   to delay until that task wakes up and realizes everything is
   over. But that would involve keeping track of all the threads,
   and I'm not going there right now. */

#ifdef GAL_THREADS
static int force_task_finish = 0;
static GalUtil_ConditionVar wait_cond;
static int wait_cond_init = 0;

void releaseConditionVar(void)
{
  /* SAM 12/13/00: On Linux, but not Solaris, it seems that
     destroying this condition variable happens "too soon"
     when you force exit() in threads. I tried to add
     an atexit() call to shut down the main loop cleanly first,
     but it didn't work. */
  if (wait_cond_init == 1) {
    GalUtil_DestroyConditionVar(&wait_cond);
  }
}
#endif /* GAL_THREADS */

void Gal_TimedTaskLoopThreadWaiter()
{
#ifndef GAL_THREADS
  GalUtil_Warn("Can't wait on threads because they're not enabled");
#else
  if (!Gal_TimedTaskThreadsEnabled()) {
    GalUtil_Warn("Can't wait on threads because they're not enabled");
  } else {
    /* First, we lock the mutex. */
    GalUtil_LockLocalMutex(&timed_task_mutex);
    /* I'll use the tt_thread_status variable as my predicate.
       Check it again, just in case. */
    if (tt_thread_status != TT_RUNNING) {
      tt_thread_status = TT_IDLE;
      GalUtil_UnlockLocalMutex(&timed_task_mutex);
      return;
    }
    force_task_finish = 0;
    while (!force_task_finish) {
      GalUtil_ConditionWait(&wait_cond, &timed_task_mutex);
    }
    /* If this dropped out of the loop by mistake, then
       I'm not sure what to do. So I'm not going to check. */
    tt_thread_status = TT_IDLE;
    GalUtil_UnlockLocalMutex(&timed_task_mutex);
    return;    
  }
#endif /* #ifndef GAL_THREADS */
}   

/* SAM 9/27/99: Added this function to take the place of all calls
   to GalIO_AddTimedTask(WithIO), to worry about the "right thing to do"
   when threads are requested. Remember, we must use nanosleep() to
   sleep, because it's thread-safe. See GalUtil_MilliSleep.

   These tasks should all call ReAddTask() when they
   restart themselves. In the timed task case, ReAddTask() will just call
   the appropriate timed task function; in the thread case, it will
   set data which will indicate to the thread that
   it should continue. These tasks also should be started with a
   read_blocking_available argument, and restarted as well; in the
   timed task it will always be passed to the task as 0, but it
   will be passed through to the task in the timed task case.
   If it's 1, it's up to the task function to set the blocking
   on the appropriate connection. In the thread case, if
   read_blocking_available is 0, GalUtil_MilliSleep() will be called
   before continuing. */

static void __Gal_UpdateTaskPkg(Gal_TaskPkg *p,
				void *caller_data,
				long num_millisecs,
				void (*cleanup_fn)(void *),
				GAL_SOCKET *read_socket,
				GAL_SOCKET *write_socket,
				GAL_SOCKET *err_socket,
				FILE *read_file,
				FILE *write_file,
				FILE *err_file,
				Gal_TaskConditionFn condition)
{
  /* Mark the package as not done, and reset the time. */
  p->done = 0;
  __Gal_SetTaskPkgMillisecs(p, num_millisecs);
  p->read_socket = read_socket;
  p->write_socket = write_socket;
  p->err_socket = err_socket;
  p->read_file = read_file;
  p->write_file = write_file;
  p->err_file = err_file;
  p->condition = condition;
  if ((p->caller_data) && (p->cleanup_fn) &&
	((caller_data != p->caller_data) || (cleanup_fn != p->cleanup_fn)))
    (*p->cleanup_fn)(p->caller_data);
  p->caller_data = caller_data;
  p->cleanup_fn = cleanup_fn;
}

static void __Gal_SetTaskPkgMillisecs(Gal_TaskPkg *p,
				      long num_millisecs)
{
  long secs, usecs;
  
  p->num_millisecs = num_millisecs;
  secs = (p->num_millisecs) / 1000;
  usecs = ((p->num_millisecs) % 1000) * 1000;
  _gal_gettimeofday(&p->tv);
  p->tv.tv_sec += secs;
  p->tv.tv_usec += usecs;
}
  

static Gal_TaskPkg *NewTaskPkg(void (*task)(Gal_TaskPkg *),
			       void *caller_data,
			       long num_milliseconds,
			       int read_blocking,
			       void (*cleanup_fn)(void *),
			       GAL_SOCKET *read_socket,
			       GAL_SOCKET *write_socket,
			       GAL_SOCKET *err_socket,
			       FILE *read_file,
			       FILE *write_file,
			       FILE *err_file,
			       Gal_TaskConditionFn condition)
{  
  Gal_TaskPkg *new_pkg = (Gal_TaskPkg *) calloc(1, sizeof(Gal_TaskPkg));

  new_pkg->task = task;
  new_pkg->read_blocking = read_blocking;
  new_pkg->executing = 0;
  __Gal_UpdateTaskPkg(new_pkg, caller_data, num_milliseconds, 
		      cleanup_fn, read_socket, write_socket, err_socket,
		      read_file, write_file, err_file, condition);
  return new_pkg;
}

/* This will be a detached thread. I will use thread-specific data
   to handle the case of readding. */

#ifdef GAL_THREADS
static int thread_counter = 0;

static void run_thread_cleanup(void *arg)
{
  GalUtil_Debug1("Exiting thread %d\n", GalUtil_CurrentThreadID());
  run_task_pkg_cleanup((Gal_TaskPkg *) arg);
  /* Decrement the thread counter. */
  GalUtil_LockLocalMutex(&timed_task_mutex);
  thread_counter--;
  if ((tt_thread_status != TT_RUNNING) &&
      (thread_counter == 0)) {
    /* If this is the last thread, disable threads. */
    force_task_finish = 1;
    /* I don't really care how many are waiting. */

    GalUtil_ConditionBroadcast(&wait_cond);
  }  
  GalUtil_UnlockLocalMutex(&timed_task_mutex);
}

/* SAM 2/7/00: I had originally set up the thread checking as just
   an exit from the loop in run_task_thread(), but the tasks
   themselves might not return at the proper granularity (if,
   for instance, they keep reading data because they're always receiving
   connections, or if they're reading using a function like recv()
   which doesn't seem to be a cancellation point). So I will
   try to set this up as a cancellation and cleanup thing, and
   write a function which permits cleanup checks. */

static void *run_task_thread(void *arg)
{
  Gal_TaskPkg *pkg = (Gal_TaskPkg *) arg;
  GalUtil_Debug1("Entering thread %d\n", GalUtil_CurrentThreadID());
  /* Increment the thread counter. */
  GalUtil_LockLocalMutex(&timed_task_mutex);
  thread_counter++;
  GalUtil_UnlockLocalMutex(&timed_task_mutex);

  /* Set the cleanup function to be the thread cleanup function. */

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(run_thread_cleanup, (void *) pkg);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(run_thread_cleanup, (void *) pkg);
#endif

  while (1) {

    /* First, check the condition if it's present. If not,
       if there's no blocking set, sleep the appropriate
       amount of time. We use GalUtil_MilliSleep() because it's thread-safe. */
    
    if (pkg->condition && (*pkg->condition)(pkg->caller_data)) {      
      pkg->run_reasons = GAL_CONDITION_SATISFIED;      
    } else if (!pkg->read_blocking) {	
      GalUtil_MilliSleep(pkg->num_millisecs);

      /* If immediate exit is requested, exit as well. Check this
	 after the task too. */
      GalUtil_LockLocalMutex(&timed_task_mutex);

      if (tt_thread_status == TT_IMMEDIATE_EXIT) {
	GalUtil_UnlockLocalMutex(&timed_task_mutex);
	GalUtil_ThreadCancel(GalUtil_CurrentThreadID());
	GalUtil_ThreadTestCancel();
	break;
      }

      pkg->run_reasons = GAL_TIMER_EXPIRED;
      GalUtil_UnlockLocalMutex(&timed_task_mutex);
    } else {
      pkg->run_reasons = GAL_THREAD_READABLE;
    }
    /* Set done. */
    pkg->done = 1;
    /* Check for cancellation, then disable it until the task
       is done. This will guarantee that a task is never run
       when it's been cancelled. */
    GalUtil_ThreadTestCancel();
    /* SAM 2/8/00: Originally, cancellation was disabled during
       tasks, but for long-running (blocking) reads, this meant
       that cancellations were never seen. I've now enabled
       cancellation. */
    /* pthread_setcancelstate(PTHREAD_CANCEL_DISABLE,
       &cancel_state); */
    /* pthread_setcancelstate(PTHREAD_CANCEL_ENABLE,
       &cancel_state); */
    pkg->executing = 1;
    (*pkg->task)(pkg);
    pkg->executing = 0;
    pkg->run_reasons = 0;
    /* pthread_setcancelstate(cancel_state, &cancel_state);*/
    /* If it's still set, exit. */
    if (pkg->done)
      break;
    /* If exit is requested, exit as well. Check this
       on wakeup too. */
    GalUtil_LockLocalMutex(&timed_task_mutex);
    if (tt_thread_status != TT_RUNNING) {
      GalUtil_UnlockLocalMutex(&timed_task_mutex);
      GalUtil_ThreadCancel(GalUtil_CurrentThreadID());
      GalUtil_ThreadTestCancel();
      break;
    }
    GalUtil_UnlockLocalMutex(&timed_task_mutex);
  }
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(1);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(1);
#endif
  return (void *) NULL;
}
#endif /* GAL_THREADS */

int Gal_TaskPkgBlocking(Gal_TaskPkg *pkg)
{
  return pkg->read_blocking;
}

void *Gal_TaskPkgData(Gal_TaskPkg *pkg)
{
  return pkg->caller_data;
}

int Gal_TaskPkgRunReasons(Gal_TaskPkg *pkg)
{
  return pkg->run_reasons;
}

/* 4/18/02: Must be protected with push/pop for thread cancellation. */

static int __Gal_RemoveTaskPkg(Gal_TaskPkg *p)
{
  int j;
  int retval = 0;
  
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) &timed_task_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) &timed_task_mutex);
#endif
  GalUtil_LockLocalMutex(&timed_task_mutex);
  
  for (j=0;j<timed_num;j++) {
    if (timed[j] == p) {
      int i;
      
      run_task_pkg_cleanup(p);
      for(i=j;i<timed_num-1;i++) {
	timed[i] = timed[i+1];
      }		
      timed_num--;
#if TIMED_TASK_DEBUG
      print_timed_tasks("removed");
#endif
      compute_fdset();
      retval = 1;
      break;
    }
  }

  GalUtil_UnlockLocalMutex(&timed_task_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
  
  return retval;
}

/* To avoid memory leaks here, we must remember to free the pkg.
   This happens in run_task_pkg_cleanup(). */

void Gal_RemoveTask(Gal_TaskPkg *task_pkg)
{
#ifndef GAL_THREADS
  __Gal_RemoveTaskPkg(task_pkg);
#else
  if (!Gal_TimedTaskThreadsEnabled()) {
    __Gal_RemoveTaskPkg(task_pkg);
  } else {
    /* Cancel the thread. */
    GalUtil_ThreadCancel(task_pkg->thread);
  }
#endif /* #ifndef GAL_THREADS */
}

#if 0
static void __Gal_ForceTaskExit()
{
  /* Gal_EndTasks(1); */
  /* I don't care how many tasks there still are.
     Kill them all. */
  force_task_finish = 1;
  GalUtil_ConditionBroadcast(&wait_cond);  
}
#endif

/* SAM 7/9/02: Adding a little tweak where -1 for num_milliseconds
   (which is an argument to all the API calls) means
   "don't start the task". It's important in the threaded
   case that EVERYTHING be set up before the tasks are
   started.

   Gal_StartTask returns 1 if the loop is finished and the
   task has been destroyed, 0 if it hasn't been destroyed. */

int Gal_StartTask(Gal_TaskPkg *pkg, int num_millisecs)
{
  if ((pkg->num_millisecs == -1) && (num_millisecs > -1)) {    
#ifndef GAL_THREADS
    return __Gal_ScheduleTimedTask(pkg, num_millisecs);
#else
    if (!Gal_TimedTaskThreadsEnabled()) {
      return __Gal_ScheduleTimedTask(pkg, num_millisecs);
    } else if (tt_thread_status == TT_RUNNING) {
      GalUtil_ThreadID new_thread;

      __Gal_SetTaskPkgMillisecs(pkg, num_millisecs);
      
      if(wait_cond_init == 0) {
	GalUtil_InitConditionVar(&wait_cond);
	atexit(releaseConditionVar);
	/* I tried to add this in, but then things hung on
	   c-C in Linux. */
	/* atexit(__Gal_ForceTaskExit); */
	wait_cond_init = 1;
      }
    
      /* Create a detached thread. Signals will be blocked
	 by virtue of the signal handler already having been
	 initialized. But just to be on the safe side... */
    
      if (!Gal_SignalsInitialized())
	Gal_InitializeSignals();
      GalUtil_ThreadCreate(&new_thread, run_task_thread, (void *) pkg);
      pkg->thread = new_thread;
      return 0;
    } else {
      run_task_pkg_cleanup(pkg);
      return 1;
    }
#endif /* #ifndef GAL_THREADS */
  } else {
    return 0;
  }
}

Gal_TaskPkg *Gal_AddTaskExtended(void (*task)(Gal_TaskPkg *),
				 void *caller_data,
				 long num_millisecs,
				 int read_blocking_available,
				 GAL_SOCKET *read_socket,
				 GAL_SOCKET *write_socket,
				 GAL_SOCKET *err_socket,
				 FILE *read_file, FILE *write_file,
				 FILE* err_file,
				 Gal_TaskConditionFn condition,
				 void (*cleanup_fn)(void *))
{
  Gal_TaskPkg *new_pkg;

#ifdef GAL_THREADS
  if (Gal_TimedTaskThreadsEnabled()) {
    if (tt_thread_status != TT_RUNNING) {
      return (Gal_TaskPkg *) NULL;
    }
  } else {
    read_blocking_available = 0;
  }
#else
  read_blocking_available = 0;
#endif /* #ifdef GAL_THREADS */

  new_pkg = NewTaskPkg(task, caller_data, -1, read_blocking_available,
		       cleanup_fn,
		       read_socket, write_socket, err_socket,
		       read_file, write_file, err_file, condition);
  if (Gal_StartTask(new_pkg, num_millisecs) == 1) {
    /* About to exit. */
    return (Gal_TaskPkg *) NULL;
  } else {
    return new_pkg;
  }
}

Gal_TaskPkg *Gal_AddTask(void (*task)(Gal_TaskPkg *), void *caller_data,
			 long num_millisecs, int read_blocking_available,
			 void (*cleanup_fn)(void *))
{
  return Gal_AddTaskExtended(task, caller_data, num_millisecs,
			     read_blocking_available,
			     0, 0, 0, 0, 0, 0, NULL, cleanup_fn);
}

Gal_TaskPkg *Gal_AddTaskWithSocketIO(void (*task)(Gal_TaskPkg *), void *caller_data,
				     long num_millisecs,
				     int read_blocking_available,
				     GAL_SOCKET *read_socket,
				     GAL_SOCKET *write_socket,
				     void (*cleanup_fn)(void *))
{
  return Gal_AddTaskExtended(task, caller_data, num_millisecs,
			     read_blocking_available,
			     read_socket, write_socket, 0, 0, 0, 0,
			     NULL, cleanup_fn);
}

Gal_TaskPkg *Gal_AddTaskWithFileIO(void (*task)(Gal_TaskPkg *), void *caller_data,
				   long num_millisecs,
				   int read_blocking_available,
				   FILE *read_file, FILE *write_file,
				   void (*cleanup_fn)(void *))
{
  return Gal_AddTaskExtended(task, caller_data, num_millisecs,
			     read_blocking_available,
			     0, 0, 0, read_file, write_file, 0,
			     NULL, cleanup_fn);
}

/* If either the caller_data or the cleanup_fn changed,
   run the cleanup function as long as there's both an
   old object and an old cleanup function. */

void Gal_ReAddTaskExtended(Gal_TaskPkg *p, void *caller_data,
			   long num_millisecs, int read_blocking_available,
			   GAL_SOCKET *read_socket, GAL_SOCKET *write_socket,
			   GAL_SOCKET *err_socket,
			   FILE *read_file, FILE *write_file, FILE *err_file,
			   Gal_TaskConditionFn condition,
			   void (*cleanup_fn)(void *))
{
#ifndef GAL_THREADS
  __Gal_UpdateTaskPkg(p, caller_data, -1, cleanup_fn,
		      read_socket, write_socket, err_socket,
		      read_file, write_file, err_file, condition);
  __Gal_ScheduleTimedTask(p, num_millisecs);
#else
  if (!Gal_TimedTaskThreadsEnabled()) {
    __Gal_UpdateTaskPkg(p, caller_data, -1, cleanup_fn,
			read_socket, write_socket, err_socket,
			read_file, write_file, err_file, condition);
    __Gal_ScheduleTimedTask(p, num_millisecs);
  } else if (tt_thread_status == TT_RUNNING) {
    /* The thread will continue. */
    p->read_blocking = read_blocking_available;
    
    p->done = 0;
    p->num_millisecs = num_millisecs;
    /* This will also update the fire time, but we don't care. */
    __Gal_UpdateTaskPkg(p, caller_data, num_millisecs, cleanup_fn,
			read_socket, write_socket, err_socket,
			read_file, write_file, err_file, condition);
  }
#endif /* #ifndef GAL_THREADS */
}

void Gal_ReAddTask(Gal_TaskPkg *p, void *caller_data,
		   long num_millisecs, int read_blocking_available,
		   void (*cleanup_fn)(void *))
{
  Gal_ReAddTaskExtended(p, caller_data, num_millisecs,
			read_blocking_available,
			0, 0, 0, 0, 0, 0, NULL, cleanup_fn);
}

void Gal_ReAddTaskWithSocketIO(Gal_TaskPkg *p, void *caller_data,
			       long num_millisecs,
			       int read_blocking_available,
			       GAL_SOCKET *read_socket,
			       GAL_SOCKET *write_socket,
			       void (*cleanup_fn)(void *))
{
  Gal_ReAddTaskExtended(p, caller_data, num_millisecs,
			read_blocking_available,
			read_socket, write_socket, 0, 0, 0, 0,
			NULL, cleanup_fn);
}

void Gal_ReAddTaskWithFileIO(Gal_TaskPkg *p, void *caller_data,
			     long num_millisecs, int read_blocking_available,
			     FILE *read_file, FILE *write_file,
			     void (*cleanup_fn)(void *))
{
  Gal_ReAddTaskExtended(p, caller_data, num_millisecs,
			read_blocking_available,
			0, 0, 0, read_file, write_file, 0,
			NULL, cleanup_fn);
}

/* Control for ending the tasks. */

void Gal_EndTasks(int immediate)
{
  GalUtil_LockLocalMutex(&timed_task_mutex);
  if (tt_loop_status == TT_RUNNING) {
    if (immediate) {
      tt_loop_status = TT_IMMEDIATE_EXIT;
    } else {
      tt_loop_status = TT_DEFERRED_EXIT;
    }
  }
  if (tt_thread_status == TT_RUNNING) {
    if (immediate) {
      tt_thread_status = TT_IMMEDIATE_EXIT;
    } else {
      tt_thread_status = TT_DEFERRED_EXIT;
    }
  }
  GalUtil_UnlockLocalMutex(&timed_task_mutex);
}

/* Control for possibly terminating a loop with a
   blocking read. */

void Gal_MaybeEndTask(int immediate, int deferred)
{
#ifndef GAL_THREADS
  return;
#else
  /* int cancel_state; */
  
  GalUtil_LockLocalMutex(&timed_task_mutex);
  switch (tt_thread_status) {
  case TT_IMMEDIATE_EXIT:
    if (immediate) {
      GalUtil_UnlockLocalMutex(&timed_task_mutex);
      GalUtil_ThreadCancel(GalUtil_CurrentThreadID());
    }
    return;
  case TT_DEFERRED_EXIT:
    if (deferred) {
      GalUtil_UnlockLocalMutex(&timed_task_mutex);
      GalUtil_ThreadCancel(GalUtil_CurrentThreadID());
    }
    break;
  case TT_IDLE:
  case TT_RUNNING:
  default:
    GalUtil_UnlockLocalMutex(&timed_task_mutex);
    return;
  }
#endif /* #ifndef GAL_THREADS */
}

/* SAM 6/12/00: Introduced the notion of idle tasks, which
   are done every time through the loop when there's nothing else
   to do. You can do whatever you want in these: update
   windows, allow scripting language background actions to
   be updated, etc. */

typedef struct {
  Gal_IdleFunction idle_func;
  void *client_data;
} IdleEntry;

#define MAX_IDLE_ENTRIES 10

static IdleEntry idle[MAX_IDLE_ENTRIES];

void Gal_init_idle_tasks()
{
  int i;
  for (i = 0; i < MAX_IDLE_ENTRIES; i++) {
    idle[i].idle_func = (Gal_IdleFunction) NULL;
    idle[i].client_data = (void *) NULL;
  }
}

/* This function returns 1 on success, 0 if there are no
   spaces left. */

int Gal_AddIdleFunction(Gal_IdleFunction func, void *client_data)
{
  int i;
  for (i = 0; i < MAX_IDLE_ENTRIES; i++) {
    if (!idle[i].idle_func) {
      idle[i].idle_func = func;
      idle[i].client_data = client_data;
      return 1;
    }
  }
  return 0;
}

void Gal_RemoveIdleFunction(Gal_IdleFunction func)
{
  int i;
  for (i = 0; i < MAX_IDLE_ENTRIES; i++) {
    if (func == idle[i].idle_func) {
      idle[i].idle_func = (Gal_IdleFunction) NULL;
      idle[i].client_data = (void *) NULL;
      break;
    }
  }
}

void Gal_RunIdleFunctions()
{
  int i;
  for (i = 0; i < MAX_IDLE_ENTRIES; i++) {
    if (idle[i].idle_func) {
      (*idle[i].idle_func)(idle[i].client_data);
    }
  }
}
      
/* 
 *  for Emacs...
 *  Local Variables:
 *  mode: c
 *  fill-column: 110
 *  comment-column: 80
 *  c-indent-level: 2
 *  c-continued-statement-offset: 2
 *  c-brace-offset: -2
 *  c-argdecl-indent: 2
 *  c-label-offset: -2
 *  End:
 */
