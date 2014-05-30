/*
  This file (c) Copyright 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef __H_GALAXY_GTHREAD_
#define __H_GALAXY_GTHREAD_

#include "galaxy/util.h"

/* GAL_THREADS is the marker of thread compilation. See rules.make. */

#ifdef GAL_THREADS

/* This should be conditionalized on each platform that supports
 * pthreads, and another conditionalization added for other thread
 * libraries.
 */

#ifdef WIN32
#include <windows.h>
#include <process.h>
#define GAL_WIN32_THREADS
#else
/* At least MacOS X needs sys/types.h included
   with pthread.h, even though pthread.h should include
   it itself. */
#include <sys/types.h>
#include <pthread.h>
#include <unistd.h>
#include <time.h>
/* #include <sys/systeminfo.h> */
#define GAL_PTHREADS
#endif /* WIN32 */

#endif /* GAL_THREADS */

/* Mutexes
 *
 * GalUtil_LocalMutex
 *  A light-weight mutex that can only be used in a single process.
 *  NOTE: You must ensure that GalUtil_InitLocalMutex has been called on
 *  any GalUtil_LocalMutex before you use it.
 */

#ifndef GAL_THREADS
typedef long GalUtil_LocalMutex;

#define GalUtil_InitLocalMutex(mutex) *mutex = 0
#define GalUtil_LockLocalMutex(mutex) *mutex = 1
#define GalUtil_UnlockLocalMutex(mutex) *mutex = 0
#define GAL_MUTEX_ALREADY_LOCKED 1

#endif

#ifdef GAL_WIN32_THREADS
/* SCW 9/25/00: Note that for Win32, we are using a CRITICAL_SECTION instead of a handle to a kernel mutex object. */
typedef CRITICAL_SECTION GalUtil_LocalMutex;

#define GalUtil_InitLocalMutex(mutex) InitializeCriticalSection(mutex)
#define GalUtil_LockLocalMutex(mutex) EnterCriticalSection(mutex)
#define GalUtil_UnlockLocalMutex(mutex) LeaveCriticalSection(mutex)
#define GAL_MUTEX_ALREADY_LOCKED 1
#endif

#ifdef GAL_PTHREADS
typedef pthread_mutex_t GalUtil_LocalMutex;
#define GalUtil_InitLocalMutex(mutex) pthread_mutex_init(mutex,0)

#define GalUtil_LockLocalMutex(mutex) pthread_mutex_lock(mutex)
#define GalUtil_UnlockLocalMutex(mutex) pthread_mutex_unlock(mutex)
#define GAL_MUTEX_ALREADY_LOCKED EBUSY

#endif

/* Returns an initialized mutex */
GalUtil_LocalMutex* GalUtil_CreateLocalMutex(void);
void GalUtil_DestroyLocalMutex(GalUtil_LocalMutex *mutex);
int GalUtil_TryLockLocalMutex(GalUtil_LocalMutex *mutex);

/* Thread control
 *
 */


#ifndef GAL_THREADS
typedef unsigned long GalUtil_ThreadID;
typedef unsigned long GalUtil_ConditionVar;

#define GalUtil_CurrentThreadID() 0
#define GalUtil_EqualThreadID(x,y) (x == y)

#endif

#ifdef GAL_WIN32_THREADS
typedef unsigned long GalUtil_ThreadID;
typedef HANDLE GalUtil_ConditionVar;

#define GalUtil_CurrentThreadID() GetCurrentThreadId()
#define GalUtil_EqualThreadID(x,y) (x == y)

#endif


#ifdef GAL_PTHREADS
typedef pthread_t GalUtil_ThreadID;
typedef pthread_cond_t GalUtil_ConditionVar;

#define GalUtil_CurrentThreadID() pthread_self()
#define GalUtil_EqualThreadID(x,y) (pthread_equal(x,y))

#endif

#ifdef GAL_THREADS
void GalUtil_ThreadPushCleanup(void (*handler)(void*), void *arg);
int GalUtil_ThreadPopCleanup(int execute);
int GalUtil_ThreadCancel(GalUtil_ThreadID tid);
void GalUtil_ThreadTestCancel();
void GalUtil_ThreadCreate(GalUtil_ThreadID *tid, void* (*func)(void*), void *arg);
void GalUtil_InitConditionVar(GalUtil_ConditionVar *condVar);
void GalUtil_DestroyConditionVar(GalUtil_ConditionVar *condVar);
void GalUtil_ConditionWait(GalUtil_ConditionVar *condVar, GalUtil_LocalMutex *mutex);
void GalUtil_ConditionBroadcast(GalUtil_ConditionVar *condVar);
void GalUtil_ThreadExit();
#endif

void GalUtil_MilliSleep(int msecs);

/* Some additional Win32-specific definitions. */

#ifdef GAL_WIN32_THREADS

/* This structure encapsulates information used by Win32 implementation of
   GalUtil_ThreadPushCleanup and GalUtil_ThreadPopCleanup. */
typedef struct cNode *cleanupNodePtr;
typedef struct cNode {
	void (*func) (void*); /* cleanup function */
	void *arg;            /* argument to cleanup function */
	cleanupNodePtr next;  /* next node in the stack */
} cleanupNode;

/* This structure encapsulates a thread's stack of cleanup functions and
   Win32 HANDLE. */
typedef struct tlsNode *threadLocalStorageNodePtr;
typedef struct tlsNode {
	cleanupNodePtr cleanupStack;
	HANDLE *threadHandlePtr;
} threadLocalStorageNode;

/* This structure encapsulates information used by Win32 implementation of
   GalUtil_ThreadCancel and GalUtil_ThreadTestCancel. */
typedef struct cfNode *cancelFlagNodePtr;
typedef struct cfNode {
	GalUtil_ThreadID *threadId; /* thread id */
	int cancelFlag;             /* has this thread been cancelled (1) or not (0) */
	cancelFlagNodePtr next;     /* next node in the stack */
} cancelFlagNode;

/* This structure is used to pass in the function and argument that used in Win32 threads. */
typedef struct fNode *funcStructPtr;
typedef struct fNode {
	void* (*func) (void*); /* function */
	void *arg;             /* argument to function */
} funcStruct;

void GalUtil_InitMainWin32Thread();

#endif

#endif /* __H_GALAXY_GTHREAD_ */
