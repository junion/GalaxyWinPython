/*
  This file (c) Copyright 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"
#include "galaxy/gthread.h"
/* For the sleeper. */
#ifdef WIN32
#include <sys/timeb.h>
#include <sys/types.h>
#else
#include <time.h>
#ifndef HAVE_NANOSLEEP_HEADER
extern int nanosleep(const struct timespec *, struct timespec *);
#endif
#endif /* WIN32 */

#ifdef GAL_WIN32_THREADS

/* Thread local storage index used to store per-thread data. */
static DWORD tlsIndex = 0xFFFFFFFF;

/* Thread counter. */
static int threadCounter = 0;

/* Used to control access to shared collection of thread cancellation flags. 
   It is also used to control access to the thread counter. */
static GalUtil_LocalMutex *threadCancelMutex = NULL;

/* Shared collection of thread cancellation flags. */
static cancelFlagNodePtr threadCancelFlags = NULL;

static int getThreadCancelFlag(GalUtil_ThreadID threadId);
static void removeThreadCancelNode(GalUtil_ThreadID threadId);
static void* runWin32Thread(void* funcAndArgPtr);

/* Returns the value of the cancellation flag for the specified thread.
   A return value of 1 indicates the thread has been cancelled (via a call
   to GalUtil_ThreadCancel). A return value of 0 indicates the thread has
   not been cancelled. A return value of -1 indicates an error condition
   (most likely the specified thread id is invalid.) */
static int
getThreadCancelFlag(GalUtil_ThreadID threadId)
{
	cancelFlagNodePtr currentNode;
	int cancelFlag = -1;

	GalUtil_LockLocalMutex(threadCancelMutex);
	
	currentNode = threadCancelFlags;

	/* SCW 9/26/00: Do sequential search for now. Hash table would be better.  */
	while(currentNode != NULL && !(GalUtil_EqualThreadID(*(currentNode->threadId), threadId)))
		currentNode = currentNode->next;
	
	if(currentNode != NULL)
		cancelFlag = currentNode->cancelFlag;
	
	GalUtil_UnlockLocalMutex(threadCancelMutex);
	
	return cancelFlag;
}

/* Removes the thread cancellation info for the specified thread. */
static void
removeThreadCancelNode(GalUtil_ThreadID threadId)
{
	cancelFlagNodePtr currentNode;
	cancelFlagNodePtr prevNode = NULL;

	GalUtil_LockLocalMutex(threadCancelMutex);
	
	currentNode = threadCancelFlags;
	
	/* SCW 9/26/00: Do sequential search for now. Hash table would be better.  */
	
	while(currentNode != NULL && !(GalUtil_EqualThreadID(*(currentNode->threadId), threadId))) {
		prevNode = currentNode;
		currentNode = currentNode->next;
	}

	if(currentNode != NULL) {
		if(prevNode == NULL) /* Remove first node */
			threadCancelFlags = currentNode->next;
		else
			prevNode->next = currentNode->next;
		free(currentNode->threadId);
		free(currentNode);
	}

	GalUtil_UnlockLocalMutex(threadCancelMutex);
}
#endif

GalUtil_LocalMutex*
GalUtil_CreateLocalMutex(void)
{
  GalUtil_LocalMutex* mutex = malloc(sizeof(GalUtil_LocalMutex));
  GalUtil_Assert(mutex != NULL,"Error alloc'ing memory for mutex in GalUtil_CreateLocalMutex");
  GalUtil_InitLocalMutex(mutex);
  return mutex;
}

void
GalUtil_DestroyLocalMutex(GalUtil_LocalMutex *mutex)
{
#ifdef GAL_WIN32_THREADS
  DeleteCriticalSection(mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_mutex_destroy(mutex);
#endif
  free(mutex);
}

int GalUtil_TryLockLocalMutex(GalUtil_LocalMutex *mutex)
{
#ifdef GAL_WIN32_THREADS
  if(TryEnterCriticalSection(mutex))
	  return 0;
  else
	  return GAL_MUTEX_ALREADY_LOCKED;
#endif
#ifdef GAL_PTHREADS
  return pthread_mutex_trylock(mutex);
#endif
#ifndef GAL_THREADS
  *mutex = 1;
  return 0;
#endif
}

/* Pushes the specified cleanup function and argument onto the top of the 
   cleanup function stack for the calling thread. */
void
GalUtil_ThreadPushCleanup(void (*handler)(void*), void *arg) 
{
#ifdef GAL_WIN32_THREADS
	cleanupNodePtr topNode;
	threadLocalStorageNodePtr tls;

	cleanupNodePtr node = malloc(sizeof(cleanupNode));
	GalUtil_Assert(node != NULL,"Error alloc'ing memory for thread cleanup data in GalUtil_ThreadPushCleanup (Win32)");
	node->func = handler;
	node->arg = arg;
	node->next = NULL;

	GalUtil_Assert(tlsIndex != 0xFFFFFFFF,"Local thread storage index not initialized in GalUtil_ThreadPushCleanup (Win32)");

	tls = (threadLocalStorageNodePtr) TlsGetValue(tlsIndex);
	GalUtil_Assert(tls != NULL,"Local thread storage is NULL in GalUtil_ThreadPushCleanup (Win32)");
	topNode = tls->cleanupStack;
	if(topNode != NULL)
		node->next = topNode;
	tls->cleanupStack = node;
#endif

#ifdef GAL_PTHREADS
	/* SCW 10/3/00: Because Solaris implements pthread_cleanup_pop and
           pthread_cleanup_push as macros joined at the hip, the functions
           cannot be called in different scopes. */
	/*pthread_cleanup_push(handler, arg);*/
#endif
}

#if defined(GAL_WIN32_THREADS) || defined(GAL_PTHREADS)
void _gal_unlock_mutex(void *mutex)
{
  GalUtil_UnlockLocalMutex((GalUtil_LocalMutex *) mutex);
}
#endif

/* If the argument is non-zero, the cleanup function 
   at the top of the calling thread's cleanup stack
   is removed and executed. If the argument is zero, the top
   function is removed but not called. If the pop was
   successful, the return value is zero (this return value only 
   applies to Win32). */
int 
GalUtil_ThreadPopCleanup(int execute) 
{
#ifdef GAL_WIN32_THREADS
	cleanupNodePtr topNode;
	threadLocalStorageNodePtr tls;

	GalUtil_Assert(tlsIndex != 0xFFFFFFFF,"Local thread storage index not initialized in GalUtil_ThreadPopCleanup (Win32)");

	tls = (threadLocalStorageNodePtr) TlsGetValue(tlsIndex);
	GalUtil_Assert(tls != NULL,"Local thread storage is NULL in GalUtil_ThreadPopCleanup (Win32)");
	topNode = tls->cleanupStack;
	if(topNode == NULL) {
		return 1;
	} else {
		tls->cleanupStack = topNode->next;
		
		if(execute != 0)
			topNode->func(topNode->arg);

		free(topNode);
		return 0;
	}
#endif
#ifdef GAL_PTHREADS
	/* SCW 10/3/00: Because Solaris implements pthread_cleanup_pop and
           pthread_cleanup_push as macros joined at the hip, the functions
           cannot be called in different scopes. */
	/*pthread_cleanup_pop(execute);*/
	return 0;
#endif

#if (!defined(GAL_WIN32_THREADS) && !defined(GAL_PTHREADS))
	return 1;
#endif
}

/* Returns 0 if the specified thread was found and marked as cancelled. Error otherwise. */
int 
GalUtil_ThreadCancel(GalUtil_ThreadID threadId)
{
#ifdef GAL_WIN32_THREADS
	cancelFlagNodePtr currentNode;
	int result;

	GalUtil_LockLocalMutex(threadCancelMutex);
	
	currentNode = threadCancelFlags;
	/* SCW 9/26/00: Do sequential search for now. Hash table would be better.  */
	while(currentNode != NULL && !(GalUtil_EqualThreadID(*(currentNode->threadId), threadId)))
		currentNode = currentNode->next;
	
	if(currentNode == NULL)
		result = 1; /* PThread API returns ESRCH in the event of an error. */
	else {
		result = 0;
		currentNode->cancelFlag = 1;
	}

	GalUtil_UnlockLocalMutex(threadCancelMutex);
	
	return result;
#endif
#ifdef GAL_PTHREADS
	return pthread_cancel(threadId);
#endif
	return 1;
}

/* Tests if the calling thread has been marked for cancellation. If it has,
   the thread's cleanup routines are called and then the thread exits. */
void 
GalUtil_ThreadTestCancel()
{
#ifdef GAL_WIN32_THREADS
	threadLocalStorageNodePtr tls;

	if(getThreadCancelFlag(GalUtil_CurrentThreadID()) == 1)
	{
		/* Run through all cleanup routines. */
		while(GalUtil_ThreadPopCleanup(1) == 0);
		
		/* Remove the calling thread's cleanup node. */
		removeThreadCancelNode(GalUtil_CurrentThreadID());
		
		GalUtil_Assert(tlsIndex != 0xFFFFFFFF,"Local thread storage index not initialized in GalUtil_ThreadTestCancel (Win32)");

		tls = (threadLocalStorageNodePtr) TlsGetValue(tlsIndex);
		GalUtil_Assert(tls != NULL,"Local thread storage is NULL in GalUtil_ThreadTestCancel (Win32)");
		CloseHandle(*(tls->threadHandlePtr));
		free(tls->threadHandlePtr);
		
		/* SCW 10/2/00: This call to "free" is causing a strange runtime error in the debug version of the library. */
#ifndef _DEBUG
		free(tls);
#endif

		GalUtil_LockLocalMutex(threadCancelMutex);
		/* Decrement the thread counter. */
		threadCounter -= 1;
		/* Release the calling thread's local storage index if there are no more threads. */
		if(threadCounter == 0) {
			TlsFree(tlsIndex);
		}
		GalUtil_UnlockLocalMutex(threadCancelMutex);
	
		/* Exit the thread. */
		_endthreadex(0);
	} 
#endif

#ifdef GAL_PTHREADS
	pthread_testcancel();
#endif
}

#ifdef GAL_WIN32_THREADS
/* This is an attempt to release the Win32 HANDLE to the thread once the thread is done. This also handles
   thread cleanup when the thread exits normally. */
static void*
runWin32Thread(void* funcAndArgPtr)
{
	void *result;
	HANDLE *threadHandlePtr = malloc(sizeof(HANDLE*)); /* Freed in GalUtil_ThreadTestCancel. */
	funcStructPtr fPtr;
	threadLocalStorageNodePtr tls = malloc(sizeof(threadLocalStorageNodePtr)); /* Freed in GalUtil_ThreadTestCancel. */

	GalUtil_Assert(threadHandlePtr != NULL,"Error alloc'ing memory for thread HANDLE pointer in runWin32Thread");
	GalUtil_Assert(tls != NULL,"Error alloc'ing memory for thread local storage in runWin32Thread");

	GalUtil_Assert(tlsIndex != 0xFFFFFFFF,"Local thread storage index not initialized in runWin32Thread");

	GalUtil_Assert(TlsSetValue(tlsIndex, (void*) tls) != 0,"Error while setting thread local storage in runWin32Thread");

	GalUtil_Assert(DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(), threadHandlePtr, 0, FALSE, DUPLICATE_SAME_ACCESS) != 0,
		           "Error duplicating Win32 thread HANDLE in runWin32Thread");
	
	tls->threadHandlePtr = threadHandlePtr;
	tls->cleanupStack = NULL;

	fPtr = (funcStructPtr) funcAndArgPtr;
	result = fPtr->func(fPtr->arg);
	GalUtil_ThreadExit();

	/* Currently, the return from the thread routine is not accessible (since GalUtil_ThreadExit exits the thread). */
	return result;
}
#endif

#ifdef GAL_WIN32_THREADS
/* This function handles initializing the cancel flag and thread local storage for the main Win32 thread.
   You apparently get this "initialization" for free when using PThreads on Unix. */
void
GalUtil_InitMainWin32Thread()
{
	cancelFlagNodePtr node = malloc(sizeof(cancelFlagNode));
	GalUtil_ThreadID *threadIdPtr = malloc(sizeof(GalUtil_ThreadID)); /* This memory freed when and if the cancellation is freed. */
	HANDLE *threadHandlePtr = malloc(sizeof(HANDLE*)); /* This is not currently freed anywhere. */
	threadLocalStorageNodePtr tls = malloc(sizeof(threadLocalStorageNodePtr)); /* This is not currently freed anywhere. */

	GalUtil_Assert(node != NULL,"Error alloc'ing memory for thread cancel data in GalUtil_InitMainWin32Thread");
	GalUtil_Assert(threadIdPtr != NULL,"Error alloc'ing memory for thread id in GalUtil_InitMainWin32Thread");
	GalUtil_Assert(threadHandlePtr != NULL,"Error alloc'ing memory for thread HANDLE pointer in runWin32Thread");
	GalUtil_Assert(tls != NULL,"Error alloc'ing memory for thread local storage in runWin32Thread");
	
	if(threadCancelMutex == NULL)
		threadCancelMutex = GalUtil_CreateLocalMutex();

	if(tlsIndex == 0xFFFFFFFF) {
		tlsIndex = TlsAlloc();
		GalUtil_Assert(tlsIndex != 0xFFFFFFFF,"Error alloc'ing local thread storage in GalUtil_InitMainWin32Thread");
	}


	GalUtil_LockLocalMutex(threadCancelMutex);

	node->threadId = threadIdPtr;
	node->cancelFlag = 0;
	node->next = threadCancelFlags;
	threadCancelFlags = node;
	threadCounter += 1;

	GalUtil_UnlockLocalMutex(threadCancelMutex);

	GalUtil_Assert(TlsSetValue(tlsIndex, (void*) tls) != 0,"Error while setting thread local storage in GalUtil_InitMainWin32Thread");

	GalUtil_Assert(DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(), threadHandlePtr, 0, FALSE, DUPLICATE_SAME_ACCESS) != 0,
		           "Error duplicating Win32 thread HANDLE in GalUtil_InitMainWin32Thread");
	
	tls->threadHandlePtr = threadHandlePtr;
	tls->cleanupStack = NULL;
}
#endif

void
GalUtil_ThreadCreate(GalUtil_ThreadID *threadId, void* (*func)(void*), void *arg)
{
#ifdef GAL_WIN32_THREADS
	cancelFlagNodePtr node;
	funcStructPtr funcAndArgPtr;
	
	if(threadCancelMutex == NULL)
		threadCancelMutex = GalUtil_CreateLocalMutex();

	if(tlsIndex == 0xFFFFFFFF) {
		tlsIndex = TlsAlloc();
		GalUtil_Assert(tlsIndex != 0xFFFFFFFF,"Error alloc'ing local thread storage in GalUtil_ThreadCreate (Win32)");
	}
	
	funcAndArgPtr = malloc(sizeof(funcStructPtr)); /* This is freed in runWin32Thread. */
	GalUtil_Assert(funcAndArgPtr != NULL,"Error alloc'ing memory for thread function and argument in GalUtil_ThreadCreate (Win32)");
	funcAndArgPtr->func = func;
	funcAndArgPtr->arg = arg;


	GalUtil_LockLocalMutex(threadCancelMutex);

	/* Finally, really start the thread. */
	GalUtil_Assert((_beginthreadex(NULL, 0, (unsigned int (__stdcall *) (void *)) runWin32Thread, (void*) funcAndArgPtr, 0, threadId)) != 0,
				   "Error creating thread in GalUtil_ThreadCreate (Win32)");
	
	/* Allocate a cancel flag for the new thread. */
	node = malloc(sizeof(cancelFlagNode));
	GalUtil_Assert(node != NULL,"Error alloc'ing memory for thread cancel data in GalUtil_ThreadCreate (Win32)");
	/* SAM 8/2/02: Eep. Can't save the threadId directly.
	   It's not really referenceable memory. */
	node->threadId = malloc(sizeof(GalUtil_ThreadID));
	GalUtil_Assert(node->threadId != NULL,"Error alloc'ing memory for thread id in GalUtil_ThreadCreate");
	*(node->threadId) = *threadId;
	node->cancelFlag = 0;
	node->next = threadCancelFlags;
	threadCancelFlags = node;
	
	threadCounter += 1;

	GalUtil_UnlockLocalMutex(threadCancelMutex);
#endif

#ifdef GAL_PTHREADS
	pthread_attr_t attr;
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	pthread_create(threadId, &attr, func, arg);
#endif
}

void 
GalUtil_InitConditionVar(GalUtil_ConditionVar *condVar)
{
#ifdef GAL_WIN32_THREADS
	*condVar = CreateEvent(NULL, TRUE, FALSE, NULL);
#endif
#ifdef GAL_PTHREADS
	GalUtil_Assert(pthread_cond_init(condVar, NULL) == 0,"Error initializing condition variable in GalUtil_InitConditionVar (PThread)");
#endif
	/* How to handle Pthread condition variable init? */
} 

void
GalUtil_DestroyConditionVar(GalUtil_ConditionVar *condVar)
{
#ifdef GAL_WIN32_THREADS
  CloseHandle(*condVar);
#endif
#ifdef GAL_PTHREADS
  GalUtil_Assert(pthread_cond_destroy(condVar) == 0, "Error destroying condition variable in GalUtil_DestroyConditionVar (PThread)");
#endif
}

/* The specified mutex must be locked when this function is called. */
void
GalUtil_ConditionWait(GalUtil_ConditionVar *condVar, GalUtil_LocalMutex *mutex)
{
#ifdef GAL_WIN32_THREADS
	GalUtil_UnlockLocalMutex(mutex);
	GalUtil_ThreadTestCancel(); /* First test if the calling thread has been marked for cancellation. */
	WaitForSingleObject(*condVar, INFINITE);
	GalUtil_LockLocalMutex(mutex);
#endif 

#ifdef GAL_PTHREADS
    pthread_cond_wait(condVar, mutex);
#endif 
}

void
GalUtil_ConditionBroadcast(GalUtil_ConditionVar *condVar)
{
#ifdef GAL_WIN32_THREADS
	PulseEvent(*condVar);
#endif 

#ifdef GAL_PTHREADS
    pthread_cond_broadcast(condVar);
#endif
}

void
GalUtil_ThreadExit()
{
#ifdef GAL_WIN32_THREADS
	GalUtil_ThreadCancel(GalUtil_CurrentThreadID());
	GalUtil_ThreadTestCancel();
#endif 

#ifdef GAL_PTHREADS
    pthread_exit(NULL);
#endif
}

/* Thread-safe sleep, we think. Always available, not just
   with threads. */

void GalUtil_MilliSleep(int msecs)
{
#ifdef WIN32
        /* Since nanosleep() is a thread cancellation point in POSIX,
	   test if the calling thread has been marked for cancellation. */
        GalUtil_ThreadTestCancel();
	Sleep(msecs);
#else
	struct timespec ts;
	ts.tv_sec = msecs / 1000;
	msecs %= 1000;
	ts.tv_nsec = msecs * 1000000;
	nanosleep(&ts, NULL);
#endif
}  
