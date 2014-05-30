/*
  This file (c) Copyright 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdlib.h>
#include "io_internal.h"

/* If I have to write another copy of queue code, I'm going
   to scream. This is a singly linked list. */

/* I need to manage sets of connections. I have been doing this through
   arrays, but linked lists will be better. When we add, we
   return the new queue. */

GalIO_PointerQueue *_GalIO_NewPointerQueue(int type, int volatile_queue,
					   int mutexable,
					   void (*free_fn))
{
  GalIO_PointerQueue *q = (GalIO_PointerQueue *) calloc(1, sizeof(GalIO_PointerQueue));

  q->type = type;
  q->volatile_queue = volatile_queue;
  if (mutexable) {
    q->mutex = GalUtil_CreateLocalMutex();
  }
  q->free_fn = free_fn;
  q->status = PQ_FREE;
  return q;
}

static GalIO_PointerQueueElement *__GalIO_NewPQE(GalIO_PointerQueue *queue)
{
  GalIO_PointerQueueElement *pqe;
  
  if (queue->volatile_queue && queue->cache) {
    pqe = queue->cache;
    queue->cache = queue->cache->next;
    pqe->next = (GalIO_PointerQueueElement *) NULL;
    pqe->status = PQ_IN_USE;
    return pqe;
  } else {
    pqe = (GalIO_PointerQueueElement *) calloc(1, sizeof(GalIO_PointerQueueElement));
    pqe->status = PQ_IN_USE;
    return pqe;
  }
}

static void __GalIO_FreePQE(GalIO_PointerQueue *queue,
			    GalIO_PointerQueueElement *pqe)
{
  if (queue->volatile_queue) {
    pqe->next = queue->cache;
    queue->cache = pqe;
    pqe->data = (void *) NULL;
    pqe->status = PQ_FREE;
  } else {
    free(pqe);
  }
}

/* Adding things to the queue. Never enqueue NULL. */
   
static void __GalIO_MTQueueEnqueue(GalIO_PointerQueue *queue,
				   void *element)
{
  GalIO_PointerQueueElement *pqe;

  if (!element)
    return;

  pqe = __GalIO_NewPQE(queue);
  pqe->data = element;  
  if (!queue->head) {
    queue->head = queue->tail = pqe;
  } else {
    queue->tail->next = pqe;
    queue->tail = pqe;
  }
}

void _GalIO_QueueEnqueue(GalIO_PointerQueue *queue,
			 void *element)
{
  if (!element)
    return;
  
  if (queue->mutex) {
    GalUtil_LockLocalMutex(queue->mutex);
    __GalIO_MTQueueEnqueue(queue, element);
    GalUtil_UnlockLocalMutex(queue->mutex);
  } else {
    __GalIO_MTQueueEnqueue(queue, element);
  }
}

/* Treating the queue as a random access list. Return the
   element if found, otherwise NULL. */

static void __GalIO_MTQueueDetach(GalIO_PointerQueue *queue,
				  GalIO_PointerQueueElement *last_pqe,
				  GalIO_PointerQueueElement *pqe)
{
  /* If we're removing the tail, back up. */
  if (pqe == queue->tail) {
    queue->tail = last_pqe;
    if (last_pqe)
      last_pqe->next = (GalIO_PointerQueueElement *) NULL;
  }
  /* If we're removing the head, move forward. */
  if (pqe == queue->head) {
    queue->head = pqe->next;
  }
  /* If we're not at the head of the list, excise
     the element from the previous's next slot. */
  if (last_pqe) {
    last_pqe->next = pqe->next;
  }
  __GalIO_FreePQE(queue, pqe);
}

static void *__GalIO_MTQueueDequeue(GalIO_PointerQueue *queue, void *element)
{
  
  GalIO_PointerQueueElement *pqe = queue->head;
  GalIO_PointerQueueElement *last_pqe = (GalIO_PointerQueueElement *) NULL;

  /* Find the first element which matches. */
  while (pqe) {
    if (pqe->data == element) {
      break;
    }
    last_pqe = pqe;
    pqe = pqe->next;
  }
  if (!pqe) {
    return (void *) NULL;
  } else {
    __GalIO_MTQueueDetach(queue, last_pqe, pqe);
    return element;
  }
}

void *_GalIO_QueueDequeue(GalIO_PointerQueue *queue, void *element)
{
  void *result;
  
  if (queue->mutex) {
    GalUtil_LockLocalMutex(queue->mutex);
    result = __GalIO_MTQueueDequeue(queue, element);
    GalUtil_UnlockLocalMutex(queue->mutex);
    return result;
  } else {
    return __GalIO_MTQueueDequeue(queue, element);
  }
}

static void *__GalIO_MTQueuePop(GalIO_PointerQueue *queue)
{
  GalIO_PointerQueueElement *pqe = queue->head;
  void *data;

  if (!pqe) {
    return (void *) NULL;
  } else {
    if (pqe == queue->tail) {
      queue->tail = (GalIO_PointerQueueElement *) NULL;
    }
    queue->head = queue->head->next;
    data = pqe->data;
    __GalIO_FreePQE(queue, pqe);
    return data;
  }
}

void *_GalIO_QueuePop(GalIO_PointerQueue *queue)
{
  void *result;
  
  if (queue->mutex) {
    GalUtil_LockLocalMutex(queue->mutex);
    result = __GalIO_MTQueuePop(queue);
    GalUtil_UnlockLocalMutex(queue->mutex);
    return result;
  } else {
    return __GalIO_MTQueuePop(queue);
  }
}

/* Sort of "pop if", but it searches the whole queue.
   5/10/02: I think I've fixed this so that it will
   work if it's reentrant (obviously, we won't
   need to worry about this in the threaded situation). See
   __GalIO_MTQueueApply for a more detailed description. */

static void __GalIO_MTQueueHarvest(GalIO_PointerQueue *queue)
{
  GalIO_PointerQueueElement *pqe = queue->head;
  GalIO_PointerQueueElement *last_pqe = (GalIO_PointerQueueElement *) NULL;
  GalIO_PointerQueueElement *next_pqe;
  
  while (pqe) {
    if (pqe->status == PQ_DESTROYED) {
      /* detach it, don't increment the last_pqe. */
      next_pqe = pqe->next;
      __GalIO_MTQueueDetach(queue, last_pqe, pqe);
      pqe = next_pqe;
    } else {
      last_pqe = pqe;
      pqe = pqe->next;
    }
  }
}

static void *__GalIO_MTQueueDequeueIf(GalIO_PointerQueue *queue,
				      int (*fn)(void *data, void *caller_data),
				      void *caller_data)
{
  
  GalIO_PointerQueueElement *pqe = queue->head;
  GalIO_PointerQueueElement *last_pqe = (GalIO_PointerQueueElement *) NULL;
  void *element = (void *) NULL;
  int previous_pq_status = queue->status;

  queue->status = PQ_IN_USE;
  /* Find the first element which matches. */
  while (pqe) {
    if (pqe->status == PQ_DESTROYED) {
      pqe = pqe->next;
    } else {
      if ((*fn)(pqe->data, caller_data)) {
	element = pqe->data;
	break;
      }
      last_pqe = pqe;
      pqe = pqe->next;
    }
  }
  queue->status = previous_pq_status;
  if (!pqe) {
  } else if (previous_pq_status == PQ_IN_USE) {
    pqe->status = PQ_DESTROYED;
  } else {      
    __GalIO_MTQueueDetach(queue, last_pqe, pqe);
  }
  /* At the end, if the previous status was PQ_FREE,
     then we harvest the destroyed elements. */
  if (previous_pq_status == PQ_FREE)
    __GalIO_MTQueueHarvest(queue);
  return element;
}

/* 4/18/02: because we don't know what fn is, we must assume that
   it's not thread-cancellation-safe. */

extern void _gal_unlock_mutex(void *mutex);

void *_GalIO_QueueDequeueIf(GalIO_PointerQueue *queue,
			    int (*fn)(void *data, void *caller_data),
			    void *caller_data)
{
  void *result;
  
  if (queue->mutex) {
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			      (void *) queue->mutex);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_push(_gal_unlock_mutex,
			 (void *) queue->mutex);
#endif
    GalUtil_LockLocalMutex(queue->mutex);
    result = __GalIO_MTQueueDequeueIf(queue, fn, caller_data);
    GalUtil_UnlockLocalMutex(queue->mutex);
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_pop(0);
#endif
    return result;
  } else {
    return __GalIO_MTQueueDequeueIf(queue, fn, caller_data);
  }
}

/* Accessing elements of the queue. */

static void *__GalIO_MTQueueNth(GalIO_PointerQueue *queue, int i)
{
  int j = 0;
  GalIO_PointerQueueElement *pqe = queue->head;

  if (i < 0)
    return (void *) NULL;

  while (pqe) {
    if (i == j) {
      return pqe->data;
    }
    j++;
    pqe = pqe->next;
  }
  return (void *) NULL;
}	

void *_GalIO_QueueNth(GalIO_PointerQueue *queue, int i)
{
  void *result;
  
  if (queue->mutex) {
    GalUtil_LockLocalMutex(queue->mutex);
    result = __GalIO_MTQueueNth(queue, i);
    GalUtil_UnlockLocalMutex(queue->mutex);
    return result;
  } else {
    return __GalIO_MTQueueNth(queue, i);
  }
}

static int __GalIO_MTQueueLength(GalIO_PointerQueue *queue)
{
  int i = 0;
  GalIO_PointerQueueElement *pqe = queue->head;

  while (pqe) {
    i++;
    pqe = pqe->next;
  }
  return i;
}  

int _GalIO_QueueLength(GalIO_PointerQueue *queue)
{
  int result;

  if (queue->mutex) {
    GalUtil_LockLocalMutex(queue->mutex);
    result = __GalIO_MTQueueLength(queue);
    GalUtil_UnlockLocalMutex(queue->mutex);
    return result;
  } else {
    return __GalIO_MTQueueLength(queue);
  }
}   

/* Applying to all members of the queue. */

/* The fn return 1 (continue), 0 (halt), -1 (dequeue). */

/* SAM 5/10/02: There's a nasty, tricky bug where this
   function can be called recursively, and things can
   be removed. Can we accommodate this?
   Well, when we recurse, what can happen?
   (1) If you remain
   in the queue, you can gain a next element. Not a problem, since
   we don't capture the next element until after
   we apply the function.
   (2) Some element in the queue ahead of you has been removed.
   A problem, because although you haven't been, and you've already
   looked at that element, the previous element becomes
   important if you yourself are about to be removed.
   (3) Some element after you in the queue has been removed.
   Not a problem, since you haven't been, and you
   haven't looked at your next yet.
   (4) You have been removed. This can absolutely be a problem.
   If the queue isn't volatile, you'll get a seg fault, since
   you've been freed; if the queue is volatile, you'll now be
   in the cache, which means your next is no longer an
   element in the list. And, for that matter, the next you
   WOULD have looked at is now inaccessible.
   There's also a larger problem where you'll get a mutex
   deadlock if you run threaded. Urk.

   I tried marking the use state of the individual elements, but
   (3) remained a problem. The best answer may be to mark the use
   state of the entire queue. */

static void __GalIO_MTQueueApply(GalIO_PointerQueue *queue,
				 int (*fn)(void *data, void *caller_data),
				 void *caller_data)
{
  GalIO_PointerQueueElement *pqe = queue->head;
  GalIO_PointerQueueElement *last_pqe = (GalIO_PointerQueueElement *) NULL;
  GalIO_PointerQueueElement *next_pqe;
  int result;
  int previous_pq_status = queue->status;
  int break_loop = 0;

  queue->status = PQ_IN_USE;

  while (pqe && !break_loop) {
    /* I originally ran the operation with the mutex
       unlocked, to avoid deadlocks. But the possibility that
       this queue could be changed while another operation
       was running ended up seeming worse than the
       possibility of deadlocks. So I'm removing the
       mutex unlock to see what happens. */
    /*
    if (queue->mutex) {
      GalUtil_UnlockLocalMutex(queue->mutex);
    }
    */
    if (pqe->status == PQ_DESTROYED) {
      result = -1;
    } else {
      result = (*fn)(pqe->data, caller_data);
    }
    /*
    if (queue->mutex) {
      GalUtil_LockLocalMutex(queue->mutex);
    }
    */
    switch (result) {
    case 0:
      /* Halt. Return. */
      break_loop = 1;
      break;
    case -1:
      /* Remove this item from the queue. Don't "increment" last_pqe.
         If the queue was previously in use, don't actually
	 delete the elements; just mark them as destroyed. */
      next_pqe = pqe->next;
      if (previous_pq_status == PQ_IN_USE) {
	pqe->status = PQ_DESTROYED;
      } else {
	__GalIO_MTQueueDetach(queue, last_pqe, pqe);
      }
      pqe = next_pqe;
      break;
    default:
      /* Do nothing. */
      last_pqe = pqe;
      pqe = pqe->next;
      break;
    }    
  }
  queue->status = previous_pq_status;
  /* At the end, if the previous status was PQ_FREE,
     then we harvest the destroyed elements. */
  if (previous_pq_status == PQ_FREE)
    __GalIO_MTQueueHarvest(queue);
}

/* 4/18/02: Must assume not thread-cancellation safe. */

void _GalIO_QueueApply(GalIO_PointerQueue *queue,
		       int (*fn)(void *data, void *caller_data),
		       void *caller_data)
{
  if (!queue)
    return;
  
  if (queue->mutex) {
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			      (void *) queue->mutex);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_push(_gal_unlock_mutex,
			 (void *) queue->mutex);
#endif
    GalUtil_LockLocalMutex(queue->mutex);
    __GalIO_MTQueueApply(queue, fn, caller_data);
    GalUtil_UnlockLocalMutex(queue->mutex);
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_pop(0);
#endif
  } else {
    __GalIO_MTQueueApply(queue, fn, caller_data);
  }
}

/* Working without a net. Either the source or the target
   had better have no free function, otherwise we're setting
   ourselves up for major bus errors... */

/* copy_fn may not be thread-cancellation-safe. So in order to bracket
   the push/pop appropriately, we have to write this as a 
   nested set of calls. Ugh. */
   
static void __GalIO_MTQueueImport(GalIO_PointerQueue *target_queue,
				  GalIO_PointerQueue *source_queue,
				  int level,
				  void *(*copy_fn)(void *))
{
  GalIO_PointerQueueElement *source_pqe;
  
  switch (level) {
  case 0:
    if (target_queue->mutex) {
#ifdef GAL_WIN32_THREADS
      GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
				(void *) target_queue->mutex);
#endif
#ifdef GAL_PTHREADS
      pthread_cleanup_push(_gal_unlock_mutex,
			   (void *) target_queue->mutex);
#endif
      GalUtil_LockLocalMutex(target_queue->mutex);
      __GalIO_MTQueueImport(target_queue, source_queue, 1, copy_fn);
      GalUtil_UnlockLocalMutex(target_queue->mutex);
#ifdef GAL_WIN32_THREADS
      GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
      pthread_cleanup_pop(0);
#endif
    } else {
      __GalIO_MTQueueImport(target_queue, source_queue, 1, copy_fn);
    }
    break;
  case 1:
    if (source_queue->mutex) {
#ifdef GAL_WIN32_THREADS
      GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
				(void *) source_queue->mutex);
#endif
#ifdef GAL_PTHREADS
      pthread_cleanup_push(_gal_unlock_mutex,
			   (void *) source_queue->mutex);
#endif
      GalUtil_LockLocalMutex(source_queue->mutex);
      __GalIO_MTQueueImport(target_queue, source_queue, 2, copy_fn);
      GalUtil_UnlockLocalMutex(source_queue->mutex);
#ifdef GAL_WIN32_THREADS
      GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
      pthread_cleanup_pop(0);
#endif
    } else {
      __GalIO_MTQueueImport(target_queue, source_queue, 2, copy_fn);
    }
    break;
  case 2:
    source_pqe = source_queue->head;
    while (source_pqe) {
      if (copy_fn)
	__GalIO_MTQueueEnqueue(target_queue, (*copy_fn)(source_pqe->data));
      else
	__GalIO_MTQueueEnqueue(target_queue, source_pqe->data);
      source_pqe = source_pqe->next;
    }
    break;
  }
}

void _GalIO_QueueImport(GalIO_PointerQueue *target_queue,
			GalIO_PointerQueue *source_queue,
			void *(*copy_fn)(void *))
{
  if (copy_fn)
    __GalIO_MTQueueImport(target_queue, source_queue, 0, copy_fn);
  else
    __GalIO_MTQueueImport(target_queue, source_queue, 2, copy_fn);
}

/* 4/18/02: Reorganized so that we can do mutex cleanup push/pop. */

static GalIO_PointerQueue *
__GalIO_MTQueueCopy(GalIO_PointerQueue *source_queue,
		    void *(*copy_fn)(void *))
{
  GalIO_PointerQueue *new_q;
  GalIO_PointerQueueElement *source_pqe;
  
  new_q = _GalIO_NewPointerQueue(source_queue->type,
				 source_queue->volatile_queue,
				 source_queue->mutex ? 1 : 0,
				 source_queue->free_fn);
  source_pqe = source_queue->head;
  while (source_pqe) {
    __GalIO_MTQueueEnqueue(new_q, (*copy_fn)(source_pqe->data));
    source_pqe = source_pqe->next;
  }
  return new_q;
}
  
GalIO_PointerQueue *_GalIO_QueueCopy(GalIO_PointerQueue *source_queue,
				     void *(*copy_fn)(void *))
{
  GalIO_PointerQueue *new_q;
  
  if (source_queue->mutex) {
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			      (void *) source_queue->mutex);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_push(_gal_unlock_mutex,
			 (void *) source_queue->mutex);
#endif
    GalUtil_LockLocalMutex(source_queue->mutex);
    new_q = __GalIO_MTQueueCopy(source_queue, copy_fn);
    GalUtil_UnlockLocalMutex(source_queue->mutex);
#ifdef GAL_WIN32_THREADS
    GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
    pthread_cleanup_pop(0);
#endif
  } else {
    new_q = __GalIO_MTQueueCopy(source_queue, copy_fn);
  }
  return new_q;
}
  
/* Freeing up. */


static void __GalIO_MTQueueDestroy(GalIO_PointerQueue *queue)
{
  GalIO_PointerQueueElement *pqe;
  
  while (queue->cache) {
    pqe = queue->cache;
    queue->cache = queue->cache->next;
    free(pqe);
  }
  while (queue->head) {
    pqe = queue->head;
    queue->head = queue->head->next;
    if (pqe->data && queue->free_fn) {
      (*queue->free_fn)(pqe->data);
    }
    free(pqe);
  }
  free(queue);
}

void _GalIO_QueueDestroy(GalIO_PointerQueue *queue)
{
  if (!queue)
    return;

  if (queue->mutex) {
    GalUtil_LocalMutex *queue_mutex = queue->mutex;
    GalUtil_LockLocalMutex(queue_mutex);
    __GalIO_MTQueueDestroy(queue);
    GalUtil_UnlockLocalMutex(queue_mutex);
    GalUtil_DestroyLocalMutex(queue_mutex);
  } else {
    __GalIO_MTQueueDestroy(queue);
  }
}

/* Status. */

int _GalIO_QueueNonEmpty(GalIO_PointerQueue *queue)
{
  return queue && (queue->head != (void *) NULL);
}
