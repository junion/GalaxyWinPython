/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "gal_internal.h"
#include "galaxy/tag_enum.h"
#include "galaxy/gthread.h"

#define GAL_TAG(x,y) {x, y},
static Gal_TagMap Gal_ErrorTagMap[] =
{
#include "galaxy/error_tags.h"
  {NULL, 0}
};
#undef GAL_TAG

static GalUtil_LocalMutex tag_initialization_mutex;

void _Gal_init_error_tags(void)
{
  GalUtil_InitLocalMutex(&tag_initialization_mutex);
}

static Gal_TagArray Gal_ErrorTagArray = NULL;

/* The caller holds the mutex. */

static void Gal_InitializeErrorTags(void)
{
  if (Gal_ErrorTagArray == NULL) {
    Gal_ErrorTagArray = Gal_InitializeTagArray(Gal_ErrorTagMap);
  }
}

/* SAM 4/18/02: Gal_AddToTagArray contains a call to GalUtil_Warn.
   Must be push/popped. */

extern void _gal_unlock_mutex(void *mutex);

void Gal_AddErrorTags(Gal_TagMap *map)
{
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			    (void *) &tag_initialization_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex,
		       (void *) &tag_initialization_mutex);
#endif
  GalUtil_LockLocalMutex(&tag_initialization_mutex);

  Gal_InitializeErrorTags();
  Gal_AddToTagArray(map, Gal_ErrorTagArray);
  
  GalUtil_UnlockLocalMutex(&tag_initialization_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
}

char *Gal_ErrorTagString(int tag)
{
  char *s;
  GalUtil_LockLocalMutex(&tag_initialization_mutex);
  Gal_InitializeErrorTags();
  s = Gal_GetTagArrayTag(tag, Gal_ErrorTagArray);
  GalUtil_UnlockLocalMutex(&tag_initialization_mutex);
  return s;
}

void Gal_FreeErrorTags()
{
  GalUtil_LockLocalMutex(&tag_initialization_mutex);
  Gal_FreeTagArray(Gal_ErrorTagArray);
  Gal_ErrorTagArray = NULL;
  GalUtil_UnlockLocalMutex(&tag_initialization_mutex);
}
