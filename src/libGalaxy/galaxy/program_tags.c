/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy.h"
#include "galaxy/tag_enum.h"
#include "galaxy/gthread.h"

#define GAL_TAG(x,y) {x, y},
static Gal_TagMap Gal_ProgramTagMap[] =
{
#include "galaxy/extra_program_tags.h"
#include "galaxy/program_tags.h"
  {NULL, 0}
};
#undef GAL_TAG

/* For mutexes, let's just be coarse grained. */

static GalUtil_LocalMutex tag_mutex;

void _Gal_init_program_tags(void)
{
  GalUtil_InitLocalMutex(&tag_mutex);
}

static Gal_HashTable Gal_ProgramTagHash = NULL;
static Gal_TagArray Gal_ProgramTagArray = NULL;

/* This function is currently mutexed by the caller. */

static void Gal_InitializeProgramTags(void)
{
  if (Gal_ProgramTagHash == NULL)
    Gal_ProgramTagHash = Gal_InitializeTagTable(Gal_ProgramTagMap, 1000);
  if (Gal_ProgramTagArray == NULL)
    Gal_ProgramTagArray = Gal_InitializeTagArray(Gal_ProgramTagMap);
}

/* SAM 4/18/02: Tag table insertions are not
   thread-cancellation-safe, due to possible calls to Gal_FreeFrame. */

extern void _gal_unlock_mutex(void *mutex);

void Gal_AddProgramTags(Gal_TagMap *map)
{
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			    (void *) &tag_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex,
		       (void *) &tag_mutex);
#endif
  GalUtil_LockLocalMutex(&tag_mutex);
  
  Gal_InitializeProgramTags();
  Gal_AddToTagTable(map, Gal_ProgramTagHash);
  Gal_AddToTagArray(map, Gal_ProgramTagArray);
  
  GalUtil_UnlockLocalMutex(&tag_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
}

/* SAM 4/18/02: Also not cancellation-safe. */

char *Gal_ProgramTagString(int tag)
{
  char *s;

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			    (void *) &tag_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex,
		       (void *) &tag_mutex);
#endif
  GalUtil_LockLocalMutex(&tag_mutex);
  
  Gal_InitializeProgramTags();
  s = Gal_GetTagArrayTag(tag, Gal_ProgramTagArray);
  
  GalUtil_UnlockLocalMutex(&tag_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
  
  if (s == NULL)
    s = "";
  return s;
}

int Gal_GetProgramTag(const char *tag_name)
{
  Gal_Object int_obj;

  GalUtil_LockLocalMutex(&tag_mutex);
  Gal_InitializeProgramTags();
  if ((int_obj = Gal_GetHash(tag_name, Gal_ProgramTagHash))) {
    GalUtil_UnlockLocalMutex(&tag_mutex);
    return(Gal_IntValue(int_obj));
  } else {
    GalUtil_UnlockLocalMutex(&tag_mutex);
    return(GAL_NO_PROGRAM_TAG);
  }
}

/* SAM: We should never be returning static global variables.
   So I added a hash table copying function. This is currently
   only used to duplicate the hash table to use elsewhere, and
   that's all it should be ever used for. See read_program.c. */

/* SAM 4/18/02: Also not cancellation-safe. */

Gal_HashTable Gal_GetProgramTagHash(void)
{
  Gal_HashTable new_hash;

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			    (void *) &tag_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex,
		       (void *) &tag_mutex);
#endif
  GalUtil_LockLocalMutex(&tag_mutex);

  new_hash = Gal_CopyHash(Gal_ProgramTagHash);

  GalUtil_UnlockLocalMutex(&tag_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif

  return new_hash;
}

void Gal_FreeProgramTags()
{
  #ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			    (void *) &tag_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex,
		       (void *) &tag_mutex);
#endif
  GalUtil_LockLocalMutex(&tag_mutex);
  Gal_FreeHash(Gal_ProgramTagHash);
  Gal_ProgramTagHash = NULL;
  Gal_FreeTagArray(Gal_ProgramTagArray);
  Gal_ProgramTagArray = NULL;

  GalUtil_UnlockLocalMutex(&tag_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
}
