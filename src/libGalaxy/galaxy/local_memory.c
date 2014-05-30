/*
  This file (c) Copyright 2002 The MITRE Corporation.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "gal_internal.h"

#ifdef WIN32
#include <stdlib.h>
#include <malloc.h>
#endif

/* SAM 2/26/02: In order to manage memory better, and to
   unify some behavior which appears in about four different
   places, I've encapsulated the Vlist-based local memory cache
   that MIT originally developed. One thing missing from the
   original implementation is a record of all the chunks
   allocated, which I need if I'm going to clean up all
   outstanding memory to see what's left. */   

/* SAM 4/18/02: This code should have NO PRINTOUTS IN IT
   anywhere. (f)printf() is a mutex cancellation point, and this code
   is frequently employed in mutexed code. If there were any
   printouts in here, we'd have to do a cleanup push/pop
   pair for every mutex lock this is called in the context
   of. Bad, bad.

   This file currently contains nothing accept calls to itself,
   to malloc, and to free, and to the vdata stuff, which has
   the same properties. */

void
_Gal_LMInitialize(_Gal_LocalMemory *new_mem, int sizeof_elt, int elt_increment)
{
  GalUtil_InitLocalMutex(&(new_mem->mem_mutex));
  
  new_mem->serial_no = 1;
  new_mem->active_elements = 0;
  new_mem->alloc_calls = 0;
  new_mem->free_elements = (Vlist) NULL;
  new_mem->memory_chunks = (void **) NULL;
  new_mem->num_memory_chunks = 0;
  new_mem->elt_increment = elt_increment;
  new_mem->sizeof_elt = sizeof_elt;
}

_Gal_LocalMemory *
_Gal_LMCreate(int sizeof_elt, int elt_increment)
{
  _Gal_LocalMemory *new_mem = (_Gal_LocalMemory *) calloc(1, sizeof(_Gal_LocalMemory));
  _Gal_LMInitialize(new_mem, sizeof_elt, elt_increment);
  return new_mem;
}

void *_Gal_LMAllocate(_Gal_LocalMemory *mem, int *serial_ptr)
{
  void *new_elt;

  GalUtil_LockLocalMutex(&(mem->mem_mutex));
  mem->alloc_calls++;
  if (mem->free_elements == NULL) {
    mem->free_elements = _gal_alloc_vlist();
    _gal_vl_set_size(mem->free_elements, mem->elt_increment);
  }
  new_elt = _gal_pop_vdata(mem->free_elements);
  if (new_elt == NULL) {
    /* Allocate a new chunk. */
    void *new_chunk = (void *) calloc(mem->elt_increment, mem->sizeof_elt);
    /* Now go through it by bytes. */
    char *chunk_ptr = (char *) new_chunk;
    int j;
    
    for (j = 0; j < mem->elt_increment; j++) {
      _gal_push_vdata(mem->free_elements, (void *) (chunk_ptr + (j * mem->sizeof_elt)));
    }

    if (!mem->memory_chunks) {
      mem->memory_chunks = (void **) calloc(1, sizeof(void *));
      mem->memory_chunks[0] = new_chunk;
      mem->num_memory_chunks = 1;
    } else {
      mem->memory_chunks = (void *) realloc(mem->memory_chunks,
					    (mem->num_memory_chunks + 1) * sizeof(void *));
      mem->memory_chunks[mem->num_memory_chunks++] = new_chunk;
    }
    new_elt = _gal_pop_vdata(mem->free_elements);
  }
  
  if (new_elt) {
    mem->active_elements++;
    if (serial_ptr) {
      *serial_ptr = mem->serial_no;      
    }
    mem->serial_no++;
  } else if (serial_ptr) {
    *serial_ptr = -1;
  }
  
  GalUtil_UnlockLocalMutex(&(mem->mem_mutex));
  
  return new_elt;
}    

void _Gal_LMDeallocate(_Gal_LocalMemory *mem, void *elt)
{
  GalUtil_LockLocalMutex(&(mem->mem_mutex));
  mem->active_elements--;
  _gal_push_vdata(mem->free_elements, elt);
  GalUtil_UnlockLocalMutex(&(mem->mem_mutex));
}

void _Gal_LMDoElements(_Gal_LocalMemory *mem, void (*elt_fn)(void *))
{
  int i, j;

  GalUtil_LockLocalMutex(&(mem->mem_mutex));
  
  for (i = 0; i < mem->num_memory_chunks; i++) {
    char *byte_chunk = (char *) mem->memory_chunks[i];
    for (j = 0; j < mem->elt_increment; j++) {
      (*elt_fn)(byte_chunk + (j * mem->sizeof_elt));
    }
  }
  
  GalUtil_UnlockLocalMutex(&(mem->mem_mutex));
}

int _Gal_LMFree(_Gal_LocalMemory *mem, void (*free_fn)(void *elt), int free_mem)
{
  int i;
  int active_elements;

  GalUtil_LockLocalMutex(&(mem->mem_mutex));

  active_elements = mem->active_elements;
  
  if (mem->active_elements == 0) {
    if (mem->free_elements) {
      _gal_free_vlist(mem->free_elements);
      mem->free_elements = (Vlist) NULL;
    }
    if (free_fn) {
      _Gal_LMDoElements(mem, free_fn);
    }
    for (i = 0; i < mem->num_memory_chunks; i++) {
      free(mem->memory_chunks[i]);
    }
    if (mem->memory_chunks)
      free(mem->memory_chunks);
    mem->memory_chunks = (void **) NULL;
    mem->num_memory_chunks = 0;
    GalUtil_UnlockLocalMutex(&(mem->mem_mutex));
    if (free_mem)
      free(mem);
  } else {
    GalUtil_UnlockLocalMutex(&(mem->mem_mutex));
  }
  return active_elements;
}
