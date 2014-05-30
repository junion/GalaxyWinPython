/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <stdlib.h>

#include "gal_internal.h"

#ifdef WIN32
#include "galaxy/sysdep.h"
#endif

#define MAXVLIST 1000

#define VL_HASHED 1
#define VL_SORTED 2
#define VL_ACSORT 4
#define VL_ALLOCATED 8

static int (*keyfn)() = NULL;

/* static int curvlist = 0; */
static VLIST free_vlists = {NULL,0,0,0};

static int    h_add_vdata(Vlist vp, void *sm, int uniq);

static GalUtil_LocalMutex vlist_mutex;

static _Gal_LocalMemory *__Gal_VlistMemory = (_Gal_LocalMemory *) NULL;

/* SAM 4/18/02: This code should have NO PRINTOUTS IN IT
   anywhere. (f)printf() is a mutex cancellation point, and this code
   is frequently employed in mutexed code. If there were any
   printouts in here, we'd have to do a cleanup push/pop
   pair for every mutex lock this is called in the context
   of. Bad, bad. I'll only make exceptions for GalUtil_Fatal and
   for the code which tries to free the vlist structure for
   final memory management.

   This file currently contains nothing accept calls to itself,
   to malloc, and to free. */

void _Gal_init_vlist(void)
{
  GalUtil_InitLocalMutex(&vlist_mutex);
  __Gal_VlistMemory = _Gal_LMCreate(sizeof(VLIST), MAXVLIST);
  /* Use a static element for the free vlists. That way,
     when I check how many elements are active, this one
     won't count. */
  __Gal_VlistMemory->free_elements = &free_vlists;
}

Vlist _gal_alloc_vlist(void)
{
  VLIST *vp;

  GalUtil_LockLocalMutex(&vlist_mutex);

  if (!__Gal_VlistMemory) {
    GalUtil_UnlockLocalMutex(&vlist_mutex);
    GalUtil_Fatal("Vlist memory not initialized; call Gal_InitializeStatics()");
  }

  vp = (VLIST *) _Gal_LMAllocate(__Gal_VlistMemory, (int *) NULL);
  GalUtil_UnlockLocalMutex(&vlist_mutex);
  
  if (vp == NULL) {
    return (Vlist) NULL;
  }

  vp->flags = VL_ALLOCATED;
  vp->cursize = 0;
  return((Vlist)vp);
}

static void _gal_i_free_vlist(Vlist vp)
{
  int i;

  for(i=0;i<vp->maxsize;i++) vp->data[i] = NULL;
  vp->cursize = 0;
  vp->flags = 0;
}

void _gal_free_vlist(Vlist vp)
{
  if (vp) {
    _gal_i_free_vlist(vp);
    _Gal_LMDeallocate(__Gal_VlistMemory, (void *) vp);
  }
}

static void __free_data(void *elt)
{
  Vlist vp = (Vlist) elt;

  if (vp->data) free(vp->data);
}

int _Gal_FreeAllVlists()
{
  int success = 0;
  
  GalUtil_LockLocalMutex(&vlist_mutex);
  if (__Gal_VlistMemory) {
    int active_elements;
    
    /* Don't free static memory. */
    __Gal_VlistMemory->free_elements = (Vlist) NULL;
    active_elements = _Gal_LMFree(__Gal_VlistMemory, __free_data, 1);

    if (active_elements > 0) {
      GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't free vlist repository; %d left", active_elements);
      /* Restore the list. */
      __Gal_VlistMemory->free_elements = &free_vlists;
    } else {
      __Gal_VlistMemory = (_Gal_LocalMemory *) NULL;
      _gal_i_free_vlist(&free_vlists);
      free_vlists.maxsize = 0;
      free(free_vlists.data);
      success = 1;
    }
  }
  GalUtil_UnlockLocalMutex(&vlist_mutex);
  return success;
}

void _gal_vl_set_size(Vlist vp, int newsize)
{
  if (vp->data)
    free(vp->data);
  vp->data = (void **)calloc(newsize,sizeof(void *));
  vp->maxsize = newsize;
  vp->cursize = 0;
}

void **_gal_get_vdata(Vlist vp, int *np)
{
  if(vp->flags & VL_HASHED)
    *np = vp->maxsize;
  else
    *np = vp->cursize;
  return(vp->data);
}

int _gal_add_vdata(Vlist vp, void *sm)
{
  int i;
  int newsize;
  void **newdata;
  if(vp->flags & VL_HASHED) return(h_add_vdata(vp,sm,1));

  if(vp->cursize == vp->maxsize){
    newsize = (2 * vp->maxsize) + 20;
    newdata = (void **)calloc(newsize,sizeof(void *));
    vp->maxsize = newsize;
    for(i=0;i<vp->cursize;i++){
      newdata[i] = vp->data[i];
    }
    if(vp->data != NULL)
      free(vp->data);

    vp->data = newdata;
  }
  
  vp->data[vp->cursize++] = sm;
  vp->flags &= ~VL_SORTED;
  vp->flags &= ~VL_ACSORT;
  return(1);
}

static int h_add_vdata(Vlist vp, void *sm, int uniq)
{
  int i;
  int newsize;
  void **newdata;
  int oldsize;
  void **olddata;
  int hash;
  void **sp = &sm;

  if(vp->cursize == vp->maxsize){
    newsize = (2 * vp->maxsize) + 20;
    newdata = (void **)calloc(newsize,sizeof(void *));
    oldsize = vp->maxsize;
    olddata = vp->data;
    vp->cursize = 0;
    vp->maxsize = newsize;
    vp->data = newdata;
    for(i=0;i<oldsize;i++){
      if(olddata[i] == NULL) continue;
      h_add_vdata(vp,olddata[i],1);
    }
    if(olddata != NULL)
      free(olddata);
  }
  if(keyfn == NULL){
    hash = *((int *)sp);
  }
  else hash = (*keyfn)(sm);
  hash = hash % vp->maxsize;
  for(i=hash;i<vp->maxsize;i++){

    if(vp->data[i] == NULL){
      vp->data[i] = sm;
      vp->cursize++;
      return(1);
    }

    if(keyfn == NULL){
      if(vp->data[i] == sm) return(0);
    }
    else
      if((*keyfn)(vp->data[i]) == (*keyfn)(sm)) return(0);
  }
  for(i=0;i<hash;i++){
    if(vp->data[i] == NULL){
      vp->data[i] = sm;
      vp->cursize++;
      return(1);
    }

    if(keyfn == NULL){
      if(vp->data[i] == sm) return(0);
    }
    else
      if((*keyfn)(vp->data[i]) == (*keyfn)(sm)) return(0);

  }
  return(0);
}

void *_gal_pop_vdata(Vlist vp)
{
  void *sm;
  if(vp->cursize <= 0) return(NULL);
  --(vp->cursize);
  sm = vp->data[vp->cursize];
  return(sm);
}

void *_gal_push_vdata(Vlist vp, void *cp)
{
  _gal_add_vdata(vp,cp);
  return(cp);
}

int _gal_size_vdata(Vlist vp)
{
  return(vp->cursize);
}
