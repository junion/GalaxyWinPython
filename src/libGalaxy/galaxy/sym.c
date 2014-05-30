/*
  Portions of this file (c) Copyright 1998 - 2000 M.I.T.
  Portions of this file (c) Copyright 2002 The MITRE Corporation.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include "gal_internal.h"

/* definitions left over from lm incarnation */
#define TP_SYM  2

/* This is for the main symbol table */
#define DEF_TABLESIZE 50000

static HTABLE sym_table;

/*
 *   Allocate entries in blocks.
 *   This keeps track of only one block at a time; earlier blocks are "lost".
 *   There is no way to free blocks (esp. since the syms may be allocated to
 *   different hash tables), so there is a recycling list instead.
 */

/* SAM 4/18/02: To the greatest degree possible, these functions
   should have NO PRINTOUTS in them, because printouts are
   thread cancellation points and I'm trying to minimize the
   number of places where cancellation is possible. Gal_ClearHash()
   and Gal_FreeHash() and Gal_SetHash()
   are NOT cancellation-safe, because they
   call Gal_FreeObject, which can call Gal_FreeFrame, which 
   notifies you if you're freeing an unregistered frame, which
   I think is important. add_sym(), sym_name() is fine, unless the hash
   function isn't. Gal_GetTagArrayTag is fine,
   Gal_HashHasKey, Gal_GetHash() too,
   not Gal_MakeHash() or Gal_CopyHash(). Gal_HashListNext is OK,
   Gal_HashListRewind, Gal_InitializeTagArray, not
   Gal_InitializeTagTable, Gal_AddToTagTable (calls Gal_SetHash()).
   Gal_GetTagArrayTag is OK. */

typedef int (*hash_fn_ptr)(char *str, HTABLE *hp);

/* all calls directly accessing hash tables have been
   made static. --Christine 4/3/96
   */

/* Static Function Prototypes */
static void push_free_syms(SYM *syms);
static void init_table(HTABLE *hp, int size);
static void clear_table(HTABLE *hp);
static int def_hash(const char *str, HTABLE *hp);
static Sym alloc_sym();
static Sym sym_ht_lookup(const char *str, HTABLE *hp);
static Sym sym_ht_insert(const char *str, HTABLE *hp);
static void sym_ht_clear(HTABLE *hp);
static Sym alloc_entry(void);
static int def_sym_hash(const char *str);
static Sym sym_lookup(HTABLE *hp, const char *str);
static Sym sym_insert(HTABLE *hp, const char *str);
static void init_sym_table(int size);

/* This is for random symbol hash tables */

/* SAM 2/26/02: The SYM structure has a next pointer. You
   can free a linked list of SYM structures. It used to be that
   the way you free them is by keeping a linked list of
   free SYM structures. However, the memory management here
   had the same problem as the management for frames and
   objects: there was no list of all objects allocated.
   After thinking about it for a while, it seemed to me that
   using the local memory abstraction would work just fine
   here as well, and would save me a good deal of grief. */

static _Gal_LocalMemory *__Gal_SymMemory = (_Gal_LocalMemory *) NULL;

static GalUtil_LocalMutex sym_lock;

#define CHUNK 1000

void _Gal_init_sym(void)
{
  GalUtil_InitLocalMutex(&sym_lock);
  __Gal_SymMemory = _Gal_LMCreate(sizeof(SYM), CHUNK);
}

static Sym alloc_sym(void)
{
  SYM *sp;
  int index;

  GalUtil_LockLocalMutex(&sym_lock);
  if (!__Gal_SymMemory) {
    GalUtil_UnlockLocalMutex(&sym_lock);
    GalUtil_Fatal("Symbol memory not initialized; call Gal_InitializeStatics()");
  }

  sp = (SYM *) _Gal_LMAllocate(__Gal_SymMemory, &index);
  GalUtil_UnlockLocalMutex(&sym_lock);

  if (sp == NULL) {
    return (Sym) NULL;
  }

  sp->type = TP_SYM;
  sp->flags = 0;
  sp->index = index;

  return(sp);
}

static void __maybe_free_rules(void *elt)
{
  Sym s = (Sym) elt;
  if (s->rules) {
    _gal_free_vlist(s->rules);
    s->rules = (Vlist) NULL;
  }
}

int _Gal_FreeAllSyms()
{
  int success = 0;
  int active_elements = 0;
  
  GalUtil_LockLocalMutex(&sym_lock);
  if (__Gal_SymMemory) {
    active_elements = _Gal_LMFree(__Gal_SymMemory, __maybe_free_rules, 1);

    if (active_elements == 0) {
      __Gal_SymMemory = (_Gal_LocalMemory *) NULL;
      success = 1;
    }
  }      
  GalUtil_UnlockLocalMutex(&sym_lock);
  if (active_elements > 0) {
    GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't free symbol repository; %d left", active_elements);
  }
  return success;
}

static void push_free_syms(SYM *syms)
{
  SYM *next, *next_next;
  
  if (syms == NULL)
    return;

  next = syms;

  /* Loop through the linked list of things to free.
     Unlink the list, free the contents, deallocate the symbol. */
  
  while (next) {
    if (next->val) {
      Gal_FreeObject(next->val);
      next->val = NULL;
    }
    if (next->key) {
      free(next->key);
      next->key = NULL;
    }
    next_next = next->next;
    next->next = (SYM *) NULL;
    next->table_next = (SYM *) NULL;
    _Gal_LMDeallocate(__Gal_SymMemory, (void *) next);
    next = next_next;
  }
}

static void init_table(HTABLE *hp, int size)
{
  hp->table = (SYM **)calloc(size,sizeof(SYM *));
  hp->size = size;
  hp->xfactor = (size / 100) - 1;
  hp->mfactor = size - 1;
  hp->hashfn = NULL;
}

static void clear_table(HTABLE *hp)
{
   int i;
   for(i=0;i<hp->size;i++)
     hp->table[i] = NULL;
   hp->list = NULL;
   hp->list_next = NULL;
}

static void free_table(HTABLE *hp)
{
  if (hp)
  {
    if (hp->table)
      free(hp->table);
    free(hp);
  }
}

static int def_hash(const char *str, HTABLE *hp)
{
  unsigned hash = 0;
  const char *cp;
  for(cp=str;*cp;cp++) hash = (hash * hp->xfactor) + *cp;
  return(hash % hp->mfactor);
}

static Sym sym_ht_lookup(const char *str, HTABLE *hp)
{
   int hsh;
   SYM *sp = NULL;

   if (hp && str)
   {
     if(hp->hashfn == NULL)
       hsh = def_hash(str, hp);
     else hsh = (*(hp->hashfn))(str, hp);

     sp = hp->table[hsh];
     while(sp != NULL){
       if(strcmp(sp->key,str) == 0){
	 return((Sym)sp);
       }
       sp = sp->next;
     }
   }
   return(sp);
}

static Sym sym_ht_insert(const char *str, HTABLE *hp)
{
  int hsh;
  SYM *sp = NULL;

  if (hp && str)
  {
    if(hp->hashfn == NULL)
      hsh = def_hash(str, hp);
    else hsh = (*(hp->hashfn))(str, hp);

    sp = alloc_sym();
    if(sp == NULL)
      return(NULL);
    sp->count = 0;
    sp->key = _gal_strdup(str);
    sp->next = hp->table[hsh];
    hp->table[hsh] = sp;
    sp->table_next = hp->list;
    hp->list = sp;
  }
  return(sp);
}

static void sym_ht_clear(HTABLE *hp)
{
  int i;

  if (hp)
  {
    for(i=0;i<hp->size;i++)
    {
      if (hp->table[i])
      {
	push_free_syms(hp->table[i]);
	hp->table[i] = NULL;
      }
    }
    hp->list = NULL;
    hp->list_next = NULL;
  }
}

static Sym alloc_entry()
{
  SYM *ep = alloc_sym();

  if (!ep) {
    return (Sym) NULL;
  }
  
  ep->rules = _gal_alloc_vlist();

  return(ep);
}

/* this corresponds to the default sym hash size of 50000 */
static int def_sym_hash(const char *str)
{
  unsigned hash = 0;
  const char *cp;
  for(cp=str;*cp;cp++) hash = (hash * 499) + *cp;
  return(hash % 49999);
}

static Sym sym_lookup(HTABLE *hp, const char *str)
{
   int hsh;
   SYM *ep;
   if(hp->hashfn == NULL)
     hsh = def_sym_hash(str);
   else hsh = (*(hp->hashfn))(str, hp);

   ep = hp->table[hsh];
   while(ep != NULL){
     if(strcmp(ep->key,str) == 0){
       return((Sym)ep);
     }
     ep = ep->next;
   }
   return((Sym)ep);
}

static Sym sym_insert(HTABLE *hp, const char *str)
{
   int hsh;
   SYM *sp;
   if(hp->hashfn == NULL)
     hsh = def_sym_hash(str);
   else hsh = (*(hp->hashfn))(str, hp);

   sp = alloc_entry();
   if(sp == NULL) return(NULL);
   sp->count = 0;
   sp->key = _gal_strdup(str);
   sp->next = hp->table[hsh];
   hp->table[hsh] = sp;
   sp->table_next = hp->list;
   hp->list = sp;

   return(sp);
}

/********************************************************************************
 *
 *	START OF PUBLIC FUNCTIONS
 *
 ********************************************************************************/

/********************************************************************************
 * Generic symbol hash tables
 ********************************************************************************/

int Gal_HashHasKey(const char *key, Gal_HashTable ht) {
  if (sym_ht_lookup(key, ht))
    return 1;
  return 0;
}

Gal_HashTable Gal_MakeHash(int size)
{
  HTABLE *hp;

  hp = (HTABLE *)calloc(1,sizeof(HTABLE));
  if (hp)
  {
    init_table(hp, size);
    clear_table(hp);
    return(hp);
  }
  GalUtil_WarnWithLocation(__FUNCTION__, "Failed to allocate hash table of size %d", size);
  return(NULL);
}

/* SAM 10/31/99: This function has to COPY the object that it's
   bringing in, because when the hash table is freed, the
   symbols are pushed onto a free list, and when they're
   removed from the free list, the Gal_Object value is freed. */

Gal_HashTable Gal_CopyHash(Gal_HashTable old_ht)
{
  if (old_ht) {
    HTABLE *new_ht = Gal_MakeHash(old_ht->size);
    SYM *sp = old_ht->list;

    if (new_ht) {
      while (sp) {
	Gal_SetHash(sp->key, Gal_CopyObject(sp->val), new_ht);
	sp = sp->table_next;
      }
    }
    return new_ht;
  } else return (Gal_HashTable) NULL;
}

Gal_Object Gal_GetHash(const char *str, Gal_HashTable hp)
{
  SYM *sp = NULL;

  sp = sym_ht_lookup(str, hp);
  if (sp)
    return(sp->val);
  return(NULL);
}

void Gal_MapHash(Gal_HashTable hp, void (*fn)(char *str, Gal_Object val, Gal_HashTable hp))
{
  int i;

  for (i = 0; i < hp->size; i++) {
    SYM *sp = hp->table[i];
    SYM *sp_next;
    while (sp) {
      sp_next = sp->next;
      if (sp->val)
	(*fn)(sp->key, sp->val, hp);
      sp = sp_next;
    }
  }
}  

Gal_Object Gal_SetHash(const char *str, Gal_Object val, Gal_HashTable hp)
{
  SYM *sp = NULL;

  sp = sym_ht_lookup(str, hp);
  if (sp)
  {
    Gal_FreeObject(sp->val);
    sp->val = val;
    return(val);
  }

  sp = sym_ht_insert(str, hp);
  if (sp)
  {
    sp->val = val;
    return(val);
  }
  return(NULL);
}

void Gal_ClearHash(Gal_HashTable hp)
{
    sym_ht_clear(hp);
}

void Gal_FreeHash(Gal_HashTable hp)
{
  /* push syms onto the free list */
  sym_ht_clear(hp);

  /* free the table */
  free_table(hp);
}

char *Gal_HashListNext(Gal_HashTable hp, Gal_Object *value)
{
  SYM *sp = NULL;

  if (hp)
  {
    sp = hp->list_next;
    if (sp)
    {
      hp->list_next = sp->table_next;
      if (value)
	*value = sp->val;
      return(sp->key);
    }
  }
  if (value) *value = NULL;
  return(NULL);
}

void Gal_HashListRewind(Gal_HashTable hp)
{
  if (hp)
  {
    hp->list_next = hp->list;
  }
}

/********************************************************************************
 * string-to-int tag hashing
 ********************************************************************************/

Gal_HashTable Gal_InitializeTagTable(Gal_TagMap *map, int size)
{
  Gal_HashTable ht = Gal_MakeHash(size);
  int i;

  if (ht)
  {
    for (i=0; map[i].tag_name; i++)
      Gal_SetHash(map[i].tag_name, Gal_IntObject(map[i].tag), ht);
  }
  return(ht);
}

void Gal_AddToTagTable(Gal_TagMap *map, Gal_HashTable ht)
{
  int i;

  if (ht)
  {
    for (i=0; map[i].tag_name; i++)
      Gal_SetHash(map[i].tag_name, Gal_IntObject(map[i].tag), ht);
  }
}

/********************************************************************************
 * int-to-string tag conversion
 ********************************************************************************/

Gal_TagArray Gal_InitializeTagArray(Gal_TagMap *map)
{
  int i, max, min, size;
  Gal_TagArray tag_array = NULL;

  if (!map)
    return NULL;

  max = 0;
  for (i=0; map[i].tag_name; i++)
    if (map[i].tag > max)
      max = map[i].tag;

  min = max;
  for (i=0; map[i].tag_name; i++)
    if (map[i].tag < min)
      min = map[i].tag;

  size = max - min + 1;

  if (!size)
    return NULL;

  tag_array = (Gal_TagArray)calloc(1, sizeof(struct _gal_tag_array));
  tag_array->tags = (char **)calloc(size + 1, sizeof(char *));
  tag_array->offset = min;
  tag_array->size = size;

  for (i=0; map[i].tag_name; i++)
    tag_array->tags[map[i].tag - min] = map[i].tag_name;

  return(tag_array);
}

void Gal_AddToTagArray(Gal_TagMap *map, Gal_TagArray tag_array)
{
  int i, min, max, size;
  int offset_shift;
  char **tags;

  if (!map || !tag_array)
    return;

  min = tag_array->offset;
  for (i=0; map[i].tag_name; i++)
    if (map[i].tag < min)
      min = map[i].tag;

  max = min + tag_array->size - 1;
  for (i=0; map[i].tag_name; i++)
    if (map[i].tag > max)
      max = map[i].tag;

  size = max - min + 1;
  offset_shift = tag_array->offset - min;

  tags = (char **)calloc(size + 1, sizeof(char *));

  for (i=0; i<tag_array->size; i++)
  {
    tags[i + offset_shift] = tag_array->tags[i];
  }
  free(tag_array->tags);

  for (i=0; map[i].tag_name; i++)
  {
    int index = map[i].tag - min;
    if (tags[index])
      GalUtil_WarnWithLocation(__FUNCTION__, "Overlapping tag %d found (%s %s) while adding tag to tag array", map[i].tag, tags[index], map[i].tag_name);
    tags[index] = map[i].tag_name;
  }
  tag_array->tags = tags;
  tag_array->offset = min;
  tag_array->size = size;
}

char *Gal_GetTagArrayTag(int tag, Gal_TagArray tag_array)
{
  if (tag_array)
  {
    int index = tag - tag_array->offset;

    if (index < 0 || index >= tag_array->size)
      return NULL;

    return(tag_array->tags[index]);
  }
  return(NULL);
}

void Gal_FreeTagArray(Gal_TagArray tag_array)
{
  free(tag_array->tags);
  free(tag_array);
}

/********************************************************************************
 * Global symbol table
 ********************************************************************************/

static void init_sym_table(int size)
{
  init_table(&sym_table,size);
  clear_table(&sym_table);
}

/* SAM 2/25/02: I need this to make it easier
   to check for memory leaks. */

void _Gal_DestroySymTable()
{
  if (sym_table.size > 0) {
    Gal_ClearHash(&sym_table);
    free(sym_table.table);
    sym_table.table = (SYM **) NULL;
    sym_table.size = 0;
    _Gal_FreeAllSyms();
  }
}

Sym add_sym(const char *str)
{
   SYM *ep;

   if (str == NULL) return(NULL);
   if (strlen(str) == 0) return(NULL);
   if(sym_table.size <= 0) init_sym_table(DEF_TABLESIZE);
   ep = (SYM *)sym_lookup(&sym_table,str);
   if(ep == NULL){
      ep = (SYM *)sym_insert(&sym_table,str);
   }
   return((Sym)ep);
}

char *sym_name(Sym sp)
{
  if(sp == NULL) return(NULL);
  return(sp->key);
}
