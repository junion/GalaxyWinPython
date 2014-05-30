/*
  This file (c) Copyright 1998 - 2000 M.I.T.
            (c) Copyright 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* This is the local types file for the galaxy/ subdirectory. */

#ifndef _GAL_TYPES_INTERNAL_H
#define _GAL_TYPES_INTERNAL_H

typedef struct _gal_obj
{
  /* see Gal_ObjectType in include/galaxy/galaxy.h */
  short vtype;
  void *value;
  int length;
  int count;
  int manage_memory;
} TOBJ, *Gal_Object;

typedef struct avpair
{
  void *att;
  void *val;
} AV, *Av;

typedef struct vlist
{
  void **data;
  int cursize;
  int maxsize;
  int flags;
} VLIST, *Vlist;

typedef Vlist Plist;

typedef struct _gal_sym
{
  char  type;
  char  flags;
  char *key;
  int   count;
  Gal_Object  val;
  unsigned int index;
  struct _gal_sym *next;
  struct _gal_sym *table_next;
  Vlist rules;
  Plist plist;
} SYM, *Gal_Symbol;

typedef struct _gal_frame
{
  int   ftype;		/* qset, clause, etc */
  Gal_Symbol name;      /* Flight, Fare, etc. */
  Plist plist;
  int   npred;
  int   maxpred;
  Gal_Object *pred;     /* for now keep type info */
  Vlist values;         /* list of objects; for now we keep values on frames */
  int   flags;          /* used during evaluation */

  int	serial;		/* used for debugging */
} NFRAME, *Gal_Frame;

typedef struct _gal_ht
{
   int size;
   int xfactor;
   int mfactor;
   int (*hashfn)(const char *str, struct _gal_ht *hp);
   SYM **table;
   SYM *list;
   SYM *list_next;
} HTABLE, *Gal_HashTable;

#endif
