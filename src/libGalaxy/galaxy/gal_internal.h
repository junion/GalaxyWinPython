/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _GAL_INTERNAL_H
#define _GAL_INTERNAL_H

/* prevent any code which includes this file from
 * getting the "virtual" typedefs declared in common_decls.h 
 */

#define _GAL_LOCAL_HEADERS_

#include "galaxy/common_decls.h"
#include "gal_types_internal.h"
#include "../io/galio_types_internal.h"
#include "galaxy/generic-server-types.h"
/* for backwards compatibility */
typedef Gal_Frame Nframe;
typedef Gal_Object TObj;
typedef Gal_Symbol Sym;

#include "galaxy/galaxy.h"

typedef GAL_SERVER_FUNCTION_MAP SERVER_FUNCTION_MAP;

/* Function protos for file error_tags.c */
void _Gal_init_error_tags(void);

/* Function protos for file hub_server.c */
void _Gal_init_hub_server(void);

/* Function protos for file broker_data.c */
void _Gal_init_broker(void);

/* Function protos for file ip_util.c */
void _Gal_init_ip_util(void);

/* Function protos for file nfio.c */
Gal_Frame  _gal_read_frame(void *streamp, int (*next_char_fn)(void *));
Gal_Object _gal_read_object(void *streamp, int (*next_char_fn)(void *));
void _Gal_init_nfio(void);

/* Function protos for file nframe.c */
void       _Gal_init_nframe(void);
Sym        _gal_fr_sym_name(Nframe fr);
void       _gal_fr_delpred(Gal_Frame fr, Gal_Symbol key);
Gal_Object _gal_fr_getprop(Gal_Frame fr, Gal_Symbol key);
void       _gal_free_frame_internal(Gal_Frame fr, int level);

/* Function protos for file plist.c */
Plist  _gal_alloc_plist(void);
void   _gal_free_avpair(Av ap);
void  *_gal_av_att(Av ap);
void  *_gal_av_val(Av ap);
void  *_gal_getprop(Plist plist, void *key);
void  *_gal_remprop(Plist plist, void *key);
void  *_gal_setprop(Plist plist, void *key, void *value);
void **_gal_get_atts(Plist plist, int *num_atts);
int _gal_get_num_atts(Plist plist, int count_null_values);

/* Function protos for file program_tags.c */
void _Gal_init_program_tags(void);

/* Function protos for file signal.c */
void _Gal_init_signal(void);

/* Function protos for file stream_util.c */
void _Gal_init_stream_util(void);

/* Function protos for file sym.c */
void _Gal_init_sym(void);

/* Function protos for file vlist.c */
int    _gal_add_vdata(Vlist vp, void *sm);
int    _gal_size_vdata(Vlist vp);
Vlist  _gal_alloc_vlist(void);
void   _gal_free_vlist(Vlist vp);
void   _gal_vl_set_size(Vlist vp, int newsize);
void  *_gal_pop_vdata(Vlist vp);
void  *_gal_push_vdata(Vlist vp, void *cp);
void **_gal_get_vdata(Vlist vp, int *np);

/* No function protos for file sockqueue.c */

/* NO Function protos for file string_util.c */

/* Function protos for file sym.c */
Gal_Symbol add_sym(const char *str);
char *sym_name(Gal_Symbol ep);

/* Function protos for file timed_tasks.c */
void _Gal_init_timed_tasks(void);

/* Function protos for file tobj.c */
void       _Gal_init_tobj(void);
int        _gal_match_tobj(TObj to, TObj sem);
void      *_gal_to_value(Gal_Object to);
Gal_Object wrap_tsym(Gal_Symbol value, Gal_ObjectType type);
void       _gal_free_object_internal(Gal_Object to, int level);
void      *_gal_object_value_warn(Gal_Object to, Gal_ObjectType type, const char *caller, const char *key);

/* Function protos for file uucode.c */
char *_gal_uuencode(char *source, int len);
char *_gal_uudecode(char *source, int len);

/* Function protos for file vlist.c */
void _Gal_init_vlist(void);

/* Function protos for dynamic_buffer.c */

int _Gal_ExpandByteBuffer(Gal_StringBuffer *b, int increment);
int _Gal_ExpandDataBuffer(Gal_StringBuffer *b, int increment);

/* Local memory structure. */

typedef struct __Gal_LocalMemory {
  /* Mutex for memory repository. */
  GalUtil_LocalMutex mem_mutex;
  /* Serial number of frame. Reassigned with each
     call to allocate(). */
  int serial_no;
  /* Number of active elements. */
  int active_elements;
  /* Number of calls to allocate(). */
  int alloc_calls;
  /* List of free elements. */
  Vlist free_elements;
  /* Array of all memory chunks allocated. */
  void **memory_chunks;
  /* Length of memory_chunks array */
  int num_memory_chunks;
  /* Number of elements to add with each call
     to the expander. */
  int elt_increment;
  /* Size of each element. */
  int sizeof_elt;
} _Gal_LocalMemory;

void
_Gal_LMInitialize(_Gal_LocalMemory *new_mem, int sizeof_elt,
		  int elt_increment);
_Gal_LocalMemory *
_Gal_LMCreate(int sizeof_elt, int elt_increment);
void *_Gal_LMAllocate(_Gal_LocalMemory *mem, int *serial_ptr);
void _Gal_LMDeallocate(_Gal_LocalMemory *mem, void *elt);
int _Gal_LMFree(_Gal_LocalMemory *mem, void (*free_fn)(void *elt), int free_mem);
void _Gal_LMDoElements(_Gal_LocalMemory *mem, void (*elt_fn)(void *));

/* Function protos for read_program.c */

int _Gal_CreateFullPath(char *path, int path_size, int num_components, ...);
int _Gal_SplitPath(char *out_path, int path_size, const char *in_path);

#endif
