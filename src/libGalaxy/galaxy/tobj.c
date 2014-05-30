/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"
/*#ifndef WIN32
#include <strings.h>
#endif */

#define DEBUG_MEMORY 0

#include "gal_internal.h"

/* Static Function Prototypes */
static int to_type_eq(Gal_Object t1, Gal_Object t2);
static Gal_Object make_tobj(int vtype, void *value, int length,
			    int manage_memory);
static int __Gal_IObjectEqual(Gal_Object obj1, Gal_Object obj2,
			      int ignore_case, int match_frame);


/* Externs for broker proxy objects. */

/* I should just include generic-server.h, but I'm lazy. */

extern
void GalSS_FreeBrokerProxy(GalSS_BrokerProxy *p);
extern
GalSS_BrokerProxy *GalSS_CreateBrokerProxy(char *host, int port,
					   char *call_id,
					   Gal_ObjectType object_type,
					   Gal_Object obj_data);
extern
GalIO_BrokerStruct *GalSS_BrokerProxyBroker(GalSS_BrokerProxy *bp);
extern
Gal_ObjectType GalSS_BrokerProxyObjectType(GalSS_BrokerProxy *bp);
extern
Gal_Object GalSS_BrokerProxyObject(GalSS_BrokerProxy *bp);
extern
GalSS_BrokerProxy *GalSS_CopyBrokerProxy(GalSS_BrokerProxy *bp);

/*
 * Storage allocation similar to frames; vtype == 0 means unallocated
 */

#define CHUNK 2000

static _Gal_LocalMemory *__Gal_TobjMemory = (_Gal_LocalMemory *) NULL;

static GalUtil_LocalMutex tobj_mutex;

void _Gal_init_tobj(void)
{
  GalUtil_InitLocalMutex(&tobj_mutex);
  __Gal_TobjMemory = _Gal_LMCreate(sizeof(TOBJ), CHUNK);
}

/* SAM 10/31/00: If manage_memory is 1, then free it when you
   exit, otherwise, don't. Only used for strings, floats, tokens
   so far. */

static Gal_Object make_tobj(int vtype, void *value,
			    int length, int manage_memory)
{
  Gal_Object new_obj;
  int serial;

  GalUtil_LockLocalMutex(&tobj_mutex);
  if (!__Gal_TobjMemory) {
    GalUtil_UnlockLocalMutex(&tobj_mutex);
    GalUtil_Fatal("Gal_Object memory not initialized; call Gal_InitializeStatics()");
  }

  new_obj = (Gal_Object) _Gal_LMAllocate(__Gal_TobjMemory, &serial);
  GalUtil_UnlockLocalMutex(&tobj_mutex);
  
  if (new_obj == NULL) {
    GalUtil_WarnWithLocation(__FUNCTION__, "Gal_Object allocation error");
    return (Gal_Object) NULL;
  }
  new_obj->vtype = vtype;
  new_obj->value = value;
  new_obj->length = length;
  new_obj->count = 0;
  new_obj->manage_memory = manage_memory;
#if DEBUG_MEMORY
  printf("Allocated object 0x%x:\n", (int) new_obj);
  Gal_PPObject(new_obj);
  fflush(stdout);
#endif
  return(new_obj);
}

static void __maybe_print_obj(void *elt)
{
  Gal_Object obj = (Gal_Object) elt;
  if (obj->vtype != GAL_FREE) {
#if DEBUG_MEMORY
    printf("0x%x\n", (int) obj);
#endif
    Gal_PPObject(obj);
  }
}

int _Gal_FreeAllObjects()
{
  int success = 0;
  
  GalUtil_LockLocalMutex(&tobj_mutex);
  if (__Gal_TobjMemory) {
    int active_elements = _Gal_LMFree(__Gal_TobjMemory, NULL, 1);

    if (active_elements > 0) {
      GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't free object repository; %d left", active_elements);
      _Gal_LMDoElements(__Gal_TobjMemory, __maybe_print_obj);
    } else {
      __Gal_TobjMemory = (_Gal_LocalMemory *) NULL;
      success = 1;
    }
  }
  GalUtil_UnlockLocalMutex(&tobj_mutex);
  return success;
}

/* SAM 9/14/99: The mutex for freeing frames and objects must be VERY
   local, otherwise you run the risk of getting deadlocks between
   threads (where one thread starts with freeing a frame object
   and another starts with freeing a frame) and within threads
   (when you recursively try to free a frame or an object).
   This might be computationally inefficient, but it is the safest
   thing. Thread-specific data is another option, but passing frames
   or objects from thread to thread would become a Pandora's box. */

void Gal_FreeObject(Gal_Object to)
{
  _gal_free_object_internal(to, 1);
}

void _gal_free_object_internal(Gal_Object to, int level)
{
  if(to == NULL) return;

#if DEBUG_MEMORY
  printf("Freeing object 0x%x\n", (int) to);
  fflush(stdout);
#endif

  switch(to->vtype)
  {
  case GAL_FREE:
    return;    
  case GAL_STRING:
  case GAL_TOKEN:
  case GAL_FLOAT:
    if (to->value && to->manage_memory)
      free(to->value);
    break;
  case GAL_FRAME:
    if (to->value && to->manage_memory)
      _gal_free_frame_internal(to->value, level + 1);
    break;
  case GAL_LIST:
    if (to->value && to->manage_memory)
      Gal_FreePointerBuffer((Gal_PointerBuffer *) to->value);
    break;
  case GAL_BINARY:
  case GAL_INT_16:
  case GAL_INT_32:
  case GAL_INT_64:
  case GAL_FLOAT_32:
  case GAL_FLOAT_64:
    if (to->value && to->manage_memory)
      Gal_FreeByteBuffer((Gal_StringBuffer *) to->value);
    break;
  case GAL_PROXY:
    if (to->value && to->manage_memory)
      GalSS_FreeBrokerProxy((GalSS_BrokerProxy *) to->value);
    break;
  default: {}
  }
  /* just to be sure */
  to->vtype = GAL_FREE;
  to->value = NULL;
  _Gal_LMDeallocate(__Gal_TobjMemory, (void *) to);
}

void Gal_FreeWrapper(Gal_Object to)
{
  if(to == NULL) return;

  if(to->vtype == GAL_FREE)
    return;

  if(to->vtype == GAL_FLOAT && to->value)
    free(to->value);
  
  to->vtype = GAL_FREE;
  to->value = NULL;
  _Gal_LMDeallocate(__Gal_TobjMemory, (void *) to);
}

char *Gal_ObjectTypeString (Gal_ObjectType object_type)
{
  switch (object_type)
  {
  /* Gal_Object types */
  case GAL_FREE:
    return("GAL_FREE");
  case GAL_FRAME:
    return("GAL_FRAME");
  case GAL_STRING:
    return("GAL_STRING");
  case GAL_TOKEN:
    return("GAL_TOKEN");
  case GAL_INT:
    return("GAL_INT");
  case GAL_FLOAT:
    return("GAL_FLOAT");
  case GAL_SYMBOL:
    return("GAL_SYMBOL");
  case GAL_KEYWORD:
    return("GAL_KEYWORD");
  case GAL_TAG:
    return("GAL_TAG");
  case GAL_LIST:
    return("GAL_LIST");
  case GAL_PTR:
    return("GAL_PTR");

  /* Gal_Frame types */
  case GAL_TOPIC_FRAME:
    return("GAL_TOPIC_FRAME");
  case GAL_CLAUSE_FRAME:
    return("GAL_CLAUSE_FRAME");
  case GAL_PRED_FRAME:
    return("GAL_PRED_FRAME");

  /* Binary data types */
  case GAL_BINARY:
    return("GAL_BINARY");
  case GAL_INT_16:
    return("GAL_INT_16");
  case GAL_INT_32:
    return("GAL_INT_32");
  case GAL_INT_64:
    return("GAL_INT_64");
  case GAL_FLOAT_32:
    return("GAL_FLOAT_32");
  case GAL_FLOAT_64:
    return("GAL_FLOAT_64");
  default:
    GalUtil_WarnWithLocation(__FUNCTION__, "Can't return string for unknown object type %d", object_type);
    return("UNKNOWN");
  }
}

Gal_ObjectType Gal_GetObjectType(Gal_Object to)
{
  if(to == NULL) return(GAL_FREE);
  return(to->vtype);
}

char *Gal_GetObjectTypeString(Gal_Object to)
{
  return Gal_ObjectTypeString(Gal_GetObjectType(to));
}

Gal_ObjectType Gal_GetDetailedType(Gal_Object to)
{
  Gal_Frame fr;

  if(to == NULL) return(GAL_FREE);

  if (to->vtype == GAL_FRAME)
  {
    fr = (Gal_Frame)to->value;
    switch(Gal_GetFrameType(fr))
    {
    case GAL_CLAUSE:
      return(GAL_CLAUSE_FRAME);
    case GAL_TOPIC:
      return(GAL_TOPIC_FRAME);
    case GAL_PRED:
      return(GAL_PRED_FRAME);
    case GAL_NULLFRAME:
      return(GAL_FREE);
    }
  }
  return(to->vtype);
}

/*
 *  this copies the string
 *  New policy: if value == NULL return NULL - CP 9/4/98
 */

Gal_Object Gal_CreateStringObject(char *cp, int manage_memory)
{
  if(cp == NULL) return(NULL);
      
  return(make_tobj(GAL_STRING,cp,0,manage_memory));
}

Gal_Object Gal_StringObject(const char *value)
{
  if (value == NULL) return(NULL);
  
  return Gal_CreateStringObject(_gal_strdup(value), 1);
}

Gal_Object Gal_CreateTokenObject(char *cp, int manage_memory)
{
  if(cp == NULL) return(NULL);

  return(make_tobj(GAL_TOKEN,cp,0,manage_memory));
}

Gal_Object Gal_TokenObject(const char *value)
{
  if(value == NULL) return(NULL);
    
  return Gal_CreateTokenObject(_gal_strdup(value), 1);
}

Gal_Object Gal_PointerObject(void *value)
{
  return(make_tobj(GAL_PTR,value,0,0));
}

Gal_Object wrap_tsym(Gal_Symbol value, Gal_ObjectType type)
{
  return(make_tobj(type,value,0,0));
}

Gal_Object Gal_SymbolObject(const char *value)
{
  return(make_tobj(GAL_SYMBOL,add_sym(value),0,0));
}

Gal_Object Gal_KeywordObject(const char *value)
{
  return(make_tobj(GAL_KEYWORD,add_sym(value),0,0));
}

Gal_Object Gal_TagObject(const char *value)
{
  return(make_tobj(GAL_TAG,add_sym(value),0,0));
}

Gal_Object Gal_IntObject(int value)
{
  return(make_tobj(GAL_INT,(void *)value,0,0));
}

Gal_Object Gal_CreateFloatObject(float *fptr, int manage_memory)
{
  return(make_tobj(GAL_FLOAT,fptr,0,manage_memory));
}

Gal_Object Gal_FloatObject(float value)
{
  float *fptr;

  fptr = (float *)calloc(1,sizeof(float));
  *fptr = value;
  return Gal_CreateFloatObject(fptr, 1);
}

static Gal_Object wrap_tint(void *value)
{
  return(make_tobj(GAL_INT,value,0,0));
}

/*
 * New policy: if value == NULL return NULL - dg 12/13/94
 */

Gal_Object Gal_CreateFrameObject(Gal_Frame value, int manage_memory)
{
  if(value == NULL) return(NULL);
  return(make_tobj(GAL_FRAME, value, 0, manage_memory));
}

Gal_Object Gal_FrameObject(Gal_Frame value)
{
  if(value == NULL) return(NULL);
  return(make_tobj(GAL_FRAME, value, 0, 1));
}

/* I need these for dynamic lists. */

void *_gal_copy_object(void *obj)
{
  return (void *) Gal_CopyObject((Gal_Object) obj);
}

void _gal_free_object(void *obj)
{
  Gal_FreeObject((Gal_Object) obj);
}

/* List object with memory management API. Always dynamic. */

Gal_Object Gal_CreateListObject(Gal_Object *values, int n,
				void (*free_fn)(void *),
				int manage_memory)
{
  Gal_PointerBuffer *b = Gal_MakePointerBuffer((void **) values,
					       GAL_OBJECT_PTYPE,
					       n, n, manage_memory, 1,
					       free_fn, 10, 0);
  return(make_tobj(GAL_LIST, (void *) b, 1, 1));
}

/* Duplicating previous behavior: no matter whether the
   objects should be freed, they are. */

Gal_Object Gal_ListObject(Gal_Object *objs, int n)
{
  int i;
  Gal_Object *values = (Gal_Object *) NULL;
  Gal_PointerBuffer *b;

  if (n > 0)
    values = (Gal_Object *) calloc(n, sizeof(Gal_Object));
  
  for (i = 0; i < n; i++) {
    values[i] = objs[i];
  }
  b = Gal_MakePointerBuffer((void **) values,
			    GAL_OBJECT_PTYPE,
			    n, n, 1, 0,
			    _gal_free_object, 10, 0);
  return make_tobj(GAL_LIST, (void *) b, 1, 1);
}

Gal_Object Gal_ListObjectFromElements(int n, ...)
{
  va_list args;
  int i;
  Gal_Object *values = (Gal_Object *) NULL;
  Gal_PointerBuffer *b;

  if (n > 0) {    
    values = (Gal_Object *) calloc(n, sizeof(Gal_Object));
    va_start(args, n);
    for (i = 0; i < n; i++) {
      values[i] = va_arg(args, Gal_Object);
    }
    va_end(args);
  }
  b = Gal_MakePointerBuffer((void **) values,
			    GAL_OBJECT_PTYPE,
			    n, n, 1, 0,
			    _gal_free_object, 10, 0);
  return make_tobj(GAL_LIST, (void *) b, 1, 1);
}

/* Binary objects. */

static Gal_Object __Gal_CreateArrayObjectFromByteBuffer(Gal_StringBuffer *b,
							Gal_ObjectType t,
							int manage_memory)
{
  return(make_tobj(t, (void *)b, 1, manage_memory));
}

/* Always dynamic. */

Gal_Object Gal_CreateBinaryObject(void *data, int size, int manage_memory)
{
  Gal_StringBuffer *b = Gal_MakeByteBuffer((char *) data, size, size,
					   manage_memory, 1, 1024, 128);
  return __Gal_CreateArrayObjectFromByteBuffer(b, GAL_BINARY, 1);
}
  
/* this copies the data */

Gal_Object Gal_BinaryObject(void *data, int size)
{
  char *s = (char *) NULL;
  Gal_StringBuffer *b;

  if (size > 0) {
    s = (char *) malloc(size);
    memcpy((void *) s, data, size);
  }
  b = Gal_MakeByteBuffer(s, size, size,
			 1, 0, 0, 0);
  return __Gal_CreateArrayObjectFromByteBuffer(b, GAL_BINARY, 1);
}

/* And make sure it can be expanded. This can expand any of the
   array types. */

int Gal_ArrayObjectAdd(Gal_Object obj, void *data, int size)
{
  if (!obj) return 0;
  
  switch(obj->vtype) {    
  case GAL_BINARY:
  case GAL_INT_16:
  case GAL_INT_32:
  case GAL_INT_64:
  case GAL_FLOAT_32:
  case GAL_FLOAT_64:   
    return Gal_ByteBufferAppend((Gal_StringBuffer *) obj->value,
				(char *) data, size);
  default:
    return 0;
  }
}

int Gal_ArrayObjectExpandable(Gal_Object obj)
{
  if (!obj) return 0;
  
  switch(obj->vtype) {    
  case GAL_BINARY:
  case GAL_INT_16:
  case GAL_INT_32:
  case GAL_INT_64:
  case GAL_FLOAT_32:
  case GAL_FLOAT_64:
    return Gal_ByteBufferDynamic((Gal_StringBuffer *) obj->value);
  default:
    return 0;
  }
}

/* And now all the other binary types, all of which
   will be expandable. */

Gal_Object Gal_CreateInt16Object(void *data, int num_int_16,
				 int manage_memory)
{
  Gal_StringBuffer *b = Gal_MakeDataBuffer((char *) data,
					   num_int_16, num_int_16,
					   manage_memory, 1, 1024, 128, 2);
  return __Gal_CreateArrayObjectFromByteBuffer(b, GAL_INT_16, 1);
}

Gal_Object Gal_CreateInt32Object(void *data, int num_int_32,
				 int manage_memory)
{
  Gal_StringBuffer *b = Gal_MakeDataBuffer((char *) data,
					   num_int_32, num_int_32,
					   manage_memory, 1, 1024, 128, 4);
  return __Gal_CreateArrayObjectFromByteBuffer(b, GAL_INT_32, 1);
}

Gal_Object Gal_CreateInt64Object(void *data, int num_int_64,
				 int manage_memory)
{
  Gal_StringBuffer *b = Gal_MakeDataBuffer((char *) data,
					   num_int_64, num_int_64,
					   manage_memory, 1, 1024, 128, 8);
  return __Gal_CreateArrayObjectFromByteBuffer(b, GAL_INT_64, 1);
}

Gal_Object Gal_CreateFloat32Object(void *data, int num_float_32,
				   int manage_memory)
{
  Gal_StringBuffer *b = Gal_MakeDataBuffer((char *) data,
					   num_float_32, num_float_32,
					   manage_memory, 1, 1024, 128, 4);
  return __Gal_CreateArrayObjectFromByteBuffer(b, GAL_FLOAT_32, 1);
}

Gal_Object Gal_CreateFloat64Object(void *data, int num_float_64,
				   int manage_memory)
{
  Gal_StringBuffer *b = Gal_MakeDataBuffer((char *) data,
					   num_float_64, num_float_64,
					   manage_memory, 1, 1024, 128, 8);
  return __Gal_CreateArrayObjectFromByteBuffer(b, GAL_FLOAT_64, 1);
}

/* Copy always copies the string, and manages the memory. */

Gal_Object Gal_CopyObject(Gal_Object to)
{
  float *fptr;
  Gal_PointerBuffer *new_b;
  void *data, *old_data;
  int size;
  GalSS_BrokerProxy *bp;
  
  if(to == NULL) return(NULL);
  switch (to->vtype) {
  case GAL_STRING:
    return(Gal_StringObject(to->value));
  case GAL_TOKEN:
    return(Gal_TokenObject(to->value));
  case GAL_PTR:
    return(Gal_PointerObject(to->value));
  case GAL_FRAME:
    return(Gal_FrameObject(Gal_CopyFrame(to->value)));
  case GAL_INT:
    return(wrap_tint(to->value));
  case GAL_FLOAT:
    fptr = (float *)(to->value);
    return(Gal_FloatObject(*fptr));
  case GAL_SYMBOL:
  case GAL_KEYWORD:
  case GAL_TAG:
    return(wrap_tsym(to->value, to->vtype));
  case GAL_LIST:
    new_b = Gal_PointerBufferCopy((Gal_PointerBuffer *) to->value,
				  _gal_copy_object, _gal_free_object);
    return make_tobj(GAL_LIST, (void *) new_b, 1, 1);
  case GAL_BINARY:
    /* SAM 2/19/02: Make sure that if the buffer is expandable,
       you create an expandable buffer, and similarly for it
       not being expandable. */
    if (Gal_ByteBufferDynamic((Gal_StringBuffer *) to->value)) {
      old_data = Gal_BinaryValue(to, &size);
      if (size > 0) {
	data = (void *) malloc(size);
	memcpy(data, old_data, size);
      } else {
	data = (void *) NULL;
      }
      return Gal_CreateBinaryObject(data, size, 1);
    } else {
      return(Gal_BinaryObject((void *) Gal_ByteBufferBytes((Gal_StringBuffer *) to->value), Gal_ByteBufferSize((Gal_StringBuffer *) to->value)));
    }
  case GAL_INT_16:
    old_data = Gal_Int16Value(to, &size);
    if (size > 0) {
      data = (void *) malloc(size * 2);
      memcpy(data, old_data, size * 2);
    } else {
      data = (void *) NULL;
    }
    return Gal_CreateInt16Object(data, size, 1);
  case GAL_INT_32:
    old_data = Gal_Int32Value(to, &size);
    if (size > 0) {
      data = (void *) malloc(size * 4);
      memcpy(data, old_data, size * 4);
    } else {
      data = (void *) NULL;
    }
    return Gal_CreateInt32Object(data, size, 1);
  case GAL_INT_64:
    old_data = Gal_Int64Value(to, &size);
    if (size > 0) {
      data = (void *) malloc(size * 8);
      memcpy(data, old_data, size * 8);
    } else {
      data = (void *) NULL;
    }
    return Gal_CreateInt64Object(data, size, 1);
  case GAL_FLOAT_32:
    old_data = Gal_Float32Value(to, &size);
    if (size > 0) {
      data = (void *) malloc(size * 4);
      memcpy(data, old_data, size * 4);
    } else {
      data = (void *) NULL;
    }
    return Gal_CreateFloat32Object(data, size, 1);
  case GAL_FLOAT_64:
    old_data = Gal_Float64Value(to, &size);
    if (size > 0) {
      data = (void *) malloc(size * 8);
      memcpy(data, old_data, size * 8);
    } else {
      data = (void *) NULL;
    }
    return Gal_CreateFloat64Object(data, size, 1);
  case GAL_PROXY:
    bp = (GalSS_BrokerProxy *) to->value;
    /* We need to copy the object, because the original
       proxy object probably owns the reference. Just like the list case. */
    return Gal_ProxyObject(GalSS_CopyBrokerProxy(bp));
  default:
    return(NULL);
  }
}

void *_gal_to_value(Gal_Object to)
{
  if(to == NULL) return(NULL);
  return(to->value);
}

void *_gal_object_value_warn(Gal_Object to, Gal_ObjectType type, const char *caller, const char *key)
{
  if(to == NULL) return(NULL);
  if(to->vtype == type)
  {
    /* if the object is a list or binary data, type check
       and return the object */
    switch (to->vtype) {
    case GAL_LIST:
    case GAL_BINARY:
    case GAL_INT_16:
    case GAL_INT_32:
    case GAL_INT_64:
    case GAL_FLOAT_32:
    case GAL_FLOAT_64:
      return((void *)to);
    default:
      return((void *)to->value);
    }
  }
  if(to->vtype == GAL_FRAME)
  {
    if (type == GAL_CLAUSE_FRAME && Gal_ClauseFramep((Gal_Frame)to->value))
      return((void *)to->value);
    else if (type == GAL_TOPIC_FRAME && Gal_TopicFramep((Gal_Frame)to->value))
      return((void *)to->value);
    else if (type == GAL_PRED_FRAME && Gal_PredFramep((Gal_Frame)to->value))
      return((void *)to->value);
  }
  if (caller)
  {
    if (key)
      GalUtil_WarnWithLocation(caller, "Object (key %s) type %s does not match specified type %s", key, Gal_ObjectTypeString(to->vtype), Gal_ObjectTypeString(type));
    else
      GalUtil_WarnWithLocation(caller, "Object type %s does not match specified type %s", Gal_ObjectTypeString(to->vtype), Gal_ObjectTypeString(type));
  }

  return(NULL);
}

Gal_Frame Gal_FrameValue(Gal_Object to)
{
  return((Gal_Frame) _gal_object_value_warn(to, GAL_FRAME, __FUNCTION__, NULL));
}

Gal_Frame Gal_ClauseValue(Gal_Object to)
{
  return((Gal_Frame) _gal_object_value_warn(to, GAL_CLAUSE_FRAME, __FUNCTION__, NULL));
}

Gal_Frame Gal_TopicValue(Gal_Object to)
{
  return((Gal_Frame) _gal_object_value_warn(to, GAL_TOPIC_FRAME, __FUNCTION__, NULL));
}

Gal_Frame Gal_PredValue(Gal_Object to)
{
  return((Gal_Frame) _gal_object_value_warn(to, GAL_PRED_FRAME, __FUNCTION__, NULL));
}

char *Gal_KeywordValue(Gal_Object to)
{
  Gal_Symbol key = NULL;

  if (to == NULL)
    return NULL;

  if (Gal_Symbolp(to))
    key = (Gal_Symbol) _gal_object_value_warn(to, GAL_SYMBOL, __FUNCTION__, NULL);
  else if (Gal_Keywordp(to))
    key = (Gal_Symbol) _gal_object_value_warn(to, GAL_KEYWORD, __FUNCTION__, NULL);
  else if (Gal_Tagp(to))
    key = (Gal_Symbol) _gal_object_value_warn(to, GAL_TAG, __FUNCTION__, NULL);
  else
      GalUtil_WarnWithLocation(__FUNCTION__, "Can't get value of object (type %s) because it is not a symbol, keyword, or tag",
		   Gal_ObjectTypeString(to->vtype));

  return(sym_name(key));
}

char *Gal_StringValue(Gal_Object to)
{
  Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;
  if (to == NULL)
    return NULL;

  if (Gal_Stringp(to))
    return (char *) _gal_object_value_warn(to, GAL_STRING, __FUNCTION__, NULL);
  if (Gal_Tokenp(to))
    return (char *) _gal_object_value_warn(to, GAL_TOKEN, __FUNCTION__, NULL);
  else
      GalUtil_WarnWithLocation(__FUNCTION__, "Can't get value of object %s (type %s) because it is not a string or token",
		   Gal_ObjectString(to, &buf), Gal_ObjectTypeString(to->vtype));

  if (buf) Gal_FreeStringBuffer(buf);
  return(NULL);
}

void *Gal_PointerValue(Gal_Object to)
{
  return((char *) _gal_object_value_warn(to, GAL_PTR, __FUNCTION__, NULL));
}

int Gal_IntValue(Gal_Object to)
{
  return((int) _gal_object_value_warn(to, GAL_INT, __FUNCTION__, NULL));
}

float Gal_FloatValue(Gal_Object to)
{
  float *fptr;

  fptr = (float *) _gal_object_value_warn(to, GAL_FLOAT, __FUNCTION__, NULL);
  if (fptr)
    return(*fptr);
  return(0);
}

Gal_Object *Gal_ListValue(Gal_Object obj, int *length)
{
  Gal_Object list_obj;

  list_obj = (Gal_Object) _gal_object_value_warn(obj, GAL_LIST, __FUNCTION__, NULL);
  if (list_obj)
  {
    if (length)
      *length = Gal_PointerBufferSize((Gal_PointerBuffer *) list_obj->value);
    return((Gal_Object *) Gal_PointerBufferPointers((Gal_PointerBuffer *) list_obj->value));
  } else {
    if (length)
      *length = 0;
    return(NULL);
  }
}

int Gal_ListLength(Gal_Object to)
{
  if(to == NULL) return(0);
  if(to->vtype != GAL_LIST)
    return(0);
  return(Gal_PointerBufferSize((Gal_PointerBuffer *) to->value));
}

int Gal_ListObjectAdd(Gal_Object obj, Gal_Object elt)
{
  if(obj == NULL) return(0);
  if(obj->vtype != GAL_LIST)
    return(0);
  return Gal_PointerBufferAdd((Gal_PointerBuffer *) obj->value, (void *) elt);
}

int Gal_ListObjectExpandable(Gal_Object obj)
{
  if(obj == NULL) return(0);
  if(obj->vtype != GAL_LIST)
    return(0);
  return Gal_PointerBufferDynamic((Gal_PointerBuffer *) obj->value);
}  

Gal_Object Gal_GetListObject(Gal_Object obj, int n)
{
  if(obj == NULL) return(NULL);
  if(obj->vtype != GAL_LIST)
    return(obj);
  return (Gal_Object) Gal_PointerBufferNthElement((Gal_PointerBuffer *) obj->value, n);
}

int Gal_SetListObject(Gal_Object obj, int n, Gal_Object elt)
{
  if(obj == NULL) return(0);
  if(obj->vtype != GAL_LIST)
    return(0);
  return Gal_PointerBufferSetNthElement((Gal_PointerBuffer *) obj->value, n, (void *) elt);
}

void *Gal_GetListValue(Gal_Object obj, int n, Gal_ObjectType type)
{
  Gal_Object list_obj;

  list_obj = Gal_GetListObject(obj, n);
  return(_gal_object_value_warn(list_obj, type, __FUNCTION__, NULL));
}

/* SAM 11/03/00: There was never any code to check list equality.
   Sheesh. This isn't going to be efficient, but I can't help that. */

static int __Gal_ListEqual(Gal_Object obj1, Gal_Object obj2,
			   int ignore_case, int match_frame)
{
  Gal_PointerBuffer *p1, *p2;
  Gal_Object *objs;
  int size1, size2, i, j, found;
  Gal_Object test;
  
  if ((obj1->vtype != GAL_LIST) || (obj2->vtype != GAL_LIST))
    return 0;
  p1 = (Gal_PointerBuffer *) obj1->value;
  p2 = (Gal_PointerBuffer *) obj2->value;
  /* Lists must be of the same length, except for matching
     where the obj1 must have at least as many elements
     as obj2. */
  if ((!match_frame) && (Gal_PointerBufferSize(p1) != Gal_PointerBufferSize(p2)))
    return 0;
  else if (match_frame && (Gal_PointerBufferSize(p1) < Gal_PointerBufferSize(p2)))
    return 0;
  /* How can we do this without allocating huge chunks of memory?
     Not going to worry about it at the moment. We'll allocate
     an array the length of the things to be compared, and then
     load the array with the contents of one of the elements.
     We'll remove the elements as they match. If any element
     doesn't have a match, fail. */
  /* Except in the match_frame case, where the idea is that
     every object in obj2 must match something in obj1, but
     not vice versa. So we should start with obj2. */
  size1 = Gal_PointerBufferSize(p1);
  size2 = Gal_PointerBufferSize(p2);
  if (size2 > 0)
    objs = (Gal_Object *) calloc(size2, sizeof(Gal_Object));
  else
    objs = (Gal_Object *) NULL;
  for (i = 0; i < size1; i++) {
    objs[i] = (Gal_Object) Gal_PointerBufferNthElement(p1, i);
  }
  for (i = 0; i < size2; i++) {
    test = (Gal_Object) Gal_PointerBufferNthElement(p2, i);
    found = 0;
    for (j = 0; j < size1; j++) {
      /* If we find an equal element, remove it from the
	 list of things to check. */
      /* Because of match_frame, the order of the
	 arguments to __Gal_IObjectEqual is important. */
      if (objs[j] && __Gal_IObjectEqual(objs[j], test, ignore_case, match_frame)) {
	objs[j] = (Gal_Object) NULL;
	found = 1;
	break;
      }
    }
    /* If we didn't find an equal element, fail. */
    if (!found) {
      if (objs)
	free(objs);
      return 0;
    }
  }
  /* We don't need to check the list again, because if
     everything found a match, the list will be empty, in
     the non-match_frame case. In the match_frame case, everyone
     in obj2 matched, and we don't care about the elements in obj1
     that didn't. */
  if (objs)
    free(objs);
  return 1;
}

void *_Gal_ArrayValue(Gal_Object array_obj, int *size)
{
  if (array_obj) {
    if (size) {
      *size = Gal_DataBufferSize((Gal_StringBuffer *) array_obj->value);
    }
    return((void *) Gal_DataBufferData((Gal_StringBuffer *) array_obj->value));
  }
  return(NULL);
}

static void *__Gal_ArrayValueWarn(Gal_Object obj, int *size, Gal_ObjectType t)
{
  Gal_Object array_obj;

  array_obj = (Gal_Object) _gal_object_value_warn(obj, t, __FUNCTION__, NULL);
  return _Gal_ArrayValue(array_obj, size);
}

int Gal_ObjectByteCount(Gal_Object obj)
{
  switch (obj->vtype) {
  case GAL_STRING:
    return strlen((char *) obj->value);
  case GAL_BINARY:
  case GAL_INT_16:
  case GAL_INT_32:
  case GAL_INT_64:
  case GAL_FLOAT_32:
  case GAL_FLOAT_64:
    return Gal_DataBufferByteCount((Gal_StringBuffer *) obj->value);
  default:
    return 0;
  }
}

static int __Gal_ArraySize(Gal_Object to, Gal_ObjectType t)
{
  if(to == NULL) return(0);
  if(to->vtype != t)
    return(0);
  return(Gal_DataBufferSize((Gal_StringBuffer *) to->value));
}

void *Gal_BinaryValue(Gal_Object obj, int *size)
{
  return __Gal_ArrayValueWarn(obj, size, GAL_BINARY);
}

int Gal_BinarySize(Gal_Object to)
{
  return __Gal_ArraySize(to, GAL_BINARY);
}

void *Gal_Int16Value(Gal_Object obj, int *size)
{
  return __Gal_ArrayValueWarn(obj, size, GAL_INT_16);
}

int Gal_Int16Size(Gal_Object to)
{
  return __Gal_ArraySize(to, GAL_INT_16);
}

void *Gal_Int32Value(Gal_Object obj, int *size)
{
  return __Gal_ArrayValueWarn(obj, size, GAL_INT_32);
}

int Gal_Int32Size(Gal_Object to)
{
  return __Gal_ArraySize(to, GAL_INT_32);
}

void *Gal_Int64Value(Gal_Object obj, int *size)
{
  return __Gal_ArrayValueWarn(obj, size, GAL_INT_64);
}

int Gal_Int64Size(Gal_Object to)
{
  return __Gal_ArraySize(to, GAL_INT_64);
}

void *Gal_Float32Value(Gal_Object obj, int *size)
{
  return __Gal_ArrayValueWarn(obj, size, GAL_FLOAT_32);
}

int Gal_Float32Size(Gal_Object to)
{
  return __Gal_ArraySize(to, GAL_FLOAT_32);
}

void *Gal_Float64Value(Gal_Object obj, int *size)
{
  return __Gal_ArrayValueWarn(obj, size, GAL_FLOAT_64);
}

int Gal_Float64Size(Gal_Object to)
{
  return __Gal_ArraySize(to, GAL_FLOAT_64);
}

static int to_type_eq(Gal_Object t1, Gal_Object t2)
{
  if(t1 == NULL) return(0);
  if(t2 == NULL) return(0);
  return(t1->vtype == t2->vtype);
}

int Gal_Stringp(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_STRING);
}

int Gal_Tokenp(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_TOKEN);
}

int Gal_Pointerp(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_PTR);
}

int Gal_Symbolp(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_SYMBOL);
}

int Gal_Keywordp(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_KEYWORD);
}

int Gal_Tagp(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_TAG);
}

int Gal_Intp(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_INT);
}

int Gal_Floatp(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_FLOAT);
}

int Gal_Framep(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_FRAME);
}

int Gal_Topicp(Gal_Object to)
{
  if(to == NULL) 
    return(0);
  return((to->vtype == GAL_FRAME) && (Gal_TopicFramep(to->value)));
}

int Gal_Clausep(Gal_Object to)
{
  if(to == NULL)
    return(0);
  return((to->vtype == GAL_FRAME) && (Gal_ClauseFramep(to->value)));
}

int Gal_Predp(Gal_Object to)
{
  if(to == NULL) return(0);
  return((to->vtype == GAL_FRAME) && (Gal_PredFramep(to->value)));
}

int Gal_Listp(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_LIST);
}

int Gal_Binaryp(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_BINARY);
}

int Gal_Int16p(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_INT_16);
}

int Gal_Int32p(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_INT_32);
}

int Gal_Int64p(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_INT_64);
}

int Gal_Float32p(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_FLOAT_32);
}

int Gal_Float64p(Gal_Object to)
{
  if(to == NULL) return(0);
  return(to->vtype == GAL_FLOAT_64);
}

/* ignore_case is only true at the top level, and should
   recurse into the list matching. match_frame is true
   all the way down, and is argument order dependent. We'll recurse
   through the list matching too. */

static int __Gal_IObjectEqual(Gal_Object obj1, Gal_Object obj2,
			      int ignore_case, int match_frame)
{
  int size1, size2, factor;
  void *data1, *data2;  
  GalSS_BrokerProxy *bp1, *bp2;
    
  if (obj1 == obj2)
    return 1;
    
  if(!to_type_eq(obj1, obj2)) return(0);
  switch (obj1->vtype) {
  case GAL_STRING:
  case GAL_TOKEN:
    if((_gal_to_value(obj1) == NULL) && (_gal_to_value(obj2) == NULL)) return(1);
    if((_gal_to_value(obj1) == NULL) || (_gal_to_value(obj2) == NULL)) return(0);
    if((!ignore_case) &&
       strcmp(_gal_to_value(obj1), _gal_to_value(obj2)) == 0)
      return(1);
    else if(ignore_case &&
	    _gal_strcasecmp(_gal_to_value(obj1),_gal_to_value(obj2)) == 0)
      return(1);
    else return(0);
  case GAL_INT:
    if(_gal_to_value(obj1) == _gal_to_value(obj2)) return(1);
    else return(0);
  case GAL_FLOAT:
    if (Gal_FloatValue(obj1) == Gal_FloatValue(obj2)) return(1);
    else return(0);
  case GAL_SYMBOL:
  case GAL_KEYWORD:
  case GAL_TAG:
    if(_gal_to_value(obj1) == _gal_to_value(obj2)) return(1);
    else return(0);
  case GAL_LIST:
    return __Gal_ListEqual(obj1, obj2, ignore_case, match_frame);
  case GAL_FRAME:
    if (match_frame)
      return Gal_MatchFrame(_gal_to_value(obj1),_gal_to_value(obj2));
    else
      return Gal_FrameEqual(_gal_to_value(obj1),_gal_to_value(obj2));
  case GAL_BINARY:
  case GAL_INT_16:
  case GAL_INT_32:
  case GAL_INT_64:
  case GAL_FLOAT_32:
  case GAL_FLOAT_64:
    data1 = __Gal_ArrayValueWarn(obj1, &size1, obj1->vtype);
    data2 = __Gal_ArrayValueWarn(obj2, &size2, obj1->vtype);
    /* We need to multiply the size by the chunking. */
    factor = ((Gal_StringBuffer *) obj1->value)->chunk_size;
    if ((size1 == size2) && (memcmp(data1, data2, size1 * factor) == 0))
      return(1);
    return(0);
  case GAL_PROXY:
    bp1 = (GalSS_BrokerProxy *) obj1->value;
    bp2 = (GalSS_BrokerProxy *) obj2->value;
    if ((strcmp(bp1->host, bp2->host) == 0) &&
	(bp1->port == bp2->port) &&
	(strcmp(bp1->call_id, bp2->call_id = 0)) &&
	(bp1->object_type == bp2->object_type) &&
	__Gal_IObjectEqual(bp1->obj, bp2->obj, ignore_case, match_frame)) {
      return 1;
    } else {
      return 0;
    }
  default:
    return 0;
  }
}

/*
 * true iff to is same type and value as sem. Lists now work.
 *
 */

int Gal_ObjectEqual(Gal_Object obj1, Gal_Object obj2)
{
  return __Gal_IObjectEqual(obj1, obj2, 0, 0);
}

/*
 *  calls _gal_strcasecmp instead of strcmp, doesn't recurse.
 */

int Gal_ObjectCaseEqual(Gal_Object to, Gal_Object sem)
{
  return __Gal_IObjectEqual(to, sem, 1, 0);
}

/*
 *  calls _gal_strcasecmp instead of strcmp
 *  calls MatchFrame instead of FrameEqual
 *  recurses!
 */

int _gal_match_tobj(Gal_Object to, Gal_Object sem)
{
  return __Gal_IObjectEqual(to, sem, 1, 1);
}

/*
 * Proxy object support. See Gal_FreeObject, Gal_CopyObject,
 * Gal_PrObject. See ServerStub/broker_proxy.c for
 *  proxification/unproxification.
 */

/* Returns the inbound or outbound broker associated with a GAL_PROXY
   object, or NULL. */

GalIO_BrokerStruct *Gal_ProxyObjectBroker(Gal_Object obj)
{
  if (obj == NULL)
    return (GalIO_BrokerStruct *) NULL;
  if (obj->vtype != GAL_PROXY)
    return (GalIO_BrokerStruct *) NULL;
  if (!obj->value)
    return (GalIO_BrokerStruct *) NULL;
  return GalSS_BrokerProxyBroker((GalSS_BrokerProxy *) obj->value);
}

/* If obj is GAL_PROXY object, returns the object type for the broker
   stream. Otherwise, returns the type for the obj. */

Gal_ObjectType Gal_ProxyObjectType(Gal_Object obj)
{
  if (obj == NULL)
    return Gal_GetObjectType(obj);
  if (obj->vtype != GAL_PROXY)
    return Gal_GetObjectType(obj);
  if (!obj->value)    
    return Gal_GetObjectType(obj);
  return GalSS_BrokerProxyObjectType((GalSS_BrokerProxy *) obj->value);
}

/* Returns the object associated with a GAL_PROXY object. It does not
   transfer ownership of the object. */

Gal_Object Gal_ProxyObjectObject(Gal_Object obj)
{
  if (obj == NULL)
    return obj;
  if (obj->vtype != GAL_PROXY)
    return obj;
  if (!obj->value)
    return obj;
  return GalSS_BrokerProxyObject((GalSS_BrokerProxy *) obj->value);
}

/* Returns 1 if obj is a GAL_PROXY, 0 otherwise. */

int Gal_Proxyp(Gal_Object obj)
{
  if (obj == NULL)
    return 0;
  return (obj->vtype == GAL_PROXY);
}

Gal_Object Gal_ProxyObject(GalSS_BrokerProxy *p)
{
  return Gal_CreateProxyObject(p, 1);
}

GalSS_BrokerProxy *Gal_ProxyValue(Gal_Object to)
{
  return((GalSS_BrokerProxy *) _gal_object_value_warn(to, GAL_PROXY, __FUNCTION__, NULL));
}

Gal_Object Gal_CreateProxyObject(GalSS_BrokerProxy *p, int manage_memory)
{
  return make_tobj(GAL_PROXY, (void *) p, 1, manage_memory);
}

/* 
 *  for Emacs...
 *  Local Variables:
 *  mode: c
 *  fill-column: 110
 *  comment-column: 80
 *  c-indent-level: 2
 *  c-continued-statement-offset: 2
 *  c-brace-offset: -2
 *  c-argdecl-indent: 2
 *  c-label-offset: -2
 *  End:
 */
