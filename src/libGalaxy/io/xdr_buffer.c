/*
  This file (c) Copyright 2000 The MITRE Corporation.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <limits.h>
#include <stdlib.h>
#define __GALIO_XDR_BUFFER_C__
#include "io_internal.h"
#undef __GALIO_XDR_BUFFER_C__

#ifndef HAVE_XDR_SIZEOF
/* We needed to use our own version of xdr_sizeof. */
unsigned long	xdr_sizeof(xdrproc_t, void *);
#endif

/* These are defined in gal_internal.h, but I can't load it
   because of header conflicts. */

extern char *sym_name(Gal_Symbol ep);
extern
Gal_Object wrap_tsym(Gal_Symbol value, Gal_ObjectType type);
extern Gal_Symbol add_sym(char *str);

/* And this is in generic-server.h. */

extern
GalSS_BrokerProxy *GalSS_CreateBrokerProxy(char *host, int port,
					   char *call_id,
					   Gal_ObjectType object_type,
					   Gal_Object obj_data);

/* We want to be able to recursively encode objects, and we want
   to be able to extend the buffer appropriately. So at each point,
   we need to check the size, and if there's not enough room, we need
   to expand the buffer and rebuild the XDR object.

   I want to use byte buffers, even though some of the mechanisms
   won't be exploited, because the memory is locally managed. I'll use
   the same trick I used in pr_util.c. */

#define BUF_INC (10 * 1024)
#define BUF_TRIGGER 1024

/* Preliminaries. Utilities and toplevel functions. */

extern int _Gal_ExpandByteBuffer(Gal_StringBuffer *buf, int increment);

static int __GalIO_XDREnsureBufferSize(XDR *xdr_s, Gal_StringBuffer *buf,
				     int size)
{
  u_int xdr_pos = xdr_getpos(xdr_s);

  buf->bufpos = xdr_pos;
  if (!_Gal_ExpandByteBuffer(buf, size)) {
    /* Expansion failed. */
    return 0;
  } else {
    /* Reset the XDR buffer. */
    xdrmem_create(xdr_s, buf->buf, buf->bufsize, XDR_ENCODE);
    xdr_setpos(xdr_s, xdr_pos);
    return 1;
  }
}

static Gal_StringBuffer *__GalIO_XDRCreateStringBuffer(XDR *xdr_s)
{
  Gal_StringBuffer *buf;

  buf = Gal_MakeByteBuffer((char *) NULL, 0, 0, 0, 1, BUF_INC, BUF_TRIGGER);
  
  xdrmem_create(xdr_s, buf->buf, 0, XDR_ENCODE);
  return buf;
}

static char *__GalIO_XDRLiberateStringBuffer(XDR *xdr_s, int res,
					   Gal_StringBuffer *buf,
					   int *bufposptr)
{
  char *irpbuf;
  
  if (res == 0) {
    *bufposptr = 0;
    free(buf->buf);
    irpbuf = (char *) NULL;
  } else {
    irpbuf = buf->buf;
    *bufposptr = xdr_getpos(xdr_s);
  }
  Gal_FreeByteBuffer(buf);
  return(irpbuf);
}

/*
 *
 *        Encoding
 *
 */

static int __GalIO_XDREncodeObjectData(XDR *xdr_s, Gal_StringBuffer *buf,
				       Gal_ObjectType t, void *data,
				       int num_elements);


/* The guts of the operation. */

/* Primitives. */

static int __GalIO_XDREncodeString(XDR *xdr_s, Gal_StringBuffer *buf,
				   const char *s)
{
  if (!s)
    return 0;
  if (!__GalIO_XDREnsureBufferSize(xdr_s, buf,
				 xdr_sizeof((xdrproc_t) xdr_wrapstring, (void *) &s)))
    return 0;
  /* Need to cancel the const. This is not dangerous, because
     xdr_string shouldn't be altering the string it's given
     when it's encoding. */
  if (xdr_string(xdr_s, (char **) &s, strlen(s)) == FALSE)
    return 0;
  return 1;
}

static int __GalIO_XDREncodeInt(XDR *xdr_s, Gal_StringBuffer *buf, int i)
{
  if (!__GalIO_XDREnsureBufferSize(xdr_s, buf,
				 xdr_sizeof((xdrproc_t) xdr_int, (void *) &i)))
    return 0;
  if (xdr_int(xdr_s, &i) == FALSE)
    return 0;
  return 1;
}

static int __GalIO_XDREncodeElement(XDR *xdr_s, Gal_StringBuffer *buf,
				    Gal_ObjectType t, void *data)
{
  Gal_PointerBuffer *p;
  Gal_StringBuffer *s;
  
  switch (t) {
  case GAL_FRAME:
  case GAL_STRING:
  case GAL_TOKEN:
  case GAL_INT:
  case GAL_FLOAT:
  case GAL_SYMBOL:
  case GAL_KEYWORD:
  case GAL_TAG:
  case GAL_PROXY:
    return __GalIO_XDREncodeObjectData(xdr_s, buf, t, data, 1);
  case GAL_LIST:
    /* Pass in the contents of the pointer buffer. */
    p = (Gal_PointerBuffer *) data;    
    return __GalIO_XDREncodeObjectData(xdr_s, buf, t,
				       p->buf, p->bufpos);
  case GAL_BINARY:
  case GAL_INT_16:
  case GAL_INT_32:
  case GAL_INT_64:
  case GAL_FLOAT_32:
  case GAL_FLOAT_64:
    /* Pass in the contents and the number of elements. */
    s = (Gal_StringBuffer *) data;
    return __GalIO_XDREncodeObjectData(xdr_s, buf, t,
				       Gal_DataBufferData(s),
				       Gal_DataBufferSize(s));
  case GAL_FREE:
    GalUtil_Warn("Can't encode XDR element of type GAL_FREE");
    /* Fall through. */
  default:
    return 0;
  }
}


char *_GalIO_XDREncodeToplevel(Gal_Object o, int *bufsizeptr)
{
  int res;
  XDR xdr_s;
  Gal_StringBuffer *buf;

  if (!o) {
    return (char *) NULL;
  }
  
  buf = __GalIO_XDRCreateStringBuffer(&xdr_s);

  res = __GalIO_XDREncodeElement(&xdr_s, buf, o->vtype, o->value);
  
  return __GalIO_XDRLiberateStringBuffer(&xdr_s, res, buf, bufsizeptr);
}

char *_GalIO_XDREncodeObject(Gal_ObjectType t, void *data,
			     int size, int *bufposptr)
{
  int res;
  XDR xdr_s;
  Gal_StringBuffer *buf = __GalIO_XDRCreateStringBuffer(&xdr_s);

  res = __GalIO_XDREncodeObjectData(&xdr_s, buf, t, data, size);
  
  return __GalIO_XDRLiberateStringBuffer(&xdr_s, res, buf, bufposptr);
}

char *_GalIO_XDREncodeFrame(Gal_Frame fr, int *bufsizeptr)
{
  return _GalIO_XDREncodeObject(GAL_FRAME, (void *) fr, 1, bufsizeptr);
}

typedef struct __galio_xdr_props {
  XDR *xdr_s;
  Gal_StringBuffer *buf;
  int failure;
} __galio_xdr_props;

static int __galio_xdr_encode_props(const char *key, Gal_Object cur_obj,
				    void *caller_data)
{
  __galio_xdr_props *xdr_props = (__galio_xdr_props *) caller_data;
  
  /* Encode the key name. */
  if (!__GalIO_XDREncodeString(xdr_props->xdr_s, xdr_props->buf, key)) {
    xdr_props->failure = 1;
    return 0;
  }
  /* Encode the value, which is a Gal_Object. */
  if (!__GalIO_XDREncodeElement(xdr_props->xdr_s,
			       xdr_props->buf, cur_obj->vtype,
			       cur_obj->value)) {
    xdr_props->failure = 1;
    return 0;
  }
  return 1;
}

static int __galio_xdr_encode_preds(Gal_Object o, void *caller_data)
{
  __galio_xdr_props *xdr_props = (__galio_xdr_props *) caller_data;
  
  /* Next, encode each of the predicates. */
  if (!__GalIO_XDREncodeElement(xdr_props->xdr_s, xdr_props->buf,
			       o->vtype, o->value)) {
    xdr_props->failure = 1;
    return 0;
  }
  return 1;
}

static int __GalIO_XDREncodeObjectData(XDR *xdr_s, Gal_StringBuffer *buf,
				       Gal_ObjectType t, void *data,
				       int num_elements)
{
  int num_preds;
  int i, j;
  Gal_Frame fr;
  Gal_Object *list_buf;
  __galio_xdr_props xdr_props;
  _GalIO_int_16 i16;
  _GalIO_int_32 i32;
  _GalIO_int_64 i64;
  _GalIO_float_32 f32;
  _GalIO_float_64 f64;
  GalSS_BrokerProxy *p;
  
  /* First, encode the type. */
  if (!__GalIO_XDREncodeInt(xdr_s, buf, t))
    return 0;

  /* Next, do something different for each type of object. */
  switch (t) {
  case GAL_FRAME:
    /* Data is a frame. */
    fr = (Gal_Frame) data;
    if (!fr)
      return 0;
    /* First, encode the frame type. */
    if (!__GalIO_XDREncodeInt(xdr_s, buf, fr->ftype))
      return 0;
    /* Next, encode the frame name. */
    if (!__GalIO_XDREncodeString(xdr_s, buf, Gal_FrameName(fr)))
      return 0;
    /* The rest of this is taken from pr_util.c, in terms of
       looping through the properties. */
    
    /* Encode the number of properties. */
    if (!__GalIO_XDREncodeInt(xdr_s, buf, Gal_NumNonNullProperties(fr)))
      return 0;

    /* Do the properties. */
    xdr_props.buf = buf;
    xdr_props.xdr_s = xdr_s;
    xdr_props.failure = 0;
    Gal_DoProperties(fr, __galio_xdr_encode_props, (void *) &xdr_props);
    if (xdr_props.failure)
      return 0;
    
    /* Next, encode the number of predicates. */
    num_preds = fr->npred;
    if (!__GalIO_XDREncodeInt(xdr_s, buf, num_preds))
      return 0;
    xdr_props.failure = 0;
    Gal_DoPreds(fr, __galio_xdr_encode_preds, (void *) &xdr_props);
    if (xdr_props.failure)
      return 0;
    
    break;
  case GAL_STRING:
  case GAL_TOKEN:
    /* Data is a string. */
    if (!__GalIO_XDREncodeString(xdr_s, buf, (char *) data))
      return 0;
    break; 
  case GAL_INT:
    /* Data is an integer. */
    if (!__GalIO_XDREncodeInt(xdr_s, buf, (int) data))
      return 0;
    break;
  case GAL_FLOAT:
    /* Data is a float pointer. */
    if (!__GalIO_XDREnsureBufferSize(xdr_s, buf, xdr_sizeof((xdrproc_t) xdr_float, data)))
      return 0;
    xdr_float(xdr_s, data);
    break;
  case GAL_SYMBOL:
  case GAL_KEYWORD:
  case GAL_TAG:
    if (!data)
      return 0;
    /* Data is a symbol structure. Encode the name. */
    if (!__GalIO_XDREncodeString(xdr_s, buf, sym_name((Gal_Symbol) data)))
      return 0;
    break;
  case GAL_PROXY:
    /* Data is a GAL_PROXY object. Encode call_id, host,
       port, object type. */
    p = (GalSS_BrokerProxy *) data;
    if (!p)
      return 0;
    if (!__GalIO_XDREncodeString(xdr_s, buf, p->call_id))
      return 0;
    if (!__GalIO_XDREncodeString(xdr_s, buf, p->host))
      return 0;
    if (!__GalIO_XDREncodeInt(xdr_s, buf, p->port))
      return 0;
    if (!__GalIO_XDREncodeInt(xdr_s, buf, p->object_type))
      return 0;
    break;
  case GAL_LIST:
    /* Data is a pointer buffer whose elements are Gal_Objects.
       Encode like predicate list. */
    list_buf = (Gal_Object *) data;
    if ((num_elements > 0) && !list_buf)
      return 0;
    if (!__GalIO_XDREncodeInt(xdr_s, buf, num_elements))
      return 0;
    for (i = 0; i < num_elements; i++) {
      if (!__GalIO_XDREncodeElement(xdr_s, buf,
				   list_buf[i]->vtype,
				   list_buf[i]->value))
	return 0;
    }
    break;
  case GAL_BINARY:
    /* Data is a byte array. */
    if ((num_elements > 0) && !data)
      return 0;
    /* Required size is the number of bytes + an integer for the size. */
    if (!__GalIO_XDREnsureBufferSize(xdr_s, buf, num_elements + xdr_sizeof((xdrproc_t) xdr_int, &num_elements)))
      return 0;
    xdr_bytes(xdr_s, (char **) &data, &num_elements, num_elements);
    break;    
  case GAL_INT_16:
    if (!xdr_gal_int16) {
      GalUtil_Warn("Can't XDR encode type GAL_INT_16");
      return 0;
    }
    if ((num_elements > 0) && !data)
      return 0;
    /* I need to allocate enough to hold all the encoded elements.
       So I need to know how to encode the data. */
    /* Data is a byte array. */
    j = num_elements * xdr_sizeof((xdrproc_t) xdr_gal_int16, &i16);
    /* Required size is the size for elements + an integer for the size. */
    if (!__GalIO_XDREnsureBufferSize(xdr_s, buf, j + xdr_sizeof((xdrproc_t) xdr_int, &j)))
      return 0;
    xdr_array(xdr_s, (char **) &data, &num_elements, num_elements, 2, xdr_gal_int16);
    break;
  case GAL_INT_32:
    if (!xdr_gal_int32) {
      GalUtil_Warn("Can't XDR encode type GAL_INT_32");
      return 0;
    }
    if ((num_elements > 0) && !data)
      return 0;
    /* I need to allocate enough to hold all the encoded elements.
       So I need to know how to encode the data. */
    /* Data is a byte array. */
    j = num_elements * xdr_sizeof((xdrproc_t) xdr_gal_int32, &i32);
    /* Required size is the size for elements + an integer for the size. */
    if (!__GalIO_XDREnsureBufferSize(xdr_s, buf, j + xdr_sizeof((xdrproc_t) xdr_int, &j)))
      return 0;
    xdr_array(xdr_s, (char **) &data, &num_elements, num_elements, 4, xdr_gal_int32);
    break;
  case GAL_INT_64:
    if (!xdr_gal_int64) {
      GalUtil_Warn("Can't XDR encode type GAL_INT_64");
      return 0;
    }
    if ((num_elements > 0) && !data)
      return 0;
    /* I need to allocate enough to hold all the encoded elements.
       So I need to know how to encode the data. */
    /* Data is a byte array. */
    j = num_elements * xdr_sizeof((xdrproc_t) xdr_gal_int64, &i64);
    /* Required size is the size for elements + an integer for the size. */
    if (!__GalIO_XDREnsureBufferSize(xdr_s, buf, j + xdr_sizeof((xdrproc_t) xdr_int, &j)))
      return 0;
    xdr_array(xdr_s, (char **) &data, &num_elements, num_elements, 8, xdr_gal_int64);
    break;
  case GAL_FLOAT_32:
    if (!xdr_gal_float32) {
      GalUtil_Warn("Can't XDR encode type GAL_FLOAT_32");
      return 0;
    }
    if ((num_elements > 0) && !data)
      return 0;
    /* I need to allocate enough to hold all the encoded elements.
       So I need to know how to encode the data. */
    /* Data is a byte array. */
    j = num_elements * xdr_sizeof((xdrproc_t) xdr_gal_float32, &f32);
    /* Required size is the size for elements + an integer for the size. */
    if (!__GalIO_XDREnsureBufferSize(xdr_s, buf, j + xdr_sizeof((xdrproc_t) xdr_int, &j)))
      return 0;
    xdr_array(xdr_s, (char **) &data, &num_elements, num_elements, 4, xdr_gal_float32);
    break;
  case GAL_FLOAT_64:
    if (!xdr_gal_float64) {
      GalUtil_Warn("Can't XDR encode type GAL_FLOAT_64");
      return 0;
    }
    if ((num_elements > 0) && !data)
      return 0;
    /* I need to allocate enough to hold all the encoded elements.
       So I need to know how to encode the data. */
    /* Data is a byte array. */
    j = num_elements * xdr_sizeof((xdrproc_t) xdr_gal_float64, &f64);
    /* Required size is the size for elements + an integer for the size. */
    if (!__GalIO_XDREnsureBufferSize(xdr_s, buf, j + xdr_sizeof((xdrproc_t) xdr_int, &j)))
      return 0;
    xdr_array(xdr_s, (char **) &data, &num_elements, num_elements, 8, xdr_gal_float64);
    break;
  default:
    /* Can't encode pointers or anything else like that.
       Fail the whole thing. */
    GalUtil_Warn("Can't XDR encode type %s", Gal_ObjectTypeString(t));
    return 0;
  }
  return 1;
}
  
/*
 *
 *        Decoding 
 *
 */

static int __GalIO_XDRDecodeObjectData(XDR *xdr_s, Gal_ObjectType *t_ptr,
				       void **data_ptr, int *size_ptr);

int _GalIO_XDRDecodeObject(char *buf, int object_size,
			   Gal_ObjectType *t_ptr,
			   void **data_ptr, int *size_ptr)
{			 
  XDR xdr_s;
  int status;
  
  xdrmem_create(&xdr_s, buf, object_size, XDR_DECODE);

  status = __GalIO_XDRDecodeObjectData(&xdr_s, t_ptr, data_ptr, size_ptr);
  if (status < 0) {
    return 0;
  } else {
    return status;
  }
}

Gal_Object _GalIO_XDRDecodeToplevel(char *buf, int object_size)
{  
  
  int size = 0;
  void *data = (void *) NULL;
  Gal_ObjectType t = 0;
  int success;

  success = _GalIO_XDRDecodeObject(buf, object_size, &t, &data, &size);
  if (!success)
    return (Gal_Object) NULL;
  else
    return _GalIO_CreateObject(data, t, size);
}

Gal_Object _GalIO_CreateObject(void *data, Gal_ObjectType t, int size)
{
  switch (t) {
  case GAL_FRAME:
    return Gal_FrameObject((Gal_Frame) data);
  case GAL_STRING:    
    return Gal_CreateStringObject((char *) data, 1);
  case GAL_TOKEN:    
    return Gal_CreateTokenObject((char *) data, 1);
  case GAL_INT:    
    return Gal_IntObject((int) data);
  case GAL_FLOAT:
    return Gal_CreateFloatObject((float *) data, 1);
  case GAL_SYMBOL:
  case GAL_KEYWORD:
  case GAL_TAG:
    return wrap_tsym((Gal_Symbol) data, t);
  case GAL_LIST:
    return Gal_CreateListObject((Gal_Object *) data, size,
				_gal_free_object, 1);
  case GAL_BINARY:    
    return Gal_CreateBinaryObject(data, size, 1);
  case GAL_INT_16:
    return Gal_CreateInt16Object(data, size, 1);
  case GAL_INT_32:
    return Gal_CreateInt32Object(data, size, 1);
  case GAL_INT_64:
    return Gal_CreateInt64Object(data, size, 1);
  case GAL_FLOAT_32:
    return Gal_CreateFloat32Object(data, size, 1);
  case GAL_FLOAT_64:
    return Gal_CreateFloat64Object(data, size, 1);
  case GAL_PROXY:
    return Gal_ProxyObject((GalSS_BrokerProxy *) data);
  default:
    return (Gal_Object) NULL;
  }
}

static Gal_Object __GalIO_XDRDecodeElement(XDR *xdr_s,
					   int *unsupported_type_ptr)
{
  int size = 0;
  void *data = (void *) NULL;
  Gal_ObjectType t = 0;
  int success;

  success = __GalIO_XDRDecodeObjectData(xdr_s, &t, &data, &size);
  if (success < 0) {
    *unsupported_type_ptr = 1;
    return (Gal_Object) NULL;
  } else if (!success) {
    *unsupported_type_ptr = 0;
    return (Gal_Object) NULL;
  } else {
    *unsupported_type_ptr = 0;
    return _GalIO_CreateObject(data, t, size);
  }
}

/* 1 is success, 0 is failure, -1 is unsupported type
   (currently, just GAL_PROXY). */

static int __GalIO_XDRDecodeObjectData(XDR *xdr_s, Gal_ObjectType *t_ptr,
				       void **data_ptr, int *size_ptr)
{
  int num_props;
  int num_preds;
  int i;
  Gal_Object cur_obj;
  Gal_Frame fr;
  int ftype;
  /* This had better be big enough. I don't want to
     malloc and free because the name is malloc'ed
     and freed when a symbol is created. */
  char fname[1024];
  /* because I can't pass the address of fname... */
  char *fnamep = fname;
  char key[1024];
  /* because I can't pass the address of key... */
  char *keyp = key;
  char *token = (char *) NULL;
  int i_value;
  float f_value;
  Gal_ObjectType t;
  Gal_Object *obj_array;
  int j, k;
  float *fptr;
  int unsupported_type;
  char *host = (char *) NULL, *call_id = (char *) NULL;
  int port, object_type;
  GalSS_BrokerProxy *bp;
  
  /* First, decode the type. */
  if (xdr_int(xdr_s, (int *) &t) == FALSE)
    return 0;

  *t_ptr = t;
  
  /* Next, do something different for each type of object. */
  switch (t) {
  case GAL_FRAME:
    /* Data is a frame. */
    /* First, decode the frame type. */
    if (xdr_int(xdr_s, &ftype) == FALSE)
      return 0;
    /* Next, decode the frame name. */
    if (xdr_string(xdr_s, &fnamep, 1024) == FALSE)
      return 0;
    fr = Gal_MakeFrame(fnamep, ftype);

    /* Now, we decode the properties. */
    if (xdr_int(xdr_s, &num_props) == FALSE) {
      Gal_FreeFrame(fr);
      return 0;
    }
    for (i = 0; i < num_props; i++) {
      if (xdr_string(xdr_s, &keyp, 1024) == FALSE) {
	Gal_FreeFrame(fr);
	return 0;
      }
      /* Now, we recurse. */
      cur_obj = __GalIO_XDRDecodeElement(xdr_s, &unsupported_type);
      /* If the type is unsupported, don't do anything.
	 Otherwise, use whatever is returned, or abort if
	 there's nothing. */
      if (!unsupported_type) {
	if (!cur_obj) {
	  Gal_FreeFrame(fr);
	  return 0;
	}
	Gal_SetProp(fr, keyp, cur_obj);
      }
    }

    /* Next, we decode the predicates. */
    if (xdr_int(xdr_s, &num_preds) == FALSE) {
      Gal_FreeFrame(fr);
      return 0;
    }
    for (i = 0; i < num_preds; i++) {
      cur_obj = __GalIO_XDRDecodeElement(xdr_s, &unsupported_type);
      if (!unsupported_type) {
	if (Gal_Framep(cur_obj)) {
	  Gal_AddPred(fr, Gal_FrameValue(cur_obj));
	  Gal_FreeWrapper(cur_obj);
	} else {
	  Gal_FreeFrame(fr);
	  return 0;
	}
      }
    }
    /* Success */
    *size_ptr = 1;
    *data_ptr = (void *) fr;
    return 1;
  case GAL_STRING:
    /* Data is a string. */
    if (xdr_wrapstring(xdr_s, &token) == FALSE)
      return 0;
    /* Manage the memory. */
    *size_ptr = strlen(token);
    *data_ptr = (void *) token;
    return 1;
  case GAL_TOKEN:
    /* Data is a string. */
    if (xdr_wrapstring(xdr_s, &token) == FALSE)
      return 0;
    /* Manage the memory. */
    *size_ptr = strlen(token);
    *data_ptr = (void *) token;
    return 1;
  case GAL_INT:
    /* Data is an integer. */
    if (xdr_int(xdr_s, &i_value) == FALSE)
      return 0;
    *size_ptr = 1;
    *data_ptr = (void *) i_value;
    return 1;
  case GAL_FLOAT:
    /* Data is a float pointer. */
    if (xdr_float(xdr_s, &f_value) == FALSE)
      return 0;
    *size_ptr = 1;
    fptr = (float *) malloc(sizeof(float));
    *fptr = f_value;
    *data_ptr = (void *) fptr;
    return 1;
  case GAL_SYMBOL:
  case GAL_KEYWORD:
  case GAL_TAG:
    if (xdr_string(xdr_s, &keyp, 1024) == FALSE)
      return 0;
    *size_ptr = 1;
    *data_ptr = (void *) add_sym(keyp);
    return 1;
  case GAL_LIST:
    if (xdr_int(xdr_s, &i_value) == FALSE)
      return 0;
    if (i_value)
      obj_array = (Gal_Object *) calloc(i_value, sizeof(Gal_Object));
    else
      obj_array = (Gal_Object *) NULL;
    k = 0;
    for (i = 0; i < i_value; i++) {
      /* Decode each element, one at a time. */
      obj_array[k] = __GalIO_XDRDecodeElement(xdr_s, &unsupported_type);      
      if (!unsupported_type) {
	if (!obj_array[k]) {
	  for (j = 0; j < k; j++) {
	    Gal_FreeObject(obj_array[j]);
	  }
	  return 0;
	} else {
	  k++;
	}
      }
    }
    *size_ptr = k;
    *data_ptr = (void *) obj_array;
    return 1;
  case GAL_BINARY:
    if (xdr_bytes(xdr_s, &token, &i_value, UINT_MAX) == FALSE)
      return 0;
    *size_ptr = i_value;
    *data_ptr = (void *) token;
    return 1;
  case GAL_INT_16:
    if (!xdr_gal_int16) {
      GalUtil_Warn("Can't XDR decode type GAL_INT_16");
      return 0;
    }
    if (xdr_array(xdr_s, &token, &i_value, UINT_MAX,
		  2, xdr_gal_int16) == FALSE)
      return 0;
    *size_ptr = i_value;
    *data_ptr = (void *) token;
    return 1;
  case GAL_INT_32:
    if (!xdr_gal_int32) {
      GalUtil_Warn("Can't XDR decode type GAL_INT_32");
      return 0;
    }
    if (xdr_array(xdr_s, &token, &i_value, UINT_MAX,
		  4, xdr_gal_int32) == FALSE)
      return 0;
    *size_ptr = i_value;
    *data_ptr = (void *) token;
    return 1;
  case GAL_INT_64:
    if (!xdr_gal_int64) {
      GalUtil_Warn("Can't XDR decode type GAL_INT_64");
      return 0;
    }
    if (xdr_array(xdr_s, &token, &i_value, UINT_MAX,
		  8, xdr_gal_int64) == FALSE)
      return 0;
    *size_ptr = i_value;
    *data_ptr = (void *) token;
    return 1;
  case GAL_FLOAT_32:
    if (!xdr_gal_float32) {
      GalUtil_Warn("Can't XDR decode type GAL_FLOAT_32");
      return 0;
    }
    if (xdr_array(xdr_s, &token, &i_value, UINT_MAX,
		  4, xdr_gal_float32) == FALSE)
      return 0;
    *size_ptr = i_value;
    *data_ptr = (void *) token;
    return 1;
  case GAL_FLOAT_64:
    if (!xdr_gal_float64) {
      GalUtil_Warn("Can't XDR decode type GAL_FLOAT_64");
      return 0;
    }
    if (xdr_array(xdr_s, &token, &i_value, UINT_MAX,
		  8, xdr_gal_float64) == FALSE)
      return 0;
    *size_ptr = i_value;
    *data_ptr = (void *) token;
    return 1;
  case GAL_PROXY:
    /* Call ID. */
    if (xdr_wrapstring(xdr_s, &call_id) == FALSE)
      return 0;
    /* Host. */
    if (xdr_wrapstring(xdr_s, &host) == FALSE)
      return 0;
    /* Port. */
    if (xdr_int(xdr_s, &port) == FALSE)
      return 0;
    /* Object type. */
    if (xdr_int(xdr_s, &object_type) == FALSE)
      return 0;
    bp = GalSS_CreateBrokerProxy(host, port, call_id, object_type, NULL);
    *size_ptr = 1;
    *data_ptr = (void *) bp;
    free(call_id);
    free(host);
    return 1;
  default:
    /* Can't encode pointers or anything else like that.
       Fail the whole thing. */
    GalUtil_Warn("Can't XDR decode type %s", Gal_ObjectTypeString(t));
    *size_ptr = 0;
    *data_ptr = (void *) NULL;
    return 0;
  }
  GalUtil_Warn("Exiting %s from outside switch statement, which should NOT happen", __FUNCTION__);
  *size_ptr = 0;
  *data_ptr = (void *) NULL;
  return 0;
}
