/*
  This file (c) Copyright 2002 The MITRE Corporation.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* SAM 2/4/02: Initial reference implementation of broker
   proxies. We have versions to manipulate the broker proxy
   directly or wrapped in a Gal_Object. */

/* I need the internal defs. */

#include "galaxy/sysdep.h"
#include "../galaxy/gal_internal.h"
#include "generic-server-internal.h"

/* From xdr_buffer.c */

extern Gal_Object _GalIO_CreateObject(void *data, Gal_ObjectType t, int size);

/* Basics. */

GalSS_BrokerProxy *GalSS_CreateBrokerProxy(const char *host,
					   int port, const char *call_id, 
					   Gal_ObjectType object_type,
					   Gal_Object obj_data)
{
  GalSS_BrokerProxy *p = (GalSS_BrokerProxy *) calloc(1, sizeof(GalSS_BrokerProxy));
  p->call_id = _gal_strdup(call_id);
  p->host = _gal_strdup(host);
  p->port = port;
  p->object_type = object_type;
  if (obj_data)
    p->obj = obj_data;
  return p;
}

/* The broker may already have been terminated. So what I 
   need to do is make sure that when the broker is terminated,
   it's removed from the broker proxy. I can use a destroy
   callback for this.

   What do I want to have happen to the broker when the broker
   proxy is freed? If the broker is an inbound broker and
   it isn't done, then I probably started it with GalSS_Unproxify
   and I have no idea when it will be finished. Furthermore,
   it all depends on who's polling it. If you use the
   callback handlers, the broker will get destroyed when it's
   done reading. So we don't need to destroy it here. Nor
   do we want to mark it as done, since it should be marked
   as done when it receives the done message.
   
   For an out broker, if it isn't done, I do actually want to
   write it as done. Don't destroy it, yet, though. On second
   thought, it's not even clear to me I should do that, since
   it's possible that the broker was rescued from the proxy
   and stashed away. Better not to do anything, frankly.

   However, if the proxy is freed, and it's pointing to a
   broker, it had better remove itself from the broker. */
   
void GalSS_FreeBrokerProxy(GalSS_BrokerProxy *bp)
{
  free(bp->call_id);
  bp->call_id = (char *) NULL;
  free(bp->host);
  bp->host = (char *) NULL;
  if (bp->obj) {
    Gal_FreeObject(bp->obj);
    bp->obj = (Gal_Object) NULL;
  }
  if (bp->broker && bp->removal_cb) {
    GalIO_RemoveBrokerCallback(bp->broker, bp->removal_cb);
  }    
  free(bp);
}

GalSS_BrokerProxy *GalSS_CopyBrokerProxy(GalSS_BrokerProxy *bp)
{
  Gal_Object new_obj;
  
  if (!bp) {
    return (GalSS_BrokerProxy *) NULL;
  } else {
    if (bp->obj)
      new_obj = Gal_CopyObject(bp->obj);    
    else
      new_obj = (Gal_Object) NULL;
    return GalSS_CreateBrokerProxy(bp->host, bp->port,
				   bp->call_id,
				   bp->object_type,
				   new_obj);
  }
}

GalIO_BrokerStruct *GalSS_BrokerProxyBroker(GalSS_BrokerProxy *bp)
{
  if (bp) {
    return bp->broker;
  } else {
    return (GalIO_BrokerStruct *) NULL;
  }
}

Gal_ObjectType GalSS_BrokerProxyObjectType(GalSS_BrokerProxy *bp)
{
  if (bp) {
    return bp->object_type;
  } else {
    return GAL_FREE;
  }
}

Gal_Object GalSS_BrokerProxyObject(GalSS_BrokerProxy *bp)
{
  if (bp) {
    return bp->obj;
  } else {
    return (Gal_Object) NULL;
  }
}

/* OUTBOUND SIDE */

/* GalSS_ProxifyObject takes obj and produces a new Gal_Object of type
   GAL_PROXY. This object will contain a GalSS_BrokerProxy structure
   which contains a functioning outbound broker. Returns NULL if no
   broker can be created. If obj is NULL, return NULL. The ownership
   of obj is transferred to the new Gal_Object. If the object has any
   data in it already, write it out, dammit. */

/* I have to make sure that I remove the broker from the
   proxy if it's being destroyed. */

static void __galss_remove_from_broker_proxy(GalIO_BrokerStruct *b,
					     void *caller_data)
{
  GalSS_BrokerProxy *bp = (GalSS_BrokerProxy *) caller_data;
  if (bp->broker == b) {
    bp->already_brokered = 1;
    bp->broker = (GalIO_BrokerStruct *) NULL;
    /* And this is this function. */
    bp->removal_cb = NULL;
  }
}

GalSS_BrokerProxy *
GalSS_ProxifyObject(GalSS_Environment *env, Gal_Object obj,
		    int poll_ms, int timeout_seconds)
{
  GalIO_BrokerStruct *b;
  Gal_Object *elts;
  int i, j;
  GalSS_BrokerProxy *bp;

  if (!obj)
    return (GalSS_BrokerProxy *) NULL;
  
  b = GalIO_BrokerDataOutInit(GalSS_EnvComm(env),
			      -1, timeout_seconds);
  if (b && (GalIO_GetBrokerListenPort(b) > 0) &&
      (GalIO_GetBrokerCallID(b) != (char *) NULL)) {
    /* Tricky. If it's any object besides a list, I want
       to write the object. But if it's a list, I
       want to write the ELEMENTS. This is because
       the expandable arrays are homogeneous, but the
       lists are not. */
    /* If they're expandable, don't mark it as done.
       Otherwise, do. */
    switch (Gal_GetObjectType(obj)) {
    case GAL_LIST:
      elts = Gal_ListValue(obj, &i);
      for (j = 0; j < i; j++) {
	if (GalIO_BrokerWriteObject(b, elts[j]) < 0) {
	  /* It may fail. */
	  GalIO_BrokerDataDone(b);
	  GalIO_DestroyBrokerStruct(b);
	  return (GalSS_BrokerProxy *) NULL;
	}
      }
      if (!Gal_ListObjectExpandable(obj))
	GalIO_BrokerDataOutDone(b);
      break;	
    case GAL_BINARY:
    case GAL_INT_16:
    case GAL_INT_32:
    case GAL_INT_64:
    case GAL_FLOAT_32:
    case GAL_FLOAT_64:
      if (GalIO_BrokerWriteObject(b, obj) < 0) {
	GalIO_BrokerDataDone(b);
	GalIO_DestroyBrokerStruct(b);
	return (GalSS_BrokerProxy *) NULL;
      }
      if (!Gal_ArrayObjectExpandable(obj))
	GalIO_BrokerDataOutDone(b);
      break;
    default:
      if (GalIO_BrokerWriteObject(b, obj) < 0) {
	GalIO_BrokerDataDone(b);
	GalIO_DestroyBrokerStruct(b);
	return (GalSS_BrokerProxy *) NULL;
      }
      GalIO_BrokerDataOutDone(b);
      break;
    }
    bp = GalSS_CreateBrokerProxy(GalIO_IPAddress(),
				 GalIO_GetBrokerListenPort(b),
				 GalIO_GetBrokerCallID(b),
				 Gal_GetObjectType(obj), obj);
    bp->broker = b;
    bp->removal_cb = GalIO_AddBrokerCallback(b, GAL_BROKER_DESTRUCTION_EVENT,
					     __galss_remove_from_broker_proxy,
					     (void *) bp);
    GalSS_EnvStartBroker(env, b, poll_ms);
    return bp;
  } else {
    if (b) {
      GalIO_BrokerDataDone(b);
      GalIO_DestroyBrokerStruct(b);
    }
    return (GalSS_BrokerProxy *) NULL;
  }
}

Gal_Object
GalSS_ObjProxifyObject(GalSS_Environment *env, Gal_Object obj,
		       int poll_ms, int timeout_seconds)
{
  GalSS_BrokerProxy *bp;

  bp = GalSS_ProxifyObject(env, obj, poll_ms, timeout_seconds);

  if (bp) {
    return Gal_ProxyObject(bp);
  } else {
    return (Gal_Object) NULL;
  }
}


/* Creates a new Gal_Object of type GAL_PROXY, which will be
   interpreted on the receiving end as a single object of type t. This
   object can be written in parts (multiple broker writes for binary
   data, for instance). If t is -1, set up a stream which accepts any
   type of data. */

GalSS_BrokerProxy *
GalSS_ProxifyObjectType(GalSS_Environment *env,
			Gal_ObjectType t,
			int poll_ms, int timeout_seconds)
{
  GalIO_BrokerStruct *b = GalIO_BrokerDataOutInit(GalSS_EnvComm(env),
						  -1, timeout_seconds);
  GalSS_BrokerProxy *bp;
  
  if (b && (GalIO_GetBrokerListenPort(b) > 0) &&
      (GalIO_GetBrokerCallID(b) != (char *) NULL)) {
    bp = GalSS_CreateBrokerProxy(GalIO_IPAddress(),
				 GalIO_GetBrokerListenPort(b),
				 GalIO_GetBrokerCallID(b),
				 t, (Gal_Object) NULL);
    bp->broker = b;
    bp->removal_cb = GalIO_AddBrokerCallback(b, GAL_BROKER_DESTRUCTION_EVENT,
					     __galss_remove_from_broker_proxy,
					     (void *) bp);
    GalIO_CommStartBroker(GalSS_EnvComm(env), b, poll_ms);
    return bp;
  } else {
    if (b) {
      GalIO_BrokerDataOutDone(b);
      GalIO_DestroyBrokerStruct(b);
    }
    return (GalSS_BrokerProxy *) NULL;
  }
}

Gal_Object
GalSS_ObjProxifyObjectType(GalSS_Environment *env,
			   Gal_ObjectType t,
			   int poll_ms, int timeout_seconds)
{
  GalSS_BrokerProxy *bp;

  bp = GalSS_ProxifyObjectType(env, t, poll_ms,	timeout_seconds);
  if (bp) {
    return Gal_ProxyObject(bp);
  } else {
    return (Gal_Object) NULL;
  }
}

/* GalSS_ProxyListObjectAdd: If obj is an outbound GAL_PROXY object
   wrapped around an expandable GAL_LIST object, this function adds elt
   to the list and writes it to the broker. If obj is an outbound
   GAL_PROXY object of type GAL_LIST which has no underlying object
   associated with it, write elt to the broker. Otherwise, this function
   has no effect. You can do this independently with separate calls to
   the object and the broker, but this is cleaner, that is, if you want
   to keep the data. Ownership of elt is transferred to the proxy; if
   there's a GAL_LIST object, it's then transferred to the GAL_LIST
   object, otherwise Gal_FreeObject is called.

   And make sure you don't write to a done broker. */

int GalSS_ProxyListAdd(GalSS_BrokerProxy *bp, Gal_Object elt)
{
  if (bp && bp->broker && bp->broker->out_b &&
      (bp->object_type == GAL_LIST) &&
      !GalIO_BrokerIsDone(bp->broker) &&
      ((!bp->obj) || Gal_ListObjectExpandable(bp->obj))) {      
    if (GalIO_BrokerWriteObject(bp->broker, elt) < 0) {
      return -1;
    }
    if (bp->obj) {
      Gal_ListObjectAdd(bp->obj, elt);
    } else {
      Gal_FreeObject(elt);
    }
    return 1;
  }
  return -1;
}

int GalSS_ObjProxyListAdd(Gal_Object obj, Gal_Object elt)
{
  if (obj && Gal_Proxyp(obj)) {
    GalSS_BrokerProxy *bp = (GalSS_BrokerProxy *) obj->value;
    return GalSS_ProxyListAdd(bp, elt);
  }
  return -1;
}

/* GalSS_ProxyArrayObjectAdd: If obj is an outbound GAL_PROXY object
   wrapped around an expandable array type, this function calls
   Gal_ArrayObjectAdd and GalIO_BrokerWrite<blah>, where <blah> is the
   appropriate type. If obj is an outbound GAL_PROXY object of an array
   type which has no underlying object associated with it, write the data
   to the broker. Otherwise, this function has no effect. You can do this
   independently with separate calls to the object and the broker, but
   this is cleaner, that is, if you want to keep the data.

   And make sure you don't write to a done broker. */

int GalSS_ProxyArrayAdd(GalSS_BrokerProxy *bp, void *data, int size)
{
  if (bp && bp->broker && bp->broker->out_b &&
      !GalIO_BrokerIsDone(bp->broker) &&
      ((!bp->obj) || Gal_ArrayObjectExpandable(bp->obj))) {
    switch(bp->object_type) {	
    case GAL_BINARY:
      if (GalIO_BrokerWriteBinary(bp->broker, data, size) < 0) {
	return -1;
      }
      if (bp->obj) {
	Gal_ArrayObjectAdd(bp->obj, data, size);
      }
      break;
    case GAL_INT_16:
      if (GalIO_BrokerWriteInt16(bp->broker, data, size) < 0) {
	return -1;
      }
      if (bp->obj) {
	Gal_ArrayObjectAdd(bp->obj, data, size);
      }
      break;
    case GAL_INT_32:
      if (GalIO_BrokerWriteInt32(bp->broker, data, size) < 0) {
	return -1;
      }
      if (bp->obj) {
	Gal_ArrayObjectAdd(bp->obj, data, size);
      }
      break;
    case GAL_INT_64:
      if (GalIO_BrokerWriteInt64(bp->broker, data, size) < 0) {
	return -1;
      }
      if (bp->obj) {
	Gal_ArrayObjectAdd(bp->obj, data, size);
      }
      break;
    case GAL_FLOAT_32:
      if (GalIO_BrokerWriteFloat32(bp->broker, data, size) < 0) {
	return -1;
      }
      if (bp->obj) {
	Gal_ArrayObjectAdd(bp->obj, data, size);
      }
      break;
    case GAL_FLOAT_64:
      if (GalIO_BrokerWriteFloat64(bp->broker, data, size) < 0) {
	return -1;
      }
      if (bp->obj) {
	Gal_ArrayObjectAdd(bp->obj, data, size);
      }
      break;      
    default:
      return -1;
    }
    return 1;
  }
  return -1;
}

int GalSS_ObjProxyArrayAdd(Gal_Object obj, void *data, int size)
{
  if (obj && Gal_Proxyp(obj)) {
    GalSS_BrokerProxy *bp = (GalSS_BrokerProxy *) obj->value;
    return GalSS_ProxyArrayAdd(bp, data, size);
  }
  return -1;
}

/* GalSS_ProxyObjectWrite is present for completeness. It's mostly
   useful with streaming types with no cache (lists, arrays, and
   "any" streams of type -1). In the case where you use it for
   streams of non-streaming types, it writes Done(). This is
   safer than writing directly onto the broker, since it
   also checks types. NOTE THAT A LIST WRITTEN TO A LIST STREAM
   BECOMES AN ELEMENT OF THE LIST, NOT THE LIST ITSELF.

   This call does not claim the reference of the object passed to it
   when manage_memory is 0. Otherwise, it does. If there is
   already a cache, it will use it. But it won't create a cache
   if there isn't one there already. */

extern void *_Gal_ArrayValue(Gal_Object array_obj, int *size);

int GalSS_ProxyWrite(GalSS_BrokerProxy *bp, Gal_Object obj,
		     int manage_memory)
{
  int res;
  
  if (bp && bp->broker && bp->broker->out_b &&
      !GalIO_BrokerIsDone(bp->broker)) {      
    switch (bp->object_type) {
    case GAL_LIST:
      /* If the memory is managed, just call the list
	 function. Otherwise, if there's a cache,
	 write a copy just in case the list is expandable
	 (otherwise, don't write at all). If there's no
	 cache, just write it to the broker. */
      if (manage_memory) {
	return GalSS_ProxyListAdd(bp, obj);
      } else if (bp->obj) {
	if (Gal_ListObjectExpandable(bp->obj))
	  return GalSS_ProxyListAdd(bp, Gal_CopyObject(obj));
	else
	  return -1;
      } else {
	return GalIO_BrokerWriteObject(bp->broker, obj);
      }
      break;	  
    case (Gal_ObjectType) -1:
      /* Don't check the types. There will be no cache. */
      res = GalIO_BrokerWriteObject(bp->broker, obj);
      if (manage_memory)
	Gal_FreeObject(obj);
      return res;
      break;
    case GAL_BINARY:
    case GAL_INT_16:
    case GAL_INT_32:
    case GAL_INT_64:
    case GAL_FLOAT_32:
    case GAL_FLOAT_64:
      /* Check the types, write the object. */
      if (Gal_GetObjectType(obj) == bp->object_type) {
	if (bp->obj) {
	  if (Gal_ArrayObjectExpandable(bp->obj)) {
	    int size = 0;
	    void *data = _Gal_ArrayValue(obj, &size);
	    res = GalSS_ProxyArrayAdd(bp, data, size);
	  } else {
	    res = -1;
	  }
	} else {
	  res = GalIO_BrokerWriteObject(bp->broker, obj);
	}
	if (manage_memory)
	  Gal_FreeObject(obj);
	return res;
      } else {
	return -1;
      }
      break;
    default:
      /* Check the types, write the object, write done.
	 Will NOT set up a cache. */
      if (Gal_GetObjectType(obj) == bp->object_type) {
	res = GalIO_BrokerWriteObject(bp->broker, obj);
	if (res > -1) {
	  GalIO_BrokerDataOutDone(bp->broker);
	}
      } else {
	res = -1;
      }
      if (manage_memory)
	Gal_FreeObject(obj);
      return res;
    }
  }
  return -1;
}

int GalSS_ObjProxyWrite(Gal_Object proxy_obj, Gal_Object obj,
			int manage_memory)
{
  if (obj && Gal_Proxyp(proxy_obj)) {
    GalSS_BrokerProxy *bp = (GalSS_BrokerProxy *) proxy_obj->value;
    return GalSS_ProxyWrite(bp, obj, manage_memory);
  }
  return -1;
}

/* GalSS_ProxyObjectDone marks the outbound broker as done. This
   happens automatically for non-streamable objects, but is crucial for
   streamable objects (lists and arrays) and for -1. */

void GalSS_ProxyDone(GalSS_BrokerProxy *bp)
{
  if (bp && bp->broker)
    GalIO_BrokerDataOutDone(bp->broker);
}

void GalSS_ObjProxyDone(Gal_Object obj)
{
  if (obj && Gal_Proxyp(obj)) {
    GalSS_BrokerProxy *bp = (GalSS_BrokerProxy *) obj->value;
    GalSS_ProxyDone(bp);
  }
}

/* If this returns 0, you must write a Done() to the proxy
   object in order to terminate it. */

int GalSS_ProxySelfTerminates(GalSS_BrokerProxy *bp)
{
  if ((!bp) || (!bp->broker) || (!bp->broker->out_b)) {
    return 1;
  }
  switch (bp->object_type) {
  case GAL_LIST:
    /* Depends on whether it's expandable or not. If
       there's an nonexpandable object, it terminates. */
    if (bp->obj && !Gal_ListObjectExpandable(bp->obj))
      return 1;
    else
      return 0;
  case GAL_BINARY:
    /* Depends on whether it's expandable or not. If
       there's an nonexpandable object, it terminates. */
    if (bp->obj && !Gal_ArrayObjectExpandable(bp->obj))
      return 1;
    else
      return 0;
  case (Gal_ObjectType) -1:
  case GAL_INT_16:
  case GAL_INT_32:
  case GAL_INT_64:
  case GAL_FLOAT_32:
  case GAL_FLOAT_64:
    return 0;
  default:
    return 1;
  }
}


int GalSS_ObjProxySelfTerminates(Gal_Object obj)
{
  if (obj && Gal_Proxyp(obj)) {
    GalSS_BrokerProxy *bp = (GalSS_BrokerProxy *) obj->value;
    return GalSS_ProxySelfTerminates(bp);
  } else {
    return 1;
  }
}

/* For those library embeddings which don't use the ELR abstraction,
   such as Python and Allegro bindings, I need a polling function
   for the brokers, on both the inbound and outbound sides. */

int GalSS_BrokerProxyOutCallbackHandler(GalSS_BrokerProxy *bp)
{
  if (bp->broker) {
    return GalIO_BrokerDataOutCallbackHandler(bp->broker);
  } else {
    /* If there ain't no broker on the outbound side, there
       ain't never gonna be no broker.*/
    return 1;
  }
}

int GalSS_BrokerProxyWriteReady(GalSS_BrokerProxy *bp)
{
  if (bp->broker) {
    return GalIO_BrokerWriteReady(bp->broker);
  } else {
    return 0;
  }
}

void GalSS_ForceProxyExpiration(GalSS_BrokerProxy *bp)
{
  if (bp->broker) {
    GalIO_ForceBrokerExpiration(bp->broker);
  }
}

/* INBOUND SIDE */

/* We use this structure to store the information needed
   to process the callback. It works with both versions
   of the inbound call. */

typedef struct __broker_proxy_callback {
  void *caller_data;
  void (*caller_data_free_fn)(void *);
  GalSS_ProxyDataHandler data_handler;
  GalSS_ProxyDataEventHandler done_handler;
  GalSS_ProxyDataEventHandler abort_handler;
  GalSS_BrokerProxy *bp;
  Gal_Object obj;
  int immediate;
} __broker_proxy_callback;

static void __free_bpc(__broker_proxy_callback *bpc)
{
  if (bpc->caller_data && bpc->caller_data_free_fn)
    (*bpc->caller_data_free_fn)(bpc->caller_data);
  if (bpc->obj)
    Gal_FreeObject(bpc->obj);
  free(bpc);
}

static void __free_broker_proxy_callback(void *data)
{
  __broker_proxy_callback *bpc = (__broker_proxy_callback *) data;

  __free_bpc(bpc);
}

static void __broker_proxy_discard_data(void *data, Gal_ObjectType data_type,
					int size)
{
  int i;
  Gal_Object *objs;

  switch (data_type) {
  case GAL_STRING:
  case GAL_FLOAT:
    free(data);
    break;
  case GAL_FRAME:
    Gal_FreeFrame((Gal_Frame) data);
    break;
  case GAL_LIST:
    objs = (Gal_Object *) data;
    for (i = 0; i < size; i++) {
      Gal_FreeObject(objs[i]);
    }
    free(objs);
    break;
  case GAL_BINARY:
  case GAL_INT_16:
  case GAL_INT_32:
  case GAL_INT_64:
  case GAL_FLOAT_32:
  case GAL_FLOAT_64:
    free(data);
    break;
  case GAL_PROXY:
    GalSS_FreeBrokerProxy((GalSS_BrokerProxy *) data);
    break;
  default:
    break;
  }
}

/* In the case of GalSS_UnproxifyObject, this callback will
   be invoked without a function, and delayed effect. So the
   elements will be accumulated. */

static void __broker_proxy_data_handler(GalIO_BrokerStruct *b, void *data,
					Gal_ObjectType data_type,
					int size)
{
  __broker_proxy_callback *bpc = (__broker_proxy_callback *) GalIO_GetBrokerData(b);
  GalSS_BrokerProxy *bp = bpc->bp;
  Gal_Object new_obj;

  /* Lists are heterogeneous, -1 is an any stream, and
     anything else had better damn well match. */
  if ((bp->object_type != (Gal_ObjectType) -1) &&
      (bp->object_type != GAL_LIST) &&
      (bp->object_type != data_type)) {
    GalUtil_Warn("Received bad data of type %s (%s expected); discarding",
		 Gal_ObjectTypeString(data_type),
		 Gal_ObjectTypeString(bp->object_type));
    __broker_proxy_discard_data(data, data_type, size);
    return;
  }

  /* Just in case, check to see if it's done. */
  if (bp->already_brokered) {
    GalUtil_Warn("Proxy for type %s has already been completed; discarding",
		 Gal_ObjectTypeString(data_type),
		 Gal_ObjectTypeString(bp->object_type));
    __broker_proxy_discard_data(data, data_type, size);
    /* Mark it as done, just in case. */
    GalIO_BrokerDataDone(b);
    return;
  }
  
  /* For most of these, you create the object, cache it,
     and FORCE DONE. Don't wait for the done message to come.
     And yes, you can proxy a proxy. I think. */
  switch (bp->object_type) {
  case GAL_STRING:
  case GAL_TOKEN:
  case GAL_FRAME:
  case GAL_INT:
  case GAL_FLOAT:
  case GAL_SYMBOL:
  case GAL_KEYWORD:
  case GAL_TAG:    
  case GAL_PROXY:
    if ((bpc->immediate && bpc->data_handler) ||
	(!bpc->immediate)) {
      new_obj = _GalIO_CreateObject(data, data_type, size);
      if (bpc->immediate) {
	(*bpc->data_handler)(GalSS_BrokerGetEnvironment(b),
			     bpc->bp->object_type,
			     new_obj, bpc->caller_data);
      } else {
	bpc->obj = new_obj;
      }
    } else {
      __broker_proxy_discard_data(data, data_type, size);
    }
    GalIO_BrokerDataDone(b);
    break;
  case GAL_LIST:
    if ((bpc->immediate && bpc->data_handler) ||
	(!bpc->immediate)) {
      new_obj = _GalIO_CreateObject(data, data_type, size);
      if (bpc->immediate) {
	(*bpc->data_handler)(GalSS_BrokerGetEnvironment(b),
			     bp->object_type, new_obj, bpc->caller_data);
      } else {
	/* Add the element to the list. The cache will have already
	   been set up. */
	Gal_ListObjectAdd(bpc->obj, new_obj);
      }
    } else {
      __broker_proxy_discard_data(data, data_type, size);
    }
    break;
  case GAL_BINARY:
  case GAL_INT_16:
  case GAL_INT_32:
  case GAL_INT_64:
  case GAL_FLOAT_32:
  case GAL_FLOAT_64:
    if (bpc->immediate && bpc->data_handler) {
      new_obj = _GalIO_CreateObject(data, data_type, size);
      (*bpc->data_handler)(GalSS_BrokerGetEnvironment(b),
			   bp->object_type, new_obj, bpc->caller_data);
    } else if (!bpc->immediate) {
      /* Add the element to the buffer. The cache will
	 already have been set up. */
      /* Add the data to the object. The type has already been checked. */
      Gal_ArrayObjectAdd(bpc->obj, data, size);
      /* Don't forget to free the data! */
      /* Could be an array of length 0. */
      if (data) free(data);
    } else {
      __broker_proxy_discard_data(data, data_type, size);
    }
    break;
  case -1:
    /* This will always be immediate. */
    if (bpc->data_handler) {
      new_obj = _GalIO_CreateObject(data, data_type, size);
      (*bpc->data_handler)(GalSS_BrokerGetEnvironment(b),
			   bp->object_type, new_obj, bpc->caller_data);
    } else {
      __broker_proxy_discard_data(data, data_type, size);
    }
    break;
  default:
    break;
  }  
}

/* We have several layers of abstraction here. The first is just
   to unpackage the broker info. The function GalSS_EnvBrokerProxyInInit()
   uses this function. */

static GalIO_BrokerStruct *
__galss_set_up_receiving_broker(GalSS_Environment *env,
				GalSS_BrokerProxy *bp,
				GalIO_BrokerDataHandler fnptr,
				int poll_ms,
				void *refptr, void (*free_fn)(void *))
{
  GalIO_BrokerStruct *b;
  Gal_Frame f = Gal_MakeFrame("broker", GAL_CLAUSE);
  
  /* I should probably get rid of this reliance on frames,
     but I'm stuck with it for the moment because of the
     main broker API. */
  
  Gal_SetProp(f, ":call_id", Gal_StringObject(bp->call_id));
  
  b = GalSS_EnvBrokerDataInInit(env, bp->host, (unsigned short) bp->port, f,
				fnptr, poll_ms,
				refptr, free_fn);
  /* Free the frame, since it's copied on setup. */
  Gal_FreeFrame(f);
  return b;
}

/* The next level of abstraction is used both by GalSS_UnproxifyObject
   and GalSS_Unproxify. It uses the broker creation abstraction,
   and adds some stuff to it. In particular, we implement the
   generalization that streaming types should have stubs created
   before any callbacks are set up. This is in order to ensure
   that the cache will still have some integrity even if no
   data was read (i.e., length is 0). */   

static void __broker_proxy_done(GalIO_BrokerStruct *b, void *data);
static void __broker_proxy_abort(GalIO_BrokerStruct *b, void *data);

static __broker_proxy_callback *
__galss_set_up_receiving_broker_proxy(GalSS_Environment *env,
				      GalSS_BrokerProxy *bp,
				      int immediate,  
				      void *caller_data,
				      void (*caller_data_free_fn)(void *),
				      int poll_ms,
				      GalSS_ProxyDataHandler fn,
				      GalSS_ProxyDataEventHandler done_fn,
				      GalSS_ProxyDataEventHandler abort_fn,
				      int free_bpc)
{
  __broker_proxy_callback *bpc;
  GalIO_BrokerStruct *b;
  void (*bpc_free_fn)(void *);
  
  bpc = (__broker_proxy_callback *) calloc(1, sizeof(__broker_proxy_callback));
  bpc->caller_data = caller_data;
  bpc->caller_data_free_fn = caller_data_free_fn;
  bpc->data_handler = fn;
  bpc->done_handler = done_fn;
  bpc->abort_handler = abort_fn;
  bpc->immediate = immediate;
  bpc->bp = bp;

  /* Make sure that initial caches are set up for the streaming types. */

  switch (bp->object_type) {
  case GAL_LIST:
    if (!bpc->immediate) {
      /* We're cacheing the data. */
      bpc->obj = Gal_CreateListObject((Gal_Object *) NULL, 0,
				      _gal_free_object, 1);
    }
    break;
  case GAL_BINARY:
  case GAL_INT_16:
  case GAL_INT_32:
  case GAL_INT_64:
  case GAL_FLOAT_32:
  case GAL_FLOAT_64:
    if (!bpc->immediate) {
      /* We're cacheing the data. */
      bpc->obj = _GalIO_CreateObject((void *) NULL, bp->object_type, 0);
    }
    break;
  default:
	  break;
  }
  if (free_bpc)
    bpc_free_fn = __free_broker_proxy_callback;
  else
    bpc_free_fn = NULL;
  b = __galss_set_up_receiving_broker(env, bp, __broker_proxy_data_handler,
				      -1, (void *) bpc, bpc_free_fn);
  if (b) {
    bp->broker = b;
    bp->removal_cb = GalIO_AddBrokerCallback(b, GAL_BROKER_DESTRUCTION_EVENT,
					     __galss_remove_from_broker_proxy,
					     (void *) bp);
    GalIO_AddBrokerCallback(b, GAL_BROKER_DATA_DONE_EVENT,
			    __broker_proxy_done, (void *) NULL);
    GalIO_AddBrokerCallback(b, GAL_BROKER_ABORT_EVENT,
			    __broker_proxy_abort, (void *) NULL);
    GalIO_SetBrokerActive(b);
    GalSS_EnvStartBroker(env, b, poll_ms);
    return bpc;
  } else {
    __free_bpc(bpc);
    return (__broker_proxy_callback *) NULL;
  }
}

/* GalSS_UnproxifyObject: the simple way of undoing a proxy. If obj is
   not an inbound GAL_PROXY object of a single type, this function
   returns obj. If obj has already been unproxified and the resulting
   Gal_Object has been stored, return it (note that like Gal_GetObject,
   the proxy retains the ownership of the object itself).
   Otherwise, this function sets up an inbound broker, sucks down
   the data, creates a Gal_Object of the appropriate type, stores it in
   the broker for future reference, and returns it.
   If the inbound broker has already been set up, but no object
   has been found yet, that should never happen. If the inbound broker
   can't be set up, this function returns NULL. The obj retains the
   ownership of the returned object. And you can't unproxify an
   "any" stream with this function. */

static void __retrieve_obj(GalSS_Environment *env, Gal_ObjectType proxy_type,
			   Gal_Object elt, void *caller_data)
{
  GalSS_BrokerProxy *bp = (GalSS_BrokerProxy *) caller_data;
  
  bp->obj = elt;
}

Gal_Object
GalSS_UnproxifyObject(GalSS_Environment *env, GalSS_BrokerProxy *bp)
{
  if (!bp) {
    return (Gal_Object) NULL;
  }
  if (bp->object_type == (Gal_ObjectType) -1) {
    return (Gal_Object) NULL;
  }
  /* Copies might not be marked as already brokered. */
  if (bp->obj || bp->already_brokered) {
    bp->already_brokered = 1;
    return bp->obj;
  }
  if (bp->broker && bp->broker->in_b) {
    /* It's an already functioning inbound broker.
       This should never happen. */
    return (Gal_Object) NULL;
  } else {
    __broker_proxy_callback *bpc;
    GalIO_BrokerStruct *b;
    
    bpc = __galss_set_up_receiving_broker_proxy(env, bp, 0, (void *) bp,
						NULL, -1, __retrieve_obj,
						NULL, NULL, 0);
    if (bpc) {
      int res;

      b = bp->broker;
      
      /* Now, we try to get the data. */
      while (1) {
	res = GalIO_BrokerDataInCallbackHandler(b, 1);

	switch (res) {
	case 1:
	  /* done (broker destroyed), stop polling */
	  bp->already_brokered = 1;
	  __free_bpc(bpc);
	  return bp->obj;
	case 0:
	  /* not done, continue polling */
	  GalUtil_MilliSleep(1);
	  break;
	case -1:
	  /* error (broker destroyed), stop polling */
	default:
	  /* Should never happen */
	  __free_bpc(bpc);
	  bp->already_brokered = 1;
	  if (bp->obj) {
	    Gal_FreeObject(bp->obj);
	    bp->obj = (Gal_Object) NULL;
	  }
	  return (Gal_Object) NULL;
	}
      }
    } else {
      __free_bpc(bpc);
      return (Gal_Object) NULL;
    }
  }
}

Gal_Object GalSS_ObjUnproxifyObject(GalSS_Environment *env, Gal_Object obj)
{
  GalSS_BrokerProxy *bp;
  
  if (!Gal_Proxyp(obj))
    return (Gal_Object) NULL;
  bp = (GalSS_BrokerProxy *) obj->value;
  return GalSS_UnproxifyObject(env, bp);
}

/* GalSS_UnproxifyObjectWithCallback: The complex way of undoing a
   proxy. If obj is not an inbound GAL_PROXY object, this function has no
   effect. If it is a GAL_PROXY object, this function sets up an inbound
   broker. If immediate is 1, the inbound broker calls fn every time it
   reads data in from the broker; otherwise, it waits until the broker is
   done. Note that if the broker represents a stream which can take any
   type of data, immediate will be set to 1 no matter what's passed
   in. The other three arguments have the same meaning as they do for
   GalSS_EnvBrokerDataInInit. The object the callback is called with
   depends on the type of the stream. If it's anything besides -1 or
   GAL_LIST, it will be the type of the stream. Otherwise, it can be
   anything. If it's GAL_LIST and immediate, it will be the elements
   of the list; if it's GAL_LIST and not immediate, it will be the
   list itself.

   Each callback invocation creates an
   object and transfers the object reference to the callback invocation
   (i.e., the disposition is up to the user). */

/* We must make sure that the bpc lets go of the object in the
   appropriate circumstances. If there's a callback, we assume that
   the callback is disposing of the data, so we just let go.
   Otherwise, we want to free it and let go. GalSS_UnproxifyObject
   rescues the data by using a data handler. */

static void __broker_proxy_done(GalIO_BrokerStruct *b, void *data)
{
  __broker_proxy_callback *bpc = (__broker_proxy_callback *) GalIO_GetBrokerData(b);

  if (bpc->obj) {
    if ((!bpc->immediate) && bpc->data_handler) {
      (*bpc->data_handler)(GalSS_BrokerGetEnvironment(b),
			   bpc->bp->object_type,
			   bpc->obj, bpc->caller_data);
      /* If there's an object, dammit, assume that the data
	 handler has freed it. */
      bpc->obj = (Gal_Object) NULL;
    } else {
      Gal_FreeObject(bpc->obj);
      bpc->obj = (Gal_Object) NULL;
    }
  }
  if (bpc->done_handler) {
    (*bpc->done_handler)(GalSS_BrokerGetEnvironment(b),
			 bpc->bp->object_type,
			 bpc->caller_data);
  }
}


static void __broker_proxy_abort(GalIO_BrokerStruct *b, void *data)
{
  __broker_proxy_callback *bpc = (__broker_proxy_callback *) GalIO_GetBrokerData(b);

  /* Make absolutely sure the object is freed. */
  if (bpc->obj) {
    Gal_FreeObject(bpc->obj);
    bpc->obj = (Gal_Object) NULL;
  }
  if (bpc->abort_handler) {
    (*bpc->abort_handler)(GalSS_BrokerGetEnvironment(b),
			  bpc->bp->object_type,
			  bpc->caller_data);
  }
}

void GalSS_Unproxify(GalSS_Environment *env,
		     GalSS_BrokerProxy *bp,
		     GalSS_ProxyDataHandler fn,
		     GalSS_ProxyDataEventHandler done_fn,
		     GalSS_ProxyDataEventHandler abort_fn,
		     int immediate,
		     int poll_ms,
		     void *caller_data,
		     void (*caller_data_free_fn)(void *))
{
  if (bp->broker && bp->broker->in_b) {
    /* It's an already functioning inbound broker.
       This should never happen. */
    return;
  }

  if (bp->already_brokered)
    return;

  /* If it's an "any" stream, immediate is always true. */
  if (bp->object_type == (Gal_ObjectType) -1)
    immediate = 1;
  
  __galss_set_up_receiving_broker_proxy(env, bp, immediate, caller_data,
					caller_data_free_fn, poll_ms,
					fn, done_fn, abort_fn, 1);
}

void GalSS_ObjUnproxify(GalSS_Environment *env,
			Gal_Object obj,
			GalSS_ProxyDataHandler fn,
			GalSS_ProxyDataEventHandler done_fn,
			GalSS_ProxyDataEventHandler abort_fn,
			int immediate,
			int poll_ms,
			void *caller_data,
			void (*caller_data_free_fn)(void *))
{
  GalSS_BrokerProxy *bp;
  
  if (!Gal_Proxyp(obj))
    return;
  bp = (GalSS_BrokerProxy *) obj->value;
  GalSS_Unproxify(env, bp, fn, done_fn, abort_fn, immediate,
		  poll_ms, caller_data, caller_data_free_fn);
}

/* And finally, a backward compatibility function. Let's say you get
   a broker proxy, but you want to use the old callback method.
   Use this. */

GalIO_BrokerStruct *
GalSS_EnvBrokerProxyInInit(GalSS_Environment *env,
			   GalSS_BrokerProxy *bp,
			   GalIO_BrokerDataHandler fnptr,
			   int poll_ms,
			   void *refptr, void (*free_fn)(void *))
{
  if (!bp) {
    return (GalIO_BrokerStruct *) NULL;
  }
  
  if (bp->broker && bp->broker->in_b) {
    /* It's an already functioning inbound broker.
       This should never happen. */
    return (GalIO_BrokerStruct *) NULL;
  }

  return __galss_set_up_receiving_broker(env, bp, fnptr, poll_ms,
					 refptr, free_fn);
}


GalIO_BrokerStruct *
GalSS_EnvBrokerProxyObjInInit(GalSS_Environment *env,
			      Gal_Object obj,
			      GalIO_BrokerDataHandler fnptr,
			      int poll_ms,
			      void *refptr, void (*free_fn)(void *))
{
  GalSS_BrokerProxy *bp;
  
  if (!Gal_Proxyp(obj))
    return (GalIO_BrokerStruct *) NULL;
  bp = (GalSS_BrokerProxy *) obj->value;
  return GalSS_EnvBrokerProxyInInit(env, bp, fnptr, poll_ms,
				    refptr, free_fn);
}

/* For those library embeddings which don't use the ELR abstraction,
   such as Python and Allegro bindings, I need a polling function
   for the brokers, on both the inbound and outbound sides. */

int GalSS_BrokerProxyInCallbackHandler(GalSS_BrokerProxy *bp)
{
  if (bp->broker) {
    return GalIO_BrokerDataInCallbackHandler(bp->broker, 0);
  } else {
    /* If there ain't no broker on the inbound side, there
       ain't never gonna be no broker.*/
    return 1;
  }
}

/* This only works when there's something in the internal
   queue. You also need to register the socket with a socket callback. */

int GalSS_BrokerProxyReadReady(GalSS_BrokerProxy *bp)
{
  if (bp->broker) {
    return GalIO_BrokerReadReady(bp->broker);
  } else {
    return 0;
  }
}

GAL_SOCKET GalSS_GetBrokerProxySocket(GalSS_BrokerProxy *bp)
{
  if (bp->broker) {
    return GalIO_GetBrokerSocket(bp->broker);
  } else {
    return GAL_INVALID_SOCKET;
  }
}



