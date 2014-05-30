/*
  This file (c) Copyright 1998 - 2000 M.I.T.
            (c) Copyright 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* There are some common declarations which are shared among
   a set of headers in io/, galaxy/ and ServerStub/. I'll
   declare the basics here and ensure that they're all loaded
   later. */

#ifndef _COMMON_DECLS_H
#define _COMMON_DECLS_H

/* From galaxy/ */

struct _gal_frame;
struct _gal_obj;
struct _gal_sym;
struct _gal_ht;

/* Gal_Object types. Hard wire the values for backwards compatibility */
typedef enum
{
  GAL_FREE = 0,
  GAL_FRAME,
  GAL_STRING,
  GAL_INT,
  GAL_FLOAT,
  GAL_SYMBOL,
  GAL_LIST,
  GAL_PTR,
  GAL_TOPIC_FRAME,
  GAL_CLAUSE_FRAME,
  GAL_PRED_FRAME,
  GAL_BINARY,
  GAL_INT_16,
  GAL_INT_32,
  GAL_INT_64,
  GAL_FLOAT_32,
  GAL_FLOAT_64,
  GAL_KEYWORD,
  GAL_TAG,
  GAL_TOKEN,
  GAL_PROXY
} Gal_ObjectType;

/* Referenced in the declaration of GalIO_ServerStruct.
   Resolved when galaxy.h is loaded, and it's always loaded
   when this header is loaded, since this header is only
   loaded from galaxy.h. */

struct __gal_dispatch_fn_pkg;

/* From io/ */

struct __GalIO_PointerQueue;
struct __GalIO_ServerStruct;
struct __GalIO_CommStruct;
struct __GalIO_BrokerStruct;
struct __GalIO_ServerLocation;
struct __GalIO_SockQueueStruct;
struct __GalIO_Callback;

#include "galaxy/io_msg_types.h"

/* From ServerStub/ */

struct __galss_environment;

/* Now, we should be able to load in the function declarations based
   on these, and then the definitions of the types. */

/* Function declarations from io/ */

typedef int 
(*GalIO_FrameHandler)(struct __GalIO_CommStruct *gcomm,
		      struct _gal_frame *frame);

typedef int
(*GalIO_MsgQueueTestFn)(GalIO_MsgType msg_type,
			void *decoded_data,
			int decoded_size,
			Gal_ObjectType decoded_type,
			void *client_data);

typedef int 
(*GalIO_FrameReader)(struct __GalIO_CommStruct *gcomm,
		     struct _gal_frame **frame_ptr,
		     GalIO_MsgType *msg_type_ptr,
		     GalIO_MsgQueueTestFn test_fn,
		     void *client_data);

typedef int 
(*GalIO_FrameWriter)(struct __GalIO_CommStruct *gcomm,
		     struct _gal_frame *f,
		     GalIO_MsgType t, int do_blocking);

typedef struct _gal_frame *
(*GalIO_FrameDispatcher)(struct __GalIO_CommStruct *gcomm,
			 struct _gal_frame *frame, GalIO_MsgType *t);


typedef void
(*GalIO_ConnectionCallbackFn)(struct __GalIO_CommStruct *, void *);

typedef void
(*GalIO_ServerCallbackFn)(struct __GalIO_ServerStruct *, void *);

typedef void
(*GalIO_ServerConnectCallbackFn)(struct __GalIO_ServerStruct *,
				 struct __GalIO_CommStruct *, void *);

typedef void 
(*GalIO_BrokerDataHandler)(struct __GalIO_BrokerStruct *broker_struct,
			   void *object, Gal_ObjectType object_type,
			   int object_count);

typedef void 
(*GalIO_BrokerDataFinalizer)(struct __GalIO_BrokerStruct *broker_struct,
			     void *caller_data);


typedef void
(*GalIO_ConnectionBrokerCallbackFn)(struct __GalIO_CommStruct *,
				    struct __GalIO_BrokerStruct *,
				    void *);

typedef void
(*GalIO_ConnectionDispatchFnCallbackFn)(struct __galss_environment *,
					struct _gal_frame *,
					void *);

typedef void 
(*GalIO_BrokerCallbackFn)(struct __GalIO_BrokerStruct *, void *);

/* Now, load in the proper declarations. For backward compatibility,
   we hide the structure declarations for io/ and galaxy/. */

#ifndef _GAL_LOCAL_HEADERS_

/* From galaxy/ */

typedef struct _gal_frame *Gal_Frame;
typedef struct _gal_obj *Gal_Object;
typedef struct _gal_sym *Gal_Symbol;
typedef struct _gal_ht *Gal_HashTable;

/* From io/ */

typedef struct __GalIO_PointerQueue GalIO_PointerQueue;
typedef struct __GalIO_ServerStruct GalIO_ServerStruct;
typedef struct __GalIO_CommStruct GalIO_CommStruct;
typedef struct __GalIO_BrokerStruct GalIO_BrokerStruct;
typedef struct __GalIO_ServerLocation GalIO_ServerLocation;
typedef struct __GalIO_SockQueueStruct GalIO_SockQueueStruct;
typedef struct __GalIO_Callback GalIO_Callback;

#include "galaxy/generic-server-types.h"

/* for backwards compatibility */
typedef Gal_Frame Nframe;
typedef Gal_Object TObj;
typedef Gal_Symbol Sym;

#endif /* _GAL_LOCAL_HEADERS_ */

#endif
