/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef __H_IO_INTERNAL_
#define __H_IO_INTERNAL_

#include "galaxy/sysdep.h"

#ifndef WIN32
#include <sys/param.h>
#include <sys/time.h>
#else
#include <winsock2.h>
#include <time.h>
#endif

#define _GAL_LOCAL_HEADERS_

#include "galaxy/common_decls.h"
#include "../galaxy/gal_types_internal.h"
#include "galio_types_internal.h"
#include "galaxy/generic-server-types.h"
/* for backwards compatibility */
typedef Gal_Frame Nframe;
typedef Gal_Object TObj;
typedef Gal_Symbol Sym;

#include "galaxy/galaxy_io.h"

/* I don't want to wrestle with the same goddamn conditionalization
   in more than one place, but I only want to define the XDR
   stuff when we include this file in xdr_buffer.c. */

#ifdef __GALIO_XDR_BUFFER_C__

/* I need to know which of the XDR routines I can use to encode
   GAL_INT_16, etc. The same thing is determined in galaxy_io.h. Seems
   wrong to do it in two places. Some types may not be determined.
   See xdr_buffer.c. */

/* The geniuses who wrote Solaris 2.6 didn't include rpc/types
   in rcp/xdr. */
#ifndef WIN32
#include <rpc/types.h>
#endif
#include <rpc/xdr.h>

/* SAM 11/1/00: I seem to need to coerce these calls to xdrproc_t,
   even though they already should be that, at least on PPC linux. */

#endif /* __GALIO_XDR_BUFFER_C__ */

#if (SIZEOF_CHAR == 1)
typedef char _GalIO_binary;
#else
#error "need char to be 1 byte for binary data"
#endif

#if (SIZEOF_SHORT == 2)
typedef short _GalIO_int_16;
#ifdef __GALIO_XDR_BUFFER_C__
static xdrproc_t xdr_gal_int16 = (xdrproc_t) xdr_short;
#endif
#elif (SIZEOF_INT == 2)
typedef int _GalIO_int_16;
#ifdef __GALIO_XDR_BUFFER_C__
static xdrproc_t xdr_gal_int16 = (xdrproc_t) xdr_int;
#endif
#else
#error "need short or int to be 2 bytes for int16 data"
#endif

#if (SIZEOF_INT == 4)
typedef int _GalIO_int_32;
#ifdef __GALIO_XDR_BUFFER_C__
static xdrproc_t xdr_gal_int32 = (xdrproc_t) xdr_int;
#endif
#else
#error "need int to be 4 bytes for int32 data"
#endif

#if (SIZEOF_LONG == 8)
typedef long _GalIO_int_64;
#ifdef __GALIO_XDR_BUFFER_C__
static xdrproc_t xdr_gal_int64 = (xdrproc_t) xdr_long;
#endif
#elif ((SIZEOF_LONG_LONG == 8) && defined(HAVE_XDR_LONGLONG_T))
typedef long long _GalIO_int_64;
#ifdef __GALIO_XDR_BUFFER_C__
static xdrproc_t xdr_gal_int64 = (xdrproc_t) xdr_longlong_t;
#endif
#else
typedef void * _GalIO_int_64;
#ifdef __GALIO_XDR_BUFFER_C__
static xdrproc_t xdr_gal_int64 = (xdrproc_t) NULL;
#ifndef WIN32
#warning "platform doesn't support int64 data"
#endif
#endif
#endif

#if (SIZEOF_FLOAT == 4)
typedef float _GalIO_float_32;
#ifdef __GALIO_XDR_BUFFER_C__
static xdrproc_t xdr_gal_float32 = (xdrproc_t) xdr_float;
#endif
#else
#error "need float to be 4 bytes for float32 data"
#endif

#if (SIZEOF_DOUBLE == 8)
typedef double _GalIO_float_64;
#ifdef __GALIO_XDR_BUFFER_C__
static xdrproc_t xdr_gal_float64 = (xdrproc_t) xdr_double;
#endif
#else
#error "need double to be 8 bytes for float64 data"
#endif

#define GAL_DEFAULT_POLL_MS 5

/*
 *  SockQueue reads bytes from a socket as available and stores them
 *  in a buffer.  Bytes are written to a buffer and sent when the
 *  socket is not blocking.  Polling functions handle reading from
 *  and writing to the socket.  Neither the polling functions nor
 *  the SockQueue read/write functions block, but buffer overflow
 *  is still an issue.
 */

/* dump the buffers/clear error state -- for use when we lost peer, etc. */
void _GalIO_ClearSockQueue(GalIO_SockQueueStruct *q);
void _GalIO_ClearSockQueueError(GalIO_SockQueueStruct *q);

/* set the socket fd */
void _GalIO_SetSockQueueSocket(GalIO_SockQueueStruct *q, GAL_SOCKET sockfd);

/*
 *  SockObjectQueue is an object layer built on SockQueue.  It maintains
 *  queues of incoming and outgoing objects just as SockQueue buffers
 *  characters.
 */

/* returns a SockObject struct initialized for _GalIO_SockObjectQueueProcessIn or _GalIO_SockObjectQueueWrite */
GalIO_SockObjectStruct *_GalIO_NewSockObject(void);

/* Frees a SockObject */
void _GalIO_FreeSockObject(GalIO_SockObjectStruct *o);

/* returns a new SockObjectQueue struct */
GalIO_SockObjectQueueStruct *_GalIO_CreateSockObjectQueue(GAL_SOCKET sockfd, int in_size, int out_size);

/*  flush in, out, current_in, current_out, and sets socket to -1 */
void _GalIO_ClearSockObjectQueue(GalIO_SockObjectQueueStruct *queue);

/*  flush out, current_out */
void _GalIO_FlushSockObjectOutQueue(GalIO_SockObjectQueueStruct *queue);

/* Flush in, current_in */

void _GalIO_FlushSockObjectInQueue(GalIO_SockObjectQueueStruct *queue);

void _GalIO_ClearSockObjectQueueError(GalIO_SockObjectQueueStruct *queue);

void _GalIO_DestroySockObjectQueue(GalIO_SockObjectQueueStruct *queue);

/* sets sockfd in the SockQueue struct */
void _GalIO_SetSockObjectQueueSocket(GalIO_SockObjectQueueStruct *q, GAL_SOCKET sockfd);

/* returns 1 if success, 0 if no object, -1 if error */
int _GalIO_SockObjectQueueAppendOut(GalIO_SockObjectQueueStruct *queue, GalIO_SockObjectStruct *object);

/* returns non-zero if there's stuff waiting */
int _GalIO_SockObjectQueueInNonEmpty(GalIO_SockObjectQueueStruct *queue);

/* returns non-zero if there's stuff waiting */
int _GalIO_SockObjectQueueOutNonEmpty(GalIO_SockObjectQueueStruct *queue);

/* returns a SockObject initialized for _GalIO_SockObjectQueueProcessOut (bytes set to 0) */
/* returns SockObject if success, NULL otherwise */
GalIO_SockObjectStruct *_GalIO_SockObjectQueuePopOut(GalIO_SockObjectQueueStruct *queue);

/* reads from socket into SockQueue, appends objects to SockObjectQueue */
/* returns 1 if objects are queued, 0 if queue is empty, -1 if error */
int _GalIO_SockObjectQueueProcessIn(GalIO_SockObjectQueueStruct *oqueue);

/* pops objects from SockObjectQueue, writes from SockQueue to socket */
/* returns 1 if objects are queued, 0 if queue is empty, -1 if error */
int _GalIO_SockObjectQueueProcessOut(GalIO_SockObjectQueueStruct *oqueue);

typedef void
(*_GalIO_SockObjectQueuePostprocessFn)(GalIO_SockObjectQueueStruct *queue,
				       GalIO_SockObjectStruct **sos_ptr,
				       int result, void *client_data);

/* returns 1 if object read, 0 if no object, -1 if error */
int _GalIO_SockObjectQueueRead(GalIO_SockObjectQueueStruct *queue,
			       GalIO_SockObjectStruct **struct_ptr);

/* returns 1 if object read, 0 if no object, -1 if error */
int _GalIO_SockObjectQueueReadAndDecode(GalIO_SockObjectQueueStruct *queue,
					Gal_Frame *frame,
					GalIO_MsgType *msg_type_ptr,
					GalIO_MsgQueueTestFn test_fn,
					void *client_data,
					int blocking);

/* returns 1 if object queued, 0 if object written, -1 if error */
int _GalIO_SockObjectQueueWrite(GalIO_SockObjectQueueStruct *queue, void *data, GalIO_MsgType msg_type, int data_size, int free_data);

void _GalIO_SockObjectQueueImport(GalIO_SockObjectQueueStruct *target_queue,
				  GalIO_SockObjectQueueStruct *source_queue);

/* creates a sock object. */
GalIO_SockObjectStruct *_GalIO_CreateSockObject(void *data, GalIO_MsgType msg_type, int data_size, int free_data);

/* Creates an EOT object. */
GalIO_SockObjectStruct *_GalIO_CreateEOTSockObject();

void _GalIO_SockObjectMsgEnqueue(GalIO_SockObjectQueueStruct *queue,
				 GalIO_SockObjectStruct *sos,
				 void *decoded_data, 
				 Gal_ObjectType decoded_type,
				 int decoded_size);
void *_GalIO_SockObjectMsgDequeueIf(GalIO_SockObjectQueueStruct *queue,
				    int *size_ptr,
				    Gal_ObjectType *o_ptr,
				    GalIO_MsgType *t_ptr,
				    GalIO_MsgQueueTestFn test_fn,
				    void *client_data);
void *_GalIO_SockObjectMsgDequeue(GalIO_SockObjectQueueStruct *queue,
				  int *size_ptr,
				  Gal_ObjectType *o_ptr,
				  GalIO_MsgType *t_ptr);

GalIO_SockObjectStruct *_GalIO_SockObjectCreate(void *data, char *data_type, GalIO_MsgType msg_type, int data_size, int free_data);
int _GalIO_SockObjectQueueAppendAndProcess(GalIO_SockObjectQueueStruct *queue,
					   GalIO_SockObjectStruct *object);

/* broker_data.c */
int _GalIO_BrokerListenerHandshakeHandler(GalIO_ServerStruct *scomm,
					  GalIO_CommStruct *gcomm,
					  Gal_Frame input);
void _galio_create_broker_out_poll(GalIO_CommStruct *gcomm, GalIO_BrokerStruct *b,
				   void *caller_data);
void _galio_create_broker_in_poll(GalIO_CommStruct *gcomm, GalIO_BrokerStruct *b,
				  void *caller_data);

/* hub_server.c */

GalIO_PointerQueue *_GalIO_CreateConnectionQueue();
void _GalIO_ServerAddOutgoingBroker(GalIO_ServerStruct *scomm,
				    GalIO_BrokerStruct *b);
void _GalIO_ServerRemoveOutgoingBroker(GalIO_ServerStruct *scomm,
				       GalIO_BrokerStruct *b);
void _GalIO_CommLock(GalIO_CommStruct *gcomm);
void _GalIO_CommUnlock(GalIO_CommStruct *gcomm);

/* pointer_queue.c */
GalIO_PointerQueue *_GalIO_NewPointerQueue(int type, int volatile_queue,
					   int mutexable,
					   void (*free_fn));
void _GalIO_QueueEnqueue(GalIO_PointerQueue *queue,
			 void *element);
void *_GalIO_QueueDequeue(GalIO_PointerQueue *queue, void *element);
void *_GalIO_QueuePop(GalIO_PointerQueue *queue);
void _GalIO_QueueDestroy(GalIO_PointerQueue *queue);
int _GalIO_QueueNonEmpty(GalIO_PointerQueue *queue);
void _GalIO_QueueImport(GalIO_PointerQueue *target_queue,
			GalIO_PointerQueue *source_queue,
			void *(*copy_fn)(void *));
void *_GalIO_QueueNth(GalIO_PointerQueue *queue, int i);
void _GalIO_QueueApply(GalIO_PointerQueue *queue,
		       int (*fn)(void *data, void *caller_data),
		       void *caller_data);
void *_GalIO_QueueDequeueIf(GalIO_PointerQueue *queue,
			    int (*fn)(void *data, void *caller_data),
			    void *caller_data);
GalIO_PointerQueue *_GalIO_QueueCopy(GalIO_PointerQueue *source_queue,
				     void *(*copy_fn)(void *));
int _GalIO_QueueLength(GalIO_PointerQueue *queue);

/* xdr_buffer.c */

char *_GalIO_XDREncodeFrame(Gal_Frame fr, int *bufsizeptr);
char *_GalIO_XDREncodeObject(Gal_ObjectType t, void *data,
			     int size, int *bufposptr);
char *_GalIO_XDREncodeToplevel(Gal_Object o, int *bufsizeptr);

int _GalIO_XDRDecodeObject(char *buf, int object_size,
			   Gal_ObjectType *t_ptr,
			   void **data_ptr, int *size_ptr);
Gal_Object _GalIO_XDRDecodeToplevel(char *buf, int object_size);
Gal_Object _GalIO_CreateObject(void *data, Gal_ObjectType t, int size);

#endif /* #ifndef __H_IO_INTERNAL_ */
