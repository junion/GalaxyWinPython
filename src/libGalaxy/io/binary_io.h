/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef __H_BINARY_IO_
#define __H_BINARY_IO_

#include "galaxy/galaxy.h"
#include "galaxy/galaxy_io.h"

/* Read functions */
int _galio_read_object_from_sockqueue(GalIO_SockObjectQueueStruct *queue, void **object, Gal_ObjectType *object_type, int *object_count, GalIO_MsgType *msg_type_ptr);
int _galio_read_reply_msg_from_sockqueue(GalIO_SockObjectQueueStruct *queue, Nframe *frame, GalIO_MsgType *msg_type_ptr, int server_tidx, int do_block);
int _galio_read_incoming_msg_from_sockqueue(GalIO_SockObjectQueueStruct *queue, Nframe *frame, int do_block);
int _galio_read_msg_from_sockqueue(GalIO_SockObjectQueueStruct *queue,
				   Gal_Frame *frame,
				   GalIO_MsgType *msg_type_ptr,
				   GalIO_MsgQueueTestFn test_fn,
				   void *client_data,
				   int blocking);

/* Write functions */
int _galio_write_msg_to_sockqueue(GalIO_SockObjectQueueStruct *queue, Nframe frame, GalIO_MsgType msg_type);
GalIO_SockObjectStruct *_galio_create_msg_sock_object(Gal_Frame frame,
						      GalIO_MsgType msg_type);
GalIO_SockObjectStruct *
_galio_create_sock_object(Gal_Object obj);
GalIO_SockObjectStruct *
_galio_create_typed_sock_object(Gal_ObjectType t, void *data, int size);

#endif /* #ifndef __H_BINARY_IO_ */ 
