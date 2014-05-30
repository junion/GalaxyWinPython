/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"
#include "io_internal.h"
#include "binary_io.h"

int _galio_read_object_from_sockqueue(GalIO_SockObjectQueueStruct *queue,
				      void **object,
				      Gal_ObjectType *object_type,
				      int *object_count,
				      GalIO_MsgType *msg_type_ptr)
{
  int res;
  Gal_ObjectType gal_type;
  GalIO_SockObjectStruct *sos = (GalIO_SockObjectStruct *) NULL;

  /* don't need these headaches */
  if (!object || !object_type || !object_count) {
    GalUtil_WarnWithLocation(__FUNCTION__, "No place to put result of decoding XDR string");
    return(-1);
  }

  res = _GalIO_SockObjectQueueRead(queue, &sos);
  
  if (res > 0) {
    int xdr_res;
    /* We can just decode. */
    xdr_res = _GalIO_XDRDecodeObject(sos->data, sos->size,
				     &gal_type,
				     object, object_count);
    if (!xdr_res) {
      GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't decode object from XDR string");
      if (queue->manage_memory)
	_GalIO_FreeSockObject(sos);
      return -1;
    }
    *object_type = gal_type;
    if (msg_type_ptr)
      *msg_type_ptr = sos->msg_type;
    
    /* free the object */
    if (queue->manage_memory)
      _GalIO_FreeSockObject(sos);

  }
  return(res);
}

/* These functions are the tests we use to test to see
   if we've found something in the queue. */

static int __GalIO_MsgIsNew(GalIO_MsgType msg_type,
			    void *decoded_data,
			    int decoded_size,
			    Gal_ObjectType decoded_type,
			    void *client_data)
{
  return ((decoded_type == GAL_FRAME) &&
	  (msg_type == GAL_MESSAGE_MSG_TYPE));
}

static int __GalIO_MsgIsReply(GalIO_MsgType msg_type,
			      void *decoded_data,
			      int decoded_size,
			      Gal_ObjectType decoded_type,
			      void *client_data)
{
  int server_tidx = (int) client_data;
  Gal_Frame f = (Gal_Frame) decoded_data;
  Gal_Frame admin_info;
  
  return ((decoded_type == GAL_FRAME) &&
	  ((msg_type == GAL_REPLY_MSG_TYPE) ||
	   (msg_type == GAL_DESTROY_MSG_TYPE) ||
	   (msg_type == GAL_ERROR_MSG_TYPE)) &&
	  ((server_tidx == -1) ||
	   ((admin_info = Gal_GetFrame(f, GAL_HUB_OPAQUE_DATA_FRAME_KEY)) &&
	    (server_tidx == Gal_GetInt(admin_info,
				       GAL_SERVER_TOKEN_INDEX_FRAME_KEY)))));
}

int _galio_read_incoming_msg_from_sockqueue(GalIO_SockObjectQueueStruct *queue,
					    Nframe *frame,
					    int blocking)
{
  return _galio_read_msg_from_sockqueue(queue, frame, (GalIO_MsgType *) NULL,
					__GalIO_MsgIsNew, (void *) NULL,
					blocking);
}

int _galio_read_reply_msg_from_sockqueue(GalIO_SockObjectQueueStruct *queue,
					 Nframe *frame,
					 GalIO_MsgType *msg_type_ptr,
					 int server_tidx,
					 int blocking)
{
  return _galio_read_msg_from_sockqueue(queue, frame, msg_type_ptr,
					__GalIO_MsgIsReply,
					(void *) server_tidx,
					blocking);
}

/* At the moment, I will only use the message queue in
   the sock object queue for frames (incoming and
   outgoing messages). Later, I will be able to use
   the client data to pass in a tidx to match, when I
   move tidx generation into the server wrappers. At some
   point, I may find a use for the queueing for
   broker objects, but at this point it won't be
   necessary.

   This function reads through the message queue, looking
   for a matching frame. If not present, it reads from
   the sock object queue. If the element it reads from the
   sock object queue also doesn't match, it enqueues it
   in the message queue, sleeps briefly, and tries again.
   The reason for this is that if there are other threads which
   are also reading from the same connection, we want them
   to get access as quickly as possible to these elements.
   This raises the possibility that elements will be
   considered more than once if they're still in the
   message queue the next time around, but that's not going
   to be a tremendous overhead if the numbers are small
   and the tests are quick.
*/

int _galio_read_msg_from_sockqueue(GalIO_SockObjectQueueStruct *queue,
				   Gal_Frame *frame,
				   GalIO_MsgType *msg_type_ptr,
				   GalIO_MsgQueueTestFn test_fn,
				   void *client_data,
				   int blocking)
{
  if (!frame) {
    GalUtil_WarnWithLocation(__FUNCTION__, "No place to put result of decoding XDR string");
    return(0);
  }

  return _GalIO_SockObjectQueueReadAndDecode(queue, frame, msg_type_ptr,
 					     test_fn, client_data, blocking);
}

/*
 *  Write functions
 */

/* SAM 10/12/99: The MIT folks have pointed out to me that the
   function Gal_PrFrameToString may very well return a string of
   length 0, which will generate an empty frame on the other
   side of the connection, which is a very bad thing, because
   (for instance) the code for invoking server functions will
   seg fault under those circumstances. So I will cut off this
   problem right here. */

GalIO_SockObjectStruct *_galio_create_msg_sock_object(Gal_Frame frame,
						      GalIO_MsgType msg_type)
{
  char *object = NULL;
  int object_size = 0;
  int len;

  object = _GalIO_XDREncodeFrame(frame, &object_size);
  if (!object) {
    GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't encode frame into XDR string");
    return(GalIO_SockObjectStruct *) NULL;
  }
  len = object_size;
  if (len == 0) {
    free(object);
    GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't encode frame into XDR string");
    return (GalIO_SockObjectStruct *) NULL;
  } else {
    return _GalIO_CreateSockObject(object, msg_type, len, 1);
  }
}

int _galio_write_msg_to_sockqueue(GalIO_SockObjectQueueStruct *queue, Nframe frame,
				  GalIO_MsgType msg_type)
{
  GalIO_SockObjectStruct *sock_obj;

  sock_obj = _galio_create_msg_sock_object(frame, msg_type);
  if (!sock_obj) {
    return -1;
  } else {
    return _GalIO_SockObjectQueueAppendAndProcess(queue, sock_obj);
  }
}

GalIO_SockObjectStruct *
_galio_create_sock_object(Gal_Object obj)
{
  int len;
  char *s = (char *) NULL;
  
  s = _GalIO_XDREncodeToplevel(obj, &len);
  if (!s)
    return (GalIO_SockObjectStruct *) NULL;
  return _GalIO_CreateSockObject(s, GAL_OBJECT_MSG_TYPE, len, 1);
}

GalIO_SockObjectStruct *
_galio_create_typed_sock_object(Gal_ObjectType t, void *data, int size)
{
  int len;
  char *s = (char *) NULL;
  
  s = _GalIO_XDREncodeObject(t, data, size, &len);
  if (!s)
    return (GalIO_SockObjectStruct *) NULL;
  return _GalIO_CreateSockObject(s, GAL_OBJECT_MSG_TYPE, len, 1);
}
