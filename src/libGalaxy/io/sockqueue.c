/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include <errno.h>

/* The geniuses who wrote Solaris 2.6 didn't include rpc/types
   in rcp/xdr. */
#ifndef WIN32
#include <rpc/types.h>
#endif
#include <rpc/xdr.h>

#include "io_internal.h"

static char *__GalIO_MsgTypeString(GalIO_MsgType msg_type);

#ifndef HAVE_STRERROR_R
/* Just cross your fingers and pray that strerror is thread_safe. */
static char *strerror_r(int errnum, char *buf, int buflen)
{
	return strncpy(buf, _gal_strerror(errnum), buflen);
}
#endif

/* Lowest ascii printable is ' ' */
#define LOW_PRINTABLE ' '
/* Highest ascii printable is '~' */
#define HIGH_PRINTABLE '~'

static GalIO_SockObjectStruct *__GalIO_SockObjectQueuePopIn(GalIO_SockObjectQueueStruct *queue);

void __galio_format_bytes(int queue_len, char *queue);

void __galio_format_queue(int queue_len, char *queue)
{
  /* This function will do the "right thing" with non
     printing characters. */
  GalUtil_Print(-1,"Queue contents: ");
  __galio_format_bytes(queue_len, queue);
}

void __galio_format_bytes(int queue_len, char *queue)
{
  int i;
  for (i = 0; i < queue_len; i++) {
    if ((queue[i] < LOW_PRINTABLE) ||
	(queue[i] > HIGH_PRINTABLE)) {
      GalUtil_Print(-1,"\\%.3o", (unsigned char) queue[i]);
    } else {
      GalUtil_Print(-1,"%c", queue[i]);
    }
  }
  GalUtil_Print(-1,"\n");
}

/*
 *  SockQueue reads bytes from a socket as available and stores them
 *  in a buffer.  Bytes are written to a buffer and sent when the
 *  socket is not blocking.  Polling functions handle reading from
 *  and writing to the socket.  Neither the polling functions nor
 *  the SockQueue read/write functions block, but buffer overflow
 *  is still an issue.
 */

GalIO_SockQueueStruct *GalIO_CreateSockQueue(GAL_SOCKET sockfd, int in_size, int out_size)
{
  GalIO_SockQueueStruct *q = (GalIO_SockQueueStruct *)calloc(1,sizeof(GalIO_SockQueueStruct));

  GalUtil_Assert(q != 0,"Error alloc'ing in _GalIO_CreateSockQueue");

  q->sockfd = sockfd;
  q->read_blocking = 0;
  q->error = 0;
  q->read_blocking = 0;
  
  q->in_len = 0;
  q->max_in = in_size;
  q->in_queue = malloc(in_size);
  
  q->out_len = 0;
  q->max_out = out_size;
  q->out_queue = malloc(out_size);

  return q;
}

void GalIO_DestroySockQueue(GalIO_SockQueueStruct *q)
{
  if(q)
  {
    free(q->in_queue);
    free(q->out_queue);
    free(q);
  }
}

void _GalIO_ClearSockQueue(GalIO_SockQueueStruct *q)
{
  if (q)
    q->in_len = q->out_len = 0;
}

void _GalIO_ClearSockQueueError(GalIO_SockQueueStruct *q)
{
  if (q)
    q->error = 0;
}

void _GalIO_SetSockQueueSocket(GalIO_SockQueueStruct *q, GAL_SOCKET sockfd)
{
  if (q)
    q->sockfd = sockfd;
}

void GalIO_SetSockQueueComment(GalIO_SockQueueStruct *q, char *comment)
{
  if (q)
    q->comment = comment;
}

int GalIO_GetSockQueueError(GalIO_SockQueueStruct *q)
{
  if (q)
    return(q->error);
  return(-1);
}

/* returns 1 if samples in queue, 0 if all samples sent, -1 if error */

int GalIO_SockQueueProcessSend(GalIO_SockQueueStruct *q)
{
  int n;
  char errbuf[256];

  if (q == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to write to");
    return(-1);
  }

  /* if the socket isn't open don't generate an error */
  if (q->sockfd == GAL_INVALID_SOCKET)
  {
    if (q->out_len) {
      return 1;
    } else
      return 0;
  }

  if (q->error)
  {
    return(-1);
  }

  if (q->out_len) {
    GalUtil_Debug2("%s: send queue: %d\n",__FUNCTION__,q->out_len);
    if (GAL_VERBOSE >= GAL_DEBUG2_LEVEL) {
      __galio_format_queue(q->out_len, q->out_queue);
    }
    /* Make sure the socket is nonblocking. */
    GalUtil_SockBlockOff(q->sockfd);
    
    n = send(q->sockfd, q->out_queue, q->out_len, 0);

    if(n < 0) {
#ifdef WIN32
      int err = WSAGetLastError();

      if (err == WSAEWOULDBLOCK) {
    	  if (q->out_len) return 1;
	      else return 0;
      }
  
      q->error = err;
      GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Encountered socket error %d (%d) while writing to queue", err, q->sockfd);
      return -1;
#else
      if (GAL_SOCKET_ERRNO == EWOULDBLOCK) {
    	  if(q->out_len)
	        return 1;
	      else
	      return 0;
      }

      q->error = GAL_SOCKET_ERRNO;
      strerror_r(GAL_SOCKET_ERRNO, errbuf, 256);
      GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Encountered socket error %s while writing to queue", errbuf);
      return(-1);
#endif
    }
    else if(n == 0) {
      q->error = EPIPE;
      strerror_r(EPIPE, errbuf, 256);
      GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Encountered socket error %s while writing to queue", errbuf);

      return(-1);
    } 
    else {
      GalUtil_Debug2("%s: send queue: %d bytes written\n",__FUNCTION__,n);
    }

    q->out_len -= n;

    _gal_bcopy(q->out_queue+n,q->out_queue,q->out_len);
    if(q->out_len)
      return 1;
    else
      return 0;
  } else
    return 0;
}

/* returns 1 if samples in queue, 0 if queue is empty, -1 if error */

int GalIO_SockQueueProcessReceive(GalIO_SockQueueStruct *q)
{
  int n;
#ifndef WIN32
  char errbuf[256];
#endif /* WIN32 */

  if (q == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to read from");
    return(-1);
  }

  if (q->error)
  {
    return(-1);
  }

  if (q->in_len < q->max_in)
  {
    /* hmmmmm.....  someone somewhere seems to be turning blocking on.
     * This fixes it for good. */
    if (q->read_blocking)
      GalUtil_SockBlockOn(q->sockfd);
    else
      GalUtil_SockBlockOff(q->sockfd);
    
    n = recv(q->sockfd, &q->in_queue[q->in_len], q->max_in - q->in_len, 0);

    if(n < 0) {
#ifdef WIN32
      int err = WSAGetLastError();
      if (err == WSAEWOULDBLOCK) {
	      if (q->in_len) return 1;
	      else return 0;
      }

      q->error = GAL_SOCKET_ERRNO;
      GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Encountered socket error %d (%d) while reading from queue", err, q->sockfd);
      if (q->comment)
	GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Comment was %s", q->comment);
      return -1;
#else
      if (GAL_SOCKET_ERRNO == EWOULDBLOCK) {
	      if (q->in_len)
	        return 1;
	      else
	        return 0;
      }

      q->error = GAL_SOCKET_ERRNO;
      strerror_r(GAL_SOCKET_ERRNO, errbuf, 256);
      GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Encountered socket error %s while reading from queue", errbuf);
      if(q->comment)
	GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Comment was %s", q->comment);
      return(-1);
#endif
    }
    if(n == 0) {
      q->error = EPIPE;
      GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Socket error: connection disrupted or closed (errno = %d, q->in_len = %d)\n",
		 GAL_SOCKET_ERRNO, q->in_len);
      if (q->comment)
	GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Comment was %s", q->comment);
      return(-1);
    } else {
      GalUtil_Debug2("%s: receive queue: %d bytes read\n",__FUNCTION__,n);
      if (GAL_VERBOSE >= GAL_DEBUG2_LEVEL) {
	__galio_format_queue(n, &q->in_queue[q->in_len]);
      }
    }

    q->in_len += n;

    if(q->in_len)
      return 1;
    else
      return 0;
  }
  /* queue is full */
  return 1;
}

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif
/* returns 0 if success, -1 if insufficient space in queue */

int GalIO_SockQueueWrite(GalIO_SockQueueStruct *q, void *data, long len)
{
  if (q == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to write to");
    return(-1);
  }

  /* in case of overflow, nothing gets written to the queue */
  if(q->max_out < (len + q->out_len)) {

    /* try to resize it */
    /* 9/98 used to be MIN(2*max_out, max_out+len) - this appears to be a bug in case where len > max_out */
    /* changed to MAX(2*max_out, out_len+len) i.e. out_len+len is minimum, max_out*2 reduces tiny growth */
    if(!q->do_not_expand_out) {
      long newsz = MAX(2*q->max_out,q->out_len + len);
      char *newq = realloc(q->out_queue,newsz);
      GalUtil_Assert(newq != 0, "%s: failed to resize queue", __FUNCTION__);
      q->max_out = newsz;
      q->out_queue = newq;
    }
  }

  if(q->max_out - q->out_len < len) {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Write queue overflowed");
    return(-1);
  }

  /* write data to the queue */
  _gal_bcopy(data,q->out_queue+q->out_len,len);
  q->out_len += len;
  return 0;
}

/* returns 0 if no data, 1 if data, -1 if error */
/* this isn't very efficient if characters are read out of the queue one at a time. */

int GalIO_SockQueueRead(GalIO_SockQueueStruct *q, char *data, int max_data_len, int *data_len)
{
  int i;

  if (q == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to read from");
    return -1;
  }

  i = q->in_len;
  *data_len = 0;
  if(i > max_data_len)
    i = max_data_len;
  if(!i)
    return 0;

  /* if no buffer is supplied the atom is flushed */
  if (data)
    _gal_bcopy(q->in_queue,data,i);
  q->in_len -= i;

  /* shift the remaining data in the queue */
  _gal_bcopy(q->in_queue+i,q->in_queue,q->in_len);
  *data_len = i;
  return 1;
}

/* returns 0 if complete atom cannot be read, 1 if atom complete, -1 if error */

int GalIO_SockQueueReadAtom(GalIO_SockQueueStruct *q, char *data, int data_len)
{
  if (q == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to read atom from");
    return(-1);
  }

  if(q->in_len < data_len)
    return 0;

  /* if no buffer is supplied the atom is flushed */
  if (data)
    _gal_bcopy(q->in_queue,data,data_len);
  q->in_len -= data_len;

  /* shift the remaining data in the queue */
  _gal_bcopy(q->in_queue+data_len,q->in_queue,q->in_len);

  return 1;
}

/*
 *  SockObjectQueue is an object layer built on SockQueue.  It maintains
 *  queues of incoming and outgoing objects just as SockQueue buffers
 *  characters.
 */

/* This checks and digests the type and length from the header.
   returns 1 if header is okay, 0 if EOT, -1 if error.
   11/13/00: I will no longer be able to return EOT, because
   I can't distinguish between a failure to read from the buffer
   because of illformed input and a failure to find enough data. */

static int check_header(char *header, int *msg_type, int* data_size)
{
  XDR xdr_s;
  
  xdrmem_create(&xdr_s, header, 8, XDR_DECODE);

  if ((xdr_int(&xdr_s, msg_type) == FALSE) ||
      (xdr_int(&xdr_s, data_size) == FALSE)) {
    return -1;
  } else {
    return 1;
  }
}

/* returns a SockObject initialized for _GalIO_SockObjectQueueProcessIn or _GalIO_SockObjectQueueWrite */

GalIO_SockObjectStruct *_GalIO_NewSockObject(void)
{
  GalIO_SockObjectStruct *o = (GalIO_SockObjectStruct *)calloc(1,sizeof(GalIO_SockObjectStruct));

  if (o == NULL)
    return NULL;

  o->header[0] = 0;
  o->data = NULL;
  o->size = 0;

  o->bytes = 0;
  o->bytes_pending = HEADER_SIZE;
  o->cptr = o->header;

  return o;
}

void _GalIO_FreeSockObject(GalIO_SockObjectStruct *o)
{
  if (o)
  {
    if (o->data)
      free(o->data);
    free(o);
  }
}

/* This is how we create the sock object in and out pointer queues.
   The out queue is mutexable on its own, but not the in queue. */

static GalIO_PointerQueue *__GalIO_CreateSockObjectSequence(int mutexable)
{
  return _GalIO_NewPointerQueue(GAL_SOCKQUEUE_PTYPE,
				1, mutexable, NULL);
}

/* SAM 4/23/02: Added a mutex to the msg_queue, so it has its own. */

GalIO_SockObjectQueueStruct *_GalIO_CreateSockObjectQueue(GAL_SOCKET sockfd, int in_size, int out_size)
{
  GalIO_SockObjectQueueStruct *q = (GalIO_SockObjectQueueStruct *)calloc(1,sizeof(GalIO_SockObjectQueueStruct));

  GalUtil_Assert(q != 0,"Error malloc'ing in _GalIO_CreateSockObjectQueue");

  q->queue = GalIO_CreateSockQueue(sockfd, in_size, out_size);
  q->error = 0;

  /* reading objects from the socket */
  q->in = __GalIO_CreateSockObjectSequence(0);

  /* writing objects to the socket */
  q->out = __GalIO_CreateSockObjectSequence(1);
  q->queue_mutex = GalUtil_CreateLocalMutex();
  q->msg_queue = _GalIO_NewPointerQueue(GAL_SOCK_OBJECT_PTYPE, 1, 1, NULL);
  q->manage_memory = 1;

  return q;
}

/*  clears error status */

void _GalIO_ClearSockObjectQueueError(GalIO_SockObjectQueueStruct *queue)
{
  if (queue)
  {
    _GalIO_ClearSockQueueError(queue->queue);
    queue->error = 0;
  }
}


/*  flush in, out, current_in, current_out, and sets socket to -1 */

void _GalIO_ClearSockObjectQueue(GalIO_SockObjectQueueStruct *queue)
{
  if (queue)
  {
    _GalIO_FlushSockObjectInQueue(queue);    
    _GalIO_FlushSockObjectOutQueue(queue);

    _GalIO_ClearSockQueue(queue->queue);
    _GalIO_ClearSockObjectQueueError(queue);
    _GalIO_SetSockQueueSocket(queue->queue, GAL_INVALID_SOCKET);
  }
}

/* Flush in, current_in */

void _GalIO_FlushSockObjectInQueue(GalIO_SockObjectQueueStruct *queue)
{
  GalIO_SockObjectStruct *flush;

  if (queue) {    
    if (queue->manage_memory)
      _GalIO_FreeSockObject(queue->current_in);
    queue->current_in = NULL;
    while ((flush = __GalIO_SockObjectQueuePopIn(queue)))
      if (queue->manage_memory)
	_GalIO_FreeSockObject(flush);
  }
}


/*  flush out, current_out */
void _GalIO_FlushSockObjectOutQueue(GalIO_SockObjectQueueStruct *queue)
{
  GalIO_SockObjectStruct *flush;

  if (queue)
  {

    if (queue->manage_memory)
      _GalIO_FreeSockObject(queue->current_out);
    queue->current_out = NULL;
    while ((flush = _GalIO_SockObjectQueuePopOut(queue)))
      if (queue->manage_memory)
	_GalIO_FreeSockObject(flush);

  }
}

void _GalIO_DestroySockObjectQueue(GalIO_SockObjectQueueStruct *queue)
{
  _GalIO_ClearSockObjectQueue(queue);  

  if (queue)
  {
    _GalIO_QueueDestroy(queue->in);
    _GalIO_QueueDestroy(queue->out);
    _GalIO_QueueDestroy(queue->msg_queue);
    GalIO_DestroySockQueue(queue->queue);
    GalUtil_DestroyLocalMutex(queue->queue_mutex);
    free(queue);
  }
}

/* sets the socket fd in the SockQueue struct */

void _GalIO_SetSockObjectQueueSocket(GalIO_SockObjectQueueStruct *q, GAL_SOCKET sockfd)
{
  if (q)
    _GalIO_SetSockQueueSocket(q->queue, sockfd);
}

/* This better not fail. We want to take all the sock objects in the
   source queue and enqueue them in the target. Right now I'm using
   this only for outbound brokering. This will work because
   we're copying to a queue which isn't managing memory. */

void _GalIO_SockObjectQueueImport(GalIO_SockObjectQueueStruct *target_queue,
				  GalIO_SockObjectQueueStruct *source_queue)
{
  if ((!target_queue) || (!source_queue))
    return;

  /* Lock both queues. */
  GalUtil_LockLocalMutex(target_queue->queue_mutex);
  GalUtil_LockLocalMutex(source_queue->queue_mutex);
  /* If the source has incoming material and the target has
     a queue, copy. */
  if (_GalIO_QueueNonEmpty(source_queue->in) && target_queue->in &&
      (source_queue->in->type == GAL_SOCKQUEUE_PTYPE) &&
      (target_queue->in->type == GAL_SOCKQUEUE_PTYPE)) {
    _GalIO_QueueImport(target_queue->in, source_queue->in, NULL);
  }
  /* Ditto outgoing. */
  if (_GalIO_QueueNonEmpty(source_queue->out) && target_queue->out &&
      (source_queue->out->type == GAL_SOCKQUEUE_PTYPE) &&
      (target_queue->out->type == GAL_SOCKQUEUE_PTYPE)) {
    _GalIO_QueueImport(target_queue->out, source_queue->out, NULL);
  }
  /* Unlock both queues. */
  GalUtil_UnlockLocalMutex(target_queue->queue_mutex);
  GalUtil_UnlockLocalMutex(source_queue->queue_mutex);
}
    

/* returns 1 if success, 0 if no object, -1 if error */

/* Must be called with mutex locked. */

static int __GalIO_SockObjectQueueAppendIn(GalIO_SockObjectQueueStruct *queue, GalIO_SockObjectStruct *object)
{
  if ((!queue) || (!queue->in)) {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to append input to");
    return(-1);
  }

  if (!object)
    return 0;

  if (queue->in->type != GAL_SOCKQUEUE_PTYPE) {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Wrong queue type");
    return -1;
  }

  _GalIO_QueueEnqueue(queue->in, (void *) object);

  return 1;
}

/* returns 1 if success, 0 if no object, -1 if error.
   Needs no separate mutex; the out queue has a mutex of its own. This
   is so we can enqueue while the main mutex is locked. */

int _GalIO_SockObjectQueueAppendOut(GalIO_SockObjectQueueStruct *queue, GalIO_SockObjectStruct *object)
{
  if ((!queue) || (!queue->out)) {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to append output to");
    return(-1);
  }

  if (!object)
    return 0;

  if (queue->out->type != GAL_SOCKQUEUE_PTYPE) {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Wrong queue type");
    return -1;
  }
  
  _GalIO_QueueEnqueue(queue->out, (void *) object);

  return 1;
}

/* returns true if stuff in queue. I probably ought to
   lock the mutex, but by the time the query returns, it might
   not be accurate anymore. */

int _GalIO_SockObjectQueueInNonEmpty(GalIO_SockObjectQueueStruct *queue)
{
  return queue && queue->in && (_GalIO_QueueNonEmpty(queue->in) || _GalIO_QueueNonEmpty(queue->msg_queue));
}

int _GalIO_SockObjectQueueOutNonEmpty(GalIO_SockObjectQueueStruct *queue)
{
  return queue && queue->out &&
    (_GalIO_QueueNonEmpty(queue->out) ||
     queue->current_out ||
     queue->queue->out_len);
}

/* returns SockObject if success, NULL otherwise */

static GalIO_SockObjectStruct *__GalIO_SockObjectQueuePopIn(GalIO_SockObjectQueueStruct *queue)
{
  if ((!queue) || (!queue->in))
    return NULL;

  if (queue->in->type != GAL_SOCKQUEUE_PTYPE)
    return NULL;

  return (GalIO_SockObjectStruct *) _GalIO_QueuePop(queue->in);
}

/* returns a SockObject initialized for _GalIO_SockObjectQueueProcessOut (bytes set to 0) */
/* returns SockObject if success, NULL otherwise */

GalIO_SockObjectStruct *_GalIO_SockObjectQueuePopOut(GalIO_SockObjectQueueStruct *queue)
{
  GalIO_SockObjectStruct *object;

  if ((!queue) || (!queue->out))
    return NULL;

  if (queue->out->type != GAL_SOCKQUEUE_PTYPE)
    return NULL;

  object = (GalIO_SockObjectStruct *) _GalIO_QueuePop(queue->out);
  
  if (!object)
    return NULL;

  object->bytes = 0;
  return (object);
}

/* reads from socket into SockQueue, appends objects to SockObjectQueue */
/* returns 1 if objects are queued, 0 if queue is empty, -1 if error */

static int __GalIO_SockObjectQueueProcessInNoLock(GalIO_SockObjectQueueStruct *oqueue)
{
  GalIO_SockQueueStruct *squeue;
  GalIO_SockObjectStruct *current;
  int res, bytes_read, header_ok;
  char errbuf[256];

  if (oqueue == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to read from");
    return(-1);
  }

  if (oqueue->done)
  {
    if (_GalIO_QueueNonEmpty(oqueue->in))
      return(1);
    else
      return(0);
  }

  squeue = oqueue->queue;
  if (squeue == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to read from");
    return(-1);
  }

  res = GalIO_SockQueueProcessReceive(squeue);
  if (res < 0)
  {
    if (!oqueue->error)
      GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Encountered socket error while reading from queue");
    oqueue->error = squeue->error;
    return(-1);
  }

  if (oqueue->current_in == NULL)
    oqueue->current_in = _GalIO_NewSockObject();

  current = oqueue->current_in;

  if (current == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Found NULL current object while reading from queue");
    return(-1);
  }

  while(GalIO_SockQueueRead(squeue,current->cptr,current->bytes_pending,&bytes_read))
  {
    if(!bytes_read)
      break;

    current->bytes += bytes_read;
    current->cptr += bytes_read;
    current->bytes_pending -= bytes_read;

    if(current->bytes == HEADER_SIZE) {
      /* header */
      current->header[HEADER_SIZE] = 0;
      header_ok = check_header(current->header, (int *) &(current->msg_type),
			       &(current->size));
      if (header_ok < 0)
      {
	GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Found invalid header %s while reading from queue", current->header);
	return(-1);
      }
      else if (header_ok == 0)
      {
	/* This is called if there's something wrong with
	   the header. We should return a read error here. */
	if (oqueue->manage_memory)
	  _GalIO_FreeSockObject(current);
	oqueue->current_in = NULL;
	GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Found EOT at header %s while reading from queue", current->header);
	return(-1);
      }
      else {
	GalUtil_PrintWithLocation(GAL_TRANSPORT_SUMMARY_VERBOSITY_LEVEL,
				  __FUNCTION__,
				  "Reading object of type %s, data length %d\n",
				  __GalIO_MsgTypeString(current->msg_type),
				  current->size);
      }
      if (current->msg_type == GAL_DISCONNECT_MSG_TYPE) {
	if (oqueue->manage_memory)
	  _GalIO_FreeSockObject(current);
	oqueue->current_in = NULL;
	oqueue->done = 1;
	break;
      }	
      if(current->data)
      {
	free(current->data);
	current->data = NULL;
      }
      if(current->size < 0) {
	GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "atoi(%s) < 0", current->header);
	return(-1);
      }
      if(current->size == 0) {
	current->cptr = NULL;
	current->bytes_pending = 0;
      } else {
	current->data = calloc(current->size + 1, sizeof(char));
	current->cptr = current->data;
	current->bytes_pending = current->size;
      }
    }

    if (current->bytes == current->size + HEADER_SIZE) {
      if (current->data)
	current->data[current->size] = 0;
      __GalIO_SockObjectQueueAppendIn(oqueue, current);
      current = _GalIO_NewSockObject();
      oqueue->current_in = current;
    }
  }

  if (oqueue->error > 0)
  {
    strerror_r(oqueue->error, errbuf, 256);
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Encountered socket error %s while reading from queue", errbuf);
    return(-1);
  }

  if (_GalIO_QueueNonEmpty(oqueue->in))
    return(1);
  else
    return(0);
}

extern void _gal_unlock_mutex(void *mutex);

int _GalIO_SockObjectQueueProcessIn(GalIO_SockObjectQueueStruct *oqueue)
{
  int result = 0;
  int got_mutex;
  
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) oqueue->queue_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) oqueue->queue_mutex);
#endif
  got_mutex = GalUtil_TryLockLocalMutex(oqueue->queue_mutex);

  if (got_mutex == 0) {  
    result = __GalIO_SockObjectQueueProcessInNoLock(oqueue);
    GalUtil_UnlockLocalMutex(oqueue->queue_mutex);
  }
  
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
  
  return result;
}

static char *__GalIO_MsgTypeString(GalIO_MsgType msg_type)
{
  switch (msg_type) {
  case GAL_OBJECT_MSG_TYPE:
    return "GAL_OBJECT_MSG_TYPE";
  case GAL_MESSAGE_MSG_TYPE:
    return "GAL_MESSAGE_MSG_TYPE";
  case GAL_REPLY_MSG_TYPE:
    return "GAL_REPLY_MSG_TYPE";
  case GAL_DESTROY_MSG_TYPE:
    return "GAL_DESTROY_MSG_TYPE";
  case GAL_BROKER_START_MSG_TYPE:
    return "GAL_BROKER_START_MSG_TYPE";
  case GAL_BROKER_END_MSG_TYPE:
    return "GAL_BROKER_END_MSG_TYPE";
  case GAL_ERROR_MSG_TYPE:
    return "GAL_ERROR_MSG_TYPE";
  case GAL_DISCONNECT_MSG_TYPE:
    return "GAL_DISCONNECT_MSG_TYPE";
  case GAL_POSTPONE_MSG_TYPE:
    return "GAL_POSTPONE_MSG_TYPE";
  default:
    return "<unknown>";
  }
}

/* pops objects from SockObjectQueue, writes from SockQueue to socket */
/* returns 1 if objects are queued, 0 if queue is empty, -1 if error */

int _GalIO_SockObjectQueueProcessOutNoLock(GalIO_SockObjectQueueStruct *oqueue)
{
  GalIO_SockQueueStruct *squeue;
  GalIO_SockObjectStruct *current;
  int bytes_sent;
  int res;
  char errbuf[256];

  if (oqueue == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to write to");
    return(-1);
  }

  squeue = oqueue->queue;
  if (squeue == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to write to");
    return(-1);
  }

  if (oqueue->current_out == NULL)
    oqueue->current_out = _GalIO_SockObjectQueuePopOut(oqueue);

  while (oqueue->current_out)
  {
    current = oqueue->current_out;

    if (current->bytes == 0)
    {
      current->bytes_pending = HEADER_SIZE;
      current->cptr = current->header;
      GalUtil_PrintWithLocation(GAL_TRANSPORT_SUMMARY_VERBOSITY_LEVEL,
				__FUNCTION__,
				"Writing object of type %s, data length %d\n",
				__GalIO_MsgTypeString(current->msg_type),
				current->size);
    }

    res = GalIO_SockQueueWrite(squeue, (void *)current->cptr, current->bytes_pending);
    bytes_sent = current->bytes_pending;
    if (res < 0) {
      oqueue->error = squeue->error;
      strerror_r(oqueue->error, errbuf, 256);
      GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Write queue too full / socket error %s", errbuf);
      return(-1);
    }

    current->bytes += bytes_sent;
    current->cptr += bytes_sent;
    current->bytes_pending -= bytes_sent;

    if(current->bytes == HEADER_SIZE)
    {
      /* header has been sent, now send the data */
      current->bytes_pending = current->size;
      current->cptr = current->data;
    }

    if (current->bytes == current->size + HEADER_SIZE)
    {
      /* data has been sent, now send the next object */
      oqueue->current_out = _GalIO_SockObjectQueuePopOut(oqueue);

      /* free the current object and its data buffer */
      if (oqueue->manage_memory)
	_GalIO_FreeSockObject(current);
    }
  }

  /* flush the SockQueue out buffer */
  res = GalIO_SockQueueProcessSend(squeue);
  if (res < 0)
  {
    oqueue->error = squeue->error;
    /* maybe broker connection not yet opened */
    if (squeue->sockfd != GAL_INVALID_SOCKET)
    {
      strerror_r(oqueue->error, errbuf, 256);
      GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Encountered socket error (%d) %s while writing to queue", oqueue->error, errbuf);
      return(-1);
    } else {
      return 1;		/* buffering */
    }
  }

  if (res || oqueue->current_out || _GalIO_QueueNonEmpty(oqueue->out))
    return 1;
  else
    return 0;
}

/* 4/18/02: Needs to be protected, because ProcessOut ultimately
   calls both printouts and fcntl in the socket blocking fn. 4/23/02:
   Set up a trylock here, in case someone else is holding the mutex. */

/* returns 1 if objects are queued, 0 if queue is empty, -1 if error */

int _GalIO_SockObjectQueueProcessOut(GalIO_SockObjectQueueStruct *oqueue)
{
  /* Use a conservative value which will cause this to be called again. */
  int result = 1;
  int got_mutex;
  
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) oqueue->queue_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) oqueue->queue_mutex);
#endif
  got_mutex = GalUtil_TryLockLocalMutex(oqueue->queue_mutex);

  if (got_mutex == 0) {
    result = _GalIO_SockObjectQueueProcessOutNoLock(oqueue);
    GalUtil_UnlockLocalMutex(oqueue->queue_mutex);
  }
  
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif

  return result;
}

#if defined(GAL_WIN32_THREADS) || defined(GAL_PTHREADS)
static void unlock_sock_queue(void *queue_mutex)
{
  GalUtil_UnlockLocalMutex((GalUtil_LocalMutex *) queue_mutex);
}
#endif

/* returns 1 if object read, 0 if no object, -1 if error */

static int __GalIO_SockObjectQueueReadNoLock(GalIO_SockObjectQueueStruct *queue, GalIO_SockObjectStruct **struct_ptr)
{
  GalIO_SockObjectStruct *object;
  int res = 0;

  /* get the next object off the queue */
  res = __GalIO_SockObjectQueueProcessInNoLock(queue);

  /* there might be objects pending in the queue even if there was an error */
  if (res) {
    object = __GalIO_SockObjectQueuePopIn(queue);
    
    if (object)
    {
      if (struct_ptr) {
	*struct_ptr = object;
      }
      return 1;
    }
  }
  
  /* if there was an error and no object return -1 */
  if (res < 0) {
    return(-1);
  }

  return(0);
}

/* returns 1 if object read, 0 if no object, -1 if error */

int _GalIO_SockObjectQueueRead(GalIO_SockObjectQueueStruct *queue,
			       GalIO_SockObjectStruct **struct_ptr)
{
  int res = 0;
  int got_mutex;
  
  /* get the next object off the queue */
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) queue->queue_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) queue->queue_mutex);
#endif
  got_mutex = GalUtil_TryLockLocalMutex(queue->queue_mutex);

  if (got_mutex == 0) {
    res = __GalIO_SockObjectQueueReadNoLock(queue, struct_ptr);
    GalUtil_UnlockLocalMutex(queue->queue_mutex);
  }
  
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif

  return res;
}

/* SAM 4/23/02: It turns out that I need to wrap the entire thing in
   the queue mutex. If the queue mutex isn't accessible, the msg_queue
   has its own mutex which you can check. */

int _GalIO_SockObjectQueueReadAndDecode(GalIO_SockObjectQueueStruct *queue,
					Gal_Frame *frame,
					GalIO_MsgType *msg_type_ptr,
					GalIO_MsgQueueTestFn test_fn,
					void *client_data,
					int blocking)
{
  int res = 0;
  int got_mutex;
  int first_time = 1;
  int final_res = 0;
  void *obj = NULL;
  int object_count;
  Gal_Frame f = (Gal_Frame) NULL;
  int xdr_res;
  Gal_ObjectType gal_type;
  GalIO_MsgType msg_type;
  GalIO_SockObjectStruct *sos = (GalIO_SockObjectStruct *) NULL;
  int old_blocking;

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) queue->queue_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) queue->queue_mutex);
#endif
  got_mutex = GalUtil_TryLockLocalMutex(queue->queue_mutex);
  
  /* get the next object off the queue */
  while (1) {
    /* First, try reading from the message queue. You can do this
       whether or not you're holding the queue mutex, since the
       message queue has its own mutex. But if you're holding the
       queue mutex, you know that no one else will be adding anything
       from the message queue. So you don't need to check on subsequent
       times through the loop. */
    obj = (void *) NULL;
    if (first_time || (got_mutex != 0)) {
      /* If this is the first time, or if you don't own the mutex,
	 check the inbound queue. */
      obj = _GalIO_SockObjectMsgDequeueIf(queue,
					  (int *) NULL,
					  &gal_type,
					  &msg_type,
					  test_fn,
					  client_data);
    }
    if (obj) {
      f = (Gal_Frame) obj;
    } else if (got_mutex == 0) {
      /* If you have the mutex, try reading from the queue. 
	 Preserve the blocking. */
      old_blocking = queue->queue->read_blocking;
      queue->queue->read_blocking = blocking;
      res = __GalIO_SockObjectQueueReadNoLock(queue, &sos);
      queue->queue->read_blocking = old_blocking;
      if (res == -1) {
	GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't read from connection queue due to socket error");
	final_res = -1;
	break;
      } else if (res == 1) {
	/* We can just decode. */
	xdr_res = _GalIO_XDRDecodeObject(sos->data, sos->size,
					 &gal_type,
					 &obj, &object_count);
	if (!xdr_res) {
	  GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't decode XDR string");
	  if (queue->manage_memory) {
	    _GalIO_FreeSockObject(sos);
	  }
	  final_res = -1;
	  break;
	} else if (gal_type != GAL_FRAME) {
	  GalUtil_WarnWithLocation(__FUNCTION__, "Found object of type %s instead of frame while decoding XDR string", Gal_ObjectTypeString(gal_type));
	  if (queue->manage_memory) {
 	    _GalIO_FreeSockObject(sos);
 	  }
	  final_res = -1;
	  break;
	}
	f = (Gal_Frame) obj;
	if (test_fn &&
	    !(*test_fn)(sos->msg_type, (void *) f, 1, gal_type, client_data)) {
	  /* Not the right object. Enqueue and move on. */
	  _GalIO_SockObjectMsgEnqueue(queue, sos, (void *) f, gal_type, 1);
	  f = (Gal_Frame) NULL;
	} else {
	  msg_type = sos->msg_type;
	  /* free the object */
	  if (queue->manage_memory) {
	    _GalIO_FreeSockObject(sos);
	  }
	}
      } else {
	/* There's nothing in the queue. If we're not blocking,
	   return. */
	if (!blocking) {
	  final_res = res;
	  break;
	}
      }
    }
    /* If we found a frame, then return it. */
    if (f) {
      if (msg_type_ptr) {
	*msg_type_ptr = msg_type;
      }
      *frame = f;
      final_res = 1;
      break;
    }
    /* We may have done a blocking read and not gotten an
       entire message. So if we're blocking, read again.
       if we're not, bail. */
    if (!blocking) {
      final_res = 0;
      break;
    } else if (got_mutex != 0) {
      /* If we're blocking, but we don't have the mutex,
	 better sleep a little bit, because we're just going to
	 hit the msg queue again. */
      GalUtil_MilliSleep(1);
    }
    first_time = 0;
  }

  if (got_mutex == 0) {
    GalUtil_UnlockLocalMutex(queue->queue_mutex);
  }
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif

  return final_res;
}

GalIO_SockObjectStruct *_GalIO_CreateSockObject(void *data, GalIO_MsgType msg_type, int data_size, int free_data)
{
  GalIO_SockObjectStruct *object;
  /* XDR encoder for message header. */
  XDR xdr_s;

  /* error if data size < 0 */
  if (data_size < 0) {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Found invalid data size %d < 0 while creating socket object", data_size);
    return((GalIO_SockObjectStruct *) NULL);
  }

  object = _GalIO_NewSockObject();
  xdrmem_create(&xdr_s, object->header, 8, XDR_ENCODE);
  if ((xdr_int(&xdr_s, (int *) &msg_type) == FALSE) ||
      (xdr_int(&xdr_s, &data_size) == FALSE)) {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "Couldn't encode XDR header");
    return((GalIO_SockObjectStruct *) NULL);
  }
  object->size = data_size;
  object->msg_type = msg_type;
  if (free_data)
    object->data = data;
  else
  {
    if (data && data_size)
    {
      object->data = (char *)malloc(object->size);
      _gal_bcopy((char *)data, object->data, object->size);
    }
    else
      object->data = NULL;
  }
  return object;
}

/* returns 1 if object queued, 0 if object written, -1 if error */

/* 4/18/02: Needs to be protected because of blocking and printouts
   in ProcessOut. */

int _GalIO_SockObjectQueueAppendAndProcess(GalIO_SockObjectQueueStruct *queue,
					   GalIO_SockObjectStruct *object)
{
  /* error if no queue */
  if (queue == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to write to");
    return(-1);
  }
  _GalIO_SockObjectQueueAppendOut(queue, object);
  return _GalIO_SockObjectQueueProcessOut(queue);
}

int _GalIO_SockObjectQueueWrite(GalIO_SockObjectQueueStruct *queue,
				void *data, GalIO_MsgType msg_type,
				int data_size, int free_data)
{
  GalIO_SockObjectStruct *object;

  /* error if no queue */
  if (queue == NULL)
  {
    GalUtil_WarnLevelWithLocation(GAL_ERROR_DETAILS_VERBOSITY_LEVEL, __FUNCTION__, "No queue to write to");
    return(-1);
  }
  object = _GalIO_CreateSockObject(data, msg_type,
				   data_size, free_data);

  if (!object) {
    return -1;
  }
  return _GalIO_SockObjectQueueAppendAndProcess(queue, object);
}

/* returns 1 if object queued, 0 if object written, -1 if error */

GalIO_SockObjectStruct *_GalIO_CreateEOTSockObject()
{
  return _GalIO_CreateSockObject((void *) NULL, 
				 GAL_DISCONNECT_MSG_TYPE, 0, 0);
}

/*
 * Message queue management.
 */

/* So I will implement "peek" by shoving things on the message
   queue list until the right thing is found. 4/23/02: Now
   the msg queue has its own mutex. */

void _GalIO_SockObjectMsgEnqueue(GalIO_SockObjectQueueStruct *queue,
				 GalIO_SockObjectStruct *sos,
				 void *decoded_data, 
				 Gal_ObjectType decoded_type,
				 int decoded_size)
{
  GalIO_MsgQueueStruct *mq = (GalIO_MsgQueueStruct *) calloc(1, sizeof(GalIO_MsgQueueStruct));
  
  mq->decoded_data = decoded_data;
  mq->sos = sos;
  mq->decoded_type = decoded_type;
  mq->decoded_size = decoded_size;

  _GalIO_QueueEnqueue(queue->msg_queue, mq);
}

typedef struct __galio_sock_object_matcher {
  GalIO_MsgQueueTestFn test_fn;
  void *caller_data;
} __galio_sock_object_matcher;

static int __galio_sock_object_matches(void *data, void *caller_data)
{
  GalIO_MsgQueueStruct *mq = (GalIO_MsgQueueStruct *) data;
  __galio_sock_object_matcher *som = (__galio_sock_object_matcher *) caller_data;

  if (!som->test_fn)
    return 1;
  return (*(som->test_fn))(mq->sos->msg_type,
			   mq->decoded_data, mq->decoded_size,
			   mq->decoded_type, som->caller_data);
}

/* 4/18/02: Needs to be protected by push/pop for thread
   cancellation, just in case the test_fn has a cancellation
   point in it. 4/23/02: Handled by _GalIO_QueueDequeueIf. */

void *_GalIO_SockObjectMsgDequeueIf(GalIO_SockObjectQueueStruct *queue,
				    int *size_ptr,
				    Gal_ObjectType *o_ptr,
				    GalIO_MsgType *t_ptr,
				    GalIO_MsgQueueTestFn test_fn,
				    void *client_data)
{
  void *result;
  GalIO_MsgQueueStruct *mq;
  __galio_sock_object_matcher som;
  
  /* Find the first element which matches. */
  som.test_fn = test_fn;
  som.caller_data = client_data;
  mq = _GalIO_QueueDequeueIf(queue->msg_queue,
			     __galio_sock_object_matches, &som);
  /* mq may be NULL because we reached the end
     of the list, or because there was no list to start with. */
  if (!mq) {
    if (t_ptr) {
      *t_ptr = -1;
    }
    if (o_ptr) {
      *o_ptr = -1;
    }
    if (size_ptr) {
      *size_ptr = 0;
    }
    return (void *) NULL;
  } else {
    /* Unlock the mutex. */
      result = mq->decoded_data;
    if (t_ptr)
      *t_ptr = mq->sos->msg_type;
    if (o_ptr)
      *o_ptr = mq->decoded_type;
    if (size_ptr)
      *size_ptr = mq->decoded_size;
    if (queue->manage_memory) {
      _GalIO_FreeSockObject(mq->sos);
    }
    free(mq);
    return result;
  }
}

void *_GalIO_SockObjectMsgDequeue(GalIO_SockObjectQueueStruct *queue,
				  int *size_ptr,
				  Gal_ObjectType *o_ptr,
				  GalIO_MsgType *t_ptr)
{
  /* DequeueIf with no constraints. */
  return _GalIO_SockObjectMsgDequeueIf(queue, size_ptr, o_ptr, t_ptr, 
				       (GalIO_MsgQueueTestFn) NULL,
				       (void *) NULL);
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
