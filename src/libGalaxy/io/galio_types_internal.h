/*
  This file (c) Copyright 1998 - 2000 M.I.T.
            (c) Copyright 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* This is the local types file for the io/ subdirectory. */

#ifndef _GALIO_TYPES_INTERNAL_H
#define _GALIO_TYPES_INTERNAL_H

#ifndef WIN32
#include <sys/time.h>
#else
#include <winsock2.h>
#include <time.h>
#endif

#include "galaxy/gthread.h"
#include "galaxy/util.h"

/* Size of two integers in XDR. */
#define HEADER_SIZE 8

/* pointer_queue.c */

enum {PQ_FREE, PQ_IN_USE, PQ_DESTROYED};

typedef struct __GalIO_PointerQueueElement {
  void *data;
  int status;
  struct __GalIO_PointerQueueElement *next;
} GalIO_PointerQueueElement;

/* The type will be for type-checking purposes (what kind of
   contents are in the queue). The head is the head of the
   queue, the tail is the current tail, and the cache is
   where elements removed from the queue are stored until the
   queue is freed, if the queue is designated as volatile_queue.
   These queues are also designed to be mutexable.

   The free_fn, if present, is how to free each data element.
   This is handled at the level of the queue, not for each
   element. Only used when the queue is flushed or freed.
*/

typedef struct __GalIO_PointerQueue {
  int type;
  int volatile_queue;
  int status;
  GalUtil_LocalMutex *mutex;
  struct __GalIO_PointerQueueElement *head;
  struct __GalIO_PointerQueueElement *tail;
  struct __GalIO_PointerQueueElement *cache;
  void (*free_fn)(void *);
} GalIO_PointerQueue;

/* For types for the pointer queue, etc., see galaxy.h. */

/* sockqueue.c */

typedef struct __GalIO_SockQueueStruct
{
  GAL_SOCKET sockfd;
  int error;

  char *in_queue;
  int in_len,max_in;

  char *out_queue;
  int out_len,max_out;

  char *comment;
  int do_not_expand_out;
  /* Sam 9/27/99: for threads */
  int read_blocking;
  GalUtil_LocalMutex *mutex;
} GalIO_SockQueueStruct;

typedef struct sockobject
{
  char header[HEADER_SIZE + 1];
  char *data;
  int size;
  GalIO_MsgType msg_type;

  int bytes;
  int bytes_pending;
  char *cptr;

} GalIO_SockObjectStruct;

typedef struct __GalIO_MsgQueueStruct {
  GalIO_SockObjectStruct *sos;
  void *decoded_data;
  Gal_ObjectType decoded_type;
  int decoded_size;
} GalIO_MsgQueueStruct;

typedef struct
{
  GalIO_SockQueueStruct *queue;
  int error;
  int done;

  GalIO_PointerQueue *in;
  GalIO_PointerQueue *out;

  GalIO_SockObjectStruct *current_in;
  /* GalIO_SockObjectStruct *in;
     GalIO_SockObjectStruct *in_tail; */

  GalIO_SockObjectStruct *current_out;
  /* GalIO_SockObjectStruct *out;
     GalIO_SockObjectStruct *out_tail; */
  /* SAM 09/07/00: Need a mutex per queue, just to be sure.
     Otherwise, a blocking read in one thread will keep everyone
     else from reading...

     SAM 10/27/00: WAY more complicated than that. First, you
     probably want a separate mutex for in and for out. Second,
     if you have a connection and a broker in both trying to read
     (say, both trying to read a reply or one trying to read
     a new dispatch function or something), we're in deep shit
     if the connection does a blocking read. Temporary solution:
     don't have connections do blocking reads.

     SAM 4/21/02: Nope, wrong answer. We need to distinguish
     between the mutex for the queue and the mutex for the
     SockQueueStruct. The latter should only be mutexed with
     trylocks. */
  GalUtil_LocalMutex *queue_mutex;
  /* And now, I need a linked list of objects read from
     the queue, for peeking purposes. */
  GalIO_PointerQueue *msg_queue;
  /* Finally, I need the concept of "shadow" queues,
     where the sock objects aren't freed because they
     live elsewhere. */
  int manage_memory;
} GalIO_SockObjectQueueStruct;

/* hub_server.c */

typedef struct __GalIO_Callback {
  void *fn;
  void *callback_data;
  int callback_type;
  void *callback_host;
} GalIO_Callback;

typedef struct __GalIO_CommStruct
{
  int status;

  GalIO_SockObjectQueueStruct *queue;
  GAL_SOCKET sock;
  int poll_ms;
  unsigned short contact_port;
  int verified;
  int done;
  int error;
  /* Sam 9/27/99: for threads */
  int read_blocking;
  void *comm_data;
  void (*comm_data_free_fn)(void *);
  struct __GalIO_ServerStruct *server;
  GalIO_FrameHandler handler;
  char *host;
  /* These functions are called when the connection is severed. */
  GalIO_PointerQueue *shutdown_callbacks;
  /* These functions are called when the connection is destroyed. */  
  GalIO_PointerQueue *destroy_callbacks;
  /* These are called when a broker in is set up. */
  GalIO_PointerQueue *broker_in_callbacks;
  /* These are called when a broker out is set up. */
  GalIO_PointerQueue *broker_out_callbacks;
  /* These are called when a dispatch function is invoked. */
  GalIO_PointerQueue *dispatch_fn_callbacks;
  /* SAM 9/17/99: Introduced these when we collapsed the
     Gal_Server and GalIO_ServerStruct objects and encapsulating
     server information. The connection inherits the parent's
     function map. */
  struct __gal_dispatch_fn_pkg *fn_pkg;
  int validate;
  GalIO_FrameReader msg_reader;
  void (*msg_freer)(Gal_Frame, Gal_Frame);
  /* This is to write elements through the connection. */
  GalIO_FrameWriter writer;
  GalIO_FrameDispatcher dispatcher;
  /* This is used for generic reading; we're going to
     collapse the broker in with the connection. */
  int (*read_handler)(struct __GalIO_CommStruct *);
  /* This is for the continuations. See ServerStub/frame_util.c. */
  GalIO_PointerQueue *continuations;
  /* This is because we can lock environments. We'd better
     lock the underlying connection, too, so that we get
     socket errors instead of seg faults when writing through
     a closed connection. */
  GalUtil_LocalMutex *ref_mutex;
  int reference_count;

  /* This is the particular shutdown callback which
     removes the timed task. If in the threaded case,
     I let it run, it will shut down the thread
     WHILE THE DESTROY IS UNDERWAY. So we will
     remove this before we call the destroy function. */
  GalIO_Callback *task_removal_callback;
} GalIO_CommStruct;

enum {GAL_HUB_CLIENT_NEVER_CONTACTED,
      GAL_HUB_CLIENT_NEVER_CONNECTED, GAL_HUB_CLIENT_CONNECTED,
      GAL_HUB_CLIENT_DISCONNECTED};

typedef struct __GalIO_ServerLocation {
  char *host;
  unsigned short port;
  int connected;
  int poll_flags;
} GalIO_ServerLocation;

typedef struct __GalIO_ServerStruct
{
  int status;
  int max_connections;
  int num_connections;
  /* Sam 9/27/99: for threads */
  int read_blocking;
  int poll_ms;
  /* SAM 9/17/99: Introduced these when we collapsed the
     Gal_Server and GalIO_ServerStruct objects and encapsulating
     server information. */
  char *server_name;
  GalIO_PointerQueue *comm_queue;
  int done;
  int error;
  void *server_data;
  void (*server_data_free_fn)(void *);
  /* These functions are called when the server accepts a new connection. */
  GalIO_PointerQueue *connection_setup_callbacks;
  GalIO_PointerQueue *destroy_callbacks;
  int server_listen_status;  
  char *default_session_id;
  GalIO_CommStruct *gcomm_prototype;  
  /* For outgoing brokers associated with the server.
     I originally was going to use the broker queues
     themselves, but they're not mutexed appropriately. */
  GalIO_PointerQueue *brokers;
  
  /* This stuff is for the listener in the server. */
  unsigned short default_port;
  int require_port;
  GAL_SOCKET listen_sock;
  unsigned short listen_port;
  GalIO_PointerQueue *listener_startup_callbacks;
  GalIO_PointerQueue *listener_shutdown_callbacks;

  /* This stuff is for the listener in the Hub. */
  GalIO_PointerQueue *client_poll_startup_callbacks;
  GalIO_ServerLocation *server_locations;

  /* This is the particular shutdown callback which
     removes the timed task. If in the threaded case,
     I let it run, it will shut down the thread
     WHILE THE DESTROY IS UNDERWAY. So we will
     remove this before we call the destroy function. */
  GalIO_Callback *server_task_removal_callback;
  GalIO_Callback *client_task_removal_callback;

  /* Server properties are shipped to the Hub at
     connection time. */
  Gal_Frame server_properties;
  /* Additional service types can be specified beyond
     the name of the server. */
  Gal_Object service_types;
  
} GalIO_ServerStruct;

/* broker_data.c */

/* SAM 10/5/00: Incoming and outgoing brokers are so different
   at this point that we ought to have different structures. Since
   all the existing code is set up to have the same structure,
   we'll do it internally. */

typedef struct __GalIO_OutgoingBroker {
  struct timeval *expiration_time;  
  /* Outgoing listener and socket object queue. */
  GalIO_ServerStruct *server;
  GalIO_SockObjectQueueStruct *queue;
  char *call_id;
  GalIO_PointerQueue *comm_queue;
} __GalIO_OutgoingBroker;

typedef struct __GalIO_IncomingBroker {
  GalIO_CommStruct *gcomm;
  struct __galss_environment *host_env;
  Gal_Frame frame;
  GalIO_BrokerDataHandler handler;
  int active;
  int read_blocking;
  GalIO_CommStruct *host_gcomm;
} __GalIO_IncomingBroker;

typedef struct __GalIO_BrokerStruct
{
  int poll_ms;
  int done;
  int status;
  /* This is the user finalization function.
     Here for backward compatibility. */
  GalIO_Callback *finalizer_callback;
  /* This is the data the user can set. Passed to the handler. */
  void *caller_data;
  void (*caller_data_free_fn)(void *);
  __GalIO_OutgoingBroker *out_b;
  __GalIO_IncomingBroker *in_b;
  GalIO_PointerQueue *data_done_callbacks;
  GalIO_PointerQueue *abort_callbacks;
  GalIO_PointerQueue *destroy_callbacks;
  GalIO_PointerQueue *connect_callbacks;

  /* This is the particular shutdown callback which
     removes the timed task. If in the threaded case,
     I let it run, it will shut down the thread
     WHILE THE DESTROY IS UNDERWAY. So we will
     remove this before we call the destroy function. */
  struct __GalIO_Callback *task_removal_callback;
  
  /* for BrokerStruct queue */
  int queued;
  struct __GalIO_BrokerStruct *prev;
  struct __GalIO_BrokerStruct *next;
} GalIO_BrokerStruct;

#endif
