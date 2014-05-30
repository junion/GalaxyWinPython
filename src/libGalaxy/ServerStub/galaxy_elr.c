/*
  This file (c) Copyright 2001 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* In this file, we attempt to encapsulate the way that
   the external main loop in Communicator is embedded. We
   construct an object called an external loop record, which
   encapsulates the timers and file descriptor callbacks which
   are required. */

#include <stdlib.h>
#include "galaxy/galaxy_all.h"

static void __GalSS_ELRAddConnectionCallback(GalIO_ServerStruct *scomm,
                                             GalIO_CommStruct *gcomm,
                                             void *callback_data);
static void __GalSS_ELRConnectionDisconnect(GalIO_CommStruct *gcomm,
                                            void *caller_data);
static void __GalSS_ELRSetupBrokerOut(GalIO_CommStruct *gcomm,
                                      GalIO_BrokerStruct *b,
                                      void *caller_data);
static void __GalSS_ELRSetupBrokerIn(GalIO_CommStruct *gcomm,
                                     GalIO_BrokerStruct *b,
                                     void *caller_data);
static void __GalSS_ELRBrokerShutdown(GalIO_BrokerStruct *b,
                                      void *loop_data);
static void __GalSS_ELRSetupServerListener(GalIO_ServerStruct *scomm,
                                           void *callback_data);
static void __GalSS_ELRShutdownServerListener(GalIO_ServerStruct *scomm,
                                              void *callback_data);
static void __GalSS_ELRSetupServerClient(GalIO_ServerStruct *scomm,
                                         void *callback_data);
static void __GalSS_ELRShutdownServerClient(GalIO_ServerStruct *scomm,
                                            void *callback_data);
static void __GalSS_ELRDestroyServer(GalIO_ServerStruct *scomm,
                                     void *callback_data);
static int __GalSS_ELRDoConnectionCallback(GalSS_ELR *elr, int timer_or_fd);
static int __GalSS_ELRDoBrokerOutCallback(GalSS_ELR *elr, int timer_or_fd);
static int __GalSS_ELRDoBrokerInCallback(GalSS_ELR *elr, int timer_or_fd);
static int __GalSS_ELRDoServerListenerCallback(GalSS_ELR *elr,
                                               int timer_or_fd);
static int __GalSS_ELRDoServerClientCallback(GalSS_ELR *elr,
                                             int timer_or_fd);

/* First, we set up an external loop record. This requires the user
   to define four or five little functions:
   (1) a function to set a file descriptor callback;
   (2) a function to set a timer callback;
   (3) a function to unset a file descriptor callback;
   (4) a function to unset a timer callback;
   (5) an optional behavior function, which is called after
       the callback updates are called
   and to indicate if the timer is persistent (that is, if it doesn't
   need to be reset whenever it fires).

   We need timers because file descriptor callbacks aren't
   enough to flush the internal queues of connections (since
   there may not be something pending on the fd to read, but there
   may be something already read in). We can probably
   get away without file descriptor callbacks, but my guess
   is that they provide a faster response.
   
   In order to define (1) and (2), they'll have to define
   a function or functions which has the appropriate signature
   for the callback of the external loop, and these callback
   functions will have to invoke the function GalSS_ELRDoCallback. */

GalSS_ELR *GalSS_ELRCreate(GalIO_ServerStruct *scomm,
                           GalSS_ELTimerSetFn timer_set_fn,
                           GalSS_ELUnsetFn timer_unset_fn,
                           GalSS_ELFDSetFn fd_set_fn,
                           GalSS_ELUnsetFn fd_unset_fn,
                           GalSS_ELBehaviorFn behavior_fn,
                           int timer_is_persistent)
{
  GalSS_ELR *elr;

  /* Can't do this without timers. */
  if ((!timer_set_fn) || (!timer_unset_fn) ||
      (!fd_set_fn) || (!fd_unset_fn)) {
    /* In the long run, we'll probably be able to
       get away with just timers, but that's not the
       way the server listener poll works by default. */
    GalUtil_Warn("External loop requires timers and fds");
    return (GalSS_ELR *) NULL;
  }
  if (!scomm) {
    GalUtil_Warn("External loop requires server seed");
    return (GalSS_ELR *) NULL;
  }
  
  elr = (GalSS_ELR *) calloc(1, sizeof(GalSS_ELR));
  elr->local_info = (GalSS_ELRLocalInfo *) calloc(1, sizeof(GalSS_ELRLocalInfo));
  elr->global_info = (GalSS_ELRGlobalInfo *) calloc(1, sizeof(GalSS_ELRGlobalInfo));
  elr->global_info->server_client_ms = 1000;
  elr->global_info->conn_ms = 50;
  elr->global_info->broker_ms = 50;
  elr->local_info->scomm = scomm;
  elr->global_info->timer_set_fn = timer_set_fn;
  elr->global_info->timer_unset_fn = timer_unset_fn;
  elr->global_info->fd_set_fn = fd_set_fn;
  elr->global_info->fd_unset_fn = fd_unset_fn;
  elr->global_info->behavior_fn = behavior_fn;
  elr->global_info->timer_is_persistent = timer_is_persistent;
  /* Initialize the default callbacks. */
  elr->global_info->connection_callback = __GalSS_ELRDoConnectionCallback;
  elr->global_info->broker_out_callback = __GalSS_ELRDoBrokerOutCallback;
  elr->global_info->broker_in_callback = __GalSS_ELRDoBrokerInCallback;
  elr->global_info->server_listener_callback = __GalSS_ELRDoServerListenerCallback;
  elr->global_info->server_client_callback = __GalSS_ELRDoServerClientCallback;
  elr->status = GALSS_ELR_IS_ROOT;

  /* Set the connect callback for the server. This gets called
     whenever a new connection is established. */

  GalIO_AddServerConnectCallback(scomm,
                                 __GalSS_ELRAddConnectionCallback,
                                 (void *) elr);
  
  /* The server can be a listener when it starts out, or
     it can become a listener when an outgoing broker starts up. So
     we set a callback to handle whenever this happens. */
  GalIO_AddServerCallback(scomm,
                          GAL_SERVER_LISTENER_STARTUP_EVENT,
                          __GalSS_ELRSetupServerListener, (void *) elr);
  
  /* Similarly, if someone calls GalIO_ContactHub, it may lead to
     a new poller starting up. So we should deal with that
     as a callback too. */
  GalIO_AddServerCallback(scomm,
                          GAL_SERVER_CLIENT_POLL_STARTUP_EVENT,
                          __GalSS_ELRSetupServerClient, (void *) elr);
  /* And now, something that will shut down the loop when
     the server is destroyed. */
  GalIO_AddServerCallback(scomm,
                          GAL_SERVER_DESTRUCTION_EVENT,
                          __GalSS_ELRDestroyServer, (void *) elr);
  
  return elr;
}

/* Now, we functions to copy the record and to free it. */

GalSS_ELR *GalSS_ELRCopy(GalSS_ELR *source)
{
  GalSS_ELR *elr;
  
  if (!source) {
    return (GalSS_ELR *) NULL;
  }
  elr = (GalSS_ELR *) calloc(1, sizeof(GalSS_ELR));
  elr->global_info = source->global_info;
  elr->local_info = (GalSS_ELRLocalInfo *) calloc(1, sizeof(GalSS_ELRLocalInfo));
  elr->local_info->scomm = source->local_info->scomm;
  elr->local_info->gcomm = source->local_info->gcomm;
  elr->local_info->broker = source->local_info->broker;
  /* Don't copy the tag information, since we're going to
     set it for this child later. */
  return elr;
}

void GalSS_ELRShutdown(GalSS_ELR *elr)
{
  if (elr->local_info->timer_is_set && elr->global_info->timer_unset_fn) {
    (*elr->global_info->timer_unset_fn)(elr, elr->local_info->timer_tag);
    elr->local_info->timer_is_set = 0;
    elr->local_info->timer_tag = (void *) NULL;
  }

  if (elr->local_info->fd_is_set && elr->global_info->fd_unset_fn) {
    (*elr->global_info->fd_unset_fn)(elr, elr->local_info->fd_tag);
    elr->local_info->fd_is_set = 0;
    elr->local_info->fd_tag = (void *) NULL;
  }
}

/* If I'm in a callback, I don't want to destroy the elr
   yet, because the callback needs to know that the elr was
   destroyed. */

void GalSS_ELRDestroy(GalSS_ELR *elr)
{
  if (elr->status & GALSS_ELR_IN_CALLBACK) {
    elr->status = elr->status | GALSS_ELR_DESTROYED;
  } else {
    if (elr->status & GALSS_ELR_IS_ROOT) {
      /* Destroy the loop data. */
      GalSS_ELRSetLoopData(elr, (void *) NULL, NULL);
      free(elr->global_info);
    }
    free(elr->local_info);
    free(elr);
  }
}

/* The idea goes like this. When you set up Communicator to
   cooperate with an external main loop, there are two classes of
   callbacks which get established: the internal callbacks (e.g.,
   a shutdown for a created object) and the external callbacks
   (e.g., adding an "interrupt" of some sort, either based on
   a timer or a file descriptor, to do something to a Communicator
   object). These definitions "cascade" in some sense: a server
   is created, which means that the server has internal callbacks
   to start up listeners and shut down listeners, and external
   callbacks to process the input when there's something to
   process. Processing the input creates a connection, which
   spawns another set of internal and external callbacks. It all
   bottoms out at the broker, which are spawned from connections. */

/* What we're going to do is take this GalSS_ELR object and
   copy it every time a callback spawns a new Communicator object,
   and then add the Communicator object to the ELR. So the ELR
   connected to the server only has scomm set, not gcomm or broker;
   when a connection is established, the ELR is copied and the
   gcomm is added; when a broker is spawned from the connection,
   that ELR is copied and a broker is added. So you can tell
   what level you're at by what's set: if the broker is set,
   do the broker callback; else, if the connection is set, do
   the connection callback, else do the server callback.

   Actually, that's not good enough. I'm really going to have to
   do a switch on the spawning event every time, because if there's
   a server, for instance, I don't know whether to call the
   client poll or the server poll.

   And it's important to have a single callback, so that the
   programmer can write the fd and timer callbacks once. (They
   have to call a Communicator function). */

/* Functions to get and set loop data. */

void GalSS_ELRSetLoopData(GalSS_ELR *elr, void *loop_data,
                          void (*loop_data_free_fn)(void *))
{
  if (elr) {
    GalSS_ELRGlobalInfo *global_elr = elr->global_info;
    
    if (global_elr->loop_data && global_elr->loop_data_free_fn) {
      (*global_elr->loop_data_free_fn)(global_elr->loop_data);
    }
    global_elr->loop_data = loop_data;
    global_elr->loop_data_free_fn = loop_data_free_fn;
  }
}

void *GalSS_ELRGetLoopData(GalSS_ELR *elr)
{
  return elr->global_info->loop_data;
}

/* Function to update the timer intervals. -1 means don't change. */

void GalSS_ELRUpdatePollIntervals(GalSS_ELR *elr,
                                  int server_client_poll_ms,
                                  int conn_ms, int broker_ms)
{
  if (elr) {
    GalSS_ELRGlobalInfo *global_elr = elr->global_info;
    
    if (server_client_poll_ms > -1)
      global_elr->server_client_ms = server_client_poll_ms;
    if (conn_ms > -1)
      global_elr->conn_ms = conn_ms;
    if (broker_ms > -1)
      global_elr->broker_ms = broker_ms;
  }
}

/* Functions to update the callbacks. The callbacks must
   return 0 for still pollable and non-zero for not still pollable. */

void GalSS_ELRSetConnectionCallback(GalSS_ELR *elr, GalSS_ELCallbackFn fn)
{
  elr->global_info->connection_callback = fn;
}

void GalSS_ELRSetBrokerOutCallback(GalSS_ELR *elr, GalSS_ELCallbackFn fn)
{
  elr->global_info->broker_out_callback = fn;
}

void GalSS_ELRSetBrokerInCallback(GalSS_ELR *elr, GalSS_ELCallbackFn fn)
{
  elr->global_info->broker_in_callback = fn;
}

void GalSS_ELRSetServerListenerCallback(GalSS_ELR *elr, GalSS_ELCallbackFn fn)
{
  elr->global_info->server_listener_callback = fn;
}

void GalSS_ELRSetServerClientCallback(GalSS_ELR *elr, GalSS_ELCallbackFn fn)
{
  elr->global_info->server_client_callback = fn;
}


/* Accessors */

GalIO_ServerStruct *GalSS_ELRSComm(GalSS_ELR *elr)
{
  return elr->local_info->scomm;
}

GalIO_CommStruct *GalSS_ELRGComm(GalSS_ELR *elr)
{
  return elr->local_info->gcomm;
}

GalIO_BrokerStruct *GalSS_ELRBroker(GalSS_ELR *elr)
{
  return elr->local_info->broker;
}

/* Default poll functions for the callback handler.
   0 means the object is still pollable, non-zero means it's not. */

static int __GalSS_ELRDoConnectionCallback(GalSS_ELR *elr, int timer_or_fd)
{
  if ((timer_or_fd == GALSS_ELR_FD) ||
      GalIO_CommReadReady(elr->local_info->gcomm) ||
      GalIO_CommWriteReady(elr->local_info->gcomm)) {
    return GalIO_ConnectionCallbackHandler(elr->local_info->gcomm, 0);
  } else {
    return 0;
  }
}

static int __GalSS_ELRDoBrokerOutCallback(GalSS_ELR *elr, int timer_or_fd)
{
  if (GalIO_BrokerWriteReady(elr->local_info->broker)) {
    return GalIO_BrokerDataOutCallbackHandler(elr->local_info->broker);
  } else {
    return 0;
  }
}

static int __GalSS_ELRDoBrokerInCallback(GalSS_ELR *elr, int timer_or_fd)
{
  if ((timer_or_fd == GALSS_ELR_FD) ||
      GalIO_BrokerReadReady(elr->local_info->broker)) {
    return GalIO_BrokerDataInCallbackHandler(elr->local_info->broker, 0);
  } else {
    return 0;
  }
}

static int __GalSS_ELRDoServerListenerCallback(GalSS_ELR *elr,
                                               int timer_or_fd)
{
  int res = GalIO_ServerCallbackHandler(elr->local_info->scomm, 0,
                                        (GalIO_CommStruct **) NULL);
  /* 1 means it got a connection and everything's OK. */
  if (res == 1)
    res = 0;
  return res;
}  

static int __GalSS_ELRDoServerClientCallback(GalSS_ELR *elr,
                                             int timer_or_fd)
{
  GalIO_ServerCheckHubContacts(elr->local_info->scomm);
  return 0;
}

/* The callback handler itself. Note that in a number of
   cases, we do the callback if the file descriptor is
   the cause of firing OR if there's input or output to
   process. This ensures that the timer only gets fired
   when it needs to be. */

/* By the way, the elr object may not exist after the
   embedded handler is called. This is kind of a major
   problem, since we only want to reset a persistent
   object if it still exists. So GalSS_ELRDestroyCallback
   SHOULDN'T DESTROY THE ELR IF IT'S IN A CALLBACK. Duh.
   For all I know, this may be called recursively, so let's
   be careful here. */

void GalSS_ELRDoCallback(GalSS_ELR *elr, int timer_or_fd)
{
  int timer_ms = -1;
  int res = 0;
  int original_elr_status = elr->status;

  /* If it's already destroyed, the caller which marked it
     for destruction will actually do the destruction. */
  if (original_elr_status & GALSS_ELR_DESTROYED)
    return;

  elr->status = elr->status | GALSS_ELR_IN_CALLBACK;
  
  switch (elr->local_info->spawning_event) {
  case GAL_SERVER_CONNECTION_CREATION_EVENT:
    /* We're polling a connection. */
    res = (*elr->global_info->connection_callback)(elr, timer_or_fd);
    timer_ms = elr->global_info->conn_ms;
    break;
  case GAL_CONNECTION_BROKER_OUT_STARTUP_EVENT:
    /* We're polling a broker out. Only a timer here. */
    res = (*elr->global_info->broker_out_callback)(elr, timer_or_fd);
    timer_ms = elr->global_info->broker_ms;
    break;
  case GAL_CONNECTION_BROKER_IN_STARTUP_EVENT:
    /* We're polling a broker in. */
    res = (*elr->global_info->broker_in_callback)(elr, timer_or_fd);
    timer_ms = elr->global_info->broker_ms;
    break;
  case GAL_SERVER_LISTENER_STARTUP_EVENT:
    /* We're polling the server listener. Only fd here. */
    res = (*elr->global_info->server_listener_callback)(elr, timer_or_fd);
    break;
  case GAL_SERVER_CLIENT_POLL_STARTUP_EVENT:
    /* We're polling the server client. Only timer here. */
    res = (*elr->global_info->server_client_callback)(elr, timer_or_fd);
    timer_ms = elr->global_info->server_client_ms;
    break;
  }
  
  if ((!(original_elr_status & GALSS_ELR_DESTROYED)) &&
      (elr->status & GALSS_ELR_DESTROYED)) {
    /* If we've just destroyed it, die, die, die.
       No matter how far we've recursed, this will do the right
       thing, because if the original status says it's already
       in a callback, it will not be destroyed until the topmost
       call returns. */
    elr->status = original_elr_status;
    GalSS_ELRDestroy(elr);
    return;
  }
  
  /* Reset the timer if it's not persistent and hasn't been
     shut off and the object is still pollable. */
  if ((res == 0) && (timer_or_fd == GALSS_ELR_TIMER) &&
      (!elr->global_info->timer_is_persistent) &&
      elr->global_info->timer_set_fn && elr->local_info->timer_is_set &&
      (timer_ms != -1)) {
    elr->local_info->timer_tag = (*elr->global_info->timer_set_fn)(elr, timer_ms);
  }

  /* Now, return the status to its original state. */
  elr->status = original_elr_status;
}

/* GalIO_ConnectionCallbackHandler():
   -1 means an error was encountered and the connection has been destroyed.
   0 means we're in the midst of things.
   1 means we're done and the connection has been destroyed. */

static void __GalSS_ELRConnectionDisconnect(GalIO_CommStruct *gcomm,
                                            void *caller_data)
{
  GalSS_ELR *elr = (GalSS_ELR *) caller_data;

  GalSS_ELRShutdown(elr);
  GalSS_ELRDestroy(elr);
}

static void __GalSS_ELRAddConnectionCallback(GalIO_ServerStruct *scomm,
                                             GalIO_CommStruct *gcomm,
                                             void *callback_data)
{
  GalSS_ELR *elr = GalSS_ELRCopy((GalSS_ELR *) callback_data);

  elr->local_info->gcomm = gcomm;
  elr->local_info->spawning_event = GAL_SERVER_CONNECTION_CREATION_EVENT;

  /* Set the external timer and fd callbacks. */
    
  if (elr->global_info->timer_set_fn) {
    elr->local_info->timer_tag = (*elr->global_info->timer_set_fn)(elr, elr->global_info->conn_ms);
    elr->local_info->timer_is_set = 1;
  }

  if (elr->global_info->fd_set_fn) {
    elr->local_info->fd_tag = (*elr->global_info->fd_set_fn)(elr, GalIO_GetCommSocket(gcomm));
    elr->local_info->fd_is_set = 1;
  }

  if (elr->global_info->behavior_fn) {
    (*elr->global_info->behavior_fn)(elr, elr->local_info->spawning_event);
  }

  /* Finally, to support brokers, and to deal with
     disconnections, we need to use the
     data slot for the connection. */
  /* Make sure you stop polling when the connection dies. */
  GalIO_AddConnectionCallback(gcomm,
                              GAL_CONNECTION_SHUTDOWN_EVENT,
                              __GalSS_ELRConnectionDisconnect,
                              (void *) elr);
  /* And now, add the callbacks for the broker setups. */
  GalIO_AddConnectionBrokerCallback(gcomm,
                                    GAL_CONNECTION_BROKER_OUT_STARTUP_EVENT,
                                    __GalSS_ELRSetupBrokerOut,
                                    (void *) elr);
  GalIO_AddConnectionBrokerCallback(gcomm,
                                    GAL_CONNECTION_BROKER_IN_STARTUP_EVENT,
                                    __GalSS_ELRSetupBrokerIn,
                                    (void *) elr);
}

/* And now the brokers. These differ from the connection and
   server callbacks in that the programmer sets up the brokers
   explicitly, and thus needs to set up the callbacks too. */

static void __GalSS_ELRBrokerShutdown(GalIO_BrokerStruct *b, void *loop_data)
{
  GalSS_ELR *elr = (GalSS_ELR *) loop_data;

  if (elr->global_info->behavior_fn) {
    (*elr->global_info->behavior_fn)(elr, GAL_BROKER_DESTRUCTION_EVENT);
  }
  GalSS_ELRShutdown(elr);  
  GalSS_ELRDestroy(elr);
}

static void __GalSS_ELRSetupBrokerOut(GalIO_CommStruct *gcomm,
                                      GalIO_BrokerStruct *b,
                                      void *caller_data)
{
  GalSS_ELR *elr = GalSS_ELRCopy((GalSS_ELR *) caller_data);

  elr->local_info->broker = b;
  elr->local_info->spawning_event = GAL_CONNECTION_BROKER_OUT_STARTUP_EVENT;
  
  /* There's no point in an fd callback for the outgoing
     broker, since it piggybacks off of the server listener. */

  if (elr->global_info->timer_set_fn) {
    elr->local_info->timer_tag = (*elr->global_info->timer_set_fn)(elr, elr->global_info->broker_ms);
    elr->local_info->timer_is_set = 1;
  }
  if (elr->global_info->behavior_fn) {
    (*elr->global_info->behavior_fn)(elr, elr->local_info->spawning_event);
  }
  
  /* Use the caller data to set up the loop finalizer. */
  GalIO_AddBrokerCallback(b, GAL_BROKER_DESTRUCTION_EVENT,
                          __GalSS_ELRBrokerShutdown,
                          (void *) elr);
}

/* Next, the incoming broker. This is much more like the
   connection. */

static void __GalSS_ELRSetupBrokerIn(GalIO_CommStruct *gcomm,
                                     GalIO_BrokerStruct *b,
                                     void *caller_data)
{
  GalSS_ELR *elr = GalSS_ELRCopy((GalSS_ELR *) caller_data);

  elr->local_info->broker = b;
  elr->local_info->spawning_event = GAL_CONNECTION_BROKER_IN_STARTUP_EVENT;

  if (elr->global_info->timer_set_fn) {
    elr->local_info->timer_tag = (*elr->global_info->timer_set_fn)(elr, elr->global_info->broker_ms);
    elr->local_info->timer_is_set = 1;
  }

  if (elr->global_info->fd_set_fn) {
    elr->local_info->fd_tag = (*elr->global_info->fd_set_fn)(elr, GalIO_GetBrokerSocket(b));
    elr->local_info->fd_is_set = 1;
  }

  if (elr->global_info->behavior_fn) {
    (*elr->global_info->behavior_fn)(elr, elr->local_info->spawning_event);
  }
  
  /* Use the caller data to set up the loop finalizer. */
  GalIO_AddBrokerCallback(b, GAL_BROKER_DESTRUCTION_EVENT,
                          __GalSS_ELRBrokerShutdown,
                          (void *) elr);
}

static void __GalSS_ELRDestroyServer(GalIO_ServerStruct *scomm,
                                     void *callback_data)
{
  GalSS_ELR *elr = (GalSS_ELR *) callback_data;
  
  /* There's no shutdown to do here, because it happens in
     the listener and client shutdown routines. However,
     we should do the behavior. */  
  if (elr->global_info->behavior_fn) {
    (*elr->global_info->behavior_fn)(elr, GAL_SERVER_DESTRUCTION_EVENT);
    elr->global_info->behavior_fn = (GalSS_ELBehaviorFn) NULL;
  }
  /* Also, don't destroy this one, because it was returned to
     the user. */
}

static void __GalSS_ELRShutdownServerListener(GalIO_ServerStruct *scomm,
                                              void *callback_data)
{
  GalSS_ELR *elr = (GalSS_ELR *) callback_data;

  GalSS_ELRShutdown(elr);  
  if (elr->global_info->behavior_fn) {
    (*elr->global_info->behavior_fn)(elr, GAL_SERVER_LISTENER_SHUTDOWN_EVENT);
  }
  GalSS_ELRDestroy(elr);
}

/* This function is only used when the server is running its
   own listener. */

static void __GalSS_ELRSetupServerListener(GalIO_ServerStruct *scomm,
                                           void *callback_data)
{
  GalSS_ELR *elr = GalSS_ELRCopy((GalSS_ELR *) callback_data);

  /* I'd rather not copy this one, but I need to set the
     spawning event. */

  elr->local_info->spawning_event = GAL_SERVER_LISTENER_STARTUP_EVENT;
  
  /* You only need a file descriptor callback here, since
     there will be no connection requests in any internal queue. */

  if (elr->global_info->fd_set_fn) {
    elr->local_info->fd_tag = (*elr->global_info->fd_set_fn)(elr, GalIO_GetServerListenSocket(scomm));
    elr->local_info->fd_is_set = 1;
  }
  if (elr->global_info->behavior_fn) {
    (*elr->global_info->behavior_fn)(elr, elr->local_info->spawning_event);
  }
  GalIO_AddServerCallback(scomm,
                          GAL_SERVER_LISTENER_SHUTDOWN_EVENT,
                          __GalSS_ELRShutdownServerListener,
                          (void *) elr);
}

static void __GalSS_ELRShutdownServerClient(GalIO_ServerStruct *scomm,
                                            void *callback_data)
{
  GalSS_ELR *elr = (GalSS_ELR *) callback_data;

  GalSS_ELRShutdown(elr);
  /* Don't do the behavior, because this isn't the callback
     that's always fired for the server destruction. */
  GalSS_ELRDestroy(elr);
}

/* This function is used when the server is subscribing to
   Hub listeners. */

static void __GalSS_ELRSetupServerClient(GalIO_ServerStruct *scomm,
                                         void *callback_data)
{
  GalSS_ELR *elr = GalSS_ELRCopy((GalSS_ELR *) callback_data);

  /* I'd rather not copy this one, but I need to set the
     spawning event. */
  
  elr->local_info->spawning_event = GAL_SERVER_CLIENT_POLL_STARTUP_EVENT;
    
  /* Set up a periodic task to check the hub contacts. */
  if (elr->global_info->timer_set_fn) {
    elr->local_info->timer_tag = (*elr->global_info->timer_set_fn)(elr, elr->global_info->server_client_ms);
    elr->local_info->timer_is_set = 1;
  }
  if (elr->global_info->behavior_fn) {
    (*elr->global_info->behavior_fn)(elr, elr->local_info->spawning_event);
  }
  
  /* Add a shutdown callback now. */
  GalIO_AddServerCallback(scomm,
                          GAL_SERVER_DESTRUCTION_EVENT,
                          __GalSS_ELRShutdownServerClient,
                          (void *) elr);
}

/* Finally, here's the toplevel initialization function for the
   record and the server. You can do it piecemeal, but this
   should serve just about everyone's purposes. We pass in
   the loop data because it may be important when ServerStart
   is called. */

GalSS_ELR *GalSS_ELRSetupServer(GalSS_ServerArgs *external_arg_pkg,
                                int argc, char **argv,
                                GalSS_ELTimerSetFn timer_set_fn,
                                GalSS_ELUnsetFn timer_unset_fn,
                                GalSS_ELFDSetFn fd_set_fn,
                                GalSS_ELUnsetFn fd_unset_fn,
                                GalSS_ELBehaviorFn behavior_fn,
                                void *loop_data,
                                void (*loop_data_free_fn)(void *),
                                int timer_is_persistent)
{
  GalIO_ServerStruct *scomm;
  int new_argc;
  char **new_argv;
  GalSS_ServerArgs *arg_pkg;
  int free_arg_pkg = 0;
  GalSS_ELR *elr;

  /* If you want to use the built-in server arguments, you
     can use GalSS_ExtractCmdlineServerArgs. Otherwise, you can just
     call GalSS_InitializeServerToplevel(). */

  if (external_arg_pkg) {
    arg_pkg = external_arg_pkg;
  } else {
    arg_pkg = GalSS_DefaultServerArgs();
    free_arg_pkg = 1;
  }

  /* Make sure it knows that we're using our own main loop. We set this 
   before we ever parse the server arguments, because we don't even want
   the arguments pertaining to the loop type enabled for the user. */
  
  GalSS_SAFixLoopType(arg_pkg, GAL_LOOP_EXTERNAL);
  arg_pkg = GalSS_ExtractCmdlineServerArgs(arg_pkg, argc, argv,
                                           &new_argc, &new_argv);

  if (!arg_pkg) {
    /* Something bad happened, or -help was passed. */
    return (GalSS_ELR *) NULL;
  }

  /* Now, we call GalSS_InitializeServerFromServerArgs, and we don't have
     to worry about the signature of GalSS_InitializeServerToplevel. */

  scomm = GalSS_SetupServer(arg_pkg, new_argc, new_argv);
  if (free_arg_pkg)
    GalSS_FreeArgPkg(arg_pkg);
  GalSS_FreeArgv(new_argc, new_argv);
  
  if (!scomm) {
    GalUtil_Warn("Couldn't create a server\n");
    return (GalSS_ELR *) NULL;
  }

  elr = GalSS_ELRCreate(scomm, timer_set_fn, timer_unset_fn,
                        fd_set_fn, fd_unset_fn, behavior_fn,
                        timer_is_persistent);
  if (!elr) {
    GalIO_DestroyServerStruct(scomm);
    GalUtil_Warn("Couldn't create a server\n");
    return (GalSS_ELR *) NULL;
  }

  if (loop_data) {
    GalSS_ELRSetLoopData(elr, loop_data, loop_data_free_fn);
  }

  /* Now, start the server, and then the main loop. */
  if (!GalIO_ServerStart(scomm)) {
    GalUtil_Warn("Couldn't start the server\n");
    GalSS_ELRDestroy(elr);
    return (GalSS_ELR *) NULL;
  }
  return elr;
}

