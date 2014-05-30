/*
  This file (c) Copyright 2000 - 2001 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _H_GENERIC_SERVER_TYPES
#define _H_GENERIC_SERVER_TYPES

#include "galaxy/util.h"

/* Observe that galaxy_io.h or some equivalent needs to
   be loaded in order to load this file. This had to be
   broken out because I needed the types in io_internal.h. */

typedef struct __GalSS_ServerArgs {
  /* Added this for binding support. C doesn't use it yet. */
  char *server_name;
  unsigned short server_port;
  int max_conns;
  int use_color;
  int do_assert;
  int loop_type;
  int validate;
  int verbosity;
  int server_listen_status;
  char *client_pair_string;
  char *session_id;
  char *server_locations_file;
  char *slf_name;
  GalUtil_OASDescription *oas_descr;
  /* 0 for bindings. */
  int do_initialize_defaults;
  /* Flags to check. */
  int port_disabled;
  int assert_disabled;
  int color_disabled;
  int loop_type_disabled;
  int maxconns_disabled;
  int validate_disabled;
  int verbosity_disabled;
  int contact_hub_info_disabled;
  int server_listen_status_disabled;
  int server_locations_disabled;
  int slf_disabled;
} GalSS_ServerArgs;

/* SAM 10/2/99: The environment object will be used to handle the server-side
   reinitialize fakery, as well as indicating error returns, etc. This will
   be the element guaranteed to be passed to a dispatch function. */

/* SAM 10/3/00: Major revision of the semantics of environments, and
   much better use of them. No more special handling for reinitialize,
   but much better handling of continuations of threads of execution,
   support for saving away environments, etc. */

typedef struct __galss_environment {
  GalIO_CommStruct *gcomm;
  Gal_Frame hub_data;
  char *session_id;
  char *op_name;
  int return_satisfied;
  int return_required;
  int return_postponed;
  int reference_count;
  int session_id_promoted;
  double inherited_token_timestamp;
} GalSS_Environment;

/* From server_locations.c. We're trying to address the issue
   of how to locate a server, on both the Hub and server sides.
   Turns out that the MIT hub scripting doesn't care about
   the server location (it's only checked in hub_init.c), so I
   don't need to worry about modifying the scripting language. */

enum {GAL_SL_HUB_LISTENS, GAL_SL_SERVER_LISTENS};

typedef struct __galss_provider_spec {
  char *stype_name;
  char *host;
  int port;
  char *id_name;
  int id;
} GalSS_ProviderSpec;

typedef struct __galss_server_location_entry {
  GalSS_ProviderSpec provider_spec;
  int hub_or_server;
  struct __galss_server_location_entry *next;
} GalSS_ServerLocationEntry;

/* From galaxy_elr.c. */

struct __GalSS_ELR;

typedef void *(*GalSS_ELTimerSetFn)(struct __GalSS_ELR *, int ms);
typedef void *(*GalSS_ELFDSetFn)(struct __GalSS_ELR *, GAL_SOCKET fd);
typedef void (*GalSS_ELUnsetFn)(struct __GalSS_ELR *, void *tag);
typedef void (*GalSS_ELBehaviorFn)(struct __GalSS_ELR *, int condition);
typedef int (*GalSS_ELCallbackFn)(struct __GalSS_ELR *elr,
				  int timer_or_fd);

/* Some of this info is global and some is local. We should
   have two different structures for it. */

typedef struct __GalSS_ELRGlobalInfo {
  int timer_is_persistent;  
  GalSS_ELTimerSetFn timer_set_fn;
  GalSS_ELUnsetFn timer_unset_fn;  
  GalSS_ELFDSetFn fd_set_fn;
  GalSS_ELUnsetFn fd_unset_fn;
  GalSS_ELBehaviorFn behavior_fn;
  GalSS_ELCallbackFn connection_callback;
  GalSS_ELCallbackFn broker_out_callback;
  GalSS_ELCallbackFn broker_in_callback;
  GalSS_ELCallbackFn server_listener_callback;
  GalSS_ELCallbackFn server_client_callback;
  int server_client_ms;
  int conn_ms;
  int broker_ms;  
  void *loop_data;
  void (*loop_data_free_fn)(void *);
} GalSS_ELRGlobalInfo;

typedef struct __GalSS_ELRLocalInfo {
  GalIO_ServerStruct *scomm;
  GalIO_CommStruct *gcomm;
  GalIO_BrokerStruct *broker;
  void *timer_tag;
  int timer_is_set;
  void *fd_tag;
  int fd_is_set;
  int spawning_event;
} GalSS_ELRLocalInfo;

#define GALSS_ELR_IS_ROOT 1
#define GALSS_ELR_DESTROYED 2
#define GALSS_ELR_IN_CALLBACK 4

typedef struct __GalSS_ELR {
  GalSS_ELRGlobalInfo *global_info;
  GalSS_ELRLocalInfo *local_info;
  /* If it's a root, if it's in a callback,
     if it's marked for destruction. */
  int status;
} GalSS_ELR;

enum {GALSS_ELR_TIMER, GALSS_ELR_FD};

/* SAM 2/4/02: As of 4.0, we now support proxy objects. */

typedef struct GalSS_BrokerProxy {
  char *call_id;
  char *host;
  int port;
  Gal_ObjectType object_type;
  struct __GalIO_BrokerStruct *broker;
  struct __GalIO_Callback *removal_cb;
  int already_brokered;
  Gal_Object obj;
} GalSS_BrokerProxy;

#endif

