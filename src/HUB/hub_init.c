/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
/*
#ifndef WIN32
#include <unistd.h>
#include <strings.h>
#include <sys/ioctl.h>
#endif
#include <ctype.h>
*/
#include "galaxy/galaxy_all.h"

#include "hub.h"
#include "hub_internal.h"

#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))

#ifdef WIN32
static int global_next_sockid = 0;	/* WinSock SOCKET is not int, assign an int sockid */
#endif

static Gal_Frame __GalHUB_ConstructReinitializationFrame(SERVICE_TYPE **stypes, int num_stypes, SERVICE_PROVIDER *s, HUB *h, char *session_id);

static void __GalHUB_AddListenerProxy(SERVICE_TYPE *stype, HUB *h);
static int __GalHUB_InitializeListenerProxy(LISTENER_PROXY *p, HUB *h,
					    int require_port);

static void __gal_hub_free_server_msg(void *data)
{
  GalHUB_FreeServerMessage((SERVER_MESSAGE *) data);
}

extern
GalIO_PointerQueue *_GalIO_NewPointerQueue(int type, int volatile_queue,
					   int mutexable,
					   void (*free_fn));

/* also sets global Hub */
HUB *new_hub(char *filename, int validate, int debug,
	     int suppress_pacifier, char *gui_location)
{
  HUB *h;

  if (filename == NULL)
  {
    GalUtil_Warn("No program file specified");
    return(NULL);
  }

  if (!(h = (HUB*)calloc(1,sizeof(HUB))))
  {
    GalUtil_WarnWithLocation(__FUNCTION__,
			     "Could not allocate memory for Hub");
    return(NULL);
  }

  Hub = h;
  h->hub_control_file_name = _gal_strdup(filename);
  h->maxfd = 0;
  h->hub_pause = 0;
  h->globals = Gal_MakeFrame("global", GAL_CLAUSE);
  GalHUB_SetHubDefaultDomain(h, "Unspecified", "DOMAIN:");
  h->script = 1;
  h->listener_proxies = Gal_MakePointerBuffer(NULL, GAL_LISTENER_PROXY_PTYPE,
					      0, 0, 1, 1,
					      NULL, 10, 0);
  
  /* set the validation variable, before the hub is initialized.
     This is so init_hub() knows whether to validate signatures
     against the operation list. */
  
  h->validate = validate;
  
  /* set the debugging variable */
  h->debug = debug;

  h->suppress_pacifier = suppress_pacifier;

  if (gui_location) {
    /* Decode the GUI location. */
    h->gui = GalHUB_CreateSpecialServer(gui_location, "__gui__");
  }
  h->message_queue = _GalIO_NewPointerQueue(GAL_SERVER_MESSAGE_PTYPE,
					    1, 1, __gal_hub_free_server_msg);

  GalHUB_InstantiateLogRecord(h);
  
  return h;
}

HUB *init_hub(HUB *h, char *init_vars,
	      char *location_overrides,
	      char *locations_file)
{
  int ok, num_progs=0;
  
  num_progs = HC_ReadControlFile(h, h->hub_control_file_name, init_vars);
  
  if (num_progs < 0) {
    GalUtil_Warn("Problems reading control file %s",
		 h->hub_control_file_name);
    return(NULL);
  } else if (num_progs == 0) {
    GalUtil_Warn("No programs defined, running in scriptless mode");
    h->script = 0;
  }

  /* If locations were overriden, update the servers
     before making the connections. There may be multiple servers
     with the same name, in which case the override will not happen. */
  if (location_overrides) {
    override_locations(h, location_overrides);
  }

  if (locations_file) {
    override_locations_with_file(h, locations_file);
  }
  
  ok = initialize_connections(h);

  if (ok != Gal_PointerBufferSize(h->servers)) {
    GalUtil_Warn("Not all servers responded (only %d out of %d)", ok, Gal_PointerBufferSize(h->servers));
  }
  return(h);
}

void
quit_hub(HUB *h)
{
/*   GalHUB_SessionFlushLogfile(); */
  return;
}

extern void _GalIO_QueueDestroy(GalIO_PointerQueue *queue);

void free_hub(HUB *h)
{
  /* Remove all the service providers. */
  if (h->servers) {
    int i;
    int num_servers = Gal_PointerBufferSize(h->servers);
    SERVICE_PROVIDER **servers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(h->servers);

    for (i = 0; i < num_servers; i++) {
      /* Don't remove the provider from the Hub. We'll just
	 free the array. */
      /* Shut down the connection, if there is one. */
      if (servers[i]->gcomm) {
	GalIO_SetCommDone(servers[i]->gcomm);
	GalIO_DestroyCommStruct(servers[i]->gcomm);
      }
      GalHUB_RemoveServiceProvider(h, servers[i], 1, 0);
    }
    Gal_FreePointerBuffer(h->servers);
  }
  GalHUB_DestroyLocalServer(h);

  /* Free active servers. */

  if (h->active_servers) {
    GalSS_ProviderSpec **active_server_specs = (GalSS_ProviderSpec **) Gal_PointerBufferPointers(h->active_servers);
    int num_server_specs = Gal_PointerBufferSize(h->active_servers);
    int i;

    for (i = 0; i < num_server_specs; i++) {
      GalSS_FreeProviderSpec(active_server_specs[i]);
    }
    Gal_FreePointerBuffer(h->active_servers);
  }    
  
  /* Remove all the service types. */
  if (h->stypes) {
    int i;
    int num_stypes = Gal_PointerBufferSize(h->stypes);
    SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(h->stypes);

    for (i = 0; i < num_stypes; i++) {
      GalHUB_RemoveServiceType(stypes[i], h, 0, 0);
    }
    Gal_FreePointerBuffer(h->stypes);
  }
  
  if (h->hub_control_file_name)
    free(h->hub_control_file_name);
  if (h->default_domain_key)
    free(h->default_domain_key);
  if (h->init_keys)
    free_key_pairs(h->init_keys);
  if (h->user_id_key)
    free(h->user_id_key);
  if (h->session_id_key)
    free(h->session_id_key);
  if (h->data_topdir_key)
    free(h->data_topdir_key);
  Gal_FreeFrame(h->globals);
  Gal_FreeFrame(h->init_state);
  GalHUB_FreeLogRecord(h);
  /* Free the listener proxies. */
  if (h->listener_proxies) {    
    int num_proxies = Gal_PointerBufferSize(h->listener_proxies);
    LISTENER_PROXY **proxies = (LISTENER_PROXY **) Gal_PointerBufferPointers(h->listener_proxies);
    int j;

    for (j = 0; j < num_proxies; j++) {
      Gal_FreePointerBuffer(proxies[j]->stypes);
      if (proxies[j]->scomm) {
	GalIO_SetServerDone(proxies[j]->scomm);
	GalIO_DestroyServerStruct(proxies[j]->scomm);
      }
      free(proxies[j]);
    }
    Gal_FreePointerBuffer(h->listener_proxies);
  }
  
  /* GUI */
  if (h->gui) {
    GalHUB_FreeSpecialServer(h->gui);
  }
  /* Tokens */
  while (h->token) {
    h->token->ref = 0;
    destroyTokenIfDone(h, h->token, (SESSION *) NULL, (Gal_Frame) NULL, (GalIO_MsgType) 0);
  }
  /* Message queue */
  _GalIO_QueueDestroy(h->message_queue);
  free(h);
}

/* SAM 11/16/00: The provider may have multiple service types,
   each with its own operations list. */

static void
_GalHUB_InitializeServerSignatures(SERVICE_PROVIDER *s,
				   Gal_DispatchFnSignature *signatures)
{
  int num_stypes = Gal_PointerBufferSize(s->stypes);  
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(s->stypes);
  int j;
  s->signatures = signatures;
  
  if (s->signatures) {
    /* Check the signatures against the operations.
       Only notify about those operations which aren't present. */
    int i;
    char *op;
    for (j = 0; j < num_stypes; j++) {
      for (i = 0; (op = stypes[j]->operations[i]); i++) {	
	if (!Gal_FindNamedSignature(s->signatures, op)) {
	  GalUtil_Warn("Hub script declares operation `%s' for service type %s, but the provider at %s:%d does not define it\n", op, stypes[j]->name, s->host, s->port);
	}
      }
    }
  }
}

/* SAM 4/27/02: Trying to add servers to messages. */

extern
void _GalIO_QueueApply(GalIO_PointerQueue *queue,
		       int (*fn)(void *data, void *caller_data),
		       void *caller_data);

/* Return 1 to continue, 0 to halt, -1 to dequeue. */

static int __perhaps_add_provider(void *data, void *caller_data)
{
  SERVICE_PROVIDER *s = (SERVICE_PROVIDER *) caller_data;
  SERVER_MESSAGE *msg = (SERVER_MESSAGE *) data;
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(s->stypes);
  int num_stypes = Gal_PointerBufferSize(s->stypes);
  int i;

  for (i = 0; i < num_stypes; i++) {
    GalHUB_PerhapsAddProvidersToMsg(msg, stypes[i],
				    msg->namespace_array,
				    &s, 1);
    /* This means that we shouldn't consider any other
       stypes. */
    if (msg->true_provider_spec.stype_name &&
	!strcmp(msg->true_provider_spec.stype_name, stypes[i]->name)) {
      break;
    }
  }
  return 1;
}

/* SAM 11/21/00: Added properties at initialization. */

static int __GalHUB_ConfigureNewConnection(HUB *h, SERVICE_PROVIDER *s,
					   Gal_Frame reply_frame,
					   int special, int reconnect)
{
  Gal_Object sigs;
  int retval = 0;

  if (!s->gcomm) {
    if (s->status > DISCONNECTED) {
      s->status = DISCONNECTED;
    }
    retval = -1;
  } else {
    GalUtil_Print(GAL_CONNECTION_VERBOSITY_LEVEL, "Connected to provider %s\n", s->pname);
    s->sock = GalIO_GetCommSocket(s->gcomm);
#ifndef WIN32
    s->sockid = s->sock;
#else
    s->sockid = global_next_sockid++;
#endif
    s->status = FREE;
    /* maxfd is used for select(). Windows sockets ignore the maxfd argument. */
    h->maxfd = max(s->sockid, h->maxfd);
    
    /* If there are multiple possible service types for a
       provider, it's not really possible to know how the
       signatures split up among the service types. And it doesn't
       matter unless validation happens, and at this point,
       all the validation does is check to see if each
       operation is represented among the service types for the server. */
    if (h->validate && reply_frame) {
      sigs = Gal_GetObject(reply_frame, GAL_SIGNATURES_FRAME_KEY);
      if (sigs) {
	_GalHUB_InitializeServerSignatures(s, Gal_DecodeDispatchFnSignatures(sigs));
      }
    }
    /* Properties override the defaults. */
    Gal_UpdateFrameProperties(s->properties, Gal_GetFrame(reply_frame, GAL_SERVER_PROPERTIES_FRAME_KEY),
			      (char **) NULL);
  }
  
  /* If it's not a special server, announce it to the gui. */
  if (!special) {
    if (retval == -1) {
      _GalHUB_GUIAnnounceServiceProviderConnection(h, s, 0, reconnect);
    } else {
      _GalHUB_GUIAnnounceServiceProviderConnection(h, s, 1, reconnect);
      /* If it's newly connected, then try to add it to the eligible
	 providers in the message queue. Even if it was there but
	 disconnected, it will already be in the queue if it were eligible. */
      if (!reconnect) {
	/* Loop through the service types. Use the namespace array
	   which is already in the message queue. */
	_GalIO_QueueApply(h->message_queue, __perhaps_add_provider,
			  (void *) s);
      }
    }
  }
  return retval;
}

static int __GalHUB_InitClientConnection(HUB *h, SERVICE_PROVIDER *s,
					 Gal_Frame f, int silent,
					 int special, int reconnect)
{
  Gal_Frame reply_frame = (Gal_Frame) NULL;
  int status;
  
  s->gcomm = GalIO_ClientConnect(s->pname,
				 s->host, (unsigned short) s->port,
				 silent, f, &reply_frame);
  status = __GalHUB_ConfigureNewConnection(h, s, reply_frame, special, reconnect);
  if (reply_frame) 
    Gal_FreeFrame(reply_frame);
  return status;
}

/* This function takes care of accepting a connection from a
   remote server for true servers. The handshake is handled before
   any frames are shipped back and forth. */

/* These functions ought to be static in src/libGalaxy/io/hub_server.c,
   but the "hub as listener" case needs so much from this
   file here that it needs to live here as well, so some of the
   functions from hub_server.c need to be public. */

extern int __GalIO_ServerContactHandler(GalIO_ServerStruct *gcomm,
					GalIO_CommStruct **new_conn_ptr);
extern void __GalIO_IDestroyCommStruct(GalIO_CommStruct *gcomm,
				       int mutex_locked);
extern void __GalIO_ServerInsertConnection(GalIO_ServerStruct *scomm,
					   GalIO_CommStruct *gcomm,
					   int mutex_locked);
extern void __GalIO_MarkSuccessfulHandshake(GalIO_CommStruct *gcomm);
extern int __GalIO_HandshakeComplete(GalIO_CommStruct *gcomm);
extern int _GalIO_HubVerificationHandler(GalIO_CommStruct *gcomm, Gal_Frame init_frame);

/* GalIO_HubListenerHandshakeHandler: server as client: Fields
   GalIO_ServerInformationFrame(), sends
   error or pacifier. */

static int GalHUB_HubListenerHandshakeHandler(LISTENER_PROXY *p,
					      SERVICE_TYPE ***s_ptr,
					      int *num_stypes_ptr,
					      GalIO_CommStruct *new_conn,
					      Gal_Frame *info_ptr)
{
  Gal_Frame f = (Gal_Frame) NULL;
  Gal_Frame reply_f;
  char *server_name;
  char *session_id;
  int i;
  int num_stypes;
  SERVICE_TYPE **stypes;
  Gal_Object stypes_obj;
  int num_extra_stypes;
  char **extra_stype_names = (char **) NULL;
  Gal_Object *extra_stypes;
  int num_local_stypes = 0;
  SERVICE_TYPE **local_stypes = (SERVICE_TYPE **) NULL;

  if (num_stypes_ptr)
    *num_stypes_ptr = 0;
  if (s_ptr)
      *s_ptr = (SERVICE_TYPE **) NULL;
  if (info_ptr)
    *info_ptr = (Gal_Frame) NULL;
  
  /* For completeness. This should never be called, and I'm
     not sure what I'd do if it were. */
  if (__GalIO_HandshakeComplete(new_conn)) {
    return 0;
  }
  
  /* We got a connection, everything's cool so far. Now
     we process the handshake. */
  
  /* First, we read a frame from the server. */
  if (GalIO_CommReadFrame(new_conn, &f, 1) != 1) {
      /* The read failed, for some reason */
    return -1;
  }

  /* The name of the server will be the name of the frame,
     and if there's a session ID, we make it a session-specific
     server. */
  server_name = Gal_FrameName(f);
  session_id = Gal_GetString(f, GAL_SESSION_ID_FRAME_KEY);
    
  num_stypes = Gal_PointerBufferSize(p->stypes);
  stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(p->stypes);

  /* We can't have any more stypes than the number the
     listener has. */
  local_stypes = (SERVICE_TYPE **) calloc(num_stypes, sizeof(SERVICE_TYPE *));  
  num_local_stypes = 0;
  
  /* Which servers should we add the new server to? We use the
     server name, as well as the contents of the extra service types slot. */

  /* The service provider can provide
     NO MORE THAN the services the program file specifies it
     can provide. So we should check to see if all the service
     types are found in the list, and discard the ones which
     aren't, rather than adding all the service types mentioned.
     Like name matching, this is only checked when a client connects. */
  
  stypes_obj = Gal_GetObject(f, GAL_SERVICE_TYPE_FRAME_KEY);
  if (stypes_obj) {
    int j = 0;
    extra_stypes = Gal_ListValue(stypes_obj, &num_extra_stypes);
    if (num_extra_stypes > 0)
      extra_stype_names = (char **) calloc(num_extra_stypes, sizeof(char *));
    for (i = 0; i < num_extra_stypes; i++) {
      if (Gal_Stringp(extra_stypes[i]))
	extra_stype_names[j++] = Gal_StringValue(extra_stypes[i]);
    }
    num_extra_stypes = j;
  }
  
  for (i = 0; i < num_stypes; i++) {
    if (!strcmp(stypes[i]->name, server_name)) {
      local_stypes[num_local_stypes++] = stypes[i];
    } else if (extra_stype_names) {
      int j;
      for (j = 0; j < num_extra_stypes; j++) {
	if (!strcmp(stypes[i]->name, extra_stype_names[j])) {
	  local_stypes[num_local_stypes++] = stypes[i];
	  break;
	}
      }
    }
  }
  
  if (extra_stype_names)
    free(extra_stype_names);

  reply_f = (Gal_Frame) NULL;
  /* If there's no server, reject because of match failure. */
  if (num_local_stypes == 0) {
    reply_f = GalIO_CreateErrorFrame(GAL_CONN_REJECTION_ERROR,
				     "Hub listener isn't listening for named server");
  }

  /* If there is a server, but it's a special service type and
     the provider is already present, reject because of
     already filled. */
  if ((num_local_stypes == 1) && local_stypes[0]->special &&
      local_stypes[0]->special->provider) {
    reply_f = GalIO_CreateErrorFrame(GAL_CONN_REJECTION_ERROR,
				     "Special server already filled");
  }      

  if (reply_f) {
    GalIO_CommWriteMessage(new_conn, reply_f, GAL_ERROR_MSG_TYPE, 1);
    Gal_FreeFrame(reply_f);
    Gal_FreeFrame(f);
    if (local_stypes)
      free(local_stypes);
    return -1;
  } else {
    /* At this point, the connection is good. Send a pacifier return.
       Make sure the protocol version is sent. */
    Gal_SetProp(f, GAL_PROTOCOL_VERSION_FRAME_KEY,
		 Gal_IntObject(GAL_TRANSPORT_PROTOCOL_VERSION));
    GalIO_CommWriteMessage(new_conn, f, GAL_REPLY_MSG_TYPE, 1);
    if (info_ptr)
      *info_ptr = f;
    else
      Gal_FreeFrame(f);
    if (s_ptr) {
      *s_ptr = local_stypes;
    }
    if (num_stypes_ptr) {
      *num_stypes_ptr = num_local_stypes;
    }
    __GalIO_MarkSuccessfulHandshake(new_conn);
    return 0;
  }
}


void GalHUB_AcceptClientConnection(LISTENER_PROXY *p, HUB *h)
{
  Gal_Frame f = (Gal_Frame) NULL;
  Gal_Frame info_f = (Gal_Frame) NULL;
  GalIO_CommStruct *new_conn;
  int status;
  char *session_id;
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) NULL;
  int num_stypes = 0;
  
  /* Next, we accept a connection and then verify it using the
     Hub verifier. The server handler will do another poll, but
     there really ought to be something there already. */
  status = __GalIO_ServerContactHandler(p->scomm, &new_conn);
  switch (status) {
  case 1:
    /* We got a connection, everything's cool so far. Now
       we process the handshake. */
    
    status = GalHUB_HubListenerHandshakeHandler(p, &stypes, &num_stypes,
						new_conn, &info_f);
    
    switch(status) {
    case -1:
      __GalIO_IDestroyCommStruct(new_conn, 0);
      return;
    default:

      /* Insert the connection before we do anything else. */
      __GalIO_ServerInsertConnection(p->scomm, new_conn, 0);
      
      /* First, we construct the reinitialization frame. */
      session_id = Gal_GetString(info_f, GAL_SESSION_ID_FRAME_KEY);
      
      f = __GalHUB_ConstructReinitializationFrame(stypes, num_stypes,
						  (SERVICE_PROVIDER *) NULL,
						  h, session_id);

      /* Next, we verify the Hub side of the connection. */
      
      /* SAM 9/19/00: The reply for the signatures will now be obtained
	 from GalIO_HubListenerHandshakeHandler, rather than
	 _GalIO_HubVerificationHandler. The response from reinitialize
	 is meaningless. */

      while (1) {
	status = _GalIO_HubVerificationHandler(new_conn, f);
	if (status)
	  break;
      }
      
      Gal_FreeFrame(f);
      if (status == -1) {
	GalUtil_Warn("Error verifying connection to listener for service type %s",
		     stypes[0]->name);
	GalIO_SetCommDone(new_conn);
	GalIO_DestroyCommStruct(new_conn);
	if (info_f)
	  Gal_FreeFrame(info_f);
	if (stypes)
	  free(stypes);
	return;
      } else {
	/* We've got a solid connection, and it's verified. */
	int i;
	int is_special = 0;
	SERVICE_PROVIDER *new_s = GalHUB_NewServiceProvider("<remote>", -1, (SERVICE_TYPE *) NULL);
	new_s->listen_status = GAL_HUB_STYPE_SERVER;

	/* GalHUB_AddServiceProvider relies on listen status. */
	for (i = 0; i < num_stypes; i++) {
	  GalHUB_AddServiceProviderStype(new_s, stypes[i]);
	  if (stypes[i]->special)
	    is_special = 1;
	}
	GalHUB_RecordServiceProvider(h, new_s);
	free(stypes);
	new_s->gcomm = new_conn;
	/* This function relies on gcomm. This function also announces
	   the new provider to the GUI. It's never a reconnection.
	   It MAY be a special server. */
	__GalHUB_ConfigureNewConnection(h, new_s, info_f, is_special, 0);
	/* I used to add __GalHUB_ServerConnDisconnectFn as
	   a server disconnect callback, but it looks to me
	   like processNoServerMessage handles things just fine. */
	  
	/* If the handshake had a session ID in it, then we set up the
	   locks so that it reads to and writes from the specified session,
	   and we lock it. */
	if (session_id) {
	  /* First, make sure the session exists. */
	  SESSION *session = GalHUB_SessionLookupByID(session_id, 1);
	  int lock_value = GAL_SERVER_READS_ONLY_FROM_SESSION |
	    GAL_SERVER_WRITES_ONLY_TO_SESSION |
	    GAL_SESSION_WRITES_ONLY_TO_SERVER |
	    GAL_PERMANENT_LOCK;

	  GalHUB_SessionHandleLocks(session, new_s, 0,
				    lock_value, lock_value, 0, 1, 1);
	}
	/* Session ID comes out of info_f, so be sure not
	   to free info_f before the session is created. */
	if (info_f) 
	  Gal_FreeFrame(info_f);
	return;
      }
    }
    break;
  case 0:
    /* We didn't find anything. Just hang loose. */
    return;
  case -1:
    /* We got an error. Deal with this later. */
    GalUtil_Warn("Listener at port %d got an error\n", p->port);
    return;
  }  
}

static Gal_Frame
__GalHUB_ConstructReinitializationFrame(SERVICE_TYPE **stypes,
					int num_stypes,
					SERVICE_PROVIDER *s,
					HUB *h, char *session_id)
{
  Gal_Frame fr = NULL;
  Gal_Frame admin_info = Gal_MakeFrame("admin_info", GAL_CLAUSE);
  int i;
  
  /* SAM 10/17/99: Stephanie and I have agreed that the order of
     priority for the reinitialize message is cmdline > INIT: > globals.
     It turns out that the current implementation has the INITIAL_TOKEN:
     feeding the reinitialize message, which I think is kind of
     wrong. But I'll have to talk to Stephanie about that. At the
     very least, it's now cmdline > INIT: > INITIAL_TOKEN: > globals.
     So ":domain" will already have been set, as will ":user_id". */
  
  if (h->init_state) {
    fr = Gal_CopyFrame(h->init_state);
    Gal_SetFrameName(fr, "reinitialize");
  } else {
    fr = Gal_MakeFrame("reinitialize", GAL_CLAUSE);
  }
  /* Demand a reply. */
  
  Gal_SetProp(admin_info, GAL_ROUND_TRIP_FRAME_KEY, Gal_IntObject(1));

  /* Gal_SetProp(fr,":domain", Gal_CopyObject(GalHUB_GetHubDefaultDomain(h)));
     Gal_SetProp(fr,":user_id",Gal_CopyObject(GalHUB_GetHubUserID(h))); */
  if ((!Gal_GetObject(fr, ":server_type")) && s)
    Gal_SetProp(fr,":server_type", Gal_StringObject(s->iname));
  
  /* At this point, we've taken care of INITIAL_TOKEN: > globals.
     So this is the cmdline > INIT: slice. */
  /* SAM 11/16/00: One final twist. Now that we've divided things
     into service types and service providers, both can have init_kps.
     So the service provider takes precedence over the server types,
     which I will add in order. */

  for (i = 0; i < num_stypes; i++) {
    _GalHUB_CopyInitKeys(fr, stypes[i]->init_kps, 1);
  }
  if (s)
    _GalHUB_CopyInitKeys(fr, s->init_kps, 1);
  _GalHUB_CopyInitKeys(fr, h->init_keys, 1);
  
  if (session_id) {
    Gal_SetProp(admin_info, GAL_SESSION_ID_FRAME_KEY,
		Gal_StringObject(session_id));
  }
  Gal_SetProp(fr, GAL_HUB_OPAQUE_DATA_FRAME_KEY,
	      Gal_FrameObject(admin_info));
  return fr;
}


/* initialize_connection_to_server returns 1 if success */

int initialize_connection_to_server(SERVICE_PROVIDER *s, HUB *h, int silent,
				    int special, int reconnect)
{
  Gal_Frame fr = __GalHUB_ConstructReinitializationFrame((SERVICE_TYPE **) Gal_PointerBufferPointers(s->stypes), Gal_PointerBufferSize(s->stypes), s, h, (char *) NULL);
  int status;
  
  status = __GalHUB_InitClientConnection(h, s, fr, silent, special, reconnect);
  Gal_FreeFrame(fr);
  if (status == -1) {
    if (!silent)
      GalUtil_Warn("Error connecting to provider %s", s->pname);
    return 0;
  }
  return 1;
}
 
/* _GalHUB_InitializeInternalHubServer returns 1 if success */

/* Turns out, I think, that this callback is never needed. The
   Hub shuts everything down appropriately in processNoServerMessage. */

#if 0
static void __GalHUB_ServerConnDisconnectFn(GalIO_CommStruct *gcomm, void *caller_data)
{
  /* This will be called when the general connection to a
     Hub server dies. This will reset the server to DISCONNECTED
     and remove the connection. It will also remove the
     connection from the rest of the world. */
  SERVICE_PROVIDER *s = (SERVICE_PROVIDER *) caller_data;
  
  s->gcomm = (GalIO_CommStruct *) NULL;
  s->status = DISCONNECTED;  
  /* DON'T REMOVE THE SERVICE PROVIDER HERE! It happens
     in processNoServerMessage, and if you do it twice,
     very, very bad things will happen. */
  GalHUB_RemoveServiceProvider(Hub, s, 1, 1);
}
#endif

int _GalHUB_InitializeInternalHubServer(SERVICE_TYPE *stype, HUB *h)
{
  /* We set up a server, whose maximum connections are the maximum number
     of dispatch servers + 1, to leave room for the general-purpose
     connection. */
  GalUtil_Print(GAL_CONNECTION_VERBOSITY_LEVEL, "Opening listener %s\n",
		stype->name);
  /* Add a listener proxy. */
  __GalHUB_AddListenerProxy(stype, h);

  /* If the listener proxy has been initialized, announce that
     you're reusing it. */
  if (stype->listener_proxy->scomm) {
    _GalHUB_ReportListenerStatus(h, GAL_CONNECTION_VERBOSITY_LEVEL, GAL_HUB_LISTENER_INITIALIZED, GalIO_GetServerListenPort(stype->listener_proxy->scomm), 1);
    return 1;
  } else {
    /* Initialize the listener proxy. */
    GalUtil_Print(GAL_CONNECTION_VERBOSITY_LEVEL,
		  "Trying to set up listener on port %d\n",
		   stype->listener_proxy->port);
    return __GalHUB_InitializeListenerProxy(stype->listener_proxy,
					    h, stype->require_port);
  }
}

static int __GalHUB_InitializeListenerProxy(LISTENER_PROXY *p, HUB *h,
					    int require_port)
{
  /* We get the port from the server. */
  p->scomm = GalIO_ServerInit((unsigned short) p->port, require_port,
			      (GalIO_FrameHandler) NULL,
			      (void *) NULL, -1,
			      MAX_SERVERS + 1);
  if (!p->scomm) {
    _GalHUB_ReportListenerStatus(h, GAL_CONNECTION_VERBOSITY_LEVEL,
				 GAL_HUB_LISTENER_INITIALIZATION_ERROR,
				 -1, -1);
    return 0;
  } else {
    /* Bookkeeping, just in case it needed to start on a higher port. */
    p->port = GalIO_GetServerListenPort(p->scomm);
    /* maxfd is used for select(). Windows sockets ignore
       the maxfd argument. */
#ifndef WIN32
    h->maxfd = max(GalIO_GetServerListenSocket(p->scomm), h->maxfd);
#endif
    _GalHUB_ReportListenerStatus(h, GAL_CONNECTION_VERBOSITY_LEVEL,
				 GAL_HUB_LISTENER_INITIALIZED, p->port, 0);
    return 1;
  }
}

/* Here are the rules. Named contacts can only update named
   contacts. A listener can update an existing listener.
   For any other request, having more than 1 existing
   contact will cause the spec to be skipped. A single contact
   can be updated. A single listener can be updated. If the
   request is to set up a listener, a possible single contact
   will be removed. If the request is to set up a contact,
   a possible listener will be removed. */

static void maybe_override_server(HUB *h, GalSS_ProviderSpec *spec,
				  int as_client, int require_port)
{
  SERVICE_PROVIDER *s = (SERVICE_PROVIDER*) NULL;
  int i;
  int num_stypes = Gal_PointerBufferSize(h->stypes);
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(h->stypes);
  int num_providers = 0;
  char *hostname = spec->host;
  int port = spec->port;
  int provider_id = spec->id;
  char *provider_name = spec->id_name;
  int j;
  SERVICE_PROVIDER **providers;
  char *server_name = spec->stype_name;
  Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;
  
  /* On the command line, you can now clearly distinguish
     between server and client (see caller). */
  
  for (i = 0; i < num_stypes; i++) {

    num_providers = Gal_PointerBufferSize(stypes[i]->providers);
    
    if ((!as_client) && spec->stype_name &&
	!strcmp(stypes[i]->name, server_name)) {
      /* If we're setting up a listener, the server_name better
	 be present. */
      /* But provider information better not be. */
      if ((provider_id > -1) || provider_name) {
	GalUtil_Warn("No provider info permitted for Hub listener %s, skipping override",
		     GalSS_FormatProviderSpec(spec, &buf));
	if (buf) Gal_FreeStringBuffer(buf);
      } else if ((port != -1) && (stypes[i]->listener_port > -1)) {
	stypes[i]->listener_port = port;
	if (require_port) {
	  stypes[i]->require_port = 1;
	}
      } else if (port != -1) {
	/* No existing listener. */
	if (num_providers > 1) {
	  GalUtil_Warn("Can't convert multiple contacts to Hub listener %s, skipping override",
		       GalSS_FormatProviderSpec(spec, &buf));
	  if (buf) Gal_FreeStringBuffer(buf);
	} else {
	  if (num_providers == 1) {
	    /* Shut down any providers. */
	    GalHUB_RemoveServiceProvider(h, (SERVICE_PROVIDER *) Gal_PointerBufferNthElement(stypes[i]->providers, 0), 1, 1);
	  }
	  stypes[i]->listener_port = port;
	  if (require_port) {
	    stypes[i]->require_port = 1;
	  }
	}
      }
      return;
    } else if ((!spec->stype_name) &&
	       ((provider_id > -1) || provider_name)) {
      /* Making this case separate will result in some
	 redundant code, but it will make the logic cleaner. */

      /* In this case, we're fishing. If we don't find anything,
	 we just keep going. */

      s = (SERVICE_PROVIDER *) NULL;
      providers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(stypes[i]->providers);

      for (j = 0; j < num_providers; j++) {
	if (((provider_id > -1) && (providers[j]->id == provider_id)) ||
	    Gal_StringEq(provider_name, providers[j]->id_name)) {
	  /* Found it. */
	  GalHUB_ModifyServiceProvider(providers[j], hostname, port);
	  return;
	}
      }
    } else if (spec->stype_name &&
	       !strcmp(spec->stype_name, stypes[i]->name)) {
      /* What do the overrides mean in the case of a service provider?
	 For backward compatibility, only override when there's
	 a single service provider. */
      /* Shut down the listener port, if it exists. Probably. */
      int shut_down_listener_port = 1;
            
      s = (SERVICE_PROVIDER *) NULL;

      if ((provider_id > -1) || provider_name) {
	int i = 0;
	SERVICE_PROVIDER **providers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(stypes[i]->providers);

	for (j = 0; j < num_providers; j++) {
	  if (((provider_id > -1) && (providers[j]->id == provider_id)) ||
	      Gal_StringEq(provider_name, providers[j]->id_name)) {
	    /* Found it. */
	    s = providers[j];
	    break;
	  }
	}
	if (!s) {
	  /* If there aren't any, there aren't any. */
	  GalUtil_Warn("Can't find service provider %s, skipping override", GalSS_FormatProviderSpec(spec, &buf));
	  shut_down_listener_port = 0;
	} else {
	  GalHUB_ModifyServiceProvider(s, hostname, port);
	}
      } else if (num_providers == 0) {
	s = GalHUB_NewServiceProvider(hostname, port, stypes[i]);
	GalHUB_RecordServiceProvider(h, s);
      } else if (num_providers == 1) {
	s = (SERVICE_PROVIDER *) Gal_PointerBufferNthElement(stypes[i]->providers, 0);
	GalHUB_ModifyServiceProvider(s, hostname, port);
      } else {
	GalUtil_Warn("Found too many service providers %s, skipping override",
		     GalSS_FormatProviderSpec(spec, &buf));
	shut_down_listener_port = 0;
      }
      
      if (shut_down_listener_port) {
	stypes[i]->listener_port = -1;
      }

      if (buf) Gal_FreeStringBuffer(buf);
      return;
    }
  }
  GalUtil_Warn("Didn't find any server named %s to override",
	       GalSS_FormatProviderSpec(spec, &buf));
  if (buf) {
    Gal_FreeStringBuffer(buf);
  }
}

void override_locations(HUB *h, char *location_overrides)
{
  /* We tokenize the overrides, and then tease them apart.
     If there is more than one server with a given name,
     ignore the override. */
  char *lasts;
  char *tok;
  GalSS_ProviderSpec *spec;
  
  tok = _gal_strtok_r(location_overrides, " \t", &lasts);
  
  while (tok) {
    spec = GalHUB_SplitServerName(tok);
    if (spec->host || (spec->port != -1)) {
      /* As long as there's something to override... */
      if (Gal_StringEq(spec->host, "<listener>")) {
	maybe_override_server(h, spec, 0, 1);
      } else {
	maybe_override_server(h, spec, 1, 0);
      }
    }
    /* SAM 4/26/01: both these will no longer be needed after
       the call to maybe_override_server(). */
    GalSS_FreeProviderSpec(spec);
    tok = _gal_strtok_r((char *) NULL, " \t", &lasts);
  }
}

extern void _GalSS_FreeServerLocationEntry(GalSS_ServerLocationEntry *entry);

void override_locations_with_file(HUB *h, char *locations_file)
{
  /* We read in the locations file, and follow its
     instructions. Unlike the -locations overrides, you can
     set a server to be a listener here. */

  GalSS_ServerLocationEntry *locs, *current_loc;
  
  if (!locations_file)
    return;

  locs = GalSS_DigestServerLocationFile(locations_file);

  current_loc = locs;

  while (current_loc) {
    switch (current_loc->hub_or_server) {
    case GAL_SL_HUB_LISTENS:
      maybe_override_server(h, &(current_loc->provider_spec), 0, 1);
      break;
    case GAL_SL_SERVER_LISTENS:
      maybe_override_server(h, &(current_loc->provider_spec), 1, 0);
      break;
    } 
    current_loc = current_loc->next;
  }
  _GalSS_FreeServerLocationEntry(locs);
}

/* SAM 6/22/00: Servers (actually, the Hub side of the server, which
   is normally a client) can also be actual servers, which wait for
   either a general-purpose or session-specific connection. */

int
initialize_connections(HUB *h)
{
  int ok = 0;
  int i, j, k;
  SERVICE_PROVIDER *provider;
  Gal_DispatchFnPkg *fn_pkg;
  int num_stypes;
  SERVICE_TYPE **stypes;
  int num_providers;
  SERVICE_PROVIDER **providers;
  SERVICE_PROVIDER *builtin_provider = (SERVICE_PROVIDER *) NULL;

  h->maxfd = 0;

  /* initialize the local server (definitions in local-server.h) */
  GalHUB_InitializeLocalServer(h);

  /* Before we do anything else, set up the connections
     to the special servers. */

  GalHUB_InitializeSpecialServers(h);
  
  /* first eliminate any inactive servers */
  /* SAM 11/15/00: With the split into service types vs. providers,
     you can activate either entire types or specific locations,
     using the host:port notation. This will get a little complicated,
     since there are a lot of backpointers to manage. */
  if (h->active_servers) {
    stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(h->stypes);
    i = 0;
    /* Increment i only when we don't remove the current thing.
       If we remove it, a new element will be at i, and
       the buffer size will be smaller. */
    while (i < Gal_PointerBufferSize(h->stypes)) {
      if (!Gal_StringEq(stypes[i]->name, BUILTIN_SERVER)) {
	if (!GalHUB_StypeFoundInActiveServers(stypes[i]->name,
					      h->active_servers)) {
	  /* If the service type name isn't in the list, check the
	     service providers. If there aren't any of those left,
	     then get rid of the service type. */
	  k = 0;
	  providers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(stypes[i]->providers);
	  while (k < Gal_PointerBufferSize(stypes[i]->providers)) {
	    if (!GalHUB_ProviderFoundInActiveServers(providers[k], stypes[i],
						     h->active_servers)) {
	      /* Not there. Remove it. Don't increment k, because
		 the provider was at position k in this stype,
		 and now something new is at position k. Meanwhile,
		 the size of the pointer list has decreased by one. */
	      GalHUB_RemoveServiceProvider(h, providers[k], 1, 1);
	    } else {
	      k++;
	    }
	  }
	  if (Gal_PointerBufferSize(stypes[i]->providers) == 0) {	    
	    GalHUB_RemoveServiceType(stypes[i], h, 1, 1);
	  } else {
	    i++;
	  }
	} else {
	  i++;
	}
      } else {
	i++;
      }
    }
  }

  /* Now that we have all the services and service types, we
     set up all the clients and listeners. */

  num_stypes = Gal_PointerBufferSize(h->stypes);
  stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(h->stypes);

  for (i = 0; i < num_stypes; i++) {
    if (!strcmp(stypes[i]->name, BUILTIN_SERVER)) {
      /* The builtin server will not have a service provider
	 yet. So we add one. */
      provider = GalHUB_NewServiceProvider((char *) NULL, -1, stypes[i]);
      provider->status = FREE;
      provider->sockid = LOCAL;
      provider->listen_status = -1;
      GalHUB_RecordServiceProvider(h, provider);
      builtin_provider = provider;
      
      /* We also need the signatures, and to compare the
	 signatures against the declared operations. */
      if (h->validate) {
	fn_pkg = GalIO_GetCommDispatchFnPkg(h->local_server);
	if (fn_pkg) {
	  _GalHUB_InitializeServerSignatures(provider, _Gal_ListDispatchFnSignatures(fn_pkg));
	}
      }
      ok++; 
      continue;
    }
    /* First, we check to see if the service type needs to
       set up a listener. */
    if (stypes[i]->listener_port > -1) {
      _GalHUB_GUIAnnounceServiceType(h, stypes[i]);
      _GalHUB_InitializeInternalHubServer(stypes[i], h);
    }
  }
  
  /* We check the list of providers directly off the Hub, because
     that way we know we will not encounter duplicates (providers
     which are listed with multiple service types. */
  num_providers = Gal_PointerBufferSize(h->servers);
  providers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(h->servers);

  /* At this point, we announce all the providers in a single
     batch to the GUI, if it exists. */

  _GalHUB_GUIAnnounceAvailableProviders(h, providers, num_providers);

  /* However we do this, we need to make sure that we don't
     try to connect to the builtin server... */
  
  for (j = 0; j < num_providers; j++) {
    if (providers[j] == builtin_provider)
      continue;
    providers[j]->working_on.tidx = -1;
    /* all the initially specified service providers
       involve contacting remote listeners. */
    providers[j]->listen_status = GAL_HUB_STYPE_CLIENT;
    if (!providers[j]->host) {
      GalUtil_Fatal("Service provider %s has not been assigned a host name", 
		    providers[j]->pname);
    }
    _GalHUB_GUIAnnounceServiceProvider(h, providers[j]);
    if (initialize_connection_to_server(providers[j], h, 0, 0, 0)) {
      ok++;
    }
  }
    
  return(ok);
}

/* This code involves managing the listener proxies. */

static void __GalHUB_AddListenerProxy(SERVICE_TYPE *stype, HUB *h)
{
  int i;
  LISTENER_PROXY *p = (LISTENER_PROXY *) NULL;
  int num_proxies = Gal_PointerBufferSize(h->listener_proxies);
  LISTENER_PROXY **proxies = (LISTENER_PROXY **) Gal_PointerBufferPointers(h->listener_proxies);
  
  /* First, we try to find a corresponding listener proxy in
     the Hub. */
  for (i = 0; i < num_proxies; i++) {
    p = proxies[i];
    if (p && (stype->listener_port == p->port)) {
      /* We've found the proxy. */
      break;
    }
    /* Otherwise, don't save it. */
    p = (LISTENER_PROXY *) NULL;
  }

  /* Add a proxy if we don't find one. */
  if (!p) {
    p = (LISTENER_PROXY *) calloc(1, sizeof(LISTENER_PROXY));
    p->port = stype->listener_port;
    p->stypes = Gal_MakePointerBuffer(NULL, GAL_SERVICE_TYPE_PTYPE,
				      0, 0, 1, 1,
				      NULL, 10, 0);
    Gal_PointerBufferAdd(h->listener_proxies, (void *) p);
  }
  Gal_PointerBufferAdd(p->stypes, (void *) stype);
  stype->listener_proxy = p;
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
