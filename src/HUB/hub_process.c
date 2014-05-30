/*
  Portions of this file (c) Copyright 1998 - 2000 M.I.T.
  Portions of this file (c) Copyright 1999 - 2001 The MITRE Corporation.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#ifdef WIN32
#include <time.h>
#endif
#include "galaxy/sysdep.h"
#include "galaxy/galaxy_all.h"
#include "hub_internal.h"

#undef min
#undef max
#define min(a,b)  ((a)<(b) ? (a):(b))
#define max(a,b)  ((a)>(b) ? (a):(b))

struct SESSION *Sessions = NULL;

int debug_token_gc = 0;

static int
queueMessages(HUB *h, TOKEN *t, GalIO_MsgType msg_type,
	      int debug, int error_if_missing);
static void
serveQueuedMessage(HUB *h, SERVER_MESSAGE *msg, SERVICE_PROVIDER *s);
static int
readMessageFromServer(HUB *h, SERVICE_PROVIDER *s,
		      Gal_Frame *msg, GalIO_MsgType *t);
static int
writeMessageToServer(SESSION *session, SERVER_MESSAGE *msg,
		     GalIO_MsgType t, int server_tidx,
		     int do_block);

void GalHUB_IncrementTokenReference(TOKEN *t,int where)
{
  t->ref += 1;
  if(debug_token_gc)
    GalUtil_Print(-1,"INCREMENT: tidx %d now %d at %d\n",t->tidx,t->ref,where);
}

void GalHUB_DecrementTokenReference(TOKEN *t,int where)
{
  t->ref -= 1;
  if(debug_token_gc)
    GalUtil_Print(-1, "DECREMENT: tidx %d now %d at %d\n",t->tidx,t->ref,where);
}


static int provider_conditions_satisfied(SERVICE_PROVIDER *s,
					 SERVICE_TYPE *stype,
					 Gal_Frame *namespace_array)
{
  /* If the service provider has conditions, use those.
     Otherwise, if the service type has conditions, use those.
     Otherwise, return 1. */
  int val;
  Gal_Frame old_val = (Gal_Frame) NULL;
    
  if (namespace_array) {
    old_val = namespace_array[GAL_SERVER_NAMESPACE];
    namespace_array[GAL_SERVER_NAMESPACE] = s->properties;
  }
  
  if (s->conditions) {
    val = (Gal_TestConditionInNamespaces(s->conditions,
					 namespace_array,
					 GAL_SERVER_NAMESPACE) == GAL_TRUE);
  } else if (stype->conditions) {
    val = (Gal_TestConditionInNamespaces(stype->conditions,
					 namespace_array,
					 GAL_SERVER_NAMESPACE) == GAL_TRUE);
  } else {
    val = 1;
  }
  if (namespace_array) {
    namespace_array[GAL_SERVER_NAMESPACE] = old_val;
  }
  return val;
}

int GalHUB_SessionMustWriteToProvider(SESSION *session, SERVICE_PROVIDER *s)
{
  int num_lock_info = Gal_PointerBufferSize(session->lock_info);
  SESSION_LOCK_INFO **lock_info = (SESSION_LOCK_INFO **) Gal_PointerBufferPointers(session->lock_info);
  int k;
  SESSION_LOCK_INFO *sli;

  for (k = 0; k < num_lock_info; k++) {
    sli = lock_info[k];
    if (sli->provider == s) {
      if (sli->value & GAL_SESSION_WRITES_ONLY_TO_SERVER) {
	/* We've found the stype and server. */
	return 1;
      }
    }
  }
  return 0;
}

static int serverEligible(SERVICE_TYPE *stype,
			  SERVICE_PROVIDER *provider,
			  SESSION *session,
			  Gal_Frame *namespace_array,
			  int *reason_ptr)
{
  /* First, if the session can only write to this server, then
     this is the server. */
  
  if (GalHUB_SessionMustWriteToProvider(session, provider)) {
    *reason_ptr = GAL_SESSION_MUST_WRITE_TO_PROVIDER;
    return 1;
  }
      
  /* Next, we need to look through the SERVER view of the locks
     and see whether it's write locked to the session, or if
     it's write locked to a different session. We determine the
     possibilities for the eligible stype based on this. */

  /* We also check the applicability of the server given the
     conditions. The only namespaces available at this point
     are the session, the server, the global, and the token.
     The message has not been formatted yet. */

  if (((!provider->only_session_to_read_from) ||
       (provider->only_session_to_read_from->session == session)) &&
      provider_conditions_satisfied(provider, stype, namespace_array)) {
    return 1;
  }
  return 0;
}

static int serverUpAndFree(SERVICE_PROVIDER *provider)
{
  return ((provider->status > DISCONNECTED ||
	   provider->sockid == LOCAL) &&
	  ((!provider->awaiting_mm_reply) &&
	   (provider->status == FREE || provider->sockid == LOCAL)));
}

static void __GalHUB_UpdateMessageWithProviderInfo(SERVER_MESSAGE *msg,
						   SERVICE_TYPE *stype,
						   char *stype_name,
						   SERVICE_PROVIDER *provider,
						   int provider_reason)
{
  if (stype) {
    msg->stype = stype;
  }
  if (stype_name) {
    if (msg->true_provider_spec.stype_name)
      free(msg->true_provider_spec.stype_name);
    msg->true_provider_spec.stype_name = _gal_strdup(stype_name);
  }
  if (provider) {
    msg->provider = provider;
    /* Update the rest of the provider spec. */
    msg->true_provider_spec.id = provider->id;
    if (provider->id_name) {
      if (msg->true_provider_spec.id_name)
	free(msg->true_provider_spec.id_name);
      msg->true_provider_spec.id_name = _gal_strdup(provider->id_name);
    }
  }
  if (provider_reason > -1)
    msg->provider_reason = provider_reason;
}

/* We don't need to find the service provider yet. We just
   need to determine which is the most likely service type. */
  
/* SAM 3/11/02: The only information we need when we're
   checking a message in the scripting language is whether or
   not there's a service type we know of which has some server
   we may be able to use. It's a very weak test. As a result,
   we may actually gather a lot more information, but right
   now that's all we need.

   Well, we need a LIT-tle more. The scripting language needs
   to know whether certain Builtin operations never return.
   So if there's only one eligible provider, we should update
   the msg. Ditto if the service name needs to be split.
*/

int GalHUB_PerhapsAddProvidersToMsg(SERVER_MESSAGE *msg,
				    SERVICE_TYPE *stype,
				    Gal_Frame *namespace_array,
				    SERVICE_PROVIDER **providers,
				    int num_providers)
{
  int j;
  int eligible_providers = 0;
  char *op_name = msg->bare_operation;
  
  /* Make sure the operation is supported. */
  if (list_member(op_name, stype->operations) == -1)
    return 0;
    
  /* Make sure the name, if present, matches. */
  if (msg->true_provider_spec.stype_name &&
      strcmp(msg->true_provider_spec.stype_name, stype->name))
    return 0;    
  
  for (j = 0; j < num_providers; j++) {      
    /* Try to rule out by provider ID or name. */
    if ((msg->true_provider_spec.id > -1) &&
	(msg->true_provider_spec.id != providers[j]->id))	
      continue;

    if (msg->true_provider_spec.id_name &&
	(!Gal_StringEq(msg->true_provider_spec.id_name,
		       providers[j]->id_name)))
      continue;
            
    /* Now check server eligibility, but don't worry about
       freeness or upness. */
    if (serverEligible(stype, providers[j],
		       msg->session, namespace_array,
		       &(msg->provider_reason))) {
      if (msg->provider_reason > -1) {
	/* The reason for picking this provider is settled.
	   Return it. */
	__GalHUB_UpdateMessageWithProviderInfo(msg, stype,
					       stype->name,
					       providers[j], -1);
	return 1;
      } else if (((msg->true_provider_spec.id > -1) &&
		  (msg->true_provider_spec.id == providers[j]->id)) ||
		 (Gal_StringEq(msg->true_provider_spec.id_name,
			       providers[j]->id_name))) {
	/* Try to rule in based on provider ID, ONLY IF THE SERVER IS
	   ELIGIBLE. */
	__GalHUB_UpdateMessageWithProviderInfo(msg, stype,
					       stype->name,
					       providers[j],
					       GAL_PROVIDER_REQUESTED_BY_ID);
	return 1;
      } else {
	eligible_providers++;
	GalHUB_AddEligibleProvider(msg, stype, providers[j]);
      }
    }
  }
  return eligible_providers;
}


int GalHUB_FindServiceTypeForOperation(HUB *hub,
				       SERVER_MESSAGE *msg,
				       Gal_Frame *namespace_array)
						
{
  int i;
  int num_stypes = Gal_PointerBufferSize(hub->stypes);
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(hub->stypes);
  int num_eligible_stypes = 0;
  int eligible_providers;
  char *current_stype_name = (char *) NULL;
  
  int num_providers;
  SERVICE_PROVIDER **providers;
  
  /* First, we find all the service types. It's possible that there
     will be only one, especially if there's a server name. */
  
  for (i = 0; i < num_stypes; i++) {

    /* Go through all the providers. */
    num_providers = Gal_PointerBufferSize(stypes[i]->providers);
    providers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(stypes[i]->providers);
    current_stype_name = stypes[i]->name;
    
    eligible_providers = GalHUB_PerhapsAddProvidersToMsg(msg, stypes[i],
							 namespace_array,
							 providers,
							 num_providers);

    if (eligible_providers > 0)
      num_eligible_stypes++;
    /* Stype names are unique. If we're here, we've found what we're
       going to find. */
    if (msg->true_provider_spec.stype_name &&
	!strcmp(msg->true_provider_spec.stype_name, stypes[i]->name)) {
      break;
    }
  }
  
  if (num_eligible_stypes == 1) {
    /* If there's exactly one eligible stype, update
       the server message. */
    if (msg->true_provider_spec.stype_name)
      free(msg->true_provider_spec.stype_name);
    msg->true_provider_spec.stype_name = _gal_strdup(current_stype_name);
  }
    
  return num_eligible_stypes;
}

/* does session_locks and disallow_new */

void print_session_locks(int level, int debug)
{
  extern HUB *Hub;
  _GalHUB_CReportSessionLockStatus(Hub, level, 2, 0, GAL_HUB_SESSION_LOCK_STATUS, Sessions, -1, NULL, debug, NULL);
}

static void print_alarm_vars(HUB *h)
{
  SESSION *session;

  for (session = Sessions;(session);session = session->next) {
    int j;
    for(j=0;j<GalHUB_AlarmGetNum();j++) {
      
      if(session->alarm_secs_to_expiration[j] > 0 && !session->alarm_disabled[j]) {
	time_t now = time(NULL);
	_GalHUB_CReportSessionAlarmStatus(h, GAL_PINFO2_LEVEL, 2, 0,
					  GAL_HUB_SESSION_ALARM_STATUS,
					  GalHUB_AlarmIndexToName(j),
					  session->session_id,
					  session->alarm_secs_to_expiration[j],
					  (int)(session->alarm_secs_to_expiration[j] - difftime(now,session->alarm_last_reset[j])));
      }
    }
  }
}

static void process_system_error(SESSION *session,TOKEN *t2,HUB *h)
{
/*   int ridx; */

  if (!t2) return;

  GalUtil_Error("\n------- System Warning/Error -------\n");
  if (GAL_VERBOSE >= GAL_FATAL_LEVEL)
    Gal_PPFrame(t2->state);
  GalUtil_Error("------------------------------------\n\n");  

  /* Log the error first, as if it was just received. */
  GalHUB_LogfileLogError(session, NULL, t2->state, NULL);
  /* crank it through. */
  if (queueMessages(h, t2, GAL_MESSAGE_MSG_TYPE, h->debug, 0) == 0)
    /* As far as I know, the last three arguments are going to
       be ignored, because the token doesn't expect a reply. */
    destroyTokenIfDone(h, t2, session, t2->state, 0);
}

/* assumes -- not in list already */
static void add_to_server_down_list(SERVICE_PROVIDER *server,SESSION *session)
{
  session->server_down_list[session->n_server_down_list++] = server;
}

static int in_server_down_list(SERVICE_PROVIDER *server,SESSION *session)
{
  int i;

  for(i=0;i<session->n_server_down_list;i++)
    if(session->server_down_list[i] == server)
      return 1;
  return 0;
}

static void remove_from_server_down_list(SERVICE_PROVIDER *server,
					 SESSION *session)
{
  int i,j;

  for(i=0;i<session->n_server_down_list;i++)
    if(session->server_down_list[i] == server) {
      for(j=i+1;j<session->n_server_down_list;j++)
	session->server_down_list[j-1] = session->server_down_list[j];
      session->n_server_down_list--;
    }
}

static Gal_Frame generate_server_down_for_session(HUB *h, SESSION *session,
						  SERVICE_PROVIDER *server,
						  int save_frame)
{
  Nframe frame = NULL;
  TOKEN *t2;

  /* need to avoid re-entrant server_downs */
  if (session) {
    if(in_server_down_list(server,session))
      return(NULL);
    add_to_server_down_list(server,session);
  }

  frame = Gal_MakeFrame("system_error", GAL_CLAUSE);
  Gal_SetProp(frame, ":server_down", Gal_IntObject(1));
  if (server->host) Gal_SetProp(frame, ":server_host", Gal_StringObject(server->host));
  if (server->port) Gal_SetProp(frame, ":server_port", Gal_IntObject(server->port));

  if(session) {
    session->num_system_errors++;

    Gal_SetProp(frame, GAL_SESSION_ERROR_COUNT_HUB_FRAME_KEY,
		Gal_IntObject(session->num_system_errors));
    Gal_SetProp(frame, GAL_UTTERANCE_ID_FRAME_KEY,
		Gal_IntObject(session->utterance_id));
    if (session->session_id)
      Gal_SetProp(frame, GAL_SESSION_ID_FRAME_KEY,
		  Gal_StringObject(session->session_id));

    t2 = GalHUB_NewToken(frame, server, session->utterance_id,
			 session->session_id, (Gal_Frame) NULL);
    process_system_error(session,t2,h);    
    remove_from_server_down_list(server,session);
  }

  if (save_frame)
    return(frame);
  else {
    Gal_FreeFrame(frame);
    return(NULL);
  }  
}

/* returns 0 if thread dies, 1 if it survives, 
  -1 if the server doesn't exist and the .pgm file disowns it as well 
*/

static char *Exclusions[] = {GAL_TOKEN_INDEX_FRAME_KEY,
			     GAL_HUB_OPAQUE_DATA_FRAME_KEY,
			     GAL_SESSION_ID_FRAME_KEY, NULL};

static void generate_server_unavailable(HUB *h, TOKEN *bad_tok, char *op_name,
					char *server_name, SESSION *session)
{
  TOKEN *t2;
  Gal_Frame frame;

  frame = Gal_MakeFrame("system_error", GAL_CLAUSE);
  
  Gal_SetProp(frame, ":server_unavailable", Gal_IntObject(1));
  if (op_name)
    Gal_SetProp(frame, ":operation", Gal_StringObject(op_name));
  /* This really is the service name, but I have no idea
     what relies on the frame structure. */
  if (server_name)
    Gal_SetProp(frame, ":server_name", Gal_StringObject(server_name));
  /* newToken sets :session_id in the frame */
  if (session) {
    t2 = GalHUB_NewToken(frame,
			 (SERVICE_PROVIDER *) NULL, session->utterance_id,
			 session->session_id, (Gal_Frame) NULL);
  } else {
    t2 = GalHUB_NewToken(frame,
			 (SERVICE_PROVIDER *) NULL, -1, NULL, 
			 (Gal_Frame) NULL);
  }
  destroyTokenIfDone(h, bad_tok, session, 
		     frame, GAL_ERROR_MSG_TYPE);
  Gal_FreeFrame(frame);
  process_system_error(session, t2, h);
}

/* SAM 5/2/02: This information CANNOT be used to infer anything
   about a reply which has been received, because that info
   could have been freed when a postponement was received.
   The information about the stype invoked and the scriptlessness
   which is needed in handle_other_message is now stored in
   the admin_info. This information is still useful for
   situations where the Hub gets in trouble, or needs to
   know who's using the server most recently. */

static void providerHasToken(SERVICE_PROVIDER *s,
			     char *stype_name,
			     TOKEN *t, SESSION *session,
			     int scriptless)
{
  if (s->sockid != LOCAL)
    s->status = t->tidx;

  s->working_on.session = session;
  s->working_on.tidx = t->tidx;
  /* If this message was served scriptless, then
     we need to handle update_token specially later. */
  s->working_on.scriptless = scriptless;
  if (stype_name)
    s->working_on.stype_name = _gal_strdup(stype_name);
}

static void providerHasNoToken(SERVICE_PROVIDER *s)
{
  s->status = FREE;
  s->working_on.session = (SESSION *) NULL;
  s->working_on.tidx = -1;
  if (s->working_on.stype_name) {
    free(s->working_on.stype_name);
    s->working_on.stype_name = (char *) NULL;
  }
}

/* serveMessageToServer should ALWAYS free the message, unless
   it's queued the message. It must also free op_name.
   GalHUB_FreeServerMessage does NOT free the frame inside
   it, so that has to be freed as well. */

static int serveMessageToServer(HUB *h, TOKEN *t, SERVER_MESSAGE *mes, 
				SESSION *session, int debug)
{
  int res = 0;
  int no_return = mes->no_return != -1 ? mes->no_return : 0;
  int destroy_and_free = no_return || ((!t->mm) && mes->scriptless);
  char *provider_reason;
  char *op_name = mes->bare_operation;

  if (t->session_id &&
      !Gal_GetObject(mes->message, GAL_SESSION_ID_FRAME_KEY))
    /* Always have a :session_id since it's so critical */  
    Gal_SetProp(mes->message, GAL_SESSION_ID_FRAME_KEY,
		Gal_StringObject(t->session_id));

  /* op_name argument is new memory. Make sure it's appropriately freed. */

  /* SAM 12/12/00: Don't print session locks all the time. Especially
     because the servers which contacted the Hub and are locked to
     sessions will print out every single time. Arrgh. */
  
  /* print_session_locks(); */

  mes->namespace_array[GAL_SERVER_NAMESPACE] = mes->provider->properties;

  if (h->validate) {    
    /* If a response is expected, but the operation chosen
       doesn't provide a response, print a warning.

       Appropriate conditions are:

       (1) The scripting module expects a return - so destroy will
       will be false and no_return will be true.

       (2) The scripting module isn't involved, and the token is a
       module-to-module token.

       (3) The scripting module doesn't expect a return, but the
       token is supposed to be returned to the server. The problem
       with this case is that we don't know whether this is the last
       message of the program. In this case, it doesn't matter
       whether the operation provides a response or not. */

    Gal_DispatchFnSignature *sig;

    sig = Gal_FindNamedSignature(mes->provider->signatures, op_name);
    
    /* Don't do any validation if there's no signature. */
    
    if (sig) {
      if ((sig->reply_provided == GAL_REPLY_NONE) &&
	  ((!no_return && !mes->scriptless) ||
	   (mes->scriptless && t->mm))) {
	/* Cases (1) and (2). */
	GalUtil_Warn("Operation %s is being invoked expecting a return, but the server won't provide one by default; Hub will force a return\n", op_name);
      }
      if (no_return && (!mes->scriptless) && t->mm) {
	/* Case 3. */
	GalUtil_Warn("The program doesn't expect a return from operation %s, but the originating server expects a return; this may cause execution to hang\n", op_name);
      }
    }
  }

  /* SAM 3/12/02: I used to check for servers being down here,
     but now I'm assuming I'm calling this function with a
     live connection always. */
      
  GalHUB_IncrementTokenReference(t,5);      
      
  switch (mes->provider_reason) {
  case GAL_SESSION_MUST_WRITE_TO_PROVIDER:
    provider_reason = " (session locked)";
    break;
  case GAL_PROVIDER_MUST_READ_FROM_SESSION:
    provider_reason = " (server locked)";
    break;
  case GAL_SINGLE_PROVIDER_FOR_SERVICE:
    provider_reason = " (sole provider)";
    break;
  case GAL_PROVIDER_REQUESTED_BY_ID:
    provider_reason = " (requested by ID)";
    break;
  case GAL_RANDOM_SELECTION:
    provider_reason = "";
    break;
  default:
    provider_reason = "";
    break;
  }
  GalUtil_CPInfo1(2,0,"---- Serve(%s@%s:%d, token %d op_name ",
		  mes->stype->name,
		  mes->provider->host ? mes->provider->host : "<none>",
		  mes->provider->port, t->tidx,
		  provider_reason);
  GalUtil_CPInfo1(6,0,"%s%s",op_name, provider_reason);
  GalUtil_CPInfo1(2,0,")\n");
  
  if (debug) {
    /* The debug setting came from the Hub structure. */
    h->debug = _gal_hub_break(h, t, mes->provider, mes->message, session);
  }
  
  res = writeMessageToServer(session, mes, GAL_MESSAGE_MSG_TYPE, -1, 1);

  if (h->validate) {
    Gal_DispatchFnSignature *sig;

    sig = Gal_FindNamedSignature(mes->provider->signatures,
				 mes->bare_operation);
    if (sig) {
      Gal_ValidateDispatchFnInput(mes->bare_operation, mes->message,
				  sig, Exclusions);
    }
  }

  providerHasToken(mes->provider, mes->true_provider_spec.stype_name,
		   t, session, mes->scriptless);
      
  if (res < 0) {
    GalUtil_Fatal("Could not send frame to provider %s at %s:%d",
		  mes->provider->pname);
  }
      
  if (destroy_and_free) {
    providerHasNoToken(mes->provider);
    GalHUB_DecrementTokenReference(t,6);
  }
      
  Gal_FreeFrame(mes->message);	/* GC_ADDED */
  GalHUB_FreeServerMessage(mes);
  return(1);
}

/* Returns 1 if reply already provided, 0 otherwise. */

static int
queueMessages(HUB *h, TOKEN *t, GalIO_MsgType msg_type, int debug,
	      int error_if_missing)
{
  int script_claimed = 0;
  int ret_val = 0;

  /* If a target provider ID was given, bypass the scripting. */
  if (h->script && (!t->target_provider_id))
    script_claimed = HC_NextOperations(h, t, debug);
  
  if ((script_claimed == 0) && (msg_type == GAL_MESSAGE_MSG_TYPE)) {
    /* Gotta build a message. */
    SESSION *session = GalHUB_SessionLookupByID(t->session_id, 0);
    int no_return;
    SERVER_MESSAGE *msg;
    int stype_found;
    
    /* SAM 1/18/01: We know whether there's a return or not;
       if the token expects a return, then no_return should be
       0, otherwise 1. */

    if (t->mm)
      no_return = 0;
    else
      no_return = 1;

    msg = GalHUB_NewServerMessage(Gal_CopyFrame(t->state),
				  GAL_OUTGOING_MSG,
				  GAL_MESSAGE_MSG_TYPE,
				  (char *) NULL, t->target_provider_id,
				  NULL, 0, 0,
				  no_return, 1, NULL,
				  0, t,
				  session);

    /* Obviously, we're not from the script here. */
    msg->force_timestamp = GalHUB_TimestampIsForced(msg, h->log_record, 0, 0);
    GalHUB_PopulateServerMessageNamespaceArray(session, msg, t);
    
    /* And then check to see if we can enqueue it. */
    stype_found = GalHUB_FindServiceTypeForOperation(h, msg,
						     msg->namespace_array);

    if (!stype_found) {
      /* If there's a list of active servers, and no stype
	 was found, but there's a service_name, then fail
	 quietly. The reason for suppressing an error message
	 here is that in the scriptless case, if a server
	 was specified and no stype was found, that means
	 that the stype is being explicitly ignored. This is
	 also true in the script case, but that error is
	 caught much earlier. 

	 Otherwise, if error_if_missing is true,
	 then fail loudly. */
      if (((!h->active_servers) || (!msg->true_provider_spec.stype_name)) &&
	  error_if_missing) {
	GalUtil_PInfo1("No available service provider for %s!\n",
		       Gal_FrameName(t->state));
	generate_server_unavailable(h, t, msg->bare_operation,
				    msg->true_provider_spec.stype_name,
				    session);
	ret_val = 1;
      }
      Gal_FreeFrame(msg->message);
      GalHUB_FreeServerMessage(msg);
    } else {    
      GalHUB_EnqueueServerMessage(h, t, msg);
    }
  }
  return ret_val;
}

/* SAM 3/12/02: The idea is that we go through the message
   queue, trying to find a message which can be sent. This is
   somewhat brute force, since we may end up testing the
   same message over and over again. However, new messages
   will continue to be added, and each time a server reads
   a message, there's a chance it will become free.

   However, there's no other way for a server to become free.
   So if a server is checked, and it's not free, it won't
   become free until the next time it's checked. However,
   the free test is so cheap, it's not clear to me that keeping
   track of this info is useful. */

typedef struct __mqr {
  int num_servers;
  SERVICE_PROVIDER **servers;
  int start_index;
  HUB *h;
  int msgs_flushed;
} __mqr;

extern
void _GalIO_QueueApply(GalIO_PointerQueue *queue,
		       int (*fn)(void *data, void *caller_data),
		       void *caller_data);

static int __consider_providers_for_message(SERVER_MESSAGE *msg,
					    int provider_index,
					    int mqr_index,
					    int max_provider_index,
					    PROVIDER_FLAG_ARRAY *p,
					    __mqr *mqr)
{
  while ((provider_index < max_provider_index) &&
	 (mqr_index < mqr->num_servers)) {
    if (p->cells[provider_index].provider_id >
	mqr->servers[mqr_index]->id) {
      /* The provider array index exceeds the current provider.
	 Increment the provider index. */
      mqr_index++;
    } else if (p->cells[provider_index].provider_id <
	       mqr->servers[mqr_index]->id) {
      /* The provider array index trails the current provider.
	 Increment the array index. */
      provider_index++;
    } else if (serverUpAndFree(mqr->servers[mqr_index]) &&
	       serverEligible(p->cells[provider_index].stype,
			      mqr->servers[mqr_index],
			      msg->session,
			      msg->namespace_array,
			      &(msg->provider_reason))) {
      /* They're equal. Consider the provider and service type.
	 If it's up and free and the server is still eligible,
	 dispatch the message, decrement the
	 token index and return -1. Otherwise, increment the
	 provider index. */
      __GalHUB_UpdateMessageWithProviderInfo(msg,
					     p->cells[provider_index].stype,
					     p->cells[provider_index].stype->name,
					     mqr->servers[mqr_index], -1);
      serveQueuedMessage(mqr->h, msg, msg->provider);
      mqr->h->msg_queue_length--;
      mqr->msgs_flushed++;
      return -1;
    } else {
      provider_index++;
    }
  }
  return 1;
}

/* Return 1 to continue, 0 to halt, -1 to dequeue. */

int __try_to_flush_message(void *data, void *caller_data)
{
  __mqr *mqr = (__mqr *) caller_data;
  SERVER_MESSAGE *msg = (SERVER_MESSAGE *) data;
  int start_i;
  PROVIDER_FLAG_ARRAY *p = &(msg->provider_array);
  SERVICE_PROVIDER *provider;
  
  /* Don't forget, the message may have already selected
     a unique provider. If so, check it and nothing else. */

  if (msg->provider) {
    /* Check to see if this provider is available. */
    if (serverUpAndFree(msg->provider) &&
	serverEligible(msg->stype, msg->provider,
		       msg->session, msg->namespace_array,
		       &(msg->provider_reason))) {
      serveQueuedMessage(mqr->h, msg, msg->provider);
      mqr->msgs_flushed++;
      mqr->h->msg_queue_length--;
      return -1;
    } else {
      return 1;
    }
  }
    
  
  /* The elements in the message queue eligibility array are
     in provider ID order. So we find the current provider ID,
     find the corresponding place in the message array, and
     start checking. If we don't start at 0, loop back to the
     beginning. */

  provider = mqr->servers[mqr->start_index];
  
  for (start_i = 0; start_i < p->buffer_size; start_i++) {
    PROVIDER_FLAG_CELL *pfc = &(p->cells[start_i]);
    if (pfc->provider_id >= provider->id) {
      /* Found the beginning. */
      break;
    }
  }
  if (start_i < p->buffer_size) {
    /* There's a tail we should search. Now, we loop through the
       providers, moving up the mqr_index and provider_index
       appropriately. If we find an eligible server, we
       dispatch the message and return -1. Don't forget
       to decrement to token index! */
    if (__consider_providers_for_message(msg, start_i, mqr->start_index,
					 p->buffer_size, p, mqr) < 0) {
      return -1;
    }
  }
  
  /* If there's a head we should search, do it. */
  if (start_i > 0) {
    if (__consider_providers_for_message(msg, 0, 0, start_i, p, mqr) < 0) {
      return -1;
    }
  }
  
  return 1;    
}
     
static int tryToFlushMessageQueue(HUB *h, SERVICE_PROVIDER **servers,
				  int num_servers, int index)
{
  /* Always start with the server you just read from, so
     we can rotate through the servers. */
  __mqr mqr;

  mqr.num_servers = num_servers;
  mqr.servers = servers;
  mqr.start_index = index;
  mqr.h = h;
  mqr.msgs_flushed = 0;
  
  _GalIO_QueueApply(h->message_queue, __try_to_flush_message, (void *) &mqr);
  
  return mqr.msgs_flushed;
}


/* SAM 3/12/02: Serve the message. Make sure that local exits
   free the message, since serveMessageToServer does. */

static void
serveQueuedMessage(HUB *h, SERVER_MESSAGE *msg, SERVICE_PROVIDER *s)
{
  TOKEN *t;
  SESSION *session;
  
  t = GalHUB_GetTokenFromIndex(msg->tidx);
  GalUtil_CPInfo1WithLocation(__FUNCTION__, 2,0,"Serving message with token index %d to provider %s\n", msg->tidx, s->pname);
  if (!t) {
    GalUtil_Warn("No token with index %d found; ignoring retrieved message",
		 msg->tidx);
    Gal_FreeFrame(msg->message);
    GalHUB_FreeServerMessage(msg);
    return;
  }
  GalHUB_DecrementTokenReference(t,8);
  /* already incremented on last serve, then again on push 
     queue, decrement on pop...  the next serve would increment
     again, which is wrong. */
  /* Note that we need to capture the session information
     from the message. We need
     this before serveMessageToServer because
     serveMessageToServer ultimately frees
     the message structure. */
  session = msg->session;
  serveMessageToServer(h, t, msg, session, h->debug);
  destroyTokenIfDone(h, t, session, t->state, GAL_REPLY_MSG_TYPE);
}

static int readMessageFromServer(HUB *h, SERVICE_PROVIDER *s,
				 Gal_Frame *msg, GalIO_MsgType *msg_type_ptr)
{
  if (s->sockid == LOCAL)
    return read_from_local_server(h, msg, msg_type_ptr);
  else
    return GalIO_CommReadMessage(s->gcomm, msg, msg_type_ptr, 0);
}

void GalHUB_UpdateFromEntityPair(EntityPair **eps, Gal_Frame *namespace_array)
{
  int i = 0;
  Gal_Object value;
  int newly_created;
  
  if (eps) {
    while (eps[i]) {
      value = Gal_GetProgramEntity(eps[i]->source, namespace_array,
				   &newly_created);
      if (value)
	Gal_SetProgramEntityLocation(eps[i]->target, value,
				     namespace_array, newly_created);
      i++;
    }
  }
}

static void __GalHUB_UpdateMessageFromServer(SERVER_MESSAGE *msg)
{
  int num_stypes = Gal_PointerBufferSize(msg->provider->stypes);
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(msg->provider->stypes);
  int i;
  
  /* First, start with the stypes. */
  for (i = 0; i < num_stypes; i++) {
    GalHUB_UpdateFromEntityPair(stypes[i]->in, msg->namespace_array);
  }
  GalHUB_UpdateFromEntityPair(msg->provider->in, msg->namespace_array);
}

/* SAM 12/11/00: Here, we create the frame which contains
   the administrative information. The frame will be called
   admin_info (name ignored), and will contain

   GAL_SESSION_ID_FRAME_KEY (:session_id) 
   GAL_TOKEN_INDEX_FRAME_KEY (:tidx)
   GAL_OPAQUE_SCRIPT_MODULE_HUB_FRAME_KEY (:hub_program_info)
   GAL_ROUND_TRIP_FRAME_KEY (:reply_requested)

   See GalSS_CallServerFunction().
*/

static int writeMessageToServer(SESSION *session, SERVER_MESSAGE *msg,
				GalIO_MsgType msg_type,
				int server_tidx, int do_block)
{
  int res;
  double* token_timestamp_ptr;

  /* First, we do all the updates. */
  TOKEN *t = GalHUB_GetTokenFromIndex(msg->tidx);
  Gal_Frame hub_data = Gal_MakeFrame("admin_info", GAL_CLAUSE);

  /* For "server down" messages which don't have a session,
     calling HC_PrepareOperation would be a disaster. But
     the "server down" message is marked as scriptless, so
     we don't have anything to worry about. */
  
  /* SAM 12/14/01: I've moved the call to HC_PrepareOperation
     to before the construction of the hub_data, because
     I want to give HC_PrepareOperation one last chance to
     modify information before hub_data is constructed. The
     message is freed very soon after the call to writeMessageToServer,
     so there's only a slight risk that changing things in
     HC_PrepareOperation is dangerous. The particular element
     I need to change is the no_return slot, in order to support
     scripting-language Hub continuations (there's a reply
     coming, but the server shouldn't know the Hub expects one,
     because then the server bindings will force a reply
     from a dispatch function which returns NULL, and then
     the Hub will receive multiple replies for the same
     message, which is bad. Making sure this doesn't happen
     is enforced in HC_PrepareOperation, but also in
     HC_UpdateToken). */
     
  if (msg->scriptless == 0)
    HC_PrepareOperation(session, t, msg);

  /* The session ID is added earlier, because the
     damn Hub calls SessionLookupByFrame all over. */
  /* Add in the opaque information. */
  if (msg->opaque_script_info) {
    Gal_SetProp(hub_data, GAL_OPAQUE_SCRIPT_MODULE_HUB_FRAME_KEY,
		Gal_CopyObject(msg->opaque_script_info));
  }
  /* Add the token index. */
  Gal_SetProp(hub_data, GAL_TOKEN_INDEX_FRAME_KEY,
	      Gal_IntObject(msg->tidx));
  /* Add the session ID. There may not be one, due to
     this being a "server down" message. */
  if (session)
    Gal_SetProp(hub_data, GAL_SESSION_ID_FRAME_KEY,
		Gal_StringObject(session->session_id));
  
  /* SAM 9/25/00: We encode return requirements on the
     message. The server bindings now know what to do with this. */

  if (msg->no_return == 0) {
    Gal_SetProp(hub_data, GAL_ROUND_TRIP_FRAME_KEY, Gal_IntObject(1));
  }
  
  /* If we're returning a server-to-server subdialogue result,
     make sure we have the server tidx set. */
  if (server_tidx > -1)
    Gal_SetProp(hub_data, GAL_SERVER_TOKEN_INDEX_FRAME_KEY,
		Gal_IntObject(server_tidx));

  /* Set the token timestamp. */
  token_timestamp_ptr = (double*) malloc(sizeof(double));
  if(!token_timestamp_ptr) {
    GalUtil_Error("Could not allocate memory for token timestamp.");
  }

  *token_timestamp_ptr = t->timestamp;
  Gal_SetProp(hub_data, GAL_TOKEN_TIMESTAMP_FRAME_KEY,
	      Gal_CreateFloat64Object((void*)token_timestamp_ptr, 1, 1));

  /* Add the provider's canonical name. */
  if (t->owner)
    Gal_SetProp(hub_data, GAL_PROVIDER_ID_FRAME_KEY,
		Gal_StringObject(GalHUB_ServiceProviderID(t->owner)));

  /* Add the scriptless status and the stype info. These
     are needed in handle_other_message. */

  Gal_SetProp(hub_data, GAL_SCRIPT_STATUS_HUB_FRAME_KEY,
	      Gal_IntObject(msg->scriptless));

  /* Add the stype it thinks it should be. Needed for logging. */

  if (msg->true_provider_spec.stype_name)
    Gal_SetProp(hub_data, GAL_INVOKED_STYPE_HUB_FRAME_KEY,
		Gal_StringObject(msg->true_provider_spec.stype_name));
  
  /* Service types and service providers have in
     keys associated with them, which should probably
     be processed even in the scriptless case. So
     instead of doing it in the scripting, I'll
     do it here. */
  __GalHUB_UpdateMessageFromServer(msg);
  GalHUB_LogfileUpdateMsgOnSend(msg, msg_type, t);
  GalHUB_SessionHandleLocks(session, msg->provider,
			    msg->tidx, msg->lock_mask,
			    msg->lock_value, 1, 0, 0);

  Gal_SetProp(msg->message, GAL_HUB_OPAQUE_DATA_FRAME_KEY,
	      Gal_FrameObject(hub_data));
  
  /* The session ID is added to the toplevel message for Hub-internal
     purposes. I'd love to clean that up, but it's too complicated.
     So I'd like to remove the session ID from the toplevel before
     I send it, but it's possible that the programmer passed
     something named :session_id in the Hub script explicitly,
     so I can't do that. I can't even remove it immediately
     before HC_PrepareOperation, because that function ultimately
     calls functions which attempt to locate the session
     based on the frame. I believe all this stuff is redundant,
     but proving it will take more time than I currently have. */
  /* Gal_DelProp(msg->message, GAL_SESSION_ID_FRAME_KEY); */
  if (msg->provider->sockid == LOCAL) 
    return send_to_local_server(Hub, msg->message, msg_type);
  
  res = GalIO_CommWriteMessage(msg->provider->gcomm,
			       msg->message, msg_type, do_block);
  if (res > -1)
    _GalHUB_GUIAnnounceMessage(Hub, msg->message, msg_type,
			       msg->provider, "server");
  return res;
}

static void 
printMessageInfo(SERVICE_PROVIDER *s,
		 Gal_Frame frame, int tidx, GalIO_MsgType msg_type) 
{
  switch (msg_type) {
  case GAL_REPLY_MSG_TYPE:
    GalUtil_CPInfo1WithLocation(__FUNCTION__, 2,0,"Got reply from provider %s : token %d\n", s->pname, tidx);
    break;
  case GAL_DESTROY_MSG_TYPE:
    GalUtil_CPInfo1WithLocation(__FUNCTION__, 2,0,"Got destroy request from provider %s: token %d\n", s->pname, tidx);
    break;
  case GAL_ERROR_MSG_TYPE:
    GalUtil_CPInfo1WithLocation(__FUNCTION__, 2,0,"Got error from provider %s: token %d\n", s->pname, tidx);
    GalHUB_OutlineFrame(frame);
    break;
  case GAL_POSTPONE_MSG_TYPE:
    GalUtil_CPInfo1WithLocation(__FUNCTION__, 2,0,"Got postponement from provider %s: token %d\n", s->pname, tidx);
    break;
  default:
    break;
  }
}

static TOKEN *
handle_other_message(HUB *h, SERVER_MESSAGE *msg);

/* SAM 11/25/00: Moved logging for Hub reading new message out of
   logfile into here, since we're populating the SERVER_MESSAGE
   structure. */   

static TOKEN *handle_new_message(HUB *h,
				 SERVER_MESSAGE *msg,
				 Gal_Frame hub_data,
				 int is_initial_token)
{
  Gal_Frame frame = msg->message;
  SERVICE_PROVIDER *s = msg->provider;
  int hypidx, uttidx;
  LOGKEYS *lkeys;
  int is_mm = 0;
  TOKEN *t;

  /* New message */
  if (Gal_GetInt(hub_data, GAL_RESET_SESSION_FRAME_KEY))
    GalHUB_SessionNew(frame);

  if (!is_initial_token) {
    /* There will no provider if it's the initial token,
       and the tidx will already have been assigned. */
    GalUtil_CPInfo1WithLocation(__FUNCTION__, 2,0,"Got new message from provider %s\n", s->pname);

    if (Gal_GetObject(frame, GAL_TOKEN_INDEX_FRAME_KEY)) {
      /* There's something very wrong. */
      GalUtil_Warn("Received new message with :tidx; ignoring");
      return (TOKEN *) NULL;
    }
    
    if (Gal_GetInt(hub_data, GAL_ROUND_TRIP_FRAME_KEY)) {
      /* looks like initiation of a M-M dialogue */

      is_mm = 1;

      s->awaiting_mm_reply = 1;

      GalUtil_CPInfo1WithLocation(__FUNCTION__, 2,0,"Provider %s requested a reply\n", s->pname);
    } else if (GalHUB_HubContinuationMatched(h, msg)) {
      /* SAM 10/29/01: At this point, we want to see if we should
	 convert this into a reply message of some sort. The function
	 GalHUB_EnqueueHubContinuation can be used to "wait" on a
	 token for new messages which can be interpreted either
	 as replies or errors. If it can, the appropriate message
	 is augmented with administrative information and treated
	 as a reply.
       
	 We won't permit round-trip messages to be interrupted
	 by this continuation mechanism. */
      GalUtil_PInfo1WithLocation(__FUNCTION__, "New message claimed by Hub continuation\n");
      return handle_other_message(h, msg);
      return (TOKEN *) NULL;    
    }
     
    if (Gal_GetObject(frame, GAL_UTTERANCE_ID_FRAME_KEY)) {
      uttidx = Gal_GetInt(frame, GAL_UTTERANCE_ID_FRAME_KEY);
    } else { 
      if (0) GalUtil_Warn("No :utterance_id found for hyp %d", hypidx);
      uttidx = -1;
    }
    t = GalHUB_NewToken(frame, s, uttidx,
			msg->session->session_id, hub_data);
    /* If the frame has no name, it can fail to create a token,
       for instance. */
    if (t)
      t->mm = is_mm;
    else
      return (TOKEN *) NULL;
  } else {
    /* If it is the initial token, we can just find it. */
    t = GalHUB_GetTokenFromIndex(Gal_GetInt(frame, GAL_TOKEN_INDEX_FRAME_KEY));
    if (!t) {
      return (TOKEN *) NULL;
    }
  }
  msg->tidx = t->tidx;
  /* Update the log. */
  msg->namespace_array[GAL_TOKEN_NAMESPACE] = t->state;
  lkeys = GalHUB_LogfileGetLogkeysFromHub(Hub, msg,
					  (t->target_provider_id == (char *) NULL));
  if (lkeys && lkeys->in) {
    GalHUB_LogfileUpdateServerMessageFromEntityPairs(lkeys->in, msg,
						     msg->namespace_array);
  }
  return t;
}

/* Sam 10/31/99: BAD bug. Updates of tokens in partial scripting was
   failing for the non-scripted cases because we were just checking
   to see if there was a script, not whether the dispatch we
   were responding to was sent scriptless or not. So I store this
   info in the queue, and then on the server, and pass it in here. */

/* SAM 5/4/01: This function is always called with a non-NULL token.
   It returns 1 if the token should be processed further, 0 otherwise.
   See HC_UpdateToken() for more discussion. */

static int update_token(HUB *h, TOKEN *t, SERVER_MESSAGE *msg)
{
  Gal_Frame fr = msg->message;
    
  /* insert session_id into t->state */
  if (!t->session_id || Gal_StringEq(t->session_id, DEFAULT_SESSION_ID)) {
    if (Gal_GetObject(fr, GAL_SESSION_ID_FRAME_KEY)) {
      char *new_id = Gal_GetString(fr, GAL_SESSION_ID_FRAME_KEY);

      /* If it's new, use it. */
      if (strcmp(new_id, t->session_id)) {
	/* GC -spc */
	if (t->session_id)
	  free(t->session_id);
	t->session_id = _gal_strdup(new_id);
	Gal_SetProp(t->state, GAL_SESSION_ID_FRAME_KEY,
		    Gal_StringObject(t->session_id));
      }
    }
  }

  if (!msg->scriptless)
    return HC_UpdateToken(t, msg);
  else {
    Gal_FreeFrame(t->state);
    t->state = Gal_CopyFrame(msg->message);
    return 1;
  }
}

/* SAM 10/6/00: Added postponement, which simply frees the server
   without doing an update. Don't decrement the token reference,
   the message is still "in process". */

static TOKEN *
handle_other_message(HUB *h, SERVER_MESSAGE *msg)
{
  Gal_Frame frame = msg->message;
  SERVICE_PROVIDER *s = msg->provider;
  TOKEN *t = NULL;
  int tidx;
  
  tidx = Gal_GetInt(frame, GAL_TOKEN_INDEX_FRAME_KEY);

  printMessageInfo(s, frame, tidx, msg->msg_type);

  /* if this token was a response to an earlier token sent then send
     next token in queue. But be sure to rescue the values for
     in_server_t and in_server_scriptless. */

  /* SAM 5/2/02: Actually, because of postponements, the
     information in the working_on structure is pretty useless
     for a reply message. I've now made that information
     live on the admin_info. */

  switch (msg->msg_type) {
  case GAL_REPLY_MSG_TYPE:
  case GAL_ERROR_MSG_TYPE:
    /* If it's an error or a reply, we'll have the server in the
       message. In order to log TIMESTAMP: correctly, we need to know
       what server we read a message from. So we'll need to update
       the incoming reply, before we log. */
    __GalHUB_UpdateMessageWithProviderInfo(msg, (SERVICE_TYPE *) NULL,
					   (char *) NULL,
					   msg->provider, -1);
    break;
  default:
    break;
  }
  
  if ((tidx == s->working_on.tidx)) {
    providerHasNoToken(s);
  }

  /* If we just have a postponement, just return. But make sure
     that if the server is awaiting an MM reply, it's marked
     as not, since this is used to determine if you can
     get messages or not. */

  if (msg->msg_type == GAL_POSTPONE_MSG_TYPE) {
    if (s->awaiting_mm_reply)
      s->awaiting_mm_reply = 0;
    return (TOKEN *) NULL;
  }

  /* Otherwise, proceed with the update. */
  
  t = GalHUB_GetTokenFromIndex(tidx);
  
  if (!t) {
    GalUtil_Warn("Didn't find token with tidx %d; ignoring", tidx);
    return (TOKEN *) NULL;
  } else {
    msg->tidx = tidx;
  }

  msg->namespace_array[GAL_TOKEN_NAMESPACE] = t->state;
  
  if (!update_token(h,t,msg)) {
    /* SAM 10/26/01: If update_token returns 0, it's because
       the scripting language module returns 0. The scripting
       language module returns 0 in one of two cases: if
       there's no rule for that index, or if the rule was
       asynchronous and didn't deserve a return. I can't
       imagine when the first case comes up, but the second
       can come up fairly often. Here's the problem: if you
       decrement the token reference before you check this
       case, you might accidently destroy a token which
       is waiting for a reply from a SUBSEQUENT message.
       So all in all, I think we need to do the token
       reference decrement AFTER this check. */
    GalUtil_PInfo1("[Ignoring reply for token %d]\n",t->tidx);
    destroyTokenIfDone(h, t, msg->session, t->state,
		       GAL_REPLY_MSG_TYPE);
    return (TOKEN *) NULL;
  }

  /* Now, after we check update_token, we decrement the reference. */
  GalHUB_DecrementTokenReference(t,7);
  
  return t;
}

void GalHUB_ReturnMMToOwner(HUB *h, TOKEN *t, Gal_Frame frame,
			    SESSION *session, GalIO_MsgType msg_type)
{
  /* no further rule found so return to owner, or error encountered --
     this is where we send back a reply from a M-M */
  LOGKEYS *lkeys = GalHUB_LogfileGetLogkeysFromHubViaFrameName(h, t->name,
							       (t->target_provider_id == (char *) NULL));
  int force_timestamp = (msg_type == GAL_ERROR_MSG_TYPE);
  SERVER_MESSAGE *msg;
  
  /* Always log errors. Always. */
  msg = GalHUB_NewServerMessage(frame, GAL_OUTGOING_MSG,
				msg_type,
				NULL, NULL, t->owner,
				0, 0, -1, 1, NULL, force_timestamp,
				t, session);
  GalHUB_PopulateServerMessageNamespaceArray(session, msg, t);

  if (lkeys && lkeys->out) {
    GalHUB_LogfileUpdateServerMessageFromEntityPairs(lkeys->out, msg,
						     msg->namespace_array);
  }
  if (msg_type == GAL_ERROR_MSG_TYPE)
    GalHUB_LogErrorMessageKeys(msg);
      
  t->mm = 0;
  if (t->owner)
    t->owner->awaiting_mm_reply = 0;

  /* SAM 4/6/00: Make sure the token state doesn't have
     the tidx in it, just in case we're returning to the
     local server. */
  Gal_DelProp(frame, GAL_TOKEN_INDEX_FRAME_KEY);
  
  writeMessageToServer(session, msg, msg_type, t->server_tidx, 0);
  GalHUB_FreeServerMessage(msg);
}

static void processServerMessage(HUB *h, 
				 SERVER_MESSAGE *msg,
				 int verbose,
				 int is_initial_token)
{
  Gal_Frame frame = msg->message;
  SERVICE_PROVIDER *s = msg->provider;
  TOKEN *t = NULL;
  /* Make sure that if there's a session ID in the frame,
     the session exists. */
  SESSION *session;
  Gal_Frame hub_data = (Gal_Frame) NULL;
  GalIO_MsgType msg_type;
  double *token_timestamp_ptr = NULL;

  /* Do this before we delete the administrative info. */

  if (!is_initial_token) {
    _GalHUB_GUIAnnounceMessage(h, msg->message, msg->msg_type, s, "hub");

    /* Undo encapsulations. */
    
    if (Gal_GetObject(frame, GAL_HUB_OPAQUE_DATA_FRAME_KEY)) {
      Gal_Object script_info, token_timestamp_object;
    
      hub_data = Gal_GetFrame(frame, GAL_HUB_OPAQUE_DATA_FRAME_KEY);
      script_info = Gal_GetObject(hub_data,
				  GAL_OPAQUE_SCRIPT_MODULE_HUB_FRAME_KEY);
      if (script_info) {
	msg->opaque_script_info =  Gal_CopyObject(script_info);
      }

      /* Check for a timestamp. */
      token_timestamp_object = Gal_GetObject(hub_data, GAL_TOKEN_TIMESTAMP_FRAME_KEY);
      if (token_timestamp_object) {
	int size;
	token_timestamp_ptr = (double*) Gal_Float64Value(token_timestamp_object, &size);
      } 

      /* Undo the lock information. */
      if (Gal_GetObject(hub_data, GAL_LOCK_MASK_FRAME_KEY)) {
	msg->lock_mask = Gal_GetInt(hub_data, GAL_LOCK_MASK_FRAME_KEY);
	msg->lock_value = Gal_GetInt(hub_data, GAL_LOCK_VALUE_FRAME_KEY);
      }
      /* Undo session ID. Put it in the frame because just about
	 every function in the Hub expects it to be there. */
      if (Gal_GetObject(hub_data, GAL_SESSION_ID_FRAME_KEY))
	Gal_SetProp(frame, GAL_SESSION_ID_FRAME_KEY,
		    Gal_CopyObject(Gal_GetObject(hub_data,
						 GAL_SESSION_ID_FRAME_KEY)));
      /* Ditto tidx. */
      if (Gal_GetObject(hub_data, GAL_TOKEN_INDEX_FRAME_KEY))
	Gal_SetProp(frame, GAL_TOKEN_INDEX_FRAME_KEY,
		    Gal_CopyObject(Gal_GetObject(hub_data,
						 GAL_TOKEN_INDEX_FRAME_KEY)));
      /* See if you can capture the scriptless info and the
	 stype as well. */
      if (Gal_GetObject(hub_data, GAL_SCRIPT_STATUS_HUB_FRAME_KEY))
	msg->scriptless = Gal_GetInt(hub_data,
				     GAL_SCRIPT_STATUS_HUB_FRAME_KEY);
      if (Gal_GetObject(hub_data, GAL_INVOKED_STYPE_HUB_FRAME_KEY)) {
	if (msg->true_provider_spec.stype_name)
	  free(msg->true_provider_spec.stype_name);
	msg->true_provider_spec.stype_name = _gal_strdup(Gal_GetString(hub_data, GAL_INVOKED_STYPE_HUB_FRAME_KEY));
      }
      /* Make sure you free the Gal_Object, but not the frame. */
      Gal_FreeWrapper(Gal_RemProp(frame, GAL_HUB_OPAQUE_DATA_FRAME_KEY));
    } else {
      /* Print warning! I don't really want people to be writing
	 frames through bare connections anymore. */
      GalUtil_Warn("Receiving message written through bare connection\n");
      /* For example, MIT Sapphire might still have messages written
	 from bare connections. Make sure lock decode still happens. */
      GalSS_SessionDecodeLocks(frame, &(msg->lock_mask), &(msg->lock_value));
    }
  }

  /* Make sure we assign a session to the message. The session
     will be either one in the frame or the default session. */
  session = GalHUB_SessionLookupByFrame(frame, 1);

  /* However, if the session key isn't present and the server
     has a session that must be read from, use it. Otherwise,
     use the session ID from the session retrieved above
     (which will be the default session). In either
     case, make sure the frame has a session ID. */

  /* Another case: the message may not have a session ID,
     and the server may not be restricted to producing messages
     from a particular session, but we might want to guess that
     if the message is new, and the server is busy with a message
     from a particular session, we can assign that session to
     the new message. */     

  if (!Gal_GetString(frame, GAL_SESSION_ID_FRAME_KEY)) {
    /* There will be no provider if it's an initial token. */
    if (s) {
      if (s->only_session_to_write_to) {
	session = s->only_session_to_write_to->session;
      } else if (s->working_on.session) {
	session = s->working_on.session;
      }
    }
    Gal_SetProp(frame, GAL_SESSION_ID_FRAME_KEY,
		Gal_StringObject(session->session_id));
  }

  /* We can now record the session in the message. */
  msg->session = session;
  
  /* Populate the namespace array. */

  GalHUB_PopulateServerMessageNamespaceArray(session, msg, (TOKEN *) NULL);
  
  /* The incoming message types may be one of five:
     GAL_MESSAGE_MSG_TYPE (new message)
     GAL_ERROR_MSG_TYPE (message error return)
     GAL_REPLY_MSG_TYPE (message normal return)
     GAL_DESTROY_MSG_TYPE (token destruction request as return)
     GAL_POSTPONE_MSG_TYPE (clear the server's busy flag, but don't update)
  */
  
  switch (msg->msg_type) {
  case GAL_MESSAGE_MSG_TYPE:
    t = handle_new_message(h, msg, hub_data, is_initial_token);

    /* Use the timestamp sent by the server if appropriate. */
    if(t && token_timestamp_ptr && *token_timestamp_ptr != -1) {
	t->timestamp = *token_timestamp_ptr;
    } 
    break;
  case GAL_ERROR_MSG_TYPE:
  case GAL_REPLY_MSG_TYPE:
  case GAL_POSTPONE_MSG_TYPE:
  case GAL_DESTROY_MSG_TYPE:
    t = handle_other_message(h, msg);
    break;
  default:
    break;
  }

  /* Cleanup. */
  if (hub_data) {
    Gal_FreeFrame(hub_data);
    hub_data = (Gal_Frame) NULL;
  }
  if (!t) {
    Gal_FreeFrame(frame);
    GalHUB_FreeServerMessage(msg);
    return;
  }

  /* Followup. */

  switch (msg->msg_type) {
  case GAL_MESSAGE_MSG_TYPE:
    Gal_SetProp(frame, GAL_TOKEN_INDEX_FRAME_KEY, Gal_IntObject(t->tidx));
    break;
  case GAL_DESTROY_MSG_TYPE:
    if (t) {
      destroyTokenIfDone(h, t, session, t->state,
			 GAL_REPLY_MSG_TYPE);
      msg->namespace_array[GAL_TOKEN_NAMESPACE] = (Gal_Frame) NULL;
      t = (TOKEN *) NULL;
    }
    break;
  case GAL_ERROR_MSG_TYPE:
    /* If we've gotten this far and we read an error, that means
       we're going to log it. Always. All errors get logged. */
    GalHUB_LogErrorMessageKeys(msg);
    break;
  default:
    break;
  }
  
  /* There will always be a session here, even if it's the default, if
     I return a message. There are four ways there can be a session
     in the frame:
     (1) It arrives in the message.
     (2) Message comes from a server which is busy with a token which
     had a marked session (server-to-server subdialogue)
     (3) Message comes from a server which only writes to a particular session
     (4) Default is assigned.
  */
  
  GalHUB_SessionHandleLocks(session, s, t->tidx, msg->lock_mask,
			    msg->lock_value, 1, 0, 0);
  GalHUB_LogfileUpdateMsgOnRead(s, msg, msg->msg_type, t);

  /* The msg type may have been updated. Rescue it before freeing. */
  msg_type = msg->msg_type;  
  GalHUB_FreeServerMessage(msg);

  if (msg_type == GAL_ERROR_MSG_TYPE) {
    /* If it's an error, write it back to the caller.
       Don't process it any further. But make sure you do
       this after the log is updated, because writeMessageToServer()
       also writes to the log. */
    
    destroyTokenIfDone(h, t, session, frame, GAL_ERROR_MSG_TYPE);
    Gal_FreeFrame(frame);
    return;
  }
  Gal_FreeFrame(frame);

  if (verbose)
    printTokens(h);
  else if (debug_token_gc)
    printTokens(h);

  if (t) {
    _GalHUB_CReportTokenStatus(Hub, GAL_PINFO1_LEVEL, 5, 0,
			       GAL_HUB_TOKEN_STATUS, t, 0);
    
    /* queueMessages returns 1 if it's still in the program, 0 otherwise. */

    if (queueMessages(h, t, msg_type, h->debug, 1) == 0)
      destroyTokenIfDone(h,t, session, t->state, GAL_REPLY_MSG_TYPE); 
  }
}

/* SAM 10/31/99: This has to return a server, because the loop
   which calls process_pending_server checks the status of the
   variable after the call is made. */

static SERVICE_PROVIDER *
processNoServerMessage(HUB *h, SERVICE_PROVIDER *s, int verbose)
{
  
  Gal_Frame error_msg = (Gal_Frame) NULL;
  
  GalUtil_Warn("Provider %s closed connection", s->pname);

  /* free up the queues */
  GalIO_SetCommDone(s->gcomm);
  GalIO_DestroyCommStruct(s->gcomm);
  s->gcomm = NULL;
  
  /* Here's the idea. If we lose a connection whose listen_status is
     GAL_HUB_STYPE_SERVER, then we don't try to reconnect, but
     rather remove the server. But that's the only difference,
     as far as I can tell. */

  /* check to see if there's a token associated with this server.
     If so, generate a server down.  Moreover, if the token is
     awaiting an m-m, send the error as an m-m reply */
  if (s->status >= 0) {
    TOKEN *t2;
    Gal_Frame err_frame = NULL;
    err_frame = generate_server_down_for_session(h,s->working_on.session,s,1);
    t2 = GalHUB_GetTokenFromIndex(s->status);
    if (t2)
      GalHUB_DecrementTokenReference(t2,9);

    if (t2 && t2->mm && t2->owner) {	    
      GalUtil_CPInfo1WithLocation(__FUNCTION__, 5,0,"Server crashed while processing message for token %d --> returning to owner %s@%s:%d\n",
				  t2->tidx,t2->owner->iname,
				  t2->owner->host,t2->owner->port);

      if (err_frame) {
	error_msg = GalIO_CreateErrorFrame(GAL_SERVER_DOWN_ERROR, "Server down");
	Gal_SetProp(error_msg, ":hub_error_frame", Gal_FrameObject(err_frame));
      }
      destroyTokenIfDone(h, t2, (SESSION *) NULL,
			 error_msg, GAL_ERROR_MSG_TYPE);
      if (error_msg) {
	Gal_FreeFrame(error_msg);
      }
    }
  }
  if (s->status > DISCONNECTED)
    s->status = DISCONNECTED;

  /* Now, destroy the server if appropriate. */

  if (s->listen_status == GAL_HUB_STYPE_SERVER) {
    _GalHUB_GUIAnnounceServiceProviderConnection(h, s, 0, 0);
    GalHUB_RemoveServiceProvider(h, s, 1, 1);
    s = NULL;
  }
  return s;
}

static SERVICE_PROVIDER *
process_pending_server(SERVICE_PROVIDER *s, HUB *h)
{
  Gal_Frame frame = NULL;
  int res=0;
  int verbose=0;
  GalIO_MsgType msg_type;
  SERVER_MESSAGE *msg;

  /* see if you can read something */
  res = readMessageFromServer(h, s, &frame, &msg_type);

  if ((res > 0) && frame ) {
    /* If we have a frame and it's a reply message, then we check the
       signature of the operation (see the frame name), if we're
       validating. */
    if (h->validate && (msg_type == GAL_REPLY_MSG_TYPE)) {
      Gal_DispatchFnSignature *sig;

      sig = Gal_FindNamedSignature(s->signatures, Gal_FrameName(frame));
      if (sig) {
	Gal_ValidateDispatchFnOutput(Gal_FrameName(frame), frame,
				     sig, Exclusions);
      }
    }

    msg = GalHUB_NewServerMessage(frame, GAL_INCOMING_MSG, msg_type,
				  NULL, NULL, s, 0, 0, -1, -1, NULL,
				  0, NULL, NULL);

    processServerMessage(h,msg,verbose,0);
  } else if (res == -1) {
    s = processNoServerMessage(h,s,verbose);
  }
  return s;
}

void GalHUB_AttemptServerReconnection(HUB *h, SERVICE_PROVIDER *s, int special)
{
  if (s->status <= DISCONNECTED) {
    if (s->listen_status == GAL_HUB_STYPE_CLIENT) {
      GalUtil_CPInfo2(5,0,"%s: Provider %s not connected, attempting reconnection\n", __FUNCTION__, s->pname);
      initialize_connection_to_server(s, h, 1, special, 1);
      /* LOGDISCONNECTED means DISCONNECTED + already logged
	 (to avoid filling log files with dribble)
	 - also helps with notification of GUI. */
      if (s->status == DISCONNECTED) {
	s->status = LOGDISCONNECTED;
	GalHUB_LogfileLogServerStatus(Sessions, s, ":server_down");
	/* _GalHUB_GUIAnnounceServiceProviderConnection(h, s, 0, 1); */
      }
      else if (s->status > DISCONNECTED) {
	GalHUB_LogfileLogServerStatus(Sessions, s, ":server_back_up");
      }
    } else {
      _GalHUB_GUIAnnounceServiceProviderConnection(h, s, 0, 0);
    }
  }
}

/* This now also logs the failure on the first notice */
static void attempt_reconnection_to_down_servers(HUB *h)
{
  int i;
  int num_servers = Gal_PointerBufferSize(h->servers);
  SERVICE_PROVIDER **servers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(h->servers);
  
  for (i = 0; i < num_servers; i++) {
    GalHUB_AttemptServerReconnection(h, servers[i], 0);
  }
}


static void check_for_alarms(HUB *h)
{
  extern SESSION *Sessions;
  SESSION *s = Sessions;
  while(s) {
    int i;
    for(i=0;i<GalHUB_AlarmGetNum();i++) {
      time_t now = time(NULL);
      if(s->alarm_secs_to_expiration[i] > 0 && !s->alarm_disabled[i] &&
	 difftime(now,s->alarm_last_reset[i]) > s->alarm_secs_to_expiration[i]) {
	Nframe frame;
	TOKEN *t2;

	s->alarm_secs_to_expiration[i] = 0;
	s->num_system_errors++;


	_GalHUB_CReportSessionAlarmStatus(h, GAL_PINFO2_LEVEL, 2, 0, 
					  GAL_HUB_SESSION_ALARM_EXPIRED,
					  GalHUB_AlarmIndexToName(i),
					  s->session_id,
					  s->alarm_secs_to_expiration[i], 0);
	
	frame = Gal_MakeFrame("system_error", GAL_CLAUSE);
	Gal_SetProp(frame, ":alarm_expired", Gal_IntObject(1));
	Gal_SetProp(frame, ":alarm_name", Gal_StringObject(GalHUB_AlarmIndexToName(i)));
	Gal_SetProp(frame, GAL_SESSION_ERROR_COUNT_HUB_FRAME_KEY, Gal_IntObject(s->num_system_errors));
	Gal_SetProp(frame, GAL_UTTERANCE_ID_FRAME_KEY, Gal_IntObject(s->utterance_id));
	Gal_SetProp(frame, GAL_SESSION_ID_FRAME_KEY, Gal_StringObject(s->session_id));

	t2 = GalHUB_NewToken(frame, NULL, s->utterance_id,
			     s->session_id, (Gal_Frame) NULL);
	
	GalHUB_LogfileLogAlarmEvent(s,GalHUB_AlarmIndexToName(i),"alarm_expired");

	Gal_FreeFrame(frame);

	process_system_error(s,t2,h);
      }
    }
    s = s->next;
  }

  print_alarm_vars(h);
}

static int exit_hub = 0;

void force_hub_exit()
{
  exit_hub = 1;
}

int GalHUB_PerhapsAddServerToFD(SERVICE_PROVIDER *server,
				fd_set *writefd_ptr,
				fd_set *readfd_ptr)
{
  int previous_ready = 0;
  
  if (server->status <= DISCONNECTED)
    return 0;
  if (server->sockid != LOCAL) {
    /* Note the different bracketing for read and write.
       This is because you DON'T want to check the
       write socket until you know you have stuff to
       write, because select() doesn't return whether
       there's stuff on it waiting to be written,
       but rather if it's READY for writing. So it
       will always return true (EVEN IF THE REMOTE
       SIDE IS CLOSED - I hate that). The upshot is
       that num_ready will be nonzero and the
       pacifier dots will not be printed. */	   
    if (GalIO_CommWriteReady(server->gcomm)) {
      previous_ready = 1;
      FD_SET(server->sock, writefd_ptr);
    }
    if (GalIO_CommReadReady(server->gcomm))
      previous_ready = 1;
    FD_SET(server->sock, readfd_ptr);
  }
  return previous_ready;
}

SERVICE_PROVIDER *GalHUB_CheckServerFD(HUB *h, SERVICE_PROVIDER *server,
				       fd_set *writefd_ptr,
				       fd_set *readfd_ptr)
{     
  if ((server->sockid >=0) &&
      (GalIO_CommWriteReady(server->gcomm) || 
       FD_ISSET(server->sock, writefd_ptr))) {
    GalIO_QueueProcessOut(server->gcomm, 0);
  }
      
  if ((server->sockid >=0) &&
      (GalIO_CommReadReady(server->gcomm) || 
       FD_ISSET(server->sock, readfd_ptr))) {
    do {
      /* process_pending_server may destroy the
	 server if it contacted the Hub and is now
	 dead. */
      server = process_pending_server(server, h);
    } while (server && GalIO_CommReadReady(server->gcomm));
  }
  return server;
}

void
process_sessions(HUB *h)
{
  int i;
  SERVICE_PROVIDER *server;
  int num_ready;
  fd_set readfd, writefd;
  struct timeval tv, tv0;
  int interrupted;
  int num_servers;
  SERVICE_PROVIDER **servers;
  /* This is fixed at this point. */
  int num_proxies = Gal_PointerBufferSize(h->listener_proxies);
  LISTENER_PROXY **proxies = (LISTENER_PROXY **) Gal_PointerBufferPointers(h->listener_proxies);
  LISTENER_PROXY *p;
  int last_msg_queue_length = 0;
  int timed_out = 0;

  /* serve initial token, if there is one */
  /* SAM 3/8/02: Huge, huge bug. No logging for the initial token message.
     either timestamps or keys. Only IN: will ever be logged, because
     the token isn't "returned" anywhere. In order to do this
     correctly, we need to turn the token into a message.
     So I'll salt the appropriate functions with conditions for
     what to do when presented with an initial token, and then
     we can just use processServerMessage, which does the
     Right Thing. */
  
  if (h->token) {
    SERVER_MESSAGE *msg;

    msg = GalHUB_NewServerMessage(Gal_CopyFrame(h->token->state),
				  GAL_INCOMING_MSG,
				  GAL_MESSAGE_MSG_TYPE, NULL, NULL,
				  (SERVICE_PROVIDER *) NULL,
				  0, 0, -1, -1, NULL,
				  0, NULL, NULL);

    processServerMessage(h, msg, 0, 1);
  }

  /* now loop over all connections to see if there is a message     */
  /* once a message comes in - update the hub_state and then find   */
  /* the next action/server to call                                 */

/*
input_string
    1. fd_set on normal
    2. fd_set on audio/gui
    3. select
    4. fd_isset and hairy stuff on normal
    5. ditto on audio/gui
every time ray sends a connection frame i create a new session.
*/

  while (!exit_hub) {
    /* As Sam Bayer noted, bec. of the way an intialization to server is done, we
       may have some stuff in the sock/sockobject queues at the very start, and they
       won't be caught by the select. */
    int previous_ready = 0;
    check_for_alarms(h); 
    
    attempt_reconnection_to_down_servers(h);

    h->localReady = local_server_ready(h);
    
    /* in case any new servers have been added or removed in the interim */

    num_servers = Gal_PointerBufferSize(h->servers);
    servers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(h->servers);

    FD_ZERO(&readfd);     /* clear read mask */
    FD_ZERO(&writefd);    /* clear write mask */

    if (GalHUB_PerhapsAddSpecialServers(h, &writefd, &readfd))
      previous_ready = 1;

    /* set the active bits on servers */
    for (i = 0; i < num_servers; i++) {
      server = servers[i];
      if (GalHUB_PerhapsAddServerToFD(server, &writefd, &readfd))
	previous_ready = 1;
    }
    
    /* Now, set up the listener proxies. */
    for (i = 0; i < num_proxies; i++) {
      p = proxies[i];
      if (p->scomm &&
	  (GalIO_GetServerListenSocket(p->scomm) != GAL_INVALID_SOCKET)) {
	FD_SET(GalIO_GetServerListenSocket(p->scomm), &readfd);
      }
    }
    
    /* select timeout set to 1 sec ... */
    tv.tv_sec = 1;
    tv.tv_usec = 0;
    tv0.tv_sec = 0;
    tv0.tv_usec = 0;

    /* Note that Windows sockets ignore the maxfd argument. */

    interrupted = timed_out = 0;
    if (!h->localReady && !previous_ready & (h->msg_queue_length == 0)) {
      num_ready = GalUtil_SockSelect(h->maxfd+1, &readfd, &writefd, (fd_set *)0, &tv, &interrupted);
      timed_out = 1;
    } else {
      num_ready = GalUtil_SockSelect(h->maxfd+1, &readfd, &writefd, (fd_set *)0, &tv0, &interrupted);
    }
#ifdef __INSURE__
    /* Chuckleheaded Insure++ can't exit cleanly from a simple server loop. */
    if (access("/tmp/exitnow", F_OK) == 0) {
      GalUtil_Warn("Found /tmp/exitnow (forces Insure++ exit)\n");
      exit(0);
    }
#endif
    if (!h->localReady && !previous_ready && (num_ready < 0) && !interrupted) 
    { GalUtil_Warn("Error %d in select()\n", GAL_SOCKET_ERRNO);
      continue;
    }

    if (last_msg_queue_length != h->msg_queue_length) {
      /* It's time to report the queue length if the length differed 
	 from the last time it was reported. */
      _GalHUB_ReportMessageQueueStatus(h, GAL_PINFO1_LEVEL, GAL_HUB_MESSAGE_QUEUE_CONTENTS);
      last_msg_queue_length = h->msg_queue_length;
    }
    
    if (!h->localReady && !previous_ready && (num_ready <=0)) {
      if ((num_servers == 0) ||
	  (tryToFlushMessageQueue(h, servers, num_servers,
				  (rand() % num_servers)) == 0)) {
	static int count = 0;
	if (((count % 2) == 0) && (!h->suppress_pacifier) && timed_out)
	  GalUtil_PInfo1(".");
	count++;
      }
      continue;
    }

    /* then process everything that's active */
    /* Remember, the number of servers can change
       if a server dies in the middle. So make sure
       you check as you go along. The value of
       servers won't change (that can only happen
       if servers are ADDED, and they're not in this loop),
       but the number of servers may change. */
    for (i = 0; i < Gal_PointerBufferSize(h->servers); i++) {
      server = servers[i];
      if (server->sockid == LOCAL) {
	if (h->localReady) {
	  /* Read from the local server. In 3.3, this led
	     to a tight loop, because a call
	     to a local server function can result in another
	     call to a local server function. But in 4.0,
	     the common outbound queue eliminates this problem. */
	  do {
	    server = process_pending_server(server, h);
	  } while(server && local_server_ready(h));
	}
      } else {
	server = GalHUB_CheckServerFD(h, server, &writefd, &readfd);
      }
      /* And next, we try to write stuff from the queue
	 to each server. */
      if (server) 
	tryToFlushMessageQueue(h, servers, num_servers, i);
    }

    /* And now the special servers. */
    GalHUB_CheckSpecialServerFDs(h, &writefd, &readfd);
    
    /* And don't forget the listener proxies. */
    for (i = 0; i < num_proxies; i++) {
      p = proxies[i];
      if (p->scomm &&
	  FD_ISSET(GalIO_GetServerListenSocket(p->scomm), &readfd)) {
	/* If the true server has something to connect, accept it. */
	GalHUB_AcceptClientConnection(p, h);
      }
    }
  }
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
