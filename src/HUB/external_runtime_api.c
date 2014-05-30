/*
  This file (c) Copyright 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"
#include "hub_internal.h"

extern struct SESSION *Sessions;

void GalHUB_RemoveServiceProvider(HUB *h, SERVICE_PROVIDER *s,
				  int remove_from_stype, int remove_from_hub)
{
  int i;
  TOKEN *t;
  
  if (s->listen_status == GAL_HUB_STYPE_SERVER) {
    /* There's a chance that some session is locked to this
       provider by virtue of the provider being initialized with
       a specific session ID. So we look at all the sessions, and
       if we find a session with such a lock, we remove the lock
       and then see if the session is pending an end. */
    SESSION *session = Sessions;
    SESSION_LOCK_INFO *sli;
    void *lock_info;
    int try_to_end;
    SESSION *next_session;

    while (session) {
      next_session = session->next;
      i = 0;
      try_to_end = 0;
      lock_info = Gal_PointerBufferNthElement(session->lock_info, i);

      while (lock_info) {
	sli = (SESSION_LOCK_INFO *) lock_info;
	if ((sli->provider == s) && sli->via_listener) {
	  /* We want to remove it. */
	  Gal_PointerBufferRemove(session->lock_info, lock_info);
	  try_to_end = 1;
	  free(lock_info);
	} else {
	  i++;
	}
	lock_info = Gal_PointerBufferNthElement(session->lock_info, i);
      }
      if (try_to_end && session->session_ended) {
	/* The session may still be locked. */
	GalHUB_SessionEnd(h, session);
      }
      session = next_session;
    }
  }
  /* Remove the signatures. */
  if (s->signatures) {
    Gal_FreeDispatchFnSignature(s->signatures);
  }
  if (s->working_on.stype_name)
    free(s->working_on.stype_name);
  /* Make sure that if any of the tokens have this
     provider as an owner, that it's removed. */
  t = h->token;
  while (t) {
    if (t->owner == s) {
      t->owner = (SERVICE_PROVIDER *) NULL;
      /* Ain't no owner to return it to. */
      if (t->mm) {
	t->mm = 0;
      }
    }
    t = t->next;
  }
  GalHUB_LRemoveServiceProvider(h, s, remove_from_stype, remove_from_hub);
}


extern
void _GalIO_QueueEnqueue(GalIO_PointerQueue *queue,
			 void *element);

void GalHUB_EnqueueServerMessage(HUB *h, TOKEN *t, SERVER_MESSAGE *msg)
{
  _GalIO_QueueEnqueue(h->message_queue, (void *) msg);
  /* And increment the relevant token, so the token doesn't
     get destroyed until all the messages are out of the queue. */
  GalHUB_IncrementTokenReference(t, 3);
  h->msg_queue_length++;  
}

/* SAM 5/4/01: I don't know whether we need to pass the session
   in, or whether we can extract it from the token or frame.
   But right now I don't have the time to figure it out; I just
   want to cache it so it can be accessed efficiently. */

/* 4/12/02: I've split up the server name (which comes either
   from the name of the incoming frame or the target of a RULE:)
   from the provider ID (which comes either as part of a targeted
   provider ID or as the value of the PROVIDER_ID: directive).
   When in doubt, the provider ID takes precedence. */

/* SAM 4/29/02: In the process of redoing message selection, we've
   gradually migrated to a point where we have an opportunity to
   canonicalize the provider identification. It turns out that
   we need to do this as early as possible, so we can use the
   canonicalized info to retrieve timestamp information from the
   logging hash table. */

extern void _GalSS_InitializeProviderSpec(GalSS_ProviderSpec *spec);

static void __GalHUB_CanonicalizeServerMessageInfo(SERVER_MESSAGE *msg,
						   char *server_name,
						   char *provider_id_name)
{
  char *stype_name, *other_stype_name;
  int free_server_name = 0;
  int provider_id = -1, other_provider_id = -1;
  char *provider_name = (char *) NULL, *other_provider_name = (char *) NULL;

  /* If there's no server name, see if you can extract one from
     the name of the frame. Remember, it will be new memory, so
     make sure it's freed. */
  if (!server_name) {
    char *op = Gal_SplitOperationName(Gal_FrameName(msg->message),
				      &server_name);
    free_server_name = 1;
    msg->bare_operation = op;
  } else {
    /* If there's a server name, I'm pretty sure that we
       have already done the split, and the name of the frame
       is the name of the operation. */
    msg->bare_operation = _gal_strdup(Gal_FrameName(msg->message));
  }
  
  stype_name = Gal_SplitServiceTypeName(server_name, &provider_id,
					&provider_name);
  other_stype_name = Gal_SplitServiceTypeName(provider_id_name,
					      &other_provider_id,
					      &other_provider_name);

  /* Freeing the server name we may have created. */
  if (free_server_name && server_name)
    free(server_name);
  
  if (other_stype_name) {
    if (stype_name) free(stype_name);
    stype_name = other_stype_name;
  }
  
  if ((other_provider_id != -1) || other_provider_name) {
    provider_id = other_provider_id;
    if (provider_name) free(provider_name);
    provider_name = other_provider_name;
  }

  /* Make sure you try to canonicalize the name of the Builtin server
     AFTER you split the service type. */
     
  if (stype_name && !_gal_strcasecmp(stype_name, BUILTIN_SERVER)) {
    /* Because Gal_SplitServiceTypeName always creates new memory. */
    free(stype_name);
    stype_name = _gal_strdup(BUILTIN_SERVER);
  }
  _GalSS_InitializeProviderSpec(&(msg->true_provider_spec));
  msg->true_provider_spec.stype_name = stype_name;
  msg->true_provider_spec.id_name = provider_name;
  msg->true_provider_spec.id = provider_id;
}

SERVER_MESSAGE *GalHUB_NewServerMessage(Gal_Frame f,
					int direction,
					GalIO_MsgType msg_type,
					char *service_name,
					char *provider_id_name,
					SERVICE_PROVIDER *provider,
					int lock_mask,
					int lock_value,
					int no_return,
					int scriptless,
					Gal_Object opaque_script_info,
					int force_timestamp,
					TOKEN *t,
					SESSION *s)
{
  SERVER_MESSAGE *msg = (SERVER_MESSAGE *) calloc(1, sizeof(SERVER_MESSAGE));

  msg->message = f;
  msg->direction = direction;
  msg->msg_type = msg_type;
  msg->provider = provider;
  msg->provider_reason = -1;
  msg->lock_mask = lock_mask;
  msg->lock_value = lock_value;
  msg->no_return = no_return;
  msg->scriptless = scriptless;
  msg->opaque_script_info = opaque_script_info;
  msg->force_timestamp = force_timestamp;
  if (t)
    msg->tidx = t->tidx;
  msg->session = s;
  msg->provider_array.alloc_size = 0;
  msg->provider_array.buffer_size = 0;
  msg->provider_array.cells = (PROVIDER_FLAG_CELL *) NULL;
  /* Now, canonicalize the message info. */
  __GalHUB_CanonicalizeServerMessageInfo(msg, service_name, provider_id_name);
  return msg;
}

/* SAM 3/12/02: I'm going to insert these in provider order, so that
   I can search in provider order if I want. The idea will probably
   be that each time I free a server by reading from it, I go
   through the message queue starting with the first message,
   starting with the server I just freed. I may end up with
   duplicate IDs in this list, but the stypes will be different. */

void GalHUB_AddEligibleProvider(SERVER_MESSAGE *msg, SERVICE_TYPE *stype,
				SERVICE_PROVIDER *provider)
{
  PROVIDER_FLAG_ARRAY *p = &(msg->provider_array);
  int i, j, insert_i;

  /* Make sure the buffer is big enough. */
  if (p->alloc_size == p->buffer_size) {
    if (p->alloc_size == 0) {
      p->cells = (PROVIDER_FLAG_CELL *) malloc(PROVIDER_FLAG_CELL_INCREMENT *sizeof(PROVIDER_FLAG_CELL));
    } else {	
      p->cells = (PROVIDER_FLAG_CELL *) realloc((void *) p->cells, (p->alloc_size + PROVIDER_FLAG_CELL_INCREMENT) * sizeof(PROVIDER_FLAG_CELL));
    }
    p->alloc_size += PROVIDER_FLAG_CELL_INCREMENT;
  }

  /* Now, find the place to put it. We start by
     guessing the end. */
  insert_i = p->buffer_size;
  for (i = 0; i < p->buffer_size; i++) {
    if (p->cells[i].provider_id > provider->id) {
      /* Put it here. Move everything out of the
	 way, starting at the end and working your
	 way backwards. */
      for (j = p->buffer_size; j > i; j--) {
	p->cells[j].provider_id = p->cells[j - 1].provider_id;
	p->cells[j].stype = p->cells[j - 1].stype;
      }
      /* Set the insert index. */
      insert_i = i;
      break;
    }
  }
  p->cells[insert_i].provider_id = provider->id;
  p->cells[insert_i].stype = stype;
  p->buffer_size++;
}

void GalHUB_PopulateServerMessageNamespaceArray(SESSION *session, SERVER_MESSAGE *msg,
						TOKEN *t)

{
  int i;
  
  for (i = 0; i < MAX_NAMESPACES; i++) {
    switch (i) {
    case GAL_SESSION_NAMESPACE:
      if (session)
	msg->namespace_array[i] = session->session_vars;
      else
	msg->namespace_array[i] = (Gal_Frame) NULL;
      break;
    case GAL_MESSAGE_NAMESPACE:
      msg->namespace_array[i] = msg->message;
      break;
    case GAL_SERVER_NAMESPACE:
      if (msg->provider)
	msg->namespace_array[i] = msg->provider->properties;
      else
	msg->namespace_array[i] = (Gal_Frame) NULL;
      break;
    case GAL_TOKEN_NAMESPACE:
      if (t)
	msg->namespace_array[i] = t->state;
      else
	msg->namespace_array[i] = (Gal_Frame) NULL;
      break;
    case GAL_GLOBAL_NAMESPACE:
      msg->namespace_array[i] = Hub->globals;
      break;
    default:
      msg->namespace_array[i] = (Gal_Frame) NULL;
    }
  }
}

extern void _GalSS_ClearProviderSpec(GalSS_ProviderSpec *spec);

void GalHUB_FreeServerMessage(SERVER_MESSAGE *msg)
{
  if (msg->opaque_script_info)
    Gal_FreeObject(msg->opaque_script_info);
  if (msg->kp_array) {
    /* SAM 5/25/01: to make SURE that the memory mgmt is
       done right, we should free any of the elements
       in here which are newly created. */
    int i = 0;
    while (msg->kp_array[i].key) {
      if (msg->kp_array[i].newly_created)
	Gal_FreeObject(msg->kp_array[i].value);
      i++;
    }
    free(msg->kp_array);
  }
  if (msg->provider_array.cells)
    free(msg->provider_array.cells);
  _GalSS_ClearProviderSpec(&(msg->true_provider_spec));
  if (msg->bare_operation) free(msg->bare_operation);
  free(msg);
}

SERVICE_PROVIDER *
GalHUB_ProviderSpecToProvider(HUB *h, GalSS_ProviderSpec *spec)
{
  SERVICE_PROVIDER *p = (SERVICE_PROVIDER *) NULL;
  
  if (!spec)
    return (SERVICE_PROVIDER *) NULL;
  
  /* If there's provider info, then try to find the
     correct server. I need both host and port if I don't
     have the ID information. */
  if ((spec->host && (spec->port != -1)) ||
      (spec->id != -1) || spec->id_name) {
    int num_servers = Gal_PointerBufferSize(h->servers);
    SERVICE_PROVIDER **servers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(h->servers);
    int i;

    for (i = 0; i < num_servers; i++) {
      /* Don't forget, the Builtin server won't have a host
	 associated with it. And the id_name could be empty. */
      SERVICE_PROVIDER *s = servers[i];
      if (((!spec->host) || Gal_StringEq(spec->host, s->host)) &&
	  ((spec->port == -1) || (spec->port == s->port)) &&
	  ((spec->id == -1) || (spec->id == s->id)) &&
	  ((!spec->id_name) || Gal_StringEq(spec->id_name, s->id_name))) {
	/* We've found our provider. I don't
	   want to add the actual provider here, because
	   I really don't want to have to search this
	   queue when a provider vanishes. */
	if (spec->stype_name) {
	  /* Better verify that the name is on the list. */
	  int j;
	  int num_stypes = Gal_PointerBufferSize(s->stypes);
	  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(s->stypes);
	  for (j = 0; j < num_stypes; j++) {
	    if (!strcmp(spec->stype_name, stypes[j]->name)) {
	      p = s;
	      break;
	    }
	  }
	  if (p) break;
	} else {
	  p = s;
	  break;
	}
      }
    }
  }
  GalSS_FreeProviderSpec(spec);
  return p;
}

/* SAM 4/3/02: This function is here because it's only needed
   in the run-time Hub, and relies on GalHUB_RemoveServiceProvider. */

/* We always need the Hub present, since we may need to remove service
   providers. However, we need to control if you remove the service type.
   See initialize_connections() in hub_init.c */

extern void _GalHUB_RecalculateSPName(SERVICE_PROVIDER *s);

void GalHUB_RemoveServiceType(SERVICE_TYPE *stype, HUB *h,
			      int remove_from_providers, int remove_from_hub)
{
  int num_providers = Gal_PointerBufferSize(stype->providers);
  SERVICE_PROVIDER **providers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(stype->providers);
  int i;

  if (remove_from_providers) {
    for (i = 0; i < num_providers; i++) {
      Gal_PointerBufferRemove(providers[i]->stypes, (void *) stype);
      /* If there are no more service types, remove the provider. */
      if (Gal_PointerBufferSize(providers[i]->stypes) == 0) {
	/* No danger of recursion, since it has no providers on it. */
	GalHUB_RemoveServiceProvider(h, providers[i], 0, 1);
      }
      _GalHUB_RecalculateSPName(providers[i]);
    }
  }
  if (remove_from_hub)
    Gal_PointerBufferRemove(h->stypes, (void *) stype);
  /* Don't free the listener proxy here, because it
     might be shared by multiple service types. */
  free(stype->name);
  Gal_FreePointerBuffer(stype->providers);
  if (stype->operations) {
    for (i = 0; stype->operations[i]; i++) {
      free(stype->operations[i]);
    }
    free(stype->operations);
  }
  if (stype->init_kps)
    free_key_pairs(stype->init_kps);
  if (stype->properties)
    free_key_pairs(stype->properties);
  if (stype->conditions)
    Gal_FreeTests(stype->conditions);
  if (stype->in)
    free_entity_pairs(stype->in);
  /* Don't free the special server, because the
     special server is freeing you. */
  free(stype);
}
