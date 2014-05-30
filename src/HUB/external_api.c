/*
  This file (c) Copyright 1998 - 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"
#include "hub_internal.h"

void _GalHUB_RecalculateSPName(SERVICE_PROVIDER *s);

static int global_server_id = 0;

int GalHUB_GetNewServerID()
{
  return global_server_id++;
}

void GalHUB_SetHubServices(HUB *hub, Gal_PointerBuffer *providers,
			   Gal_PointerBuffer *service_types)
{
  hub->stypes = service_types;
  hub->servers = providers;
}

SERVICE_PROVIDER *GalHUB_NewServiceProvider(char *host, int port,
					    SERVICE_TYPE *stype)
{
  SERVICE_PROVIDER *s = (SERVICE_PROVIDER *)calloc(1,sizeof(SERVICE_PROVIDER));
  char buf[128];
  
  if (s)
  {
    if (host) {
      s->host = _gal_strdup(host);
    }
    s->port = port;
    /* Don't waste an ID until the provider is recorded. */
    s->id = -1;
    /* Use the frame name to store the canonical name. */
    sprintf(buf, "[%d]", s->id);
    s->properties = Gal_MakeClauseFrame(buf);
    s->stypes = Gal_MakePointerBuffer(NULL, GAL_SERVICE_TYPE_PTYPE,
				      0, 0, 1, 1,
				      NULL, 10, 0);
    if (stype) {
      GalHUB_AddServiceProviderStype(s, stype);
    }
    s->sock = GAL_INVALID_SOCKET;
    s->status = DISCONNECTED;
    s->sockid = -1;	
    s->working_on.tidx = -1;
    s->working_on.scriptless = 0;
    s->working_on.session = (SESSION *) NULL;
    s->working_on.stype_name = (char *) NULL;

    return(s);
  }
  GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't allocate memory for service provider");
  return(NULL);
}

char *GalHUB_ServiceProviderID(SERVICE_PROVIDER *s)
{
  return Gal_FrameName(s->properties);
}

void GalHUB_AddServiceProviderStype(SERVICE_PROVIDER *s, SERVICE_TYPE *stype)
{
  Gal_PointerBufferAdd(s->stypes, (void *) stype);
  if (stype->properties) {
    /* Don't override. */
    _GalHUB_CopyInitKeys(s->properties, stype->properties, 0);
  }
  _GalHUB_RecalculateSPName(s);
}

void GalHUB_RecordServiceProvider(HUB *hub, SERVICE_PROVIDER *s)
{
  /* If the provider has a single stype which is special,
     then record it there. Otherwise, add it to the Hub.
     Also, add the provider to all its stypes. */
  int num_stypes = Gal_PointerBufferSize(s->stypes);
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(s->stypes);
  int i;

  s->id = GalHUB_GetNewServerID();
  _GalHUB_RecalculateSPName(s);
  if ((num_stypes == 1) && stypes[0]->special)
    stypes[0]->special->provider = s;
  else if (hub)
    Gal_PointerBufferAdd(hub->servers, (void *) s);
  /* Add the provider to all its stypes. */
  for (i = 0; i < num_stypes; i++) {
    Gal_PointerBufferAdd(stypes[i]->providers, (void *) s);
  }  
}

void GalHUB_LRemoveServiceProvider(HUB *h, SERVICE_PROVIDER *s,
				   int remove_from_stype, int remove_from_hub)
{
  int num_stypes = Gal_PointerBufferSize(s->stypes);
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(s->stypes);
  int i;

  if (remove_from_stype) {
    for (i = 0; i < num_stypes; i++) {
      Gal_PointerBufferRemove(stypes[i]->providers, (void *) s);
      if (stypes[i]->special) {
	stypes[i]->special->provider = (SERVICE_PROVIDER *) NULL;
      }
    }
  }

  /* If it's special, it won't be in this list. So this is harmless. */
  if (remove_from_hub)
    Gal_PointerBufferRemove(h->servers, (void *) s);
  if (s->host) free(s->host);
  Gal_FreePointerBuffer(s->stypes);
  if (s->id_name) free(s->id_name);
  /* If the session is read or write locked, we should remove
     the locks and then try to end the specified session. */  
  if (s->only_session_to_write_to)
    free(s->only_session_to_write_to);
  if (s->only_session_to_read_from)
    free(s->only_session_to_read_from);
  if (s->init_kps)
    free_key_pairs(s->init_kps);
  if (s->properties)
    Gal_FreeFrame(s->properties);
  if (s->conditions)
    Gal_FreeTests(s->conditions);
  if (s->in)
    free_entity_pairs(s->in);
  free(s);
}

void _GalHUB_RecalculateSPName(SERVICE_PROVIDER *s)
{
  int num_stypes = Gal_PointerBufferSize(s->stypes);
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(s->stypes);
  int i;
  
  sprintf(s->iname, "%s", stypes[0]->name);
  sprintf(s->pname, "for %s", stypes[0]->name);
  for (i = 1; i < num_stypes; i++) {
    sprintf(s->iname + strlen(s->iname), "/%s", stypes[i]->name);
    sprintf(s->pname + strlen(s->pname), ", %s", stypes[i]->name);
  }
  if (strcmp(stypes[0]->name, BUILTIN_SERVER)) {
    /* As long as this isn't the builtin server */
    if (s->listen_status == GAL_HUB_STYPE_CLIENT) {
      sprintf(s->pname + strlen(s->pname), " @ %s:%d",
	      s->host ? s->host : "", s->port);
    } else {
      sprintf(s->pname + strlen(s->pname), " (id %d)", s->id);
    }
  }
  /* And do the same for the name of the properties. */
  /* SAM 8/11/02: It seems to me that if this is supposed to
     be an ID, it ought to be an ID. This is used by the $id
     builtin entity function. You shoud also be able to
     compare. So if there's an ID name, use it, otherwise,
     use the ID. */
  if (s->id_name) {
    char *buf = calloc(strlen(s->id_name) + 3, sizeof(char));

    sprintf(buf, "[%s]", s->id_name);
    Gal_SetFrameName(s->properties, buf);
    free(buf);
  } else {
    char buf[128];
    
    sprintf(buf, "[%d]", s->id);
    Gal_SetFrameName(s->properties, buf);
  }
}

void GalHUB_ModifyServiceProvider(SERVICE_PROVIDER *s, char *hostname, int port)
{
  
  if (hostname) {
    if (s->host)
      free(s->host);
    s->host = _gal_strdup(hostname);
  }
  if (port != -1) {
    s->port = port;
  }
  _GalHUB_RecalculateSPName(s);
}


SERVICE_TYPE *GalHUB_NewServiceType(char *name, int listener_port)
{
  SERVICE_TYPE *s = (SERVICE_TYPE *)calloc(1,sizeof(SERVICE_TYPE));

  if (s)
  {
    s->name = _gal_strdup(name);
    s->listener_port = listener_port;
    s->providers = Gal_MakePointerBuffer(NULL, GAL_SERVICE_PROVIDER_PTYPE,
					 0, 0, 1, 1,
					 NULL, 10, 0);

    return(s);
  }
  GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't allocate memory for service type");
  return(NULL);
}

SERVICE_TYPE *GalHUB_FindServiceType(HUB *h, char *name)
{
  int num_stypes = Gal_PointerBufferSize(h->stypes);
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(h->stypes);
  int i;

  if (!name) return (SERVICE_TYPE *) NULL;
  
  for (i = 0; i < num_stypes; i++) {
    if (!strcmp(stypes[i]->name, name))
      return stypes[i];
  }
  return (SERVICE_TYPE *) NULL;
}

void GalHUB_SetHubActiveServers(HUB *hub, Gal_PointerBuffer *active_servers)
{
  hub->active_servers = active_servers;
}

extern void _GalSS_InitializeProviderSpec(GalSS_ProviderSpec *spec);

int GalHUB_SpecFoundInActiveServers(GalSS_ProviderSpec *test_spec,
				    Gal_PointerBuffer *active_servers)
{
  int j;
  GalSS_ProviderSpec **active_server_specs = (GalSS_ProviderSpec **) Gal_PointerBufferPointers(active_servers);
  int num_server_specs = Gal_PointerBufferSize(active_servers);
    
  for (j = 0; j < num_server_specs; j++) {
    if (GalHUB_ProviderSpecsMatch(active_server_specs[j],
				  test_spec, 0)) {
      /* The service type is present. */
      return 1;
    }
  }
  return 0;
}

int GalHUB_StypeFoundInActiveServers(char *stype_name,
				     Gal_PointerBuffer *active_servers)
{
  GalSS_ProviderSpec test_spec;
  _GalSS_InitializeProviderSpec(&test_spec);
  test_spec.stype_name = stype_name;
  return GalHUB_SpecFoundInActiveServers(&test_spec, active_servers);
}

int GalHUB_ProviderFoundInActiveServers(SERVICE_PROVIDER *p,
					SERVICE_TYPE *stype,
					Gal_PointerBuffer *active_servers)
{
  GalSS_ProviderSpec test_spec;
  _GalSS_InitializeProviderSpec(&test_spec);
  
  if (p->id > -1)
    test_spec.id = p->id;
  test_spec.host = p->host;
  test_spec.port = p->port;
  if (p->id_name)
    test_spec.id_name = p->id_name;

  test_spec.stype_name = stype->name;
  return GalHUB_SpecFoundInActiveServers(&test_spec, active_servers);
}

void GalHUB_SetHubDefaultDomain(HUB *hub, char *domain, char *domain_key)
{
  if (hub->default_domain_key) {
    free(hub->default_domain_key);
  }  
  hub->default_domain_key = _gal_strdup(domain_key);
  Gal_SetProp(hub->globals, domain_key, Gal_StringObject(domain));
}

Gal_Object GalHUB_GetHubDefaultDomain(HUB *hub)
{
  if (!hub->default_domain_key)
    return (Gal_Object) NULL;
  return Gal_GetObject(hub->globals, hub->default_domain_key);
}

void GalHUB_SetHubSessionId(HUB *hub, char *session_id, char *session_id_key)
{
  hub->session_id_key = _gal_strdup(session_id_key);
  Gal_SetProp(hub->globals, session_id_key, Gal_StringObject(session_id));
}

Gal_Object GalHUB_GetHubSessionId(HUB *hub)
{
  if (!hub->session_id_key)
    return (Gal_Object) NULL;
  return Gal_GetObject(hub->globals, hub->session_id_key);
}

void GalHUB_SetHubUserID(HUB *hub, char *user_id, char *user_id_key)
{
  if (hub->user_id_key) {
    Gal_DelProp(hub->globals, user_id_key);
    free(hub->user_id_key);
  }
  hub->user_id_key = _gal_strdup(user_id_key);
  Gal_SetProp(hub->globals, user_id_key, Gal_StringObject(user_id));
}

Gal_Object GalHUB_GetHubUserID(HUB *hub)
{
  if (!hub->user_id_key)
    return (Gal_Object) NULL;
  return Gal_GetObject(hub->globals, hub->user_id_key);
}

void GalHUB_SetHubLogTopDataDir(HUB *hub, char *dir, char *dir_key)
{
  hub->data_topdir_key = _gal_strdup(dir_key);
  Gal_SetProp(hub->globals, dir_key, Gal_StringObject(dir));
}

Gal_Object GalHUB_GetHubLogTopDataDir(HUB *hub)
{
  if (!hub->data_topdir_key)
    return (Gal_Object) NULL;
  return Gal_GetObject(hub->globals, hub->data_topdir_key);
}

/* And now, a general function. */

void GalHUB_SetHubGlobal(HUB *hub, char *key, Gal_Object value)
{
  Gal_SetProp(hub->globals, key, value);
}

Gal_Object GalHUB_GetHubGlobal(HUB *hub, char *key)
{
  return Gal_GetObject(hub->globals, key);
}

void GalHUB_DeleteHubGlobal(HUB *hub, char *key)
{
  Gal_DelProp(hub->globals, key);
}

int GalHUB_GetNumServers(HUB *hub) {
  return Gal_PointerBufferSize(hub->servers);
}

void GalHUB_SetHubInitState(HUB *hub, Gal_Frame init_state) {
  hub->init_state = init_state; 
}

void GalHUB_SetHubInitialKeys(HUB *hub, KeyPair **init_keys) {
  hub->init_keys = init_keys;
}

void GalHUB_SetHubToken(HUB *hub, TOKEN *t) {
  hub->token = t;
}

/* returns the index into the list; -1 if not found or if no list. */
int 
list_member(char *name, char **list)
{ int i;	
  char *its_name;
  if (!list) return(-1);
  for (i=0;(its_name = list[i]);i++)
  { if (Gal_StringEq(its_name, name)) return(i);
  }
  return(-1);
}

/* In some cases, the server is an actual server, in which case
   we need to start up a server, not connect to a remote server. */

void _GalHUB_CopyInitKeys(Gal_Frame fr, KeyPair **init_kps, int override)
{
  int i = 0;
  KeyPair *kp;
  
  if (init_kps) {
    while ((kp = init_kps[i++]))
      if (override || (!Gal_GetObject(fr, kp->key)))
	Gal_SetProp(fr, kp->key, Gal_CopyObject(kp->value));
  }
}

/* SAM 10/15/99: Writing a parser to segment complex server names. 
   [id]server@host:port. Both host_name and the return value are
   malloc'ed. 6/23/00: This will also work for just [id]server. */

/* SAM 11/19/00: Don't surgically alter the server string; it's
   not thread safe. */

/* We split into provider name and location, and then turn them
   into a GalSS_ProviderSpec. Then we canonicalize the Builtin server
   name. */

extern GalSS_ProviderSpec *
_GalSS_PopulateProviderSpec(char *server_chunk,
			    char *location_chunk,
			    GalSS_ProviderSpec *existing_spec);

/* If permit_location is 1, and there's a colon but no @, treat
   it as a bare location. */

GalSS_ProviderSpec *GalHUB_SplitServerAndLocation(char *server_string,
						  int permit_location)
{
  char *new_server_string = (char *) NULL;
  char *cp = (char *) NULL;
  GalSS_ProviderSpec *new_spec = (GalSS_ProviderSpec *) NULL;
  
  if (server_string) {
    cp = strchr(server_string, '@');
    if (cp) {
      /* Copy the first part of the string. */
      new_server_string = calloc((cp - server_string) + 1, 1);
      strncpy(new_server_string, server_string, cp - server_string);
      /* Skip the character. */
      cp = cp + 1;
    } else if (permit_location && strchr(server_string, ':')) {
      cp = server_string;      
    } else {
      new_server_string = server_string;
    }
    new_spec = _GalSS_PopulateProviderSpec(new_server_string, cp,
					   (GalSS_ProviderSpec *) NULL);
    if (new_server_string && (new_server_string != server_string))
      free(new_server_string);
    /* Canonicalize the Builtin server. */
    if (new_spec && new_spec->stype_name &&
	!_gal_strcasecmp(new_spec->stype_name, BUILTIN_SERVER)) {
      free(new_spec->stype_name);
      new_spec->stype_name = _gal_strdup(BUILTIN_SERVER);
    }
  }
  return new_spec;
}

GalSS_ProviderSpec *GalHUB_SplitServerName(char *server_string)
{
  return GalHUB_SplitServerAndLocation(server_string, 0);
}


/* Throughout these matching functions, the reference_spec
   is the specification in the hash table, and the spec candidate
   is the specification which is under consideration for matching. */

static int
__GalHUB_ProviderSpecsIdentical(GalSS_ProviderSpec *reference_spec,
				GalSS_ProviderSpec *spec_candidate)
{
  if (!spec_candidate) {
    if (reference_spec->id == -1 &&
	(!reference_spec->id_name) &&
	(!reference_spec->stype_name)) {
      /* We've got a match. */
      return 1;
    } else {
      return 0;
    }
  } else {
    return ((reference_spec->id == spec_candidate->id) &&
	    (((!reference_spec->id_name) && (!spec_candidate->id_name)) ||
	     Gal_StringEq(reference_spec->id_name,
			  spec_candidate->id_name)) &&
	    (((!reference_spec->stype_name) &&
	      (!spec_candidate->stype_name)) ||
	     Gal_StringEq(reference_spec->stype_name,
			  spec_candidate->stype_name)) &&
	    (((!reference_spec->host) &&
	      (!spec_candidate->host)) ||
	     Gal_StringEq(reference_spec->host,
			  spec_candidate->host)) &&
	    (reference_spec->port == spec_candidate->port));
  }
}

static int
__GalHUB_ProviderSpecsSubsume(GalSS_ProviderSpec *reference_spec,
			      GalSS_ProviderSpec *spec_candidate)
{
  if ((reference_spec->id > -1) &&
      (!spec_candidate || (reference_spec->id != spec_candidate->id)))
    return 0;
  if ((reference_spec->port > -1) &&
      (!spec_candidate || (reference_spec->port != spec_candidate->port)))
    return 0;
  if (reference_spec->host &&
      (!spec_candidate ||
       !(Gal_StringEq(reference_spec->host, spec_candidate->host))))
    return 0;
  if (reference_spec->id_name &&
      (!spec_candidate ||
       !(Gal_StringEq(reference_spec->id_name, spec_candidate->id_name))))
    return 0;
  if (reference_spec->stype_name &&
      (!spec_candidate ||
       !(Gal_StringEq(reference_spec->stype_name,
		      spec_candidate->stype_name))))
    return 0;
  /* If the msg_spec satisfies all the requirements in the
     spec (including the case where there are no requirements
     in the spec), the match succeeds. */
  return 1;
}

int GalHUB_ProviderSpecsMatch(GalSS_ProviderSpec *reference_spec,
			      GalSS_ProviderSpec *spec_candidate,
			      int literal_match)
{
  if (literal_match) {
    return __GalHUB_ProviderSpecsIdentical(reference_spec,
					   spec_candidate);
  } else {
    return __GalHUB_ProviderSpecsSubsume(reference_spec,
					 spec_candidate);
  }
}

void GalHUB_InstantiateLogRecord(HUB *h)
{
  h->log_record = (LOGRECORD *) calloc(1, sizeof(LOGRECORD));
  h->log_record->msg_ht = Gal_MakeHash(MAX_LOG_MESSAGES);
}

/* When we free the hash table here, we must make sure to free
   the LOGKEYS structure. */

void __free_log_key(char *str, Gal_Object val, Gal_HashTable hp)
{
  /* We'll set the hash value to NULL, which will free
     the val. However, we also want to make sure we
     free the contents, which will not be freed by Gal_FreeObject. */
  if (val) {
    LOGKEYS **keys = (LOGKEYS **) Gal_PointerValue(val);
    int i;
    for (i = 0; keys[i]; i++) {
      free_entity_pairs(keys[i]->in);
      free_entity_pairs(keys[i]->out);
      if (keys[i]->spec)
	GalSS_FreeProviderSpec(keys[i]->spec);
      free(keys[i]);
    }
    free(keys);
    Gal_SetHash(str, (Gal_Object) NULL, hp);
  }
}

void GalHUB_FreeLogRecord(HUB *h)
{
  if (h->log_record) {
    if (h->log_record->msg_ht) {
      Gal_MapHash(h->log_record->msg_ht, __free_log_key);
      Gal_FreeHash(h->log_record->msg_ht);
    }
    if (h->log_record->user_version)
      free(h->log_record->user_version);
    free(h->log_record);
    h->log_record = (LOGRECORD *) NULL;
  }
}

/* For timestamping and logging keys for
   the incoming and outgoing messages. I'm pairing keys and
   specifications, because messages work slightly differently than
   programs. I need to worry about duplicate provider descriptions,
   just in case. If I timestamp something, and then later try
   to add in/out, that's ok if there's no in/out already. Ditto if
   I add in/out, and then try to timestamp that entry (that's a no-op).
   But overwriting in/out is just right out. */

void GalHUB_AddMessageToTimestampTable(HUB *h, char *msg_name,
				       EntityPair **in, EntityPair **out,
				       GalSS_ProviderSpec *provider_conds,
				       int literal_match,
				       int edges_only,
				       int from_script)
{
  LOGKEYS **keys = (LOGKEYS **) Gal_PointerValue(Gal_GetHash(msg_name, h->log_record->msg_ht));
  int insertion_index = 0;

  /* Add a new one if there's none there. */

  if (!keys) {
    keys = (LOGKEYS **) calloc(2, sizeof(LOGKEYS *));
  } else {
    /* Check them. If you don't find a matching element, add a new one.
       If there's no provider condition, match with empty. */
    LOGKEYS *found_spec = (LOGKEYS *) NULL;
    
    for (insertion_index = 0; keys[insertion_index]; insertion_index++) {
      if (__GalHUB_ProviderSpecsIdentical(keys[insertion_index]->spec,
					  provider_conds) &&
	  (keys[insertion_index]->literal_match == literal_match) &&
	  (keys[insertion_index]->edges_only == edges_only) &&
	  (keys[insertion_index]->from_script == from_script)) {
	found_spec = keys[insertion_index];
	break;
      }
    }

    /* If we've found an exact match, make sure we're not overwriting.
       Then do the update and return. Make sure you free the provider_conds
       you're not using. */
    
    if (found_spec) {
      if (in || out) {
	/* If there aren't in or out, things are already recorded, and
	   it's a no-op. */
	if (found_spec->in || found_spec->out) {
	  /* Warn and abort. */
	  GalUtil_Warn("Trying to overwrite existing LOG_IN: and/or LOG_OUT: for PROGRAM: or MESSAGE:; ignoring.");
	} else {
	  /* The element is already present; record the in/out and exit. */
	  found_spec->in = copy_entity_pairs(in);
	  found_spec->out = copy_entity_pairs(out);
	}
      }
      if (provider_conds)
	GalSS_FreeProviderSpec(provider_conds);
      return;      
    }

    /* If we're here, we didn't find anything new, so we need to make
       a new one. */

    keys = (LOGKEYS **) realloc(keys, ((2 + insertion_index) *
				       sizeof(LOGKEYS *)));
    keys[insertion_index + 1] = (LOGKEYS *) NULL;
  }

  /* Create and populate the new entry. */
  
  keys[insertion_index] = (LOGKEYS *) calloc(1, sizeof(LOGKEYS));  
  keys[insertion_index]->in = copy_entity_pairs(in);
  keys[insertion_index]->out = copy_entity_pairs(out);
  keys[insertion_index]->literal_match = literal_match;
  keys[insertion_index]->edges_only = edges_only;
  keys[insertion_index]->from_script = from_script;
  if (!provider_conds) {
    keys[insertion_index]->spec = (GalSS_ProviderSpec *) calloc(1, sizeof(GalSS_ProviderSpec));
    _GalSS_InitializeProviderSpec(keys[insertion_index]->spec);
  } else {
    keys[insertion_index]->spec = provider_conds;
  }

  /* Update the keys. */
  
  Gal_SetHash(msg_name, Gal_PointerObject((void *) keys),
	      h->log_record->msg_ht);
}

/* Memory utilities. These must be here because this file is
   compiled into verify_program, and the others aren't. */

/* This really frees, so only use it when you're serious. */

void free_key_pairs(KeyPair **old_pairs)
{
  int i;

  if (old_pairs) {
    for (i = 0; old_pairs[i]; i++) {
      if (old_pairs[i]->key)
	free(old_pairs[i]->key);
      if (old_pairs[i]->value)
	Gal_FreeObject(old_pairs[i]->value);
      free(old_pairs[i]);
    }
    free(old_pairs);
  }
}

static void free_entity_pair(EntityPair *p)
{
  if (p->target) {
    Gal_FreeProgramEntity(p->target);
  }
  if (p->source) {
    Gal_FreeProgramEntity(p->source);
  }
  free(p);
}

void free_entity_pairs(EntityPair **ps)
{
  int i;

  if (ps) {
    for (i = 0; ps[i]; i++) {
      free_entity_pair(ps[i]);
    }
    free(ps);
  }
}

EntityPair **copy_entity_pairs(EntityPair **pairs)
{
  int i, j;
  EntityPair **new_pairs;
  
  if (!pairs)
    return (EntityPair **) NULL;

  /* Figure out the length. */
  for (i = 0; pairs[i]; i++);

  new_pairs = (EntityPair **) calloc(i + 1, sizeof(EntityPair *));
  for (j = 0; j < i; j++) {
    new_pairs[j] = (EntityPair *) calloc(1, sizeof(EntityPair));
    new_pairs[j]->tag = pairs[j]->tag;
    new_pairs[j]->target = Gal_CopyProgramEntity(pairs[j]->target);
    new_pairs[j]->source = Gal_CopyProgramEntity(pairs[j]->source);
  }
  return new_pairs;  
}
