/*
  Portions of this file (c) Copyright 1999 - 2000 M.I.T.
  Portions of this file (c) Copyright 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include "galaxy/galaxy_all.h"
#include "hub.h"
#include "hub_program.h"
#include "hub_internal.h"

#define DEFAULT_USER_ID "sls"
#define DEFAULT_PARA_LANG "english"
#define DEFAULT_OUT_LANG "english"
#define DEFAULT_KV_LANG "dialogue"
#define DEFAULT_SYNTH_LANG "dectalk"
#define DEFAULT_MODE "singlethread"

typedef struct control_file_parse_state {
  /* for use while reading */
  PROGRAM *current_program;
  PROGRAM *current_message;
  Gal_FileStruct *current_files;
  Gal_HashTable operations_hash;
  int operations_hashed;
  int control_flags;
  Gal_PointerBuffer *active_stypes;
  HUB *hub;
  int be_pedantic_about_messages;
  /* Duplicate pointers to Hub caches */
  Gal_PointerBuffer *stypes;
  Gal_PointerBuffer *servers;
  /* Internal result of parsing file */
  HubControlStruct *hc;
} ControlFileParseState;

#define GAL_TAG(x,y) {x, y},
static Gal_TagMap HubProgramTagMap[] =
{
#include "hub_program_tags.h"
  {NULL, 0}
};

static Gal_TagMap HubErrorTagMap[] =
{
#include "hub_error_tags.h"
  {NULL, 0}
};
#undef GAL_TAG

/* These are for namespaces. */

/* It's important when you construct the namespace
   array of frames that they be loaded in enum order.
   There's also no checking if the frame array is
   large enough; make sure it is. */

static int HubTagsInitialized = 0;

static PROGRAM *add_program(ProgramObject *obj, ControlFileParseState *control, int *error);
static PROGRAM *add_message(ProgramObject *obj, ControlFileParseState *control, int *error);
static RULE *add_rule(PROGRAM *program, int extended_syntax, int *error);
static char *program_string(ProgramObject *obj, int for_saving, int *error);
static int program_int(ProgramObject *obj, int *error);
static Gal_Frame program_frame(ProgramObject *obj, int *error);
static KeyPair **program_alarm(ProgramObject *values, int *error);
static char **program_keys(ProgramObject *values, int *count, int *error);
static KeyPair **program_key_pairs(ProgramObject *values, int *error);
static char **program_strings(ProgramObject *values, int *count, int *error);
static void program_location(int tag, ProgramObject *obj,
			     ControlFileParseState *control,
			     SERVICE_TYPE *stype,
			     SERVICE_PROVIDER **provider_ptr, int *error);
static void program_condition(ProgramObject *values, RULE *rule, ControlFileParseState *control, int *error_ptr, Gal_ProgramParser *pp);
static int validate_operation(Gal_ConditionStruct *condition, ControlFileParseState *control, int *error_ptr);
static void hash_operations(ControlFileParseState *control, int *errorp);
static void hash_global_operations(ControlFileParseState *control, SERVICE_TYPE *s);
static void read_hub_control(ControlFileParseState *control, char *directory, int *error_ptr, int already_extended);
static void read_included_file(char *filename, char *directory, ControlFileParseState *control, int *error_ptr, int already_extended);
static char *program_string_value(Gal_Object obj);
static char *fetch_program_string(ProgramObject *p, int for_saving, int *error);
static char **copy_string_array(char **old_strings);
static void
maybe_discard_current_service_provider(ControlFileParseState *control,
				       SERVICE_PROVIDER *p,
				       SERVICE_TYPE *current_server,
				       int *error);
static void free_program(PROGRAM *prog);

/* initialize a new parse state */

static ControlFileParseState *new_ControlFileParseState(HUB *h, int control_flags)
{
  ControlFileParseState *c = (ControlFileParseState *) calloc(1, sizeof(ControlFileParseState));

  c->operations_hash = Gal_MakeHash(10000);
  c->control_flags = control_flags;
  c->hub = h;
  c->hc = (HubControlStruct *) calloc(1, sizeof(HubControlStruct));

  c->hc->mode = _gal_strdup(DEFAULT_MODE);
  c->stypes = Gal_MakePointerBuffer(NULL, GAL_SERVICE_TYPE_PTYPE,
				    0, 0, 1, 1,
				    NULL, 10, 0);
  c->servers = Gal_MakePointerBuffer(NULL, GAL_SERVICE_PROVIDER_PTYPE,
				     0, 0, 1, 1,
				     NULL, 10, 0);
  GalHUB_SetHubServices(h, c->servers, c->stypes);
  c->hc->user_id_key = DEFAULT_USER_ID_KEY;
  GalHUB_SetHubUserID(h, DEFAULT_USER_ID, DEFAULT_USER_ID_KEY);

  /* Set the globals. */
  c->hc->kv_lang_key = DEFAULT_KV_LANG_KEY;
  GalHUB_SetHubGlobal(h, DEFAULT_KV_LANG_KEY,
		      Gal_StringObject(DEFAULT_KV_LANG));
  c->hc->para_lang_key = DEFAULT_PARA_LANG_KEY;
  GalHUB_SetHubGlobal(h, DEFAULT_PARA_LANG_KEY,
		      Gal_StringObject(DEFAULT_PARA_LANG));
  c->hc->synth_lang_key = DEFAULT_SYNTH_LANG_KEY;
  GalHUB_SetHubGlobal(h, DEFAULT_SYNTH_LANG_KEY,
		      Gal_StringObject(DEFAULT_SYNTH_LANG));
  c->hc->out_lang_key = DEFAULT_OUT_LANG_KEY;
  GalHUB_SetHubGlobal(h, DEFAULT_OUT_LANG_KEY,
		      Gal_StringObject(DEFAULT_OUT_LANG));
  return c;
}

static void free_list(char **str_list)
{
  if (str_list) {
    int i;
    for (i = 0; str_list[i]; i++)
      free(str_list[i]);
    free(str_list);
  }
}

void free_HubControlStruct(HubControlStruct *hc)
{
  int i;
  
  free(hc->mode);
  free_list(hc->operations_to_ignore);
  free_list(hc->timestamps);
  if (hc->messages) {
    for(i=0; hc->messages[i]; i++) {
      free_program(hc->messages[i]);
    }
    free(hc->messages);
  }
  free(hc);
}

static void free_ControlFileParseState(ControlFileParseState *c)
{
  Gal_FreeHash(c->operations_hash);
  if (c->active_stypes)
    Gal_FreePointerBuffer(c->active_stypes);
  free(c);
}

/* add a server to the hub's list of servers */

static SERVICE_TYPE *add_server_struct(ControlFileParseState *control,
				       char *server_name,
				       int *errorp)
{
  SERVICE_TYPE *stype = GalHUB_NewServiceType(server_name, -1);
  
  if (stype) {
    Gal_PointerBufferAdd(control->stypes, (void *) stype);
    return stype;
  } else {
    *errorp = GAL_ALLOCATION_FAILED;
    return (SERVICE_TYPE *) NULL;
  }
}

/* SERVICE_TYPE: is like SERVER:, but many fewer things can happen.
   The difference is controlled by whether current_provider_ptr is set. */

static SERVICE_PROVIDER *update_service_provider(SERVICE_PROVIDER *s,
						 char *hostname, int port,
						 char *id_name,
						 HUB *h, SERVICE_TYPE *stype)
{
  if (!s) {
    s = GalHUB_NewServiceProvider(hostname, port, stype);
  } else if (s->host && (s->port != -1)) {
    /* We're full, record the existing one, start over. */
    GalHUB_RecordServiceProvider(h, s);
    s = GalHUB_NewServiceProvider(hostname, port, stype);
  } else {
    if (s->host && hostname) {
      GalUtil_WarnWithLocation(__FUNCTION__,
			       "Overwriting host location %s (server %s)",
			       s->host, stype->name);
    }
    if ((s->port != -1) && (port != -1)) {
      GalUtil_WarnWithLocation(__FUNCTION__,
			       "Overwriting port location %d (server %s)",
			       s->port, stype->name);
    }
    GalHUB_ModifyServiceProvider(s, hostname, port);
  }
  if (id_name) {
    if (s->id_name) {
      free(s->id_name);
    }
    s->id_name = _gal_strdup(id_name);
  }
  return s;
}

static SERVICE_TYPE *add_service_type(ProgramObject *obj,
				      ControlFileParseState *control,
				      SERVICE_PROVIDER **current_provider_ptr,
				      int *error)
{  
  if (control) {
    /* fetch_program_string doesn't increment, so the code which
       frees the remainder of the line will cover this. */
    char *server_string = fetch_program_string(obj, 0, error);
    GalSS_ProviderSpec *spec;
    SERVICE_TYPE *server;

    /* SAM 10/15/99: Check server@host:port. It shouldn't happen. */
    spec = GalHUB_SplitServerName(server_string);

    if (spec == NULL) {
      *error = GAL_NO_SERVER_NAME;
      return NULL;
    }

    if (!current_provider_ptr) {
      if (spec->host || (spec->port != -1)) {
	*error = GAL_BAD_SERVICE_TYPE_NAME;
	GalSS_FreeProviderSpec(spec);
	return NULL;
      }
    }

    /* There must be an stype_name, and the ID can't be an integer. */
    if (!(spec->stype_name) || (spec->id != -1)) {
      *error = GAL_BAD_SERVICE_TYPE_NAME;
      GalSS_FreeProviderSpec(spec);
      return NULL;
    }

    if ((server = GalHUB_FindServiceType(control->hub, spec->stype_name))) {
      if (current_provider_ptr) {
	GalUtil_PInfo1WithLocation(__FUNCTION__, "Server %s already exists, will append locations.\n", server->name);
	/* Force a new location. */
	if (*current_provider_ptr) {
	  maybe_discard_current_service_provider(control,
						 *current_provider_ptr,
						 server, error);
	}
	*current_provider_ptr = (SERVICE_PROVIDER *) NULL;
	if (*error == GAL_NO_ERROR) {
	  *current_provider_ptr = update_service_provider(*current_provider_ptr,
							  spec->host,
							  spec->port,
							  spec->id_name,
							  control->hub, server);
	  GalSS_FreeProviderSpec(spec);
	  return server;
	} else {
	  GalSS_FreeProviderSpec(spec);
	  return NULL;
	}
      } else {
	*error = GAL_DUPLICATE_SERVICE_TYPE_NAME;
	GalSS_FreeProviderSpec(spec);
	return NULL;
      }
    }

    server = add_server_struct(control, spec->stype_name, error);
    if (server) {
      if (current_provider_ptr) {
	/* Force a new location. */
	if (*current_provider_ptr) {
	  maybe_discard_current_service_provider(control,
						 *current_provider_ptr,
						 server, error);
	}
	*current_provider_ptr = (SERVICE_PROVIDER *) NULL;
	if (*error == GAL_NO_ERROR) {
	  *current_provider_ptr = update_service_provider(*current_provider_ptr,
							  spec->host,
							  spec->port,
							  spec->id_name,
							  control->hub, server);
	} else {
	  GalSS_FreeProviderSpec(spec);
	  return NULL;
	}
      }
      GalSS_FreeProviderSpec(spec);
      return server;
    }
  } else {
    *error = GAL_NO_HUB;
  }
  return(NULL);
}


/* The builtin server is always active, and it always has
   all its operations. In particular, the local server is already
   initialized. */

/* Load local-server.h to get the names of all the builtin functions. */

#define GAL_SERVER_OP_SIGNATURE(__op__, __in_array__, __other_in_keys__, __reply__, __out_array__, __other_out_keys__) #__op__,
#define GAL_SERVER_OP(__op__) #__op__,

static char *BuiltinOperations[] = {
#include "local-server.h"
  NULL};
#undef GAL_SERVER_OP_SIGNATURE
#undef GAL_SERVER_OP

extern 
void _GalSS_InitializeProviderSpec(GalSS_ProviderSpec *spec);

static void add_builtin_server(ControlFileParseState *control, int *errorp)
{
  SERVICE_TYPE *server = GalHUB_FindServiceType(control->hub, BUILTIN_SERVER);
  int i = 0;
  
  if (server) {
    /* Already been declared; make sure all the operations are
       there. */
    if (server->operations) {
      for (i = 0; BuiltinOperations[i]; i++) {
	if (list_member(BuiltinOperations[i], server->operations) == -1) {
	  /* Add an operation. Eep. */
	  int j;
	  if (!server->operations) {
	    server->operations = (char **) calloc(2, sizeof(char *));
	    j = 0;
	  } else {
	    for (j = 0; server->operations[j]; j++);
	    server->operations = (char **) realloc(server->operations,
						   (j + 2) * sizeof(char *));
	    server->operations[j + 1] = (char *) NULL;
	  }
	  server->operations[j] = _gal_strdup(BuiltinOperations[i]);
	}
      }
    }
  } else {
    server = add_server_struct(control, BUILTIN_SERVER, errorp);
  }
  
  if (*errorp)
    return;
    
  if (server && (!server->operations)) {
    /* Make sure all the operations are there. */
    server->operations = copy_string_array(BuiltinOperations);
  }

  if (server && control->hc->active_servers) {
    if (!GalHUB_StypeFoundInActiveServers(BUILTIN_SERVER,
					  control->hc->active_servers)) {
      GalSS_ProviderSpec *new_spec = (GalSS_ProviderSpec *) calloc(1, sizeof(GalSS_ProviderSpec));
      _GalSS_InitializeProviderSpec(new_spec);
      new_spec->stype_name = _gal_strdup(BUILTIN_SERVER);

      Gal_PointerBufferAdd(control->hc->active_servers, (void *) new_spec);
    }
  } 
}

static PROGRAM *add_message(ProgramObject *obj,
			    ControlFileParseState *c,
			    int *error)
{
  PROGRAM *p;
  int i = 0;
  
  if ((!c->hub->stypes) || (Gal_PointerBufferSize(c->hub->stypes) == 0)) {
    if (!(c->control_flags & GAL_PROGRAM_VERIFY)) {
      *error = GAL_NO_SERVERS;
      return(NULL);
    } else {
      GalUtil_WarnWithLocation(__FUNCTION__, "Defining messages without defining any servers");
    }
  }

  p = (PROGRAM *) calloc(1, sizeof(PROGRAM));

  if (p) {
    if (obj->index < obj->size) {
      p->name = fetch_program_string(obj, 1, error);
      if (*error == GAL_NO_ERROR) {
	/* Make room for it. */	
	if (!c->hc->messages) {
	  c->hc->messages = (PROGRAM **) calloc(2, sizeof(PROGRAM *));
	  i = 0;
	} else {
	  /* Add one. */
	  for (i = 0; c->hc->messages[i]; i++);
	  c->hc->messages = (PROGRAM **) realloc(c->hc->messages,
					     sizeof(PROGRAM *) * (2 + i));
	  /* Set the last to NULL; we'll put the new program in the
	     next-to-last one. */
	  c->hc->messages[i + 1] = (PROGRAM *) NULL;
	}
	/* Store it. */
	c->hc->messages[i] = p;
	return p;
      } else {
	free(p);
      }
    } else {
      free(p);
      *error = GAL_NO_MESSAGE_NAME;
    }
  } else {
    *error = GAL_ALLOCATION_FAILED;
  }
  return (PROGRAM *) NULL;
}

/* add a program to the hub's list of programs */

static PROGRAM *
add_program(ProgramObject *obj, ControlFileParseState *c, int *error)
{
  int i = 0;
  PROGRAM *p;


  if ((!c->hub->stypes) || (Gal_PointerBufferSize(c->hub->stypes) == 0)) {
    if (!(c->control_flags & GAL_PROGRAM_VERIFY)) {
      *error = GAL_NO_SERVERS;
      return(NULL);
    } else {
      GalUtil_WarnWithLocation(__FUNCTION__, "Defining program without defining any servers");
    }
  }
  
  
  p = (PROGRAM *) calloc(1, sizeof(PROGRAM));

  if (p) {
    if (obj && (obj->index < obj->size))
      p->name = fetch_program_string(obj, 1, error);
    else
      p->name = _gal_strdup("main");
    if (*error == GAL_NO_ERROR) {
      /* Make room for it. */
      if (!c->hc->programs) {
	c->hc->programs = (PROGRAM **) calloc(2, sizeof(PROGRAM *));
	i = 0;
      } else {
	/* Add one. */
	for (i = 0; c->hc->programs[i]; i++);
	c->hc->programs = (PROGRAM **) realloc(c->hc->programs, sizeof(PROGRAM *) * (2 + i));
	/* Set the last to NULL; we'll put the new program in the
	   next-to-last one. */
	c->hc->programs[i + 1] = (PROGRAM *) NULL;
      }
      /* Store it. */
      c->hc->programs[i] = p;
      return p;
    } else {
      free(p->name);
      free(p);
    }
  } else {
    *error = GAL_ALLOCATION_FAILED;
  }

  return (PROGRAM *) NULL;
}

/* add a rule to the current program */

static RULE **expand_rule_buffer(RULE **old_buffer, int num)
{
  if (!old_buffer) {
    return (RULE **) calloc(num + 1, sizeof(RULE *));
  } else {
    int i, j;
    RULE **buffer;
    
    for (i = 0; old_buffer[i]; i++);
    j = num + 1 + i;
    buffer = realloc(old_buffer, sizeof(RULE *) * j);
    for (i++; i < j; i++)
      buffer[i] = (RULE *) NULL;
    return buffer;
  }
}

static RULE *add_rule(PROGRAM *program, int extended_syntax, int *error)
{  
  if (program) {
    RULE *r = (RULE *) calloc(1, sizeof(RULE));

    if (r) {
      /* rule indices aren't set until the rules
	 are folded into the program in the finalization. */
      /* r->ridx = rule_index; */
      r->extended_syntax = extended_syntax;
      return r;
    } else {
      *error = GAL_ALLOCATION_FAILED;
    }
  } else {
    *error = GAL_NO_PROGRAM;
  }
  return (RULE *) NULL;
}

/* Each RULE: block can have multiple conditions. We explode
   this into separate rules. So we need a bunch of copy functions. */

static KeyPair **copy_key_pairs(KeyPair **old_pairs)
{
  KeyPair **kp_list;
  int i, j;

  if (!old_pairs)
    return (KeyPair **) NULL;

  /* Figure out the length. */
  for (i = 0; old_pairs[i]; i++);

  kp_list = (KeyPair **) calloc(i + 1, sizeof(KeyPair *));

  for (j = 0; j < i; j++) {
    kp_list[j] = (KeyPair *) calloc(1, sizeof(KeyPair));
    kp_list[j]->key = _gal_strdup(old_pairs[j]->key);    
    kp_list[j]->tag = old_pairs[j]->tag;
    kp_list[j]->value = Gal_CopyObject(old_pairs[j]->value);
  }
  return kp_list;
}

static char **copy_string_array(char **old_strings)
{
  char **new_strings;
  int i, j;
  
  if (!old_strings)
    return (char **) NULL;

  for (i = 0; old_strings[i]; i++);

  new_strings = (char **) calloc(i + 1, sizeof(char *));
  for (j = 0; j < i; j++) {
    new_strings[j] = _gal_strdup(old_strings[j]);
  }
  return new_strings;  
}

static Gal_ProgramEntity **copy_program_entities(Gal_ProgramEntity **old_e)
{
  Gal_ProgramEntity **new_e;
  int i, j;
  
  if (!old_e)
    return (Gal_ProgramEntity **) NULL;
  
  for (i = 0; old_e[i]; i++);
  
  new_e = (Gal_ProgramEntity **) calloc(i + 1, sizeof(Gal_ProgramEntity *));
  
  for (j = 0; j < i; j++) {
    new_e[j] = Gal_CopyProgramEntity(old_e[j]);
  }
  
  return new_e;
}

static Gal_Frame *copy_frames(Gal_Frame *old_frames)
{
  Gal_Frame *new_frames;
  int i, j;
  
  if (!old_frames)
    return (Gal_Frame *) NULL;

  for (i = 0; old_frames[i]; i++);
  
  new_frames = (Gal_Frame *) calloc(i + 1, sizeof(Gal_Frame));
  
  for (j = 0; j < i; j++) {
    new_frames[j] = Gal_CopyFrame(old_frames[j]);
  }
  
  return new_frames;
}

static RULE *copy_rule(PROGRAM *program, RULE *old_r, int *error)
{
  if (program) {
    RULE *r = (RULE *) calloc(1, sizeof(RULE));
    
    if (r) {
      /* rule indices aren't set until the rules
	 are folded into the program. */
      /* r->ridx = old_r->ridx + 1; */
      /* Now, copy over all the contents from the old rule.
	 Future operations on the rule block will apply to all
	 the rules. So we need to copy each and every piece, otherwise
	 when we apply to all the rules some things will happen twice, if
	 we're augmenting entries. Don't copy server_name, op_name, tests, ridx. */
      /* Integers. */
      r->extended_syntax = old_r->extended_syntax;
      r->lock_mask = old_r->lock_mask;
      r->lock_value = old_r->lock_value;
      r->control = old_r->control;
      /* Entity pairs. */
      r->in_kps = copy_entity_pairs(old_r->in_kps);
      r->in_log_kps = copy_entity_pairs(old_r->in_log_kps);
      r->out_kps = copy_entity_pairs(old_r->out_kps);
      r->error_kps = copy_entity_pairs(old_r->error_kps);
      r->catch_error = old_r->catch_error;
      r->out_log_kps = copy_entity_pairs(old_r->out_log_kps);
      r->param_kps = copy_entity_pairs(old_r->param_kps);
      r->set_kps = copy_entity_pairs(old_r->set_kps);
      /* Key pairs. */
      r->retrieve_kps = copy_key_pairs(old_r->retrieve_kps);
      r->alarm_kps = copy_key_pairs(old_r->alarm_kps);
      /* other stuff */
      r->store_vars = copy_string_array(old_r->store_vars);
      r->del_vars = copy_program_entities(old_r->del_vars);
      r->reply_continuations = copy_frames(old_r->reply_continuations);
      r->error_continuations = copy_frames(old_r->error_continuations);
      r->provider = Gal_CopyProgramEntity(old_r->provider);      
      /* Done copying. */
      return r;
    } else {
      *error = GAL_ALLOCATION_FAILED;
    }
  } else {
    *error = GAL_NO_PROGRAM;
  }
  return (RULE *) NULL;
}


/* read a string (return error if object is not a string) */

static char *program_string(ProgramObject *p, int for_saving, int *error)
{
  if ((p->size - p->index) > 1) {
    *error = GAL_ENCLOSE_IN_QUOTES;
    return (char *) NULL;
  } else {
    return fetch_program_string(p, for_saving, error);
  }
}

/* SAM 6/27/02: Gal_ProgramStringValue does no copying, so anyone
   who wants to save away the value and ensure that it has
   control over the result needs to copy somehow. */

static char *fetch_program_string(ProgramObject *p, int for_saving, int *error)
{
  if (Gal_ProgramStringp(p->values[p->index])) {
    char *val = Gal_ProgramStringValue(p->values[p->index]);

    /* If it's a string, the original string will be freed. If it's
       a symbol, the symbol will not be freed. This is the right thing. */
    if (for_saving) {
      val = _gal_strdup(val);
    }
    return val;
  } else {
    *error = GAL_NOT_STRING;
    return NULL;
  }
}

/* read an int (return error if object is not an int). Doesn't
   increment, although it could. */

static int program_int(ProgramObject *p, int *error)
{
  if (Gal_Intp(p->values[p->index]))
    return Gal_IntValue(p->values[p->index]);
  else
  {
    *error = GAL_NOT_INT;
    return 0;
  }
}

/* read a frame (return error if object is not a frame) */

static Gal_Frame program_frame(ProgramObject *obj, int *error)
{
  if (Gal_Framep(obj->values[obj->index]))
  {
    Gal_Frame fr = Gal_FrameValue(obj->values[obj->index]);
    obj->index++;
    return Gal_CopyFrame(fr);
  }
  else
  {
    *error = GAL_NOT_FRAME;
    return NULL;
  }
}

static Gal_Frame *add_continuation_frame(Gal_Frame *continuations,
					 Gal_Frame new_frame,
					 int *error)
{
  int i;

  /* If there's already an error, don't bother. */
  if (*error) {
    return continuations;
  }

  /* If you don't have a frame, don't bother. */
  if (!new_frame) {
    *error = GAL_NOT_FRAME;
    return continuations;
  }

  /* Otherwise, extend the continuation array. */  
  if (!continuations) {
    i = 0;
    continuations = (Gal_Frame *) calloc(2, sizeof(Gal_Frame));
  } else {
    for (i = 0; continuations[i]; i++);
    continuations = (Gal_Frame *) realloc(continuations, (i + 2) * sizeof(Gal_Frame));
    continuations[i + 1] = (Gal_Frame) NULL;
  }
  continuations[i] = new_frame;
  return continuations;
}

static KeyPair **program_alarm(ProgramObject *p, int *error)
{
  int num_values = p->size - p->index;
  
  if (num_values > 0)
  {
    KeyPair **kp_list = (KeyPair **)calloc(num_values + 1, sizeof(KeyPair *));
    KeyPair *kp;
    int num_pairs = 0;
    int i = 0;

    if (kp_list)
    {
      while (p->index < p->size) {
	Gal_Object val = p->values[p->index++];

	if ((kp = (KeyPair *)calloc(1, sizeof(KeyPair))))
	  kp_list[num_pairs++] = kp;
	else
	{
	  *error = GAL_ALLOCATION_FAILED;
	  break;
	}

	if (Gal_ProgramStringp(val))
	{
	  char *str = Gal_ProgramStringValue(val);
	  if (Gal_StringEq(str, "disable"))
	  {
	    kp->key = _gal_strdup(str);
	    break;
	  }
	  kp->key = _gal_strdup(str);
	  if (i<num_values)
	    kp->value = Gal_CopyObject(p->values[p->index++]);
	  else
	  {
	    *error = GAL_BAD_ALARM_LIST;
	    break;
	  }
	}
	else *error = GAL_BAD_ALARM_KEY;
	
      }
      if (*error != GAL_NO_ERROR) {
	free_key_pairs(kp_list);
	return (KeyPair **) NULL;
      } else {
	return kp_list;
      }
    }
    else *error = GAL_ALLOCATION_FAILED;
  }
  return NULL;
}

/* convert a ProgramObject array to a Gal_List of Gal_Keyword objects */

static void __Gal_KeywordWarn(char *fn, Gal_Object val,
			      Gal_StringBuffer **bufptr)
{
  Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;
  Gal_StringBuffer **local_bufptr = bufptr;
  
  if (!local_bufptr) {
    local_bufptr = &buf;
  }

  GalUtil_WarnWithLocation(fn, "%s is not a valid keyword (type %s)",
			   Gal_ObjectString(val, local_bufptr),
			   Gal_ObjectTypeString(Gal_GetObjectType(val)));

  if (buf && !bufptr) {
    Gal_FreeStringBuffer(buf);
  }
}

static char **program_keys(ProgramObject *p, int *count, int *error)
{
  int num_values = p->size - p->index;

  if (count)
    *count = 0;
  
  if (num_values) {
    char **string_array = (char **) calloc(num_values + 1, sizeof(char *));
    Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;

    if (string_array) {
      int i = 0;

      while (p->index < p->size) {
	Gal_Object val = p->values[p->index];
	
	if (Gal_Keywordp(val)) {
	  string_array[i++] = program_string_value(val);
	  if (count)
	    (*count)++;
	  p->index++;
	} else {
	  __Gal_KeywordWarn(__FUNCTION__, val, &buf);
	  *error = GAL_NOT_VALID_KEYWORD;
	  break;
	}
      }
      if (*error == GAL_NO_ERROR) {
	if (buf) Gal_FreeStringBuffer(buf);
	return string_array;
      }
    }
    if (buf) Gal_FreeStringBuffer(buf);
  }
  return NULL;
}

/* convert a ProgramObject array to a Gal_List of Gal_Keyword objects */

static Gal_ProgramEntity **
program_entities(ProgramObject *p, Gal_ProgramParser *pp, int *error)
{
  int num_values = p->size - p->index;
  
  if (num_values) {
    Gal_ProgramEntity **e_list = (Gal_ProgramEntity **)calloc(num_values + 1, sizeof(Gal_ProgramEntity *));
    Gal_ProgramEntity *e;
    int num_entities = 0;
    Gal_NamespaceEntry *old_default = pp->default_namespace;
    int allow_complex;

    if (!pp->extended_syntax) {
      allow_complex = 0;
    } else {
      allow_complex = 1;
    }

    if (e_list) {
      while (p->index < p->size) {
	pp->default_namespace = &(HubNamespaceTable[GAL_TOKEN_NAMESPACE]);
	/* Gal_CreateProgramEntity eats up the index appropriately. */
	e = Gal_CreateProgramEntity(p, pp, allow_complex, 0, 1);
	pp->default_namespace = old_default;
	if (!e) {
	  *error = GAL_NOT_VALID_KEYWORD;
	  free(e_list);
	  break;
	}
	e_list[num_entities++] = e;
      }
      return e_list;
    }
    *error = GAL_ALLOCATION_FAILED;
  }
  return NULL;
}

/* Convert a ProgramObject array of keywords to an array of KeyPair
   structs.  Only Gal_Keywords, Gal_Lists (pairs), and the special
   keys destroy! and none! are allowed.                             */

static KeyPair **program_key_pairs(ProgramObject *p, int *error)
{
  int num_values = p->size - p->index;
  
  if (num_values) {
    KeyPair **kp_list = (KeyPair **)calloc(num_values + 1, sizeof(KeyPair *));
    KeyPair *kp;
    int num_pairs = 0;

    if (kp_list) {
      while (p->index < p->size) {
	Gal_Object val = p->values[p->index++];

	if ((kp = (KeyPair *)calloc(1, sizeof(KeyPair))))
	  kp_list[num_pairs++] = kp;
	else {
	  *error = GAL_ALLOCATION_FAILED;
	  break;
	}

	if (Gal_Tagp(val)) {
	  kp->key = _gal_strdup(Gal_KeywordValue(val));
	  kp->tag = Gal_GetProgramTag(kp->key);
	} else if (Gal_Keywordp(val)) {
	  kp->key = _gal_strdup(Gal_KeywordValue(val));
	} else if (Gal_Listp(val) && Gal_ListLength(val) == 2) {
	  Gal_Object *pair = Gal_ListValue(val, NULL);
	  if (Gal_Keywordp(pair[0])) {
	    kp->key = _gal_strdup(Gal_KeywordValue(pair[0]));
	    if (Gal_Symbolp(pair[1]) || Gal_Tagp(pair[1]))
	      kp->value = Gal_StringObject(Gal_KeywordValue(pair[1]));
	    else if (Gal_Tokenp(pair[1]))
	      kp->value = Gal_StringObject(Gal_StringValue(pair[1]));
	    else
	      kp->value = Gal_CopyObject(pair[1]);
	  } else {
	    *error = GAL_BAD_KEYWORD_PAIR;
	    break;
	  }
	} else {
	  __Gal_KeywordWarn(__FUNCTION__, val, (Gal_StringBuffer **) NULL);
	  *error = GAL_NOT_VALID_KEYWORD;
	  break;
	}
      }
      if (*error != GAL_NO_ERROR) {
	free_key_pairs(kp_list);
	return (KeyPair **) NULL;
      } else {
	return kp_list;
      }
    }
    *error = GAL_ALLOCATION_FAILED;
  }
  return NULL;
}

/* Like program_key_pairs, except used in IN, LOG_IN, OUT, LOG_OUT
   to manage namespace references. Lists at the top level might be
   complex entity references, or they may be pairs, depending on
   how the sequence is parsed. */

/* Target namespace will be -1 when we require a simple keyword
   for the target. */

/* If obligatory_value_format is set, then we allow 
   :a "b"
   (:a "b")
   If not, then we allow
   :a
   (:a "b")
   (:a :b)
*/

static int
__populate_explicit_entity_pair(ProgramObject *p,
				int target_namespace,
				int source_namespace,
				Gal_ProgramParser *pp, EntityPair *ep,
				int allow_complex,
				int obligatory_value_format,
				Gal_NamespaceEntry *old_default,
				int *error)
{
  Gal_ProgramEntity *source, *target;
  
  /* First, the target. */
  /* We can index the array this way because the
     entries are in enum order. */
  if (target_namespace == -1) {
    pp->default_namespace = (Gal_NamespaceEntry *) NULL;
  } else {
    pp->default_namespace = &(HubNamespaceTable[target_namespace]);
  }
  target = Gal_CreateProgramEntity(p, pp, allow_complex, 0, 1);
  pp->default_namespace = old_default;
  if (!target) {
    /* If we don't find a target, we'd better fail. */
    *error = GAL_BAD_KEYWORD_PAIR;
    return 0;
  } else {
    ep->target = target;
  }

  /* At this point, we may be at the end of the list, which
     would be bad. */
  if (p->index == p->size) {
    /* If we just digested the last element, it's bad news. */
    *error = GAL_BAD_KEYWORD_PAIR;
    return 0;
  }

  /* Now, do the source. */
  pp->default_namespace = &(HubNamespaceTable[source_namespace]);
  if (obligatory_value_format)
    source = Gal_CreateProgramEntity(p, pp, 0, 1, 0);
  else
    source = Gal_CreateProgramEntity(p, pp, pp->extended_syntax,
				     1, 1);
  pp->default_namespace = old_default;
  if (!source) {
    /* If we don't find a source, we'd better fail. */
    *error = GAL_BAD_KEYWORD_PAIR;
    return 0;
  } else {
    ep->source = source;	    
  }
  return 1;
}

static EntityPair **program_entity_pairs(ProgramObject *p,
					 int source_namespace,
					 int target_namespace,
					 int obligatory_value_format,
					 Gal_ProgramParser *pp,
					 int *error)
{
  int num_values = p->size - p->index;
  
  if (num_values) {
    EntityPair **ep_list = (EntityPair **)calloc(num_values + 1, sizeof(EntityPair *));
    EntityPair *ep;
    int num_pairs = 0;
    int allow_complex;
    Gal_NamespaceEntry *old_default = pp->default_namespace;

    if (!pp->extended_syntax) {
      /* If there's no extended syntax, we never allow
	 complex entities. */
      allow_complex = 0;
    } else if (target_namespace == -1) {
      allow_complex = 0;
    } else {
      allow_complex = 1;
    }

    if (ep_list) {
      while (p->index < p->size) {
	Gal_Object val = p->values[p->index];

	if (!(ep = (EntityPair *)calloc(1, sizeof(EntityPair)))) {
	  *error = GAL_ALLOCATION_FAILED;
	  break;
	}

	if (Gal_Tagp(val)) {
	  ep->tag = Gal_GetProgramTag(Gal_KeywordValue(val));
	  p->index++;
	} else if (Gal_Listp(val)) {
	  /* Try to parse a pair. The simplest way to do this is
	     to load up the arguments of the list into an array of
	     program objects. Sigh. */
	  ProgramObject sub_p;
	  
	  Gal_InstantiateProgramObjectFromList(val, &sub_p, 0);

	  if (sub_p.size < 2) {
	    *error = GAL_BAD_KEYWORD_PAIR;
	    free(ep);
	    break;
	  }
	  	  
	  if (!__populate_explicit_entity_pair(&sub_p,
					       target_namespace,
					       source_namespace,
					       pp, ep,
					       allow_complex,
					       obligatory_value_format,
					       old_default,
					       error)) {
	    free(ep);
	    break;
	  }
	  /* Now, if we're NOT at the end, it's bad news. */
	  if (sub_p.index < sub_p.size) {
	    free(ep);
	    *error = GAL_BAD_KEYWORD_PAIR;
	    break;
	  } else {
	    p->index++;
	  }	  
	} else if (obligatory_value_format) {
	  if (!__populate_explicit_entity_pair(p,
					       target_namespace,
					       source_namespace,
					       pp, ep,
					       allow_complex,
					       obligatory_value_format,
					       old_default,
					       error)) {
	    break;
	  }
	} else {
	  /* Try to parse a simple entity. Start with the source.
	     If the source parses, try to build a target for it. */
	  pp->default_namespace = &(HubNamespaceTable[source_namespace]);
	  ep->source = Gal_CreateProgramEntity(p, pp, allow_complex, 0, 1);
	  pp->default_namespace = old_default;

	  if (!ep->source) {
	    __Gal_KeywordWarn(__FUNCTION__, val, (Gal_StringBuffer **) NULL);
	    *error = GAL_NOT_VALID_KEYWORD;
	    free(ep);
	    break;
	  }

	  /* If the source object is a namespace object, and it's
	     a default namespace object, I just want to make a copy
	     whose namespace is the target namespace. If it's not a default
	     namespace object, I want to make a default namespace object
	     whose namespace is the target namespace. If it's not a namespace
	     object, just parse it again. */

	  if (target_namespace == -1) {
	    pp->default_namespace = (Gal_NamespaceEntry *) NULL;
	  } else {
	    pp->default_namespace = &(HubNamespaceTable[target_namespace]);
	  }

	  if (ep->source->entity_type == GAL_NAMESPACE_ENTITY) {
	    Gal_NamespaceProgramEntity *ne = (Gal_NamespaceProgramEntity *) ep->source->entity_data;
	    ep->target = Gal_CreateNamespaceProgramEntity(ne->key, pp);
	  }
	  pp->default_namespace = old_default;
	  
	}
	ep_list[num_pairs++] = ep;
      }
      return ep_list;
    }
    *error = GAL_ALLOCATION_FAILED;
  }
  return NULL;
}

/* convert a ProgramObject array to a Gal_List of Gal_String objects.
   SAM 11/18/00: Changed this to a dynamic list object so I can add
   elements to the list of operations for the builtin server.
   Also for active servers. */

static char **program_strings(ProgramObject *p, int *count, int *error)
{
  int num_values = p->size - p->index;

  if (count)
    *count = 0;
  
  if (num_values) {
    char **string_array = (char **) calloc(num_values + 1, sizeof(char *));
    Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;

    if (string_array) {
      int i = 0;
      
      while (p->index < p->size) {
	
	Gal_Object val = p->values[p->index];

	if (Gal_ProgramStringp(val)) {
	  string_array[i++] = program_string_value(val);
	  if (count)
	    (*count)++;
	  p->index++;
	} else {
	  GalUtil_WarnWithLocation(__FUNCTION__, "%s is not a string",
				   Gal_ObjectString(val, &buf));
	  *error = GAL_NOT_STRING;
	  break;
	}
      }
      if (buf) Gal_FreeStringBuffer(buf);
      return string_array;
    }
    if (buf) Gal_FreeStringBuffer(buf);
  }
  return NULL;
}

/*  Convert a ProgramObject array of key value pairs to an array of
    KeyPair structs.  Format is :key value or (:key value)
    special keys destroy! and none! are allowed.                    */

static Gal_Object __ensure_literal(Gal_Object o)
{
  /* This function used not to be necessary, but since I changed the
     reader to allow tokens, etc., in previously literal positions,
     I need this function here. */
  if (Gal_Stringp(o))
    return Gal_CopyObject(o);
  else if (Gal_ProgramStringp(o))
    return Gal_StringObject(Gal_ProgramStringValue(o));
  else
    return Gal_CopyObject(o);
}

static KeyPair **
program_key_value_key_pairs(ProgramObject *p, int *error)
{
  int num_values = p->size - p->index;

  if (num_values) {
    /* SAM 3/6/01: make sure the kp_list is long enough to have
       a NULL at the end, even if num_values == num_pairs. */
    KeyPair **kp_list = (KeyPair **)calloc(num_values + 1, sizeof(KeyPair *));
    KeyPair *kp;
    int num_pairs = 0;
    Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;

    if (kp_list) {
      Gal_Object val;

      while (p->index < p->size) {
	if ((kp = (KeyPair *)calloc(1, sizeof(KeyPair))))
	  kp_list[num_pairs++] = kp;
	else {
	  if (error)
	    *error = GAL_ALLOCATION_FAILED;
	  break;
	}

	val = p->values[p->index++];
	
	if (Gal_Keywordp(val) || Gal_Tagp(val)) {
	  kp->key = _gal_strdup(Gal_KeywordValue(val));
	  if (p->index < p->size) {
	    kp->value = __ensure_literal(p->values[p->index++]);
	  } else {
	    if (error)
	      *error = GAL_BAD_KV_LIST;
	    break;
	  }
	} else if (Gal_Listp(val) && Gal_ListLength(val) == 2) {
	  Gal_Object *pair = Gal_ListValue(val, NULL);
	  if (Gal_Keywordp(pair[0]) || Gal_Tagp(pair[0])) {
	    kp->key = _gal_strdup(Gal_KeywordValue(pair[0]));
	    kp->value = __ensure_literal(pair[1]);
	  } else {
	    __Gal_KeywordWarn(__FUNCTION__, pair[0], &buf);
	    if (error)
	      *error = GAL_BAD_KEYWORD_PAIR;
	    break;
	  }
	} else {
	  __Gal_KeywordWarn(__FUNCTION__, val, &buf);
	  if (error)
	    *error = GAL_BAD_KV_LIST;
	  break;
	}
      }
      if (buf) Gal_FreeStringBuffer(buf);
      if (*error != GAL_NO_ERROR) {
	free_key_pairs(kp_list);
	return (KeyPair **) NULL;
      } else {
	return kp_list;
      }
    }
    else if (error)
      *error = GAL_ALLOCATION_FAILED;
  }
  return NULL;
}

KeyPair **read_key_value_string(char *str)
{
  Gal_ProgramParser pp;
  int count = 0;
  ProgramObject *objects;
  KeyPair **result = (KeyPair **) NULL;
  
  Gal_InitializeProgramParser(&pp, Gal_MakeStringInputStream(str),
			      NULL, GAL_PGM_INIT, 0);

  objects = Gal_ReadProgramLine(&pp, &count);

  if (objects && (objects->size > 1)) {
    int error = GAL_NO_ERROR;
    
    objects->index++;    
    result = program_key_value_key_pairs(objects, &error);
    if (error != GAL_NO_ERROR) {
      /* Oops. */
      char *tag = Gal_ErrorTagString(error);
      char temp[256];

      if (!tag) {
	sprintf(temp, "Unknown Tag %d", error);
	tag = temp;
      }
      GalUtil_Warn(tag);
      GalUtil_Print(GAL_WARNING_LEVEL, str);
    }
  }
  free(pp.gs);
  Gal_ClearProgramParser(&pp);
  Gal_FreeProgramObject(objects, 0);
  return result;
}

/* set the server location (host:port). This function doesn't
   increment the index, so the relevant objects will be freed. */

static void program_location(int tag, ProgramObject *obj,
			     ControlFileParseState *control,
			     SERVICE_TYPE *stype,
			     SERVICE_PROVIDER **provider_ptr,
			     int *error)
{
  int port = -1;
  char *hostname = (char *) NULL;

  switch(tag) {
  case GAL_PGM_HOST:
    /* This isn't really for saving, but we have to make
       sure it's consistent. */
    hostname = fetch_program_string(obj, 1, error);
    break;
  case GAL_PGM_PORT:
    port = program_int(obj, error);
    break;
  case GAL_PGM_LOCATION:
    if (Gal_ProgramStringp(obj->values[obj->index])) {
      char *str = Gal_ProgramStringValue(obj->values[obj->index]);
      
      hostname = Gal_SplitLocation(str, &port);
      if ((!hostname) || (port == -1)) {
	*error = GAL_BAD_LOCATION;
      }
    } else {
      *error = GAL_BAD_LOCATION;
    }
    break;
  default:
    *error = GAL_UNKNOWN_PROGRAM_TAG;
  }

  if (*error != GAL_NO_ERROR) {
    if (hostname) free(hostname);
    return;
  }

  *provider_ptr = update_service_provider(*provider_ptr, hostname, port,
					  (char *) NULL,
					  control->hub, stype);
  /* update_service_provider always copies the hostname */
  if (hostname) free(hostname);
}

static void program_condition(ProgramObject *values, RULE *r,
			      ControlFileParseState *control, int *error_ptr,
			      Gal_ProgramParser *pp)
{
  Gal_ConditionStruct *condition = NULL;
  int error = GAL_NO_ERROR;
  Gal_NamespaceEntry *old_default;

  old_default = pp->default_namespace;
  /* We can index the array this way because the
     entries are in enum order. */
  pp->default_namespace = &(HubNamespaceTable[GAL_TOKEN_NAMESPACE]);
  condition = Gal_NewCondition(values, pp, &error);
  pp->default_namespace = old_default;
  if (condition) {
    validate_operation(condition, control, &error);
    if (error != GAL_NO_ERROR) {
      *error_ptr = error;
      return;
    }
    /* If the error wasn't set, we still want to copy
       things over. */
    r->tests = condition->tests;
    r->op_name = condition->operation;
    r->server_name = condition->server;
    free(condition);
  }

  *error_ptr = error;
}

/* Here's the problem. program_string_value can be called sometimes
   when we want to save away the object, and sometimes when we
   don't. In all cases, we want to free what we can free. However,
   Gal_ObjectToString ALWAYS returns a newly allocated string. I think
   we'll just have to rule that this always returns a newly
   allocated string. */

static char *program_string_value(Gal_Object obj)
{
  if (Gal_Stringp(obj)) {
    return _gal_strdup(Gal_StringValue(obj));
  }
  if (Gal_Keywordp(obj) || Gal_Symbolp(obj) || Gal_Tagp(obj)) {
    return _gal_strdup(Gal_KeywordValue(obj));
  } else {
    return Gal_ObjectToString(obj);
  }
}

static void program_modes(ControlFileParseState *control, ProgramObject *p,
			  int *error_ptr)
{
  char **modes = program_strings(p, NULL, error_ptr);
  int i;
  
  if (*error_ptr != GAL_NO_ERROR)
    return;
  
  if (!modes) return;
  
  for (i = 0; modes[i]; i++) {
    if (*error_ptr != GAL_NO_ERROR) {
      /* We've set an error on a previous loop.
	 Don't do anything except free the string. */
      free(modes[i]);
    } else if (Gal_StringEq(modes[i], "singlethread")) {
      if (control->current_program) {
	if (control->current_program->mode)
	  free(control->current_program->mode);
	control->current_program->mode = modes[i];
      } else {
	if (control->hc->mode)
	  free(control->hc->mode);
	control->hc->mode = modes[i];
      }
    } else if (Gal_StringEq(modes[i], "pedantic")) {      
      control->be_pedantic_about_messages = 1;
      free(modes[i]);
    } else if (Gal_StringEq(modes[i], "global_read_only")) {
      HubNamespaceTable[GAL_GLOBAL_NAMESPACE].writable = 0;
      free(modes[i]);
    } else {
      *error_ptr = GAL_UNKNOWN_MODE;
    }
  }
  free(modes);
}

static int validate_operation(Gal_ConditionStruct *condition,
			      ControlFileParseState *control, int *error_ptr)
{
  int warning;
  int valid = 0;
  SERVICE_TYPE *server = (SERVICE_TYPE *) NULL;
  int found_service_type = 0;
  
  if (!control || !condition)
    return valid;

  if (condition->server) {
    /* In order to verify and canonicalize the server name,
       we need to split it up and then reassemble it.
       We also have to be aware the the provider ID may
       constitute the whole of the server name, and we will
       end up needing to treat this case as if there were
       no service type. */
    /* Try to split off a provider ID. */
    int provider_id = -1;
    char *provider_name = (char *) NULL;
    char *old_stype = condition->server;
    char *stype = Gal_SplitServiceTypeName(old_stype, &provider_id,
					   &provider_name);

    /* Don't forget, stype may be empty. */

    if (stype && !_gal_strcasecmp(stype, BUILTIN_SERVER)) {
      /* Allocated by Gal_SplitOperationName. */
      free(stype);
      stype = _gal_strdup(BUILTIN_SERVER);
    }

    /* If there's just a provider ID, stype will be NULL. */
    if (stype) {
      found_service_type = 1;
      server = GalHUB_FindServiceType(control->hub, stype);

      /* SAM 5/6/02: This shouldn't be looking in the active
	 servers list, but rather in the pointer list of active
	 stypes. */
      if (server) {
	int check_ops = 0;
	
	if (!control->active_stypes) {
	  check_ops = 1;
	} else {
	  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(control->active_stypes);
	  int active_length = Gal_PointerBufferSize(control->active_stypes);
	  int i;

	  for (i = 0; i < active_length; i++) {
	    if (!strcmp(stype, stypes[i]->name)) {
	      check_ops = 1;
	      break;
	    }
	  }
	}
	if (check_ops) {
	  if (list_member(condition->operation, server->operations) > -1)
	    valid = 1;
	}
      }
    }
    /* Now that we've located the server, reset the condition. */
    if (provider_id > -1) {
      /* If there's an stype, we reconstitute. Otherwise, we
	 don't need to do anything, because no memory was
	 allocated. */
      if (stype) {
	/* This will be long enough, because the builtin
	   server's canonicalize and uncanonicalized names
	   will be the same length. */	 
	sprintf(old_stype, "[%d]%s", provider_id, stype);
	free(stype);
      }	
    } else if (provider_name) {
      /* See comments for provider ID. */
      if (stype) {
	sprintf(old_stype, "[%s]%s", provider_name, stype);
	free(stype);
      }
      free(provider_name);
    } else {
      free(old_stype);
      condition->server = stype;
    }
  }
  if (!found_service_type) {
    Gal_Object global_server = Gal_GetHash(condition->operation,
					   control->operations_hash);

    if (global_server) {
      valid = 1;
      if (Gal_Stringp(global_server)) {
	/* There's only one of them. Add the service type name. */
	condition->server = _gal_strdup(Gal_StringValue(global_server));
      }
    }
  }
  if ((!valid) && (Gal_PointerBufferSize(control->stypes) > 0)) {
    char *op_name;
    
    if (condition->server) {
      int op_len = strlen(condition->operation) + strlen(condition->server);

      op_name = calloc(op_len + 2, sizeof(char));      
      sprintf(op_name, "%s.%s", condition->server, condition->operation);
    } else {
      op_name = condition->operation;
    }

    if (list_member(op_name, control->hc->operations_to_ignore) > -1) {
      valid = 1;
    }
    if (op_name != condition->operation)
      free(op_name);
  }
  if (!valid) {
    /* We shouldn't ignore the fact that this isn't present. */
    if (condition->server)
      warning = GAL_INVALID_OPERATION;
    else
      warning = GAL_NO_SERVER_FOR_OPERATION;
    if (control->be_pedantic_about_messages) {
      *error_ptr = warning;
    } else {
      Gal_PrintProgramWarning(control->current_files, warning);
    }
  }
  return valid;
}

/* hash active servers and operations for operation validation */

static void hash_operations(ControlFileParseState *control, int *errorp)
{
  int i;
  SERVICE_TYPE **stypes;
  int num_stypes;
  
  add_builtin_server(control, errorp);
  /* At this pointer, the builtin server will be present in
     both the server list and active server list, and it will
     have all the appropriate operations. */

  if (*errorp)
    return;
  
  stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(control->stypes);
  num_stypes = Gal_PointerBufferSize(control->stypes);

  if (control->hc->active_servers) {
    /* If I have active servers, I really need to consult the
       locations list too, since the active servers list can
       contain host:port. I'm going to do this in a kind
       of roundabout way. For each element in the active
       servers list, I'll check to see if it's in the list
       of servers. If it's not, I'll check to see if it's
       in the list of locations, and if so, I'll add to the
       servers list all the servers for that location. THEN
       I'll mark the servers active. If there's something
       in the active servers list which doesn't appear in
       the program, no matter; the Hub treats this as a FILTER
       ON THE EXISTING SERVERS.
       */
    /* Look at all the stypes. */
    SERVICE_PROVIDER **providers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(control->servers);
    int num_providers = Gal_PointerBufferSize(control->servers);

    control->active_stypes = Gal_MakePointerBuffer(NULL,
						   GAL_SERVICE_TYPE_PTYPE,
						   0, 0, 1, 1,
						   NULL, 10, 0);
    
    /* First, we check the name of the stype. */
    
    for (i = 0; i < num_stypes; i++) {
      if (GalHUB_StypeFoundInActiveServers(stypes[i]->name,
					   control->hc->active_servers)) {
	/* The service type is active. */
	Gal_PointerBufferAdd(control->active_stypes, (void *) stypes[i]);
	hash_global_operations(control, stypes[i]);
      }
    }
    
    /* Now, check the providers. */
    for (i = 0; i < num_providers; i++) {
      SERVICE_PROVIDER *p = providers[i];
      /* Populate the test spec with the provider information.
	 Set it up so that each provider is considered in turn. */
      SERVICE_TYPE **p_stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(p->stypes);
      int num_p_stypes = Gal_PointerBufferSize(p->stypes);
      int j, k;

      /* Try each provider/stype pair. */
      for (k = 0; k < num_p_stypes; k++) {
	if (GalHUB_ProviderFoundInActiveServers(p, p_stypes[k],
						control->hc->active_servers)) {
	  /* This provider/stype pair is active. See if you can
	     find the stype in the list of stypes already found. */
	  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(control->active_stypes);
	  int active_length = Gal_PointerBufferSize(control->active_stypes);
	  int found = 0;
	  
	  for (j = 0; j < active_length; j++) {
	    if (stypes[j] == p_stypes[k]) {
	      found = 1;
	      break;
	    }
	  }
	  if (!found) {
	    Gal_PointerBufferAdd(control->active_stypes,
				 (void *) p_stypes[k]);
	    hash_global_operations(control, p_stypes[k]);
	  }
	}
      }
    }
  } else {
    for (i = 0; i < num_stypes; i++) {
      hash_global_operations(control, stypes[i]);
    }
  }
}

/*
 * A new copy of hash_value is associated with each key in the list
 * since hash tables free their objects when the table is cleared.
 * We need to set the server name. If there's already a hash
 * value, then we reset it to 1, indicating presence of more than
 * one server. That's all we really need.
 */

static void hash_global_operations(ControlFileParseState *control,
				   SERVICE_TYPE *s)
     
{
  int i;
  char **operations = s->operations;

  if ((!operations) || (!operations[0]))
    return;

  for (i = 0; operations[i]; i++) {
    Gal_Object cur_val = Gal_GetHash(operations[i], control->operations_hash);

    if (!cur_val) {
      Gal_SetHash(operations[i], Gal_StringObject(s->name),
		  control->operations_hash);
    } else if (Gal_Stringp(cur_val) &&
	       strcmp(s->name, Gal_StringValue(cur_val))) {
      /* There's already a single value, and this is a different one. */
      Gal_SetHash(operations[i], Gal_IntObject(1), control->operations_hash);
    }
  }
}

/*  The program file is processed in three stages.
 *
 *  read_program_file opens the top level program file and
 *  sets the working directory for any included files.
 *
 *  Gal_ReadProgramLine reads a logical line from a program file
 *  and parses it into its constituent tokens.
 *
 *  read_program builds the hub structure and pulls in
 *  included files using the following functions:
 *
 *    single value:    program_string, program_int, program_frame
 *    multiple values: program_keys, program_strings, program_key_values
 *    special cases:   program_alarm, program_location, program_condition, read_included_file, program_modes
 *
 */

/* Here are the complex readers for the Hub. */

/* Memory management note: all builders need to create 
   new memory for anything they save away. */

Gal_ProgramEntity *
_Gal_DefaultComplexProgramEntityConstructor(Gal_ComplexEntityDescriptor *t,
					    Gal_Object arglist,
					    Gal_ProgramParser *pp);

static Gal_ProgramEntity *__gal_id_builder(Gal_ComplexEntityDescriptor *t,
					   Gal_Object arglist,
					   Gal_ProgramParser *pp)
{
  int argnum = Gal_ListLength(arglist);
  Gal_Object o;
  
  if (argnum != 1) {
    return (Gal_ProgramEntity *) NULL;
  }

  /* We know the symbol was $id, and we found a list.
     Now what? We expect a namespace name, which we can
     use to compute the namespace ID at run_time. */
  o = Gal_GetListObject(arglist, 0);
  if (Gal_FindProgramNamespace(o, pp)) {
    /* If it really is the name of a namespace, then
       we can save it away in the arglist. */
    return _Gal_DefaultComplexProgramEntityConstructor(t, arglist, pp);
  }
  return (Gal_ProgramEntity *) NULL;
}

static Gal_Object __gal_id_getter(Gal_ComplexProgramEntity *ce,
				  Gal_Frame *namespace_array,
				  int *newly_created)
{
  int local_newly_created;
  Gal_Object o = Gal_GetProgramEntity(ce->args[0], namespace_array,
				      &local_newly_created);
  
  Gal_NamespaceEntry *ne = Gal_FindNamespaceEntry(Gal_ProgramStringValue(o),
						  HubNamespaceTable);

  if (o && local_newly_created)
    Gal_FreeObject(o);

  switch (ne->namespace_int) {
  case GAL_SESSION_NAMESPACE:
    /* Use the name of the session namespace. */
  case GAL_SERVER_NAMESPACE:
    /* Use the name of the server namespace. */
    *newly_created = 1;
    return Gal_StringObject(Gal_FrameName(namespace_array[ne->namespace_int]));
  default:
    *newly_created = 0;
    return (Gal_Object) NULL;
  }
}

static Gal_ProgramEntity *__gal_bind_builder(Gal_ComplexEntityDescriptor *t,
					     Gal_Object arglist,
					     Gal_ProgramParser *pp)
{
  int argnum = Gal_ListLength(arglist);

  /* The well-formedness is such that the arglist must
     be odd length. When we evaluate it, the first element had
     better be a string, and every alternating argument after the
     first had better end up being a string, too, since it
     needs to be a key. */

  if ((argnum % 2) == 0) {
    return (Gal_ProgramEntity *) NULL;
  }
  
  return _Gal_DefaultComplexProgramEntityConstructor(t, arglist, pp);
}

static Gal_Object __gal_bind_getter(Gal_ComplexProgramEntity *ce,
				    Gal_Frame *namespace_array,
				    int *newly_created)
{
  int local_newly_created;
  Gal_Object o = Gal_GetProgramEntity(ce->args[0], namespace_array,
				      &local_newly_created);
  int i, j;
  Gal_VarMapping *map;
  Gal_Object res = (Gal_Object) NULL;
  int *newly_created_array;

  *newly_created = 0;
  
  /* The first element needs to be a string. */
  if (!Gal_Stringp(o)) {
    if (o && local_newly_created)
      Gal_FreeObject(o);
    return (Gal_Object) NULL;
  }

  /* We have ce->num_args - 1 remaining, which means ce->num_args - 1 / 2
     pairs, which means we need the var mapping one larger. */
  map = (Gal_VarMapping *) calloc(((ce->num_args - 1) / 2) + 1,
				  sizeof(Gal_VarMapping));
  newly_created_array = (int *) calloc(ce->num_args, sizeof(int));
  
  i = 1;
  j = 0;
  /* Build the variable array. */
  while (i < ce->num_args) {
    Gal_Object sub_o = Gal_GetProgramEntity(ce->args[i], namespace_array,
					    &(newly_created_array[i]));
    if (!Gal_Stringp(sub_o)) {
      int k, q;
      
      k = 1;
      q = 0;
      while (k < i) {
	if (newly_created_array[k]) {
	  free(map[q].var);
	}
	if (newly_created_array[k + 1]) {
	  Gal_FreeObject(map[q].value);
	}
	q++;
	k += 2;
      }
      free(map);      
      
      if (sub_o && newly_created_array[i])
	Gal_FreeObject(sub_o);

      free(newly_created_array);
      
      return (Gal_Object) NULL;
    }
    
    map[j].var = Gal_StringValue(sub_o);
    /* Free the wrapper, leave the string. */
    if (newly_created_array[i]) {
      Gal_FreeWrapper(sub_o);
    }
    map[j].value = Gal_GetProgramEntity(ce->args[i + 1], namespace_array,
					&(newly_created_array[i + 1]));
    j++;
    i += 2;
  }

  /* Do the substitution. */
  res = Gal_ReadVarObjectFromString(Gal_StringValue(o), map);
  i = 1;
  j = 0;
  while (i < ce->num_args) {
    if (newly_created_array[i]) {
      free(map[j].var);
    }
    if (newly_created_array[i + 1]) {
      Gal_FreeObject(map[j].value);
    }
    j++;
    i += 2;
  }
  free(map);
  free(newly_created_array);
  *newly_created = 1;
  return res;
}

static Gal_ComplexEntityDescriptor __Gal_HubEntities[] =
{
  {"$id", __gal_id_builder, __gal_id_getter, NULL},
  {"$bind", __gal_bind_builder, __gal_bind_getter, NULL},
  {NULL, NULL, NULL, NULL}
};

static void maybe_hash_operations(ControlFileParseState *control, int *errorp)
{
  if (!control->operations_hashed) {
    hash_operations(control, errorp);
    control->operations_hashed = 1;
  }
}

static void 
finalize_current_rules(PROGRAM *current_program,
		       RULE **rule_buffer, int current_rule_index,
		       KeyPair **current_rule_control_keys,
		       KeyPair **global_alarm_kps)
{
  /* Start with the first rule. They should all have the same
     stuff in them. */

  int i = 0;
  int j;
  RULE *rptr = rule_buffer[0];
  int control = 0;
  int disable_alarm = 0;

  if (global_alarm_kps && Gal_StringEq(global_alarm_kps[0]->key, "disable"))
    disable_alarm = 1;
  
  if (rptr->out_kps){
    switch (rptr->out_kps[0]->tag) {
    case GAL_KEY_DESTROY:
      control = GAL_CONTROL_RETURN | GAL_CONTROL_ASYNCHRONOUS | GAL_CONTROL_NO_RESULT;
      break;

    case GAL_KEY_ABORT:
      control = GAL_CONTROL_RETURN;
      break;

    case GAL_KEY_NONE:
      /* Run asynchronously, doesn't return any result */
      control = GAL_CONTROL_ASYNCHRONOUS | GAL_CONTROL_NO_RESULT;
      break;
    }
  }

  if (current_rule_control_keys) {
    int cp = 0;
    KeyPair* kp;
    while(NULL != (kp = current_rule_control_keys[cp++])){
      int value = (kp->value) ? Gal_IntValue(kp->value) : 1;
      if (kp->tag == GAL_KEY_ASYNCHRONOUS){
	control = (control & ~GAL_CONTROL_ASYNCHRONOUS) | value<<GAL_CONTROL_ASYNCHRONOUS_POS;
      }
      if (kp->tag == GAL_KEY_RETURN){
	control = (control & ~GAL_CONTROL_RETURN) | value<<GAL_CONTROL_RETURN_POS;
      }	      
      if (kp->tag == GAL_KEY_NO_RESULT){
	control = (control & ~GAL_CONTROL_NO_RESULT) | value<<GAL_CONTROL_NO_RESULT_POS;
      }	      
    }
  }

  /* Add the resulting value to all the rules, and also
     update the alarms. */

  while (rule_buffer[i]) {

    RULE *rule = rule_buffer[i++];
    
    rule->control = control;
    
    if (!(rule->alarm_kps)) {
      if (!disable_alarm && global_alarm_kps) {
	/* a default value */
	rule->alarm_kps = global_alarm_kps;
      }
    }
    if (rule->alarm_kps) {
      int i = 0;
      KeyPair *kp;

      if (disable_alarm) {
	rule->alarm_kps = NULL;
	continue;
      }
      if (!rule->alarm_kps[0]->value) {
	rule->alarm_kps = NULL;
	continue;
      }

      while ((kp = rule->alarm_kps[i++]))
      {
	GalHUB_AlarmAdd(kp->key);
      }
    }
  }
  /* Finally, add all the rules. */
  
  /* i is now the initial empty cell. current_rule_index
     is the index of the last element in the rule_buffer. */
  current_program->rules = expand_rule_buffer(current_program->rules,
					      current_rule_index + 1 + current_program->num_rules);
  for (j = 0; j <= current_rule_index; j++) {
    rule_buffer[j]->ridx = current_program->num_rules;
    current_program->rules[current_program->num_rules++] = rule_buffer[j];
  }
}

/* If current_server is non-NULL, and neither host nor port
   is present, we have an empty service provider which was
   set up with the expectation that data may be forthcoming.
   We should remove that one silently. If we don't need to
   discard it, we should record it. */

static void
maybe_discard_current_service_provider(ControlFileParseState *control,
				       SERVICE_PROVIDER *p,
				       SERVICE_TYPE *current_server,
				       int *error)
{
  int silent = 0;
  
  if ((!p->host) || (p->port == -1)) {
    if (p->host) {
      GalUtil_Warn("Discarding ill-formed provider/location (host %s, no port)",
		   p->host);
    } else if (p->port != -1) {
      GalUtil_Warn("Discarding ill-formed provider/location (port %d, no host)",
		   p->port);
    } else {
      if (current_server) {
	/* Empty prospective provider, remove silently. */
	silent = 1;
      } else {
	GalUtil_Warn("Discarding ill-formed provider/location (no host, no port)");
      }
    }
    if ((!silent) && (control->control_flags & GAL_PROGRAM_VERIFY)) {
      *error = GAL_BAD_LOCATION;
    }
    /* The provider will not have been recorded yet, so
       don't try to remove it from anything. */
    GalHUB_LRemoveServiceProvider((HUB *) NULL, p, 0, 0);
  } else {
    GalHUB_RecordServiceProvider(control->hub, p);
  }
}

static void finalize_program_and_message(ControlFileParseState *control)
{
  if (control->current_program && !control->current_program->mode) {
    control->current_program->mode = _gal_strdup(control->hc->mode);
  }
  /* If there's either a current program or current message,
     and they have log keys, store them. */
  /* SAM 4/29/02: It's just wrong to split the operation
     name here. If the program is named foo.bar, indexing
     this under bar will just do the wrong thing. These
     need to be literal. */
  /* Not quite. We also want the messages to be pattern matches for
     the timestamp and logging. So we need to split it up. */
  if (control->current_message &&
      (control->current_message->log_in ||
       control->current_message->log_out)) {
    char *server_temp, *temp;
    
    GalHUB_AddMessageToTimestampTable(control->hub,
				      control->current_message->name,
				      control->current_message->log_in,
				      control->current_message->log_out,
				      (GalSS_ProviderSpec *) NULL, 1, 1, 0);
    
    /* SAM 2/8/00: For consistency, Gal_SplitOperationName()
       always returns new memory. */
	  
    server_temp = (char *) NULL;
    temp = Gal_SplitOperationName(control->current_message->name,
				  &server_temp);

    /* If they're different, record this one too, and
       use the digested server name. Actually, don't even
       check, because this entry isn't a literal match. */
    GalHUB_AddMessageToTimestampTable(control->hub, temp,
				      control->current_message->log_in,
				      control->current_message->log_out,
				      GalHUB_SplitServerName(server_temp),
				      0, 1, 0);
    if (server_temp) free(server_temp);
    free(temp);
  }
  if (control->current_program &&
      (control->current_program->log_in || control->current_program->log_out))
    GalHUB_AddMessageToTimestampTable(control->hub,
				      control->current_program->name,
				      control->current_program->log_in,
				      control->current_program->log_out,
				      (GalSS_ProviderSpec *) NULL, 1, 1, 1);
  control->current_message = NULL;  
  control->current_program = NULL;
}

extern
Gal_Object _Gal_ProgramRuleTokenizer(Gal_InputStream gs, char *token);

static Gal_TokenizerChoice __Gal_HubTokenizers[] = {
  {GAL_PGM_CONDITIONS, _Gal_ProgramRuleTokenizer},
  {GAL_NO_PROGRAM_TAG, NULL}};

/* SAM 6/28/02: Memory management note. In order to ensure
   that the programs can be freed cleanly, I'm going to adopt
   the policy that every line will be completely freed
   at the end of processing, which means that any part of the
   program which saves away the entities has to create new
   memory. */

static void read_hub_control(ControlFileParseState *control,
			     char *directory,
			     int *error_ptr, int already_extended)
{
  ProgramObject *objects;
  /* This is for SERVICE_TYPE: and SERVER: entries. */
  SERVICE_TYPE *current_service_type = NULL;
  SERVICE_TYPE *current_server = NULL;
  /* This will be >= 0 when we're in a rule. */
  int current_rule_index = -1;
  KeyPair **current_rule_control_keys = (KeyPair **) NULL;
  /* This is for the global alarms. */
  KeyPair **global_alarm_kps = (KeyPair **) NULL;
  /* This is for SERVICE_PROVIDER: entries. */
  SERVICE_PROVIDER *current_service_provider = NULL;
  int error = GAL_NO_ERROR;
  int done = 0;
  Gal_ProgramParser pp;
  int count = 0;
  Gal_NamespaceEntry *old_default;
  char *initial_reply = (char *) NULL;
  Gal_Frame init_state = (Gal_Frame) NULL;
  char *temp_s;
  char **hub_activity;
  KeyPair **temp_kp;
  char **temp_ss;
  int max_rules = 0;
  RULE **rule_buffer;
  
  if (!control || !control->current_files) {
    *error_ptr = GAL_NO_HUB;
    return;
  }

  rule_buffer = expand_rule_buffer((RULE **) NULL, max_rules);

  Gal_InitializeProgramParser(&pp, Gal_MakeFileInputStream(control->current_files->fp),
			      control->current_files, 0,
			      control->control_flags);
  pp.namespace_entries = HubNamespaceTable;
  pp.descriptor_table = __Gal_HubEntities;
  pp.extra_choices = __Gal_HubTokenizers;

  if (already_extended)
    pp.extended_syntax = 1;
  
  while (!done && (error == GAL_NO_ERROR) && (pp.error == GAL_NO_ERROR) &&
	 (objects = Gal_ReadProgramLine(&pp, &count))) {
    int tag = Gal_GetProgramObjectTag(objects, 0);

    objects->index = 1;

    switch(tag)
    {
    case GAL_PGM_COMMENT_LINE:
    case GAL_PGM_PGM_SYNTAX:
      break;
    case GAL_PGM_CONTROL:
      if (current_rule_index > -1)
	current_rule_control_keys = program_key_pairs(objects, &error);
      else
	error = GAL_NO_RULE;
      break;
    case GAL_PGM_EOF:
      /* Make sure we flush the current locations. */
      if (current_service_provider) {
	maybe_discard_current_service_provider(control,
					       current_service_provider,
					       current_server,
					       &error);
      }
      if (current_rule_index > -1) {
	finalize_current_rules(control->current_program,
			       rule_buffer,
			       current_rule_index,
			       current_rule_control_keys,
			       global_alarm_kps);
      }
      if (current_rule_control_keys)
	free_key_pairs(current_rule_control_keys);
      current_rule_control_keys = NULL;
      done = 1;
      break;
    case GAL_PGM_ALARM:
      if (current_rule_index > -1) {
	KeyPair **alarm_kps = program_alarm(objects, &error);
	int i = 0;

	if (error == GAL_NO_ERROR) {
	  while (rule_buffer[i]) {
	    if (i > 0) {
	      rule_buffer[i++]->alarm_kps = copy_key_pairs(alarm_kps);
	    } else {
	      rule_buffer[i++]->alarm_kps = alarm_kps;
	    }
	  }
	}
      } else {
	global_alarm_kps = program_alarm(objects, &error);
      }
      break;
    case GAL_PGM_BLANK_LINE:
      if (current_rule_index > -1)
	finalize_current_rules(control->current_program,
			       rule_buffer,
			       current_rule_index,
			       current_rule_control_keys,
			       global_alarm_kps);
      while (current_rule_index > -1) {
	rule_buffer[current_rule_index] = (RULE *) NULL;
	current_rule_index--;
      }
      current_rule_index = -1;
      if (current_rule_control_keys)
	free_key_pairs(current_rule_control_keys);
      current_rule_control_keys = NULL;
      /* If the current service provider isn't complete,
	 remove it, since it's already been added. But we don't
	 want to do that if we're in a SERVER: block where
	 a dummy provider has been set up. */
      if (current_service_provider) {
	maybe_discard_current_service_provider(control,
					       current_service_provider,
					       current_server,
					       &error);
      }
      current_service_provider = NULL;
      current_server = NULL;
      current_service_type = NULL;
      break;
    case GAL_PGM_CONDITIONS:
      /* These are conditions for when to use a particular service
	 provider. You can provide defaults on a service type,
	 and overrides on a service provider. This entry only
	 makes sense in the extended syntax, since you need to compare
	 the server properties to other properties, like session props. */
      old_default = pp.default_namespace;
      pp.default_namespace = &(HubNamespaceTable[GAL_SERVER_NAMESPACE]);
      
      if (current_service_type) {
	current_service_type->conditions = Gal_NewTest(objects, &error, 1, &pp);
      } else if (current_service_provider) {
	current_service_provider->conditions = Gal_NewTest(objects, &error, 1, &pp);
      } else {
	error = GAL_NO_SERVER;
      }
      /* Now, we reset the index, in order to delete everything. */
      objects->index = 1;
      pp.default_namespace = old_default;
      break;
    case GAL_PGM_CONTINUE_REPLY:
      if (current_rule_index > -1) {
	Gal_Frame reply_frame = program_frame(objects, &error);
	Gal_Frame *frames;
	int i = 0;

	if (error == GAL_NO_ERROR) {
	  while (rule_buffer[i]) {
	    
	    if (i > 0)
	      reply_frame = Gal_CopyFrame(reply_frame);
	    frames = add_continuation_frame(rule_buffer[i]->reply_continuations,
					    reply_frame, &error);
	    if (error == GAL_NO_ERROR) {
	      rule_buffer[i++]->reply_continuations = frames;
	    }
	  }
	}
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_CONTINUE_ERROR:
      if (current_rule_index > -1) {
	Gal_Frame error_frame = program_frame(objects, &error);
	Gal_Frame *frames;
	int i = 0;

	if (error == GAL_NO_ERROR) {
	  while (rule_buffer[i]) {
	    if (i > 0)
	      error_frame = Gal_CopyFrame(error_frame);
	    frames = add_continuation_frame(rule_buffer[i]->error_continuations,
					    error_frame, &error);
	    if (error == GAL_NO_ERROR) {
	      rule_buffer[i++]->error_continuations = frames;
	    }
	  }
	}
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_DEL:
      if (current_rule_index > -1) {
	Gal_ProgramEntity **dels = program_entities(objects, &pp, &error);
	int i = 0;

	if (error == GAL_NO_ERROR) {	
	  while (rule_buffer[i]) {
	    if (i > 0) {
	      rule_buffer[i++]->del_vars = copy_program_entities(dels);
	    } else {
	      rule_buffer[i++]->del_vars = dels;
	    }
	  }
	}
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_ERROR:
      /* ERROR: is like OUT:, but it's triggered when there's an error.
	 If OUT: is none!, ERROR: has no effect. */      
      if (current_rule_index > -1) {
	EntityPair **kps = program_entity_pairs(objects,
						GAL_MESSAGE_NAMESPACE,
						GAL_TOKEN_NAMESPACE,
						0, &pp, &error);
	int i = 0;

	if (error == GAL_NO_ERROR) {
	  while (rule_buffer[i]) {
	    /* Set a separate flag, because ERROR: with
	       no arguments should still catch the error. */
	    rule_buffer[i]->catch_error = 1;
	    if (i > 0) {
	      rule_buffer[i++]->error_kps = copy_entity_pairs(kps);
	    } else {
	      rule_buffer[i++]->error_kps = kps;
	    }
	  }
	}
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_CLIENT_PORT:
      /* This is where clients can contact the Hub to establish
	 this server. This is associated with the service type,
	 not the service provider. */
      if (current_service_type) {
	current_service_type->listener_port = program_int(objects, &error);
      } else {
	error = GAL_NO_SERVER;
      }
      break;
    case GAL_PGM_PORT:
      /* In a SERVICE_TYPE: block, current_server won't be set.
	 If PORT: appears in a SERVER: block, it should be
	 interpreted as a provider. */
      if (current_service_type && !current_server) {
	current_service_type->listener_port = program_int(objects, &error);
      } else if (current_service_provider) {
	program_location(tag, objects, control, current_service_type,
			 &current_service_provider, &error);
	if (error != GAL_NO_ERROR)
	  GalHUB_LRemoveServiceProvider(control->hub,
					current_service_provider,
					1, 1);
      } else {
	error = GAL_NO_SERVER;
      }
      break;
    case GAL_PGM_DOMAIN:
      temp_s = program_string(objects, 0, &error);

      if (error == GAL_NO_ERROR) {
	control->hc->domain_key = Gal_ProgramTagString(tag);
	GalHUB_SetHubDefaultDomain(control->hub, temp_s,
				   Gal_ProgramTagString(tag));
      }
      break;      
    case GAL_PGM_HOST:
      if (current_service_provider) {
	program_location(tag, objects, control, current_service_type,
			 &current_service_provider, &error);
	if (error != GAL_NO_ERROR)
	  GalHUB_LRemoveServiceProvider(control->hub,
					current_service_provider,
					1, 1);
      } else {
	error = GAL_NO_SERVER;
      }
      break;
    case GAL_PGM_INCLUDE:
      read_included_file(fetch_program_string(objects, 0, &error),
			 directory, control, &error, pp.extended_syntax);
      break;
    case GAL_PGM_IN:      
      /* SAM 11/29/00: Scott C. and I have agreed that service
	 types and service providers can also have IN: entries,
	 which will be appended to the IN: list for any
	 operation directed to them. */
	 
      if (current_rule_index > -1) {
	EntityPair **in_kps = program_entity_pairs(objects,
						   GAL_TOKEN_NAMESPACE,
						   GAL_MESSAGE_NAMESPACE,
						   0, &pp, &error);
	int i = 0;

	if (error == GAL_NO_ERROR) {
	  while (rule_buffer[i]) {
	    if (i > 0) {
	      rule_buffer[i++]->in_kps = copy_entity_pairs(in_kps);
	    } else {
	      rule_buffer[i++]->in_kps = in_kps;
	    }
	  }
	}
      } else if (current_service_type) {
	current_service_type->in = program_entity_pairs(objects,
							GAL_SERVER_NAMESPACE,
							GAL_MESSAGE_NAMESPACE,
							0, &pp, &error);
      } else if (current_service_provider) {
	current_service_provider->in = program_entity_pairs(objects,
							    GAL_SERVER_NAMESPACE,
							    GAL_MESSAGE_NAMESPACE,
							    0, &pp, &error);
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_IGNORE:
      /* This is a list of actions you can ignore if you can't find
	 (so you can have
	 paths through a program file you include which will never
	 be fired, so the reader can ignore them). */
      control->hc->operations_to_ignore = program_strings(objects, NULL, &error);
      break;
    case GAL_PGM_INIT:
      /* For some reason, I won't override an existing setting. Not
	 sure why. */
      if (current_service_type) {
	if (!current_service_type->init_kps)
	  current_service_type->init_kps = program_key_value_key_pairs(objects, &error);
      } else if (current_service_provider) {
	if (!current_service_provider->init_kps)
	  current_service_provider->init_kps = program_key_value_key_pairs(objects, &error);
      } else {
	error = GAL_NO_SERVER;
      }
      break;
    case GAL_PGM_INITIAL_REPLY:
      initial_reply = program_string(objects, 0, &error);
      if (initial_reply && (error == GAL_NO_ERROR)) {
	control->hc->initial_reply_key = Gal_ProgramTagString(tag);
	GalHUB_SetHubGlobal(control->hub, Gal_ProgramTagString(tag),
			    Gal_StringObject(initial_reply));
      }
      break;
    case GAL_PGM_INITIAL_TOKEN:
      init_state = program_frame(objects, &error);      
      if (init_state && (error == GAL_NO_ERROR)) {
	control->hc->initial_token_key = Gal_ProgramTagString(tag);
	GalHUB_SetHubGlobal(control->hub, Gal_ProgramTagString(tag),
			    Gal_FrameObject(init_state));
      }
      break;
    case GAL_PGM_KV_LANG:
      temp_s = program_string(objects, 0, &error);

      if (error == GAL_NO_ERROR) {
	GalHUB_DeleteHubGlobal(control->hub, DEFAULT_KV_LANG_KEY);
	control->hc->kv_lang_key = Gal_ProgramTagString(tag);
	GalHUB_SetHubGlobal(control->hub, Gal_ProgramTagString(tag),
			    Gal_StringObject(temp_s));
      }
      break;
    case GAL_PGM_LOCATION:
      /* There better be one or the other of these. */
      if (current_service_provider || current_server) {
	program_location(tag, objects, control, current_service_type,
			 &current_service_provider, &error);
	if (error != GAL_NO_ERROR)
	  GalHUB_LRemoveServiceProvider(control->hub,
					current_service_provider,
					1, 1);
      } else {
	error = GAL_NO_SERVER;
      }
      break;
    case GAL_PGM_LOCK:
      /* In order to decode the locks, we need to populate
	 the keys into a frame. */
      if (current_rule_index > -1) {
	char **lock_strings = program_keys(objects, NULL, &error);
	Gal_Frame fr;
	char *key;
	int k;
	int lock_mask;
	int lock_value;
	int i = 0;

	if (error == GAL_NO_ERROR) {
	  fr = Gal_MakeClauseFrame("");

	  for (k=0;(key = lock_strings[k]);k++) {
	    Gal_SetProp(fr, key, Gal_IntObject(1));
	    /* We're done. Free it. */
	    free(key);
	  }
	  GalSS_SessionDecodeLocks(fr, &lock_mask, &lock_value);
	
	  Gal_FreeFrame(fr);
	  free(lock_strings);
	
	  while (rule_buffer[i]) {
	    rule_buffer[i]->lock_mask = lock_mask;
	    rule_buffer[i++]->lock_value = lock_value;
	  }
	}
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_LOG_DIR:
      temp_s = program_string(objects, 0, &error);

      if (error == GAL_NO_ERROR) {
	GalHUB_SetHubLogTopDataDir(control->hub, temp_s,
				   Gal_ProgramTagString(tag));
	control->hc->log_dir_key = Gal_ProgramTagString(tag);
	GalHUB_SetHubGlobal(control->hub, Gal_ProgramTagString(tag),
			    Gal_StringObject(temp_s));
      }
      break;
    case GAL_PGM_LOG_IN:
      if (current_rule_index > -1) {
	EntityPair **kps = program_entity_pairs(objects,
						GAL_MESSAGE_NAMESPACE,
						-1, 0, &pp, &error);
	int i = 0;

	if (error == GAL_NO_ERROR) {
	  while (rule_buffer[i]) {
	    if (i > 0) {
	      rule_buffer[i++]->in_log_kps = copy_entity_pairs(kps);
	    } else {
	      rule_buffer[i++]->in_log_kps = kps;
	    }
	  }
	}
      } else if (control->current_program) {
	/* This will redefine log_in */
	control->current_program->log_in = program_entity_pairs(objects,
								GAL_MESSAGE_NAMESPACE,
								-1, 0, &pp, &error);
      } else if (control->current_message) {
	/* This will redefine log_in */
	control->current_message->log_in = program_entity_pairs(objects,
								GAL_MESSAGE_NAMESPACE,
								-1, 0, &pp, &error);
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_LOG_OUT:
      if (current_rule_index > -1) {
	EntityPair **kps = program_entity_pairs(objects,
						GAL_MESSAGE_NAMESPACE,
						-1, 0, &pp, &error);
	int i = 0;

	if (error == GAL_NO_ERROR) {	
	  while (rule_buffer[i]) {
	    if (i > 0) {
	      rule_buffer[i++]->out_log_kps = copy_entity_pairs(kps);
	    } else {
	      rule_buffer[i++]->out_log_kps = kps;
	    }
	  }
	}
      } else if (control->current_program) {
	/* This will redefine log_out */
	control->current_program->log_out = program_entity_pairs(objects,
								 GAL_MESSAGE_NAMESPACE,
								 -1, 0, &pp, &error);
      } else if (control->current_message) {
	/* This will redefine log_out */
	control->current_message->log_out = program_entity_pairs(objects,
								 GAL_MESSAGE_NAMESPACE,
								 -1, 0, &pp, &error);
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_LOG_HUB_ACTIVITY:
      hub_activity = program_strings(objects, NULL, &error);

      if (error == GAL_NO_ERROR) {
	int i;
		
	for (i = 0; hub_activity[i]; i++) {
	  char *activity = hub_activity[i];
	
	  if (Gal_StringEq(activity, "serve_any_session")) {
	    control->hub->log_record->log_serve_any_session = 1;
	  } else if (Gal_StringEq(activity, "serve_this_session_only")) {
	    control->hub->log_record->log_serve_this_session_only = 1;
	  } else if (Gal_StringEq(activity, "get_session_lock")) {
	    control->hub->log_record->log_get_session_lock = 1;
	  } else if (Gal_StringEq(activity, "release_session_lock")) {
	    control->hub->log_record->log_release_session_lock = 1;
	  } else if (Gal_StringEq(activity, "alarm_activity")) {
	    control->hub->log_record->log_alarm_activity = 1;
	  } else if (Gal_StringEq(activity, "system_errors")) {
	    control->hub->log_record->log_system_errors = 1;
	  }
	}
	free_list(hub_activity);
	hub_activity = (char **) NULL;
      }
      break;
    case GAL_PGM_LOG_VERSION:
      temp_s = program_string(objects, 0, &error);

      if (error == GAL_NO_ERROR) {
	control->hub->log_record->user_version = _gal_strdup(temp_s);
	control->hc->log_version_key = Gal_ProgramTagString(tag);
	GalHUB_SetHubGlobal(control->hub, Gal_ProgramTagString(tag),
			    Gal_StringObject(temp_s));
      }
      break;
    case GAL_PGM_MODE:      
      program_modes(control, objects, &error);
      break;
    case GAL_PGM_OPERATIONS:      
      if (current_service_type) {
	if (current_service_type->operations == NULL) {
	  current_service_type->operations = program_strings(objects, NULL, &error);
	}
      } else {
	error = GAL_NO_SERVER;
      }
      break;
    case GAL_PGM_OUT:
      if (current_rule_index > -1) {
	EntityPair **kps = program_entity_pairs(objects,
						GAL_MESSAGE_NAMESPACE,
						GAL_TOKEN_NAMESPACE,
						0, &pp, &error);
	int i = 0;

	if (error == GAL_NO_ERROR) {
	  while (rule_buffer[i]) {
	    if (i > 0) {
	      rule_buffer[i++]->out_kps = copy_entity_pairs(kps);
	    } else {
	      rule_buffer[i++]->out_kps = kps;
	    }
	  }
	}
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_OUT_LANG:
      temp_s = program_string(objects, 0, &error);

      if (error == GAL_NO_ERROR) {
	GalHUB_DeleteHubGlobal(control->hub, DEFAULT_OUT_LANG_KEY);
	control->hc->out_lang_key = Gal_ProgramTagString(tag);
	GalHUB_SetHubGlobal(control->hub, Gal_ProgramTagString(tag),
			    Gal_StringObject(temp_s));
      }
      break;
    case GAL_PGM_PARAM:
      if (current_rule_index > -1) {
	EntityPair **kps = program_entity_pairs(objects,
						-1, GAL_MESSAGE_NAMESPACE,
						1, &pp, &error);
	int i = 0;

	if (error == GAL_NO_ERROR) {	
	  while (rule_buffer[i]) {
	    if (i > 0) {
	      rule_buffer[i++]->param_kps = copy_entity_pairs(kps);
	    } else {
	      rule_buffer[i++]->param_kps = kps;
	    }
	  }
	}
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_PARA_LANG:
      temp_s = program_string(objects, 0, &error);

      if (error == GAL_NO_ERROR) {
	GalHUB_DeleteHubGlobal(control->hub, DEFAULT_PARA_LANG_KEY);
	control->hc->para_lang_key = Gal_ProgramTagString(tag);
	GalHUB_SetHubGlobal(control->hub, Gal_ProgramTagString(tag),
			    Gal_StringObject(temp_s));
      }
      break;
      /* See above for GAL_PGM_PORT: */
    case GAL_PGM_PROGRAM:
      /* We have to hash first, because this is where
	 the builtin server is added, and otherwise the
	 builtin server won't count against the number of
	 servers available when NO_SERVERS is checked.
	 I could just remove the NO_SERVERS check, but I've
	 chosen to do this instead. */
      maybe_hash_operations(control, &error);

      if (error == GAL_NO_ERROR) {
	/* SAM 10/26/99: We have to make sure that there is never
	   both a current program and current message, so that
	   LOG_IN: and LOG_OUT: will be associated correctly. */
	finalize_program_and_message(control);
	control->current_program = add_program(objects, control, &error);
      }
      break;
    case GAL_PGM_PROPERTIES:
      temp_kp = program_key_value_key_pairs(objects, &error);
      /* Both service providers and service types have properties,
	 the former overriding the latter. These properties are the
	 default properties assigned to the service provider,
	 until the service provider overrides them. So if there's
	 both a provider and a type, update both and do an override
	 for the provider. */

      if (error == GAL_NO_ERROR) {
	int used = 0;
	
	if (current_service_type) {
	  current_service_type->properties = temp_kp;
	  used = 1;
	}
	if (current_service_provider) {
	  _GalHUB_CopyInitKeys(current_service_provider->properties,
			       temp_kp, 1);
	}
	if ((!current_service_type) && (!current_service_provider)) {
	  error = GAL_NO_SERVER;
	}
	if (!used) {
	  free_key_pairs(temp_kp);
	}
      } else if (temp_kp) {
	free_key_pairs(temp_kp);
      }	
      break;
    case GAL_PGM_PROVIDER_NAME:
      if (current_rule_index > -1) {
	/* The provider description should be a single value or
	   keyword. */
	Gal_ProgramEntity *e;
	int i = 0;

	old_default = pp.default_namespace;
	pp.default_namespace = &(HubNamespaceTable[GAL_TOKEN_NAMESPACE]);
	e = Gal_CreateProgramEntity(objects, &pp,
				    1, 1, 1);
	pp.default_namespace = old_default;	

	if (error == GAL_NO_ERROR) {
	  if (!e) {
	    error = GAL_BAD_SERVICE_TYPE_NAME;
	  } else {
	    while (rule_buffer[i]) {
	      if (i > 0) {
		rule_buffer[i++]->provider = Gal_CopyProgramEntity(e);
	      } else {
		rule_buffer[i++]->provider = e;
	      }
	    }
	  }
	}
      } else {
	error = GAL_BAD_PROVIDER_NAME;
      }
      break;
    case GAL_PGM_PROVIDER_ID:
      if (current_service_provider) {
	/* The provider description will be a literal. */
	Gal_ProgramEntity *e = Gal_CreateProgramEntity(objects, &pp,
						       0, 1, 0);

	if (!e) {
	  error = GAL_BAD_PROVIDER_ID;
	} else {
	  int provider_id = -1;
	  char *provider_name = (char *) NULL;
	  char *stype_name;
	  char *entity_str = Gal_StringValue((Gal_Object) e->entity_data);
	
	  /* This had better damn well be a non-number, because
	     if it's a number, it's just going to get ignored. */
	  stype_name = Gal_SplitServiceTypeName(entity_str, &provider_id,
						&provider_name);
	  if (stype_name || (provider_id != -1) || (!provider_name)) {
	    error = GAL_BAD_PROVIDER_ID;
	    if (stype_name) free(stype_name);
	    if (provider_name) free(provider_name);
	  } else {
	    /* Store the name. There may already be one because
	       of the SERVER: entry. */
	    if (current_service_provider->id_name)
	      free(current_service_provider->id_name);
	    current_service_provider->id_name = provider_name;
	  }
	  Gal_FreeProgramEntity(e);
	}	
      } else {
	error = GAL_BAD_PROVIDER_ID;
      }
      break;
    case GAL_PGM_MESSAGE:
      /* We have to hash first, because this is where
	 the builtin server is added, and otherwise the
	 builtin server won't count against the number of
	 servers available when NO_SERVERS is checked.
	 I could just remove the NO_SERVERS check, but I've
	 chosen to do this instead. */
      maybe_hash_operations(control, &error);

      if (error == GAL_NO_ERROR) {
	/* Shut off and store everything. */
	finalize_program_and_message(control);
	control->current_message = add_message(objects, control, &error);
      }
      break;
    case GAL_PGM_RETRIEVE:
      if (current_rule_index > -1) {
	KeyPair **kp = program_key_pairs(objects, &error);
	int i = 0;

	if (error == GAL_NO_ERROR) {
	  while (rule_buffer[i]) {
	    if (i > 0) {
	      rule_buffer[i++]->retrieve_kps = copy_key_pairs(kp);
	    } else {
	      rule_buffer[i++]->retrieve_kps = kp;
	    }
	  }
	}
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_RULE:
      maybe_hash_operations(control, &error);
      if (error != GAL_NO_ERROR)
	break;
      if (!control->current_program)
	control->current_program = add_program(NULL, control, &error);
      if (error != GAL_NO_ERROR)
	break;
      if (control->current_program) {
	RULE *new_r;
	
	if (current_rule_index == -1) {
	  new_r = add_rule(control->current_program,
			   pp.extended_syntax, &error);
	} else {
	  new_r = copy_rule(control->current_program,
			    rule_buffer[current_rule_index],
			    &error);
	}
	
	if (error != GAL_NO_ERROR)
	  break;

	if (!new_r) {
	  error = GAL_NO_RULE;
	  break;
	}

	/* Now, add the condition to the current rule. */
	
	program_condition(objects, new_r, control, &error, &pp);

	if (error == GAL_NO_ERROR) {
	  if (max_rules < (current_rule_index + 2)) {
	    rule_buffer = expand_rule_buffer(rule_buffer, (current_rule_index + 2) - max_rules);
	    max_rules = current_rule_index + 2;
	  }
	  current_rule_index++;
	  rule_buffer[current_rule_index] = new_r;
	} else {
	  /* Free the rule. But right now, I'm just giving up. */
	}
      }
      else
	error = GAL_NO_PROGRAM;
      break;
    case GAL_PGM_SERVER:
      if (control->hc->programs && control->hc->programs[0]) {
	GalUtil_Warn("All servers should be defined before defining programs");
	error = GAL_SET_SERVERS_FIRST;
      } else {
	current_service_type = add_service_type(objects, control,
						&current_service_provider,
						&error);
	current_server = current_service_type;
      }
      break;
    case GAL_PGM_SERVERS:
      if (control->hub->stypes &&
	  (Gal_PointerBufferSize(control->hub->stypes) > 0)) {
	GalUtil_Warn("SERVERS: should be set before defining individual servers");
	error = GAL_SET_SERVERS_FIRST;
      } else {
	char **names = program_strings(objects, NULL, &error);

	if (error == GAL_NO_ERROR) {
	  int i;
	  
	  control->hc->active_servers =
	    Gal_MakePointerBuffer(NULL,
				  GAL_PROVIDER_SPEC_PTYPE,
				  0, 0, 1, 1,
				  NULL, 10, 0);
	  for (i = 0; names[i]; i++) {
	    Gal_PointerBufferAdd(control->hc->active_servers,
				 GalHUB_SplitServerAndLocation(names[i], 1));
	  }
	}
	free_list(names);
      }
      break;
    case GAL_PGM_SERVICE_PROVIDER:
      if (control->hc->programs && control->hc->programs[0]) {
	GalUtil_Warn("All servers should be defined before defining programs");
	error = GAL_SET_SERVERS_FIRST;
      } else {
	int num_stypes = 0;
	char **stype_strings = program_strings(objects, &num_stypes, &error);
	
	if (error == GAL_NO_ERROR) {
	  int k;

	  if (num_stypes == 0) {
	    error = GAL_NO_SERVER;
	  } else {
	    current_service_provider = GalHUB_NewServiceProvider((char *) NULL, -1, (SERVICE_TYPE *) NULL);
	    if (current_service_provider) {
	      /* Make sure they're all there. */
	      SERVICE_TYPE **stypes = (SERVICE_TYPE **) calloc(num_stypes,
							       sizeof(SERVICE_TYPE *));
	      for (k = 0; k < num_stypes; k++) {
		stypes[k] = GalHUB_FindServiceType(control->hub, stype_strings[k]);
		if (!stypes[k]) {
		  error = GAL_NO_SERVER;
		  break;
		}
	      }
	      if (error) {
		GalHUB_LRemoveServiceProvider((HUB *) NULL, current_service_provider, 0, 0);
		current_service_provider = (SERVICE_PROVIDER *) NULL;
	      } else {
		for (k = 0; k < num_stypes; k++) {
		  GalHUB_AddServiceProviderStype(current_service_provider,
						 stypes[k]);
		}
	      }
	      free(stypes);
	    } else {
	      error = GAL_ALLOCATION_FAILED;
	    }
	  }
	}
	free_list(stype_strings);
      }
      break;      
    case GAL_PGM_SERVICE_TYPE:
      /* This is like the entry for GAL_PGM_SERVER, except
	 it sets the current service type. Also, the location
	 can't be set as a side effect. */
      if (control->hc->programs && control->hc->programs[0]) {
	GalUtil_Warn("All servers should be defined before defining programs");
	error = GAL_SET_SERVERS_FIRST;
      } else {
	current_service_type = add_service_type(objects, control,
						(SERVICE_PROVIDER **) NULL, &error);
      }
      break;
    case GAL_PGM_SESSION_ID:
      temp_s = program_string(objects, 0, &error);

      if (error == GAL_NO_ERROR) {
	control->hc->session_id_key = Gal_ProgramTagString(tag);
	GalHUB_SetHubSessionId(control->hub, temp_s,
			       Gal_ProgramTagString(tag));
      }
      break;
    case GAL_PGM_SET:
      if (current_rule_index > -1) {
	EntityPair **kps = program_entity_pairs(objects,
						-1, GAL_TOKEN_NAMESPACE,
						1, &pp, &error);
	int i = 0;

	if (error == GAL_NO_ERROR) {
	  while (rule_buffer[i]) {
	    if (i > 0) {
	      rule_buffer[i++]->set_kps = copy_entity_pairs(kps);
	    } else {
	      rule_buffer[i++]->set_kps = kps;
	    }
	  }
	}
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_STORE:
      if (current_rule_index > -1) {
	int count;
	char **store = program_keys(objects, &count, &error);
	int i = 0;

	if (error == GAL_NO_ERROR) {
	  while (rule_buffer[i]) {
	    if (i > 0) {
	      char **new_store = calloc(count + 1, sizeof(char *));
	      int k;
	      for (k = 0; store[k]; k++) {
		new_store[k] = _gal_strdup(store[k]);
	      }
	      rule_buffer[i++]->store_vars = new_store;
	    } else {
	      rule_buffer[i++]->store_vars = store;
	    }
	  }
	}
      } else {
	error = GAL_NO_RULE;
      }
      break;
    case GAL_PGM_SYNTH_LANG:
      temp_s = program_string(objects, 0, &error);

      if (error == GAL_NO_ERROR) {
	GalHUB_DeleteHubGlobal(control->hub, DEFAULT_SYNTH_LANG_KEY);
	control->hc->synth_lang_key = Gal_ProgramTagString(tag);
	GalHUB_SetHubGlobal(control->hub, Gal_ProgramTagString(tag),
			    Gal_StringObject(temp_s));
      }
      break;
    case GAL_PGM_TIMESTAMP:
      temp_ss = program_strings(objects, NULL, &error);

      if (error == GAL_NO_ERROR) {
	int i;
	char *op=NULL, *temp, *server_temp;
	
	control->hc->timestamps = temp_ss;
	for(i = 0; (op = temp_ss[i]); i++) {
	  /* SAM 4/29/02: We need to do two things. First, we
	     need to index these things both under the literal
	     string and under the description they provide.
	     We also need to make sure that we don't override
	     any information already provided. */
	  /* So first, here's the literal string. */
	  GalHUB_AddMessageToTimestampTable(control->hub, op,
					    (EntityPair **) NULL,
					    (EntityPair **) NULL,
					    (GalSS_ProviderSpec *) NULL,
					    1, 0, 0);
	  
	  /* And now, here's the digested string. */
	  
	  /* SAM 2/8/00: For consistency, Gal_SplitOperationName()
	     always returns new memory. */
	  
	  server_temp = (char *) NULL;
	  temp = Gal_SplitOperationName(op, &server_temp);

	  /* If they're different, record this one too, and
	     use the digested server name. Actually, don't even
	     check, because this isn't a literal match. */
	  GalHUB_AddMessageToTimestampTable(control->hub, temp,
					    (EntityPair **) NULL,
					    (EntityPair **) NULL,
					    GalHUB_SplitServerName(server_temp), 0, 0, 0);
	  if (server_temp) free(server_temp);
	  free(temp);
	}
	temp_ss = (char **) NULL;
      }
      break;
    case GAL_PGM_USER_ID:
      temp_s = program_string(objects, 0, &error);

      if (error == GAL_NO_ERROR) {
	control->hc->user_id_key = Gal_ProgramTagString(tag);
	GalHUB_SetHubUserID(control->hub, temp_s, Gal_ProgramTagString(tag));
      }
      break;
    default:
      error = GAL_UNKNOWN_PROGRAM_TAG;
    }

    /* Free the program objects, but not the Gal_Objects in them. */
    Gal_FreeProgramObject(objects, 0);
    objects = (ProgramObject *) NULL;
  }

  /* Make absolutely sure that the operations are hashed, even
     if there are no programs or messages. */

  maybe_hash_operations(control, &error);

  /* Update the active servers. */

  if (control->hc->active_servers)
    GalHUB_SetHubActiveServers(control->hub, control->hc->active_servers);

  if ((pp.error != GAL_NO_ERROR) && (error == GAL_NO_ERROR))
    error = pp.error;

  if (!done && (error == GAL_NO_ERROR))
    error = GAL_STOPPED_BEFORE_EOF;

  *error_ptr = error;
  /* Make sure you free the InputStream. */
  free(pp.gs);
  Gal_ClearProgramParser(&pp);
  free(rule_buffer);
}


/*
 *  read_included_file prepends the working directory
 *  (if necessary) and loads the included file.
 */

extern
int _Gal_CreateFullPath(char *path, int path_size, int num_components, ...);
extern
int _Gal_SplitPath(char *out_path, int path_size, const char *in_path);

static void read_included_file(char *filename, char *directory,
			       ControlFileParseState *control,
			       int *error_ptr, int already_extended)
{
  FILE *fp;
  char program_file[MAX_FNAME_LENGTH];
  int error = GAL_NO_ERROR;
  char current_directory[MAX_FNAME_LENGTH];

  if (Gal_IsFullPath(filename) || !directory || !directory[0])
    strcpy(program_file, filename);
  else
    _Gal_CreateFullPath(program_file, MAX_FNAME_LENGTH, 2,
			directory, filename);

  _Gal_SplitPath(current_directory, MAX_FNAME_LENGTH, program_file);

  /* SAM 10/22/01: Opening in binary mode is harmless on Unix,
     but crucial on Windows, since ftell() is used by the file reader
     and doesn't work correctly on Windows in text mode when the
     file doesn't contain "proper" line terminations (i.e., only
     \n instead of \r\n, or whatever it is. */
  if ((fp = fopen(program_file, "rb")) == NULL)
  {
    GalUtil_Warn("Failed to open program file %s", program_file);
    error = GAL_BAD_FILENAME;
  }
  else
  {
    control->current_files = Gal_PushProgramFile(program_file, fp, control->current_files);
    read_hub_control(control, current_directory, &error, already_extended);
    if (error)
    {
      Gal_PrintProgramError(control->current_files, error);
      error = GAL_BAD_INCLUDED_FILE;
    }
    control->current_files = Gal_PopProgramFile(control->current_files);
  }
  if (error_ptr)
    *error_ptr = error;
}

HubControlStruct *load_hub_control(HUB *h, char *filename,
				   int *error_ptr, int control_flags)
{
  char directory[MAX_FNAME_LENGTH];
  int error = GAL_NO_ERROR;
  ControlFileParseState *control = new_ControlFileParseState(h, control_flags);
  HubControlStruct *hub = (HubControlStruct *) NULL;
  
  if (!HubTagsInitialized)
  {
    Gal_AddProgramTags(HubProgramTagMap);
    Gal_AddErrorTags(HubErrorTagMap);
    HubTagsInitialized = 1;
  }

  directory[0] = '\0';
  if ((control->current_files = Gal_OpenControlFile(filename, directory)))
  {
    read_hub_control(control, directory[0] ? directory : NULL, &error, 0);
    /* Shut off and store everything. */
    /* SAM 8/23/02: Scott Cyphers notes that this must happen here, and not
       in the program file parsing itself, because programs don't
       need to respect file boundaries. */
    finalize_program_and_message(control);
    if (error)
    {
      Gal_PrintProgramError(control->current_files, error);
      *error_ptr = error;
    }
    Gal_CloseControlFile(control->current_files);
    control->current_files = NULL;
    hub = control->hc;
    hub->hub = h;
  }
  free_ControlFileParseState(control);
  return hub;
}

/* SAM 6/25/02: In order to do the memory analysis in
   the Hub, I need to clean up what I can clean up.
   This shouldn't depend on global variables, but it does. */

static void free_rule(RULE *r)
{
  int i;
  
  if (r->server_name)
    free(r->server_name);
  if (r->op_name)
    free(r->op_name);
  free_entity_pairs(r->in_kps);
  free_entity_pairs(r->in_log_kps);
  free_entity_pairs(r->out_kps);
  free_entity_pairs(r->error_kps);
  free_entity_pairs(r->out_log_kps);
  free_key_pairs(r->retrieve_kps);
  free_list(r->store_vars);
  if (r->del_vars) {
    for (i = 0; r->del_vars[i]; i++) {
      Gal_FreeProgramEntity(r->del_vars[i]);
    }
    free(r->del_vars);
  }
  free_entity_pairs(r->param_kps);
  free_entity_pairs(r->set_kps);
  free_key_pairs(r->alarm_kps);
  if (r->tests)
    Gal_FreeTests(r->tests);
  if (r->reply_continuations) {
    for (i = 0; r->reply_continuations[i]; i++) {
      Gal_FreeFrame(r->reply_continuations[i]);
    }
    free(r->reply_continuations);
  }
  if (r->error_continuations) {
    for (i = 0; r->error_continuations[i]; i++) {
      Gal_FreeFrame(r->error_continuations[i]);
    }
    free(r->error_continuations);
  }
  if (r->provider)
    Gal_FreeProgramEntity(r->provider);
  free(r);
}

static void free_program(PROGRAM *prog)
{
  int i;
  
  free(prog->name);
  if (prog->mode)
    free(prog->mode);
  for (i = 0; i < prog->num_rules; i++) {
    free_rule(prog->rules[i]);
  }
  if (prog->rules)
    free(prog->rules);
  
  free_entity_pairs(prog->log_in);
  free_entity_pairs(prog->log_out);
  
  free(prog);
}

void free_hub_control(PROGRAM **global_progs)
{
  int i;
  if (global_progs) {
    for(i=0; global_progs[i]; i++) {
      free_program(global_progs[i]);
    }
    free(global_progs);
  }
  Gal_FreeProgramTags();
  Gal_FreeErrorTags();
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
