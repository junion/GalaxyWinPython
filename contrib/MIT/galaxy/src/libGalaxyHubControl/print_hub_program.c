/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include "galaxy/generic-server.h"
#include "hub.h"
#include "hub_program.h"

static void print_program_key_pair_keys(FILE *fp, KeyPair **kp_list);
static void print_program_key_pair_key_values(FILE *fp, KeyPair **kp_list);
static void print_program_list(FILE *fp, char **strings);

/*  print a Gal_Object list in program format
 *  (strings get stripped of quotes)
 */

static void print_program_key_pair_keys(FILE *fp, KeyPair **kp_list)
{
  KeyPair *kp;
  int num_pairs = 0;
  Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;

  while((kp = kp_list[num_pairs++]))
  {
    if (kp->key && kp->value)
       GalUtil_fprintf(fp, " (%s %s)", kp->key, Gal_ObjectString(kp->value, &buf));
    else if (kp->key)
      GalUtil_fprintf(fp, " %s", kp->key);
  } 
  if (buf) Gal_FreeStringBuffer(buf);
  GalUtil_fprintf(fp, "\n");
}

static void print_program_entity_pairs(FILE *fp, EntityPair **ep_list)
{
  EntityPair *ep;
  int num_pairs = 0;
  Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;

  if (ep_list) {
    while((ep = ep_list[num_pairs++])) {
      if (ep->tag) {
	GalUtil_fprintf(fp, " %s", Gal_ProgramTagString(ep->tag));
      } else if (Gal_ProgramEntitiesEqual(ep->source, ep->target)) {
	char *s = Gal_FormatEntity(ep->source, 0, &buf);
	if (s)
	  GalUtil_fprintf(fp, " %s", s);
      } else {
	char *source_string = Gal_FormatEntity(ep->source, 0, &buf);
	char *target_string;
      
	if (source_string) {
	  /* Must copy, since we're reusing the buffer. */
	  source_string = _gal_strdup(source_string);
	  target_string = Gal_FormatEntity(ep->target, 0, &buf);
	  if (target_string) {
	    GalUtil_fprintf(fp, " (%s %s)", target_string, source_string);
	  }
	  free(source_string);
	}
      }
    }
  }
  if (buf) Gal_FreeStringBuffer(buf);
  GalUtil_fprintf(fp, "\n");
}

static void print_program_entities(FILE *fp, Gal_ProgramEntity **e_list)
{
  Gal_ProgramEntity *e;
  int num_entities = 0;
  Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;

  while((e = e_list[num_entities++])) {
    char *s = Gal_FormatEntity(e, 0, &buf);
    if (s)
      GalUtil_fprintf(fp, " %s", s);
  }
  if (buf) Gal_FreeStringBuffer(buf);
  GalUtil_fprintf(fp, "\n");
}

static void print_program_key_pair_key_values(FILE *fp, KeyPair **kp_list)
{
  KeyPair *kp;
  int num_pairs = 0;
  Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;

  while((kp = kp_list[num_pairs++]))
  {
    GalUtil_fprintf(fp, " %s", kp->key);
    if (kp->value)
      GalUtil_fprintf(fp, " %s", Gal_ObjectString(kp->value, &buf));
    else
      break;
  }
  if (buf) Gal_FreeStringBuffer(buf);
  GalUtil_fprintf(fp, "\n");
}

static void print_program_list(FILE *fp, char **objects)
{
  int i;
  
  if (objects) {
    for (i = 0; objects[i]; i++) {
      GalUtil_fprintf(fp, " %s", objects[i]);
    }
  }
  GalUtil_fprintf(fp, "\n");
}

/* print a ServerStruct in program format */

void print_server(FILE *fp, SERVICE_TYPE *server)
{
  if (server == NULL)
    return;

  if (server->name)
    GalUtil_fprintf(fp, "\nSERVICE_TYPE: %s\n", server->name);
  else
    return;

  if (server->operations)
  {
    GalUtil_fprintf(fp, "OPERATIONS:");
    print_program_list(fp, server->operations);
  }
  if (server->listener_port > -1) {
    GalUtil_fprintf(fp, "PORT: %d\n", server->listener_port);
  }
  
  if (server->init_kps)
  {
    GalUtil_fprintf(fp, "INIT:");
    print_program_key_pair_key_values(fp, server->init_kps);
  }
  if (server->properties) {
    GalUtil_fprintf(fp, "PROPERTIES:");
    print_program_key_pair_key_values(fp, server->properties);
  }
  if (server->conditions) {
    GalUtil_fprintf(fp, "CONDITIONS: ");
    Gal_PrintTests(fp, server->conditions, 0);
    GalUtil_fprintf(fp, "\n");
  }
  if (server->in) {
    GalUtil_fprintf(fp, "IN: ");
    print_program_entity_pairs(fp, server->in);
  }
}

static int print_frame_key_pair(const char *key, Gal_Object val, void *data)
{
  FILE *fp = (FILE *) data;
  Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;

  GalUtil_fprintf(fp, " %s", key);
  if (val)
    GalUtil_fprintf(fp, " %s", Gal_ObjectString(val, &buf));
  if (buf) Gal_FreeStringBuffer(buf);
  return 1;
}     

void print_location(FILE *fp, SERVICE_PROVIDER *loc)
{
  int i;
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(loc->stypes);
  int num_stypes = Gal_PointerBufferSize(loc->stypes);
  
  GalUtil_fprintf(fp, "\nSERVICE_PROVIDER:");
  for (i = 0; i < num_stypes; i++) {
    GalUtil_fprintf(fp, " %s", stypes[i]->name);
  }
  GalUtil_fprintf(fp, "\n");
  
  GalUtil_fprintf(fp, "LOCATION: %s:%d\n",
	  loc->host,
	  loc->port);
  if (loc->init_kps) {
    GalUtil_fprintf(fp, "INIT:");
    print_program_key_pair_key_values(fp, loc->init_kps);
  }
  if (loc->properties && (Gal_NumProperties(loc->properties) > 0)) {
    GalUtil_fprintf(fp, "PROPERTIES:");
    Gal_DoProperties(loc->properties, print_frame_key_pair, (void *) fp);
    GalUtil_fprintf(fp, "\n");
  }
  if (loc->conditions) {
    GalUtil_fprintf(fp, "CONDITIONS: ");
    Gal_PrintTests(fp, loc->conditions, 0);
    GalUtil_fprintf(fp, "\n");
  }
  if (loc->in) {
    GalUtil_fprintf(fp, "IN: ");
    print_program_entity_pairs(fp, loc->in);
  }
  if (loc->id_name) {
    GalUtil_fprintf(fp, "PROVIDER_ID: \"[%s]\"\n", loc->id_name);
  }
}


/* print a RuleStruct in program format */

void print_rule(FILE *fp, RULE *rule)
{
  if (rule == NULL)
    return;

  GalUtil_fprintf(fp, "\n");
  
  GalUtil_fprintf(fp, "RULE: ");
  
  if (rule->tests) {
    Gal_PrintTests(fp, rule->tests, 0);
    GalUtil_fprintf(fp, " ");
  }
  GalUtil_fprintf(fp, "--> ");
  
  if (rule->server_name)
    GalUtil_fprintf(fp, "%s.", rule->server_name);
  GalUtil_fprintf(fp, "%s\n", rule->op_name);

  if (rule->provider) {
    Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;
    char *s = Gal_FormatEntity(rule->provider, 0, &buf);

    if (s) {
      GalUtil_fprintf(fp, "PROVIDER_NAME: %s\n", s);
    } 
    if (buf) Gal_FreeStringBuffer(buf);
  }

  if (rule->param_kps) {
    GalUtil_fprintf(fp, "PARAM:");
    print_program_entity_pairs(fp, rule->param_kps);
  }
  if (rule->retrieve_kps) {
    GalUtil_fprintf(fp, "RETRIEVE:");
    print_program_key_pair_keys(fp, rule->retrieve_kps);
  }
  if (rule->in_kps) {
    GalUtil_fprintf(fp, "IN:");
    print_program_entity_pairs(fp, rule->in_kps);
  }
  if (rule->set_kps) {
    GalUtil_fprintf(fp, "SET:");
    print_program_entity_pairs(fp, rule->set_kps);
  }
  if (rule->in_log_kps) {
    GalUtil_fprintf(fp, "LOG_IN:");
    print_program_entity_pairs(fp, rule->in_log_kps);
  }
  if (rule->out_kps) {
    GalUtil_fprintf(fp, "OUT:");
    print_program_entity_pairs(fp, rule->out_kps);
  }
  if (rule->out_log_kps) {
    GalUtil_fprintf(fp, "LOG_OUT:");
    print_program_entity_pairs(fp, rule->out_log_kps);
  }
  if (rule->catch_error) {
    GalUtil_fprintf(fp, "ERROR:");
    print_program_entity_pairs(fp, rule->error_kps);
  }
  if (rule->reply_continuations) {
    int i = 0;

    while (rule->reply_continuations[i]) {
      GalUtil_fprintf(fp, "CONTINUE_REPLY: ");
      Gal_PrFrameToFile(rule->reply_continuations[i], fp);
      i++;
    }
  }
  if (rule->error_continuations) {
    int i = 0;

    while (rule->error_continuations[i]) {
      GalUtil_fprintf(fp, "CONTINUE_ERROR: ");
      Gal_PrFrameToFile(rule->error_continuations[i], fp);
      i++;
    }
  }
  if (rule->store_vars) {
    GalUtil_fprintf(fp, "STORE:");
    print_program_list(fp, rule->store_vars);
  }
  if (rule->del_vars) {
    GalUtil_fprintf(fp, "DEL:");
    print_program_entities(fp, rule->del_vars);
  }
  if (rule->lock_mask || rule->lock_value) {
    GalUtil_fprintf(fp, "LOCK:");
    if (rule->lock_mask & GAL_SERVER_READS_ONLY_FROM_SESSION) {
      if (rule->lock_value & GAL_SERVER_READS_ONLY_FROM_SESSION) {
	GalUtil_fprintf(fp, " %s", GAL_HUB_SERVE_THIS_SESSION_ONLY_FRAME_KEY);
      } else {
	GalUtil_fprintf(fp, " %s", GAL_HUB_SERVE_ANY_SESSION_FRAME_KEY);
      }
    }
    if (rule->lock_mask & GAL_SESSION_WRITES_ONLY_TO_SERVER) {
      if (rule->lock_value & GAL_SESSION_WRITES_ONLY_TO_SERVER) {
	GalUtil_fprintf(fp, " %s", GAL_HUB_GET_SESSION_LOCK_FRAME_KEY);
      } else {
	GalUtil_fprintf(fp, " %s", GAL_HUB_RELEASE_SESSION_LOCK_FRAME_KEY);
      }
    }
    if (rule->lock_value & GAL_PERMANENT_LOCK) {
      GalUtil_fprintf(fp, " %s", GAL_HUB_SESSION_LOCK_PERMANENT_FRAME_KEY);
    }
    GalUtil_fprintf(fp, "\n");
  }
  if (rule->alarm_kps) {
    GalUtil_fprintf(fp, "ALARM:");
    print_program_key_pair_key_values(fp, rule->alarm_kps);
  }
  if (rule->control) {
    GalUtil_fprintf(fp, "CONTROL:");
    /* These are now canonicalized. They may reflect the
       influence of none! or destroy!. */
    if (rule->control & GAL_CONTROL_RETURN)
      GalUtil_fprintf(fp, " :return");
    if (rule->control & GAL_CONTROL_ASYNCHRONOUS)
      GalUtil_fprintf(fp, " :asynchronous");
    if (rule->control & GAL_CONTROL_NO_RESULT)
      GalUtil_fprintf(fp, " :no_result");
    GalUtil_fprintf(fp, "\n");
  }
}

/* print a ProgramStruct in program format */

void print_program(FILE *fp, char *header, PROGRAM *program)
{
  int i;

  if (program == NULL)
    return;

  if (program->name)
    GalUtil_fprintf(fp, "\n%s %s\n", header, program->name);
  else
    return;

  if (program->log_in) {
    GalUtil_fprintf(fp, "LOG_IN:");
    print_program_entity_pairs(fp, program->log_in);
  }
  if (program->log_out) {
    GalUtil_fprintf(fp, "LOG_OUT:");
    print_program_entity_pairs(fp, program->log_out);
  }    

  if (program->num_rules) {
    for (i=0; i < program->num_rules; i++)
      print_rule(fp, program->rules[i]);
  }
}

/* print a HubControlStruct as a hub control file */

void print_hub_control(FILE *fp, HubControlStruct *hub)
{
  int i;
  Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;
  SERVICE_TYPE **stypes;
  SERVICE_PROVIDER **providers;  
  int num_stypes, num_providers;
  
  if (hub == NULL)
    return;

  if (hub->domain_key)
    GalUtil_fprintf(fp, "%s %s\n", hub->domain_key,
		    Gal_StringValue(GalHUB_GetHubGlobal(hub->hub, hub->domain_key)));

  if (hub->user_id_key || hub->session_id_key)
    GalUtil_fprintf(fp, "\n");
  if (hub->user_id_key)
    GalUtil_fprintf(fp, "%s %s\n", hub->user_id_key,
		    Gal_StringValue(GalHUB_GetHubGlobal(hub->hub, hub->user_id_key)));
  if (hub->session_id_key)
    GalUtil_fprintf(fp, "%s %s\n", hub->session_id_key,
		    Gal_ObjectString(GalHUB_GetHubGlobal(hub->hub, hub->session_id_key),
				     &buf));

  if (hub->kv_lang_key || hub->out_lang_key ||
      hub->para_lang_key || hub->synth_lang_key)
    GalUtil_fprintf(fp, "\n");
  if (hub->kv_lang_key)
    GalUtil_fprintf(fp, "%s %s\n", hub->kv_lang_key,
		    Gal_StringValue(GalHUB_GetHubGlobal(hub->hub, hub->kv_lang_key)));
  if (hub->out_lang_key)
    GalUtil_fprintf(fp, "%s %s\n", hub->out_lang_key,
		    Gal_StringValue(GalHUB_GetHubGlobal(hub->hub, hub->out_lang_key)));
  if (hub->para_lang_key)
    GalUtil_fprintf(fp, "%s %s\n", hub->para_lang_key,
		    Gal_StringValue(GalHUB_GetHubGlobal(hub->hub, hub->para_lang_key)));
  if (hub->synth_lang_key)
    GalUtil_fprintf(fp, "%s %s\n", hub->synth_lang_key,
		    Gal_StringValue(GalHUB_GetHubGlobal(hub->hub, hub->synth_lang_key)));

  if (hub->initial_reply_key || hub->initial_token_key)
    GalUtil_fprintf(fp, "\n");
  if (hub->initial_reply_key)
  {
     GalUtil_fprintf(fp, "%s %s\n", hub->initial_reply_key,
		     Gal_StringValue(GalHUB_GetHubGlobal(hub->hub, hub->initial_reply_key)));
  }
  if (hub->initial_token_key)
  {
    GalUtil_fprintf(fp, "%s ", hub->initial_token_key);
    Gal_PrFrameToFile(Gal_FrameValue(GalHUB_GetHubGlobal(hub->hub, hub->initial_token_key)), fp);
  }

  if (hub->mode)
    GalUtil_fprintf(fp, "\nMODE: %s\n", hub->mode);

  if (hub->log_dir_key)
  {
    Gal_Object obj = GalHUB_GetHubGlobal(hub->hub, hub->log_dir_key);
    GalUtil_fprintf(fp, "\n%s %s\n", hub->log_dir_key, Gal_ObjectString(obj, &buf));
  }
  if (buf) Gal_FreeStringBuffer(buf);

  if (hub->log_version_key)
    GalUtil_fprintf(fp, "%s \"%s\"\n", hub->log_version_key,
		    Gal_StringValue(GalHUB_GetHubGlobal(hub->hub, hub->log_version_key)));
  
  if (hub->timestamps) {
    GalUtil_fprintf(fp, "\nTIMESTAMP:");
    print_program_list(fp, hub->timestamps);
  }

  if (hub->hub->log_record &&
      (hub->hub->log_record->log_serve_any_session ||
       hub->hub->log_record->log_serve_this_session_only ||
       hub->hub->log_record->log_get_session_lock ||
       hub->hub->log_record->log_release_session_lock ||
       hub->hub->log_record->log_alarm_activity ||
       hub->hub->log_record->log_system_errors)) {
    GalUtil_fprintf(fp, "\nLOG_HUB_ACTIVITY:");
    if (hub->hub->log_record->log_serve_any_session) {
      GalUtil_fprintf(fp, " serve_any_session");
    }
    if (hub->hub->log_record->log_serve_this_session_only) {
      GalUtil_fprintf(fp, " serve_this_session_only");
    }
    if (hub->hub->log_record->log_get_session_lock) {
      GalUtil_fprintf(fp, " get_session_lock");
    }
    if (hub->hub->log_record->log_release_session_lock) {
      GalUtil_fprintf(fp, " release_session_lock");
    }
    if (hub->hub->log_record->log_alarm_activity) {
      GalUtil_fprintf(fp, " alarm_activity");
    }
    if (hub->hub->log_record->log_system_errors) {
      GalUtil_fprintf(fp, " system_errors");
    }
    GalUtil_fprintf(fp, "\n");
  }

  if (hub->active_servers) {
    int num_specs = Gal_PointerBufferSize(hub->active_servers);
    GalSS_ProviderSpec **specs = (GalSS_ProviderSpec **) Gal_PointerBufferPointers(hub->active_servers);
    int i;
    Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;
    
    GalUtil_fprintf(fp, "\nSERVERS:");
    for (i = 0; i < num_specs; i++) {
      GalUtil_fprintf(fp, " %s", GalSS_FormatProviderSpec(specs[i], &buf));
    }
    GalUtil_fprintf(fp, "\n");
    if (buf) Gal_FreeStringBuffer(buf);
  }

  num_stypes = Gal_PointerBufferSize(hub->hub->stypes);

  if (num_stypes > 0) {
    stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(hub->hub->stypes);
    for (i=0; i < num_stypes; i++)
      print_server(fp, stypes[i]);
  }

  num_providers = Gal_PointerBufferSize(hub->hub->servers);
  
  if (num_providers > 0) {
    providers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(hub->hub->servers);
    for (i = 0; i < num_providers; i++) {
      print_location(fp, providers[i]);
    }
  }

  if (hub->programs) {
    for (i=0; hub->programs[i]; i++)
      print_program(fp, "PROGRAM:", hub->programs[i]);
  }

  if (hub->messages) {
    for (i=0; hub->messages[i]; i++)
      print_program(fp, "MESSAGE:", hub->messages[i]);
  }
}
