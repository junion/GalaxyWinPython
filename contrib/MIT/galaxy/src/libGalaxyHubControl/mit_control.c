/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdlib.h>
#include "galaxy/sysdep.h"
#include "hub_internal.h"
#include "hub_program.h"
#include "galaxy/distinguished_keys.h"

static MIT_CONTROL *getControlInfoFromToken(TOKEN *t);
static MIT_CONTROL *newControlStruct();
static void freeControlStruct(CONTROL_INFO *info);
static SERVER_MESSAGE *HC_NextOperation(HUB *h, TOKEN *t, int debug, Gal_Frame *namespace_array);

PROGRAM **global_progs = (PROGRAM **) NULL;

static PROGRAM *findProgramFromName(PROGRAM **progs, char *name)
{
  int i;
  if (progs) {
    for(i=0; progs[i]; i++) 
      if (Gal_StringEq(progs[i]->name, name))
	return progs[i];
  }
  return (PROGRAM *)NULL;
}

static void
fill_init_vars(Gal_Frame init_frame, KeyPair **kp_list)
{
  KeyPair *kp;
  int i = 0;

  if (!kp_list) return;

  while((kp = kp_list[i++]))
    Gal_SetProp(init_frame, kp->key, Gal_CopyObject(kp->value));
}

static void finalize_initial_token(HUB *true_hub, PROGRAM **programs,
				   KeyPair **startup_keys,
				   Gal_Frame init_state, char *initial_reply)
{
  /* The init_state needs to be copied. Once it's copied, we need to
     make sure it's freed at the end. */
  
  if (init_state) {
    init_state = Gal_CopyFrame(init_state);
  }
  
  /* create an initial token */
  if (!init_state && findProgramFromName(programs, "main") ) { 
    init_state = Gal_MakeFrame("main", GAL_CLAUSE);
  }
  
  if (init_state) {
    /* SAM 10/17/99: Stephanie and I have agreed that the priority for
       the initial token should be cmdline > INITIAL_TOKEN: > globals.
       So we add the globals only if they're not already there. */
    if (!Gal_GetObject(init_state, ":initialize"))
      Gal_SetProp(init_state, ":initialize", Gal_IntObject(1));
    if (!Gal_GetObject(init_state, ":domain"))
      Gal_SetProp(init_state, ":domain",
		  Gal_CopyObject(GalHUB_GetHubDefaultDomain(true_hub)));
    if (initial_reply && !Gal_GetObject(init_state, ":reply_string"))
      Gal_SetProp(init_state,":reply_string", Gal_StringObject(initial_reply));

    if (!Gal_GetObject(init_state, ":user_id"))
      Gal_SetProp(init_state, ":user_id",
		  Gal_CopyObject(GalHUB_GetHubUserID(true_hub)));

    /* SAM 10/17/99: I'm not crazy about the initial token being the
       baseline for the reinitialize message, but it seems to be.
       In any case, the startup vars can't override anything yet.
       So I'll set the Hub init state before I do anything else. */
    
    GalHUB_SetHubInitState(true_hub, Gal_CopyFrame(init_state));
    
    /* These override the hub script at hub-startup time */
    /* -init ':domain "Pegasus"' */
    if (startup_keys) {
      fill_init_vars(init_state, startup_keys);
    }

    GalHUB_SetHubToken(true_hub, GalHUB_NewToken(init_state,
						 NULL, 0, NULL,
						 (Gal_Frame) NULL));
    /* The token copies the frame, and the Hub state was
       passed a copied frame. So we'd better free this one. */
    Gal_FreeFrame(init_state);
  }
}


/* Returns -1 if there is an error, number of programs otherwise */
int
HC_ReadControlFile(HUB *true_hub, char *filename, char *startup_vars)
{
  KeyPair **startup_keys = NULL;
  int error = 0;
  HubControlStruct *control = load_hub_control(true_hub, filename, &error, 0);
  int num_progs;
  Gal_Frame init_state = (Gal_Frame) NULL;
  char *initial_reply = (char *) NULL;
  
  if (error || !control)
    return(-1);

  global_progs = control->programs;

  /* SAM 10/17/99: Yuck. The problem is that the argument to -init is
     parsed in here, when it probably ought to be in the core; but moving
     it into the core would move a lot of other things into the core, and
     we have to think about that carefully. We need to set this because
     the initial keys from INIT: aren't processed until the core for
     "reinitialize", and these keys have to be layered on top. We could
     move the production of the "reinitialize" message frame into here,
     but that seems even worse. */

  if (startup_vars) {
    startup_keys = read_key_value_string(startup_vars);
  }
  
  GalHUB_SetHubInitialKeys(true_hub, startup_keys);

  if (control->initial_token_key) {
    init_state = Gal_FrameValue(GalHUB_GetHubGlobal(true_hub, control->initial_token_key));
  }

  if (control->initial_reply_key) {
    initial_reply = Gal_StringValue(GalHUB_GetHubGlobal(true_hub, control->initial_reply_key));
  }

  free_HubControlStruct(control);

  finalize_initial_token(true_hub, global_progs, startup_keys,
			 init_state, initial_reply);

  num_progs = 0;
  if (global_progs) {
    for (num_progs = 0; global_progs[num_progs]; num_progs++);
  }
  
  GalUtil_Print(GAL_PINFO1_LEVEL, "%d service types\n%d service providers\n%d programs\n\n",  Gal_PointerBufferSize(true_hub->stypes), GalHUB_GetNumServers(true_hub), num_progs);
    
  return(num_progs);
}

void HC_FreePrograms()
{
  free_hub_control(global_progs);
}

/* in MAIN: if ridx is at the end of the line, recycle to 0 */
static int find_next_operation(TOKEN *t, int ridx, Gal_Frame *namespace_array)
{
  static int first = 1;
  int rule_index;
  RULE **rules;
  int num_rules;
  MIT_CONTROL *ctrl_info;
  extern HUB *Hub;

  /* we think this should never happen */
  if (!t || (ctrl_info = getControlInfoFromToken(t)) == NULL) 
    return(-1);	 

  if (t->destroy)
    return -1;

  rules = ctrl_info->prog->rules;
  num_rules = ctrl_info->prog->num_rules;

  if (ridx == num_rules) return(-1);

  if (first) {
    _GalHUB_ReportTokenStatus(Hub, GAL_PINFO1_LEVEL, GAL_HUB_TOKEN_STATUS, t, 0);
    first = 0;
  }

  for (rule_index = ridx; rule_index<num_rules; rule_index++) {
    if (Gal_TestConditionInNamespaces(rules[rule_index]->tests,
				      namespace_array, GAL_TOKEN_NAMESPACE) == GAL_TRUE) {
      /* only valid rules (with both server and operation)
	 are added to the program.                         */
      /* SAM 9/21/01: Well, not exactly. There can be more
	 than one service type which supports the operation,
	 and in that case, we won't decide at program file
	 load time (or even at rule selection time) which
	 is the appropriate service type. That happens later.
	 See HC_NextOperation, at least. */
      if (rules[rule_index]->server_name) {
	GalUtil_PInfo1("Found operation for token %d: %s.%s\n",
		       t->tidx,
		       rules[rule_index]->server_name,
		       rules[rule_index]->op_name);
      } else {
	GalUtil_PInfo1("Found operation for token %d: %s\n",
		       t->tidx, rules[rule_index]->op_name);
      }
      return(rule_index);
    }
  }
  return(-1);
}

static void handle_alarm_vars(RULE *rule,SESSION *session,int ridx)
{
  int i = 0;
  KeyPair *kp;

  if (!rule->alarm_kps)
    return;

  if (!session)
    return;

  /* first do enable/disable */
  while ((kp = rule->alarm_kps[i++]))
  {
    char *key = kp->key;

    if (Gal_Stringp(kp->value))
    {
      int disable = Gal_StringEq(Gal_StringValue(kp->value), GalHUB_AlarmGetDisableKey());
      if (disable)
      {
	GalHUB_SessionAlarmDisable(session, key);
      }
      else
      {
	GalHUB_SessionAlarmEnable(session, key);
      }
    }
  }

  /* then set timers */
  i = 0;
  while ((kp = rule->alarm_kps[i++]))
  {
    char *key = kp->key;

    if (Gal_Intp(kp->value))
    {
      GalHUB_SessionAlarmResetTo(session, key,Gal_IntValue(kp->value));
    }
  }
}

static char *entity_to_old_syntax_key(Gal_ProgramEntity *e)
{
  if (e->entity_type != GAL_NAMESPACE_ENTITY)
    return (char *) NULL;
  if (!((Gal_NamespaceProgramEntity *) e->entity_data)->is_default)
    return (char *) NULL;
  return ((Gal_NamespaceProgramEntity *) e->entity_data)->key;
}

static void handle_del_vars(RULE *rule, Gal_Frame fr, int ridx,
			    Gal_Frame *namespace_array)
{
  int i = 0;
  Gal_ProgramEntity *e;
  char *key;
  char session_key[1024];

  if (!rule->del_vars)
    return;

  if (rule->extended_syntax) {
    while ((e = rule->del_vars[i++])) {
      Gal_DeleteProgramEntityLocation(e, namespace_array);
    }
  } else {
    while ((e = rule->del_vars[i++])) {
      key = entity_to_old_syntax_key(e);
      if (!key)
	continue;
      GalUtil_CPInfo2(3,0,"rule %d: deleting %s\n", ridx, key);
      if (!strncmp(key, ":hub_session_", strlen(":hub_session_"))) {
	/* Remove hub_session_. */
	SESSION *session = GalHUB_SessionLookupByFrame(fr, 0);
	
	sprintf(session_key, ":%s", key + strlen(":hub_session_"));
	GalHUB_SessionDeleteVar(session, session_key);
      } else {
	Gal_DelProp(fr, key);
      }
    }	
  }
}


/* SAM 11/23/00: This function encapsulates the changes from
   key pairs to entity pairs, for best backward compatibility
   with the old style of syntax. */

static int
entity_pair_to_old_syntax_key_pair(EntityPair *ep, char **to_key_ptr,
				   char **from_key_ptr, Gal_Object *value_ptr)
{
  *value_ptr = (Gal_Object) NULL;  
  *to_key_ptr = (char *) NULL;
  *from_key_ptr = (char *) NULL;
  
  *to_key_ptr = entity_to_old_syntax_key(ep->target);
  if (!*to_key_ptr)
    return 0;
    
  if (ep->source->entity_type == GAL_OBJECT_ENTITY) {
    *value_ptr = Gal_CopyObject((Gal_Object) ep->source->entity_data);
  } else {
    *from_key_ptr = entity_to_old_syntax_key(ep->source);
    if (!*from_key_ptr)
      return 0;
  }
  return 1;
}


static void handle_set_vars(RULE *rule, Gal_Frame fr, int ridx,
			    Gal_Frame *namespace_array)
{
  int i = 0;
  Gal_StringBuffer *buf = (Gal_StringBuffer *) NULL;
  char session_key[1024];
  EntityPair *ep;
  Gal_Object value = (Gal_Object) NULL;
  char *from_key = (char *) NULL;
  char *to_key = (char *) NULL;
  int newly_created;
  
  if (!rule->set_kps) 
    return;

  /* The old way has lots of exceptions which the new way does not.
     The old way doesn't use the namespace array at all, even
     though it could; the logic is too complicated. */
  
  if (rule->extended_syntax) {
    while ((ep = rule->set_kps[i++])) {
      value = Gal_GetProgramEntity(ep->source, namespace_array,
				   &newly_created);
      if (value)
	Gal_SetProgramEntityLocation(ep->target, value,
				     namespace_array, newly_created);
    }
  } else {
    while ((ep = rule->set_kps[i++])) {
      /* value will be a copied object. */
      int do_it = entity_pair_to_old_syntax_key_pair(ep, &to_key,
						     &from_key, &value);

      if (!do_it) {
	if (value) Gal_FreeObject(value);
	continue;
      }

      /* There had better be a value already found. */
      if (!value)
	continue;

      if (!strncmp(to_key, ":hub_session_", strlen(":hub_session_"))) {
	SESSION *session = GalHUB_SessionLookupByFrame(fr, 0);
	Gal_Object to, orig_to;

	sprintf(session_key, ":%s", to_key + strlen(":hub_session_"));

	orig_to = to = value;
	if (Gal_Stringp(to)) {
	  if (!_gal_strcasecmp(Gal_StringValue(to), "hub_decrement_value")) {
	    Gal_Object old_to = GalHUB_SessionGetVar(session, session_key);
	    if (old_to) {
	      to = Gal_IntObject(Gal_IntValue(old_to) - 1);
	    } else {
	      to = Gal_IntObject(-1);
	    }
	  } else if (!_gal_strcasecmp(Gal_StringValue(to), "hub_increment_value")) {
	    Gal_Object old_to = GalHUB_SessionGetVar(session, session_key);
	    if (old_to) {
	      to = Gal_IntObject(Gal_IntValue(old_to) + 1);
	    } else {
	      to = Gal_IntObject(1);
	    }
	  }
	}

	if (to != orig_to)
	  Gal_FreeObject(orig_to);
	GalHUB_SessionSetVar(session, session_key, to);
      } else {
	GalUtil_CPInfo2(3,0,"rule %d: setting %s to %s\n",ridx,to_key,Gal_ObjectString(value, &buf));
	Gal_SetProp(fr,to_key,value);
      }
    }
  }
  
  if (buf) Gal_FreeStringBuffer(buf);
}

static void handle_param_vars(RULE *rule, Gal_Frame fr,
			      int ridx, Gal_Frame *namespace_array)
{
  int i = 0;
  EntityPair *ep;
  Gal_Object value;
  int newly_created;
  
  if (!rule->param_kps)
    return;

  /* There are no special cases here, so we can just do
     the extended algorithm. */

  while ((ep = rule->param_kps[i++])) {
    value = Gal_GetProgramEntity(ep->source, namespace_array, &newly_created);
    if (value)
      Gal_SetProgramEntityLocation(ep->target, value,
				   namespace_array, newly_created);
  }
}

static void handle_out_vars(Gal_Frame res, RULE *rule, int ridx,
			    Gal_Frame state, char *session_id, int tidx,
			    EntityPair **kps,
			    int verbose, Gal_Frame *namespace_array)    
{
  int i = 0;
  EntityPair *ep;
  char *from_key = (char *) NULL;
  char *to_key = (char *) NULL;
  Gal_Object value = (Gal_Object) NULL;
  int newly_created;
  
  if (!kps)
    return;
    
  /* The old way has lots of exceptions which the new way does not.
     The old way doesn't use the namespace array at all, even
     though it could; the logic is too complicated. */
  
  if (rule->extended_syntax) {
    while ((ep = kps[i++]) && !ep->tag) {
      value = Gal_GetProgramEntity(ep->source, namespace_array,
				   &newly_created);
      if (value)
	Gal_SetProgramEntityLocation(ep->target, value,
				     namespace_array, newly_created);
    }
  } else {
    while ((ep = kps[i++]) && !ep->tag) {
      /* value will already be a copied object. */
      int do_it = entity_pair_to_old_syntax_key_pair(ep, &to_key, &from_key, &value);

      if (!do_it) {
	if (value) Gal_FreeObject(value);
	continue;
      }

      /* since value is new, we better make sure that a new one we create is also new. */
      if (!value)
	value = Gal_CopyObject(Gal_GetObject(res, from_key));

      if (value) {
	if (!strncmp(to_key, ":hub_session_", strlen(":hub_session_"))) {
	  SESSION *session = GalHUB_SessionLookupByID(session_id,0);
	  char session_key[1024];

	  sprintf(session_key, ":%s", to_key + strlen(":hub_session_"));
	  GalHUB_SessionSetVar(session,session_key,value);
	} else {
	  Gal_SetProp(state,to_key,value);
	  if (verbose) GalUtil_CPInfo1(3,0,"update token %d: inserting %s\n",tidx,to_key);
	}
      }
      if (verbose) GalHUB_OutlineFrame(state);
    }
  }
}


/* if they want to store a var that doesn't exist, perhaps it's a remapped outvar */
static Gal_Object
fetch_remapped_outvar_for_storage(RULE *rule, Gal_Frame res, char *store_key)
{
  int i = 0;
  EntityPair *ep;
  char *to_key = (char *) NULL;
  char *from_key = (char *) NULL;
  Gal_Object value = (Gal_Object) NULL;

  if (!rule->out_kps)
    return(NULL);

  while ((ep = rule->out_kps[i++]) && !ep->tag) {
    /* value will always be a copied object. */
    int do_it = entity_pair_to_old_syntax_key_pair(ep, &to_key, &from_key, &value);
    if (do_it) {
      /* If do_it is false, this key isn't a old stype compatible key. */
      if (Gal_StringEq(to_key, store_key) && from_key && !value)
	return Gal_GetObject(res, from_key);
      else if (value)
	Gal_FreeObject(value);
    } else if (value) {
      Gal_FreeObject(value);
    }
  }
  return(NULL);
}

static void
handle_store_vars(RULE *rule, Gal_Frame res, char *session_id)
{ int i;
  char *key;
  Gal_Object var;
  if (!rule->store_vars) return;

  /* store before it gets deleted!! */
  for (i=0;(key = rule->store_vars[i]);i++)
  { var = Gal_GetObject(res, key);
    if (!var)
    { var = fetch_remapped_outvar_for_storage(rule, res, key);
    }
    if (var) GalHUB_DBStoreInHistory(session_id, key, var);
  }
}

/*  Format is :keyword or (:to_key :from_key) or (:to_key value).
 *  (:to_key :from_key) may be a remapping of keys already added to
 *  the frame (by handle_retrieve_vars, for example).
 */

static void handle_in_vars(RULE *rule, Gal_Frame fr, int ridx, Gal_Frame state,
			   Gal_Frame *namespace_array)
{
  int i = 0;
  EntityPair *ep;
  char *from_key = (char *) NULL;
  char *to_key = (char *) NULL;
  Gal_Object value = (Gal_Object) NULL;
  int newly_created;
  
  if (!rule->in_kps)
    return;

  /* The old way has lots of exceptions which the new way does not.
     The old way doesn't use the namespace array at all, even
     though it could; the logic is too complicated. */
  
  if (rule->extended_syntax) {
    while ((ep = rule->in_kps[i++])) {
      value = Gal_GetProgramEntity(ep->source, namespace_array,
				   &newly_created);
      if (value)
	Gal_SetProgramEntityLocation(ep->target, value,
				     namespace_array, newly_created);
    }
  } else {
    /* We know the locations are going to be defaults, and
       that the source may or may not be a value. */
    while ((ep = rule->in_kps[i++])) {
      /* value will always be a copied element. */
      int do_it = entity_pair_to_old_syntax_key_pair(ep, &to_key, &from_key, &value);

      if (!do_it) {
	if (value) Gal_FreeObject(value);
	continue;
      }

      if (!value) {
	if (Gal_StringEq(from_key, GAL_HUB_LOG_PREFIX_HUB_FRAME_KEY) ||
	    Gal_StringEq(from_key, GAL_HUB_LOGDIR_HUB_FRAME_KEY) ||
	    Gal_StringEq(from_key, GAL_HUB_LOGFILE_HUB_FRAME_KEY) ||
	    Gal_StringEq(from_key, GAL_HUB_PWD_HUB_FRAME_KEY)) {
	  /* SAM 11/19/00: Special case: get it from the session. */
	  SESSION *session = GalHUB_SessionLookupByFrame(state, 0);
	  if (session) {
	    value = GalHUB_SessionGetVar(session, from_key);
	  }
	  if (value)
	    value = Gal_CopyObject(value);
	} else if (!strncmp(from_key,":hub_session_",strlen(":hub_session_"))) {
	  SESSION *session = GalHUB_SessionLookupByFrame(state, 0);
	  char session_key[1024];

	  sprintf(session_key, ":%s", from_key + strlen(":hub_session_"));
	  value = Gal_CopyObject(GalHUB_SessionGetVar(session, session_key));
	} else {
	  value = Gal_CopyObject(Gal_GetObject(state, from_key));
	}
      }
      
      /* this is the case of a rewrite of a retrieve variable */
      if (!value && (from_key != to_key)) {
	value = Gal_RemProp(fr, from_key);
      }
      if (value) {
	Gal_SetProp(fr, to_key, value);
      }
    }
  }
}

static void
handle_retrieve_vars(RULE *rule, Gal_Frame fr, char *session_id)
{
  int i = 0;
  KeyPair *kp;

  if (!rule->retrieve_kps)
    return;

  while ((kp = rule->retrieve_kps[i++]))
  {
    int offset = -1;
    Gal_Object value;

    if (Gal_Intp(kp->value))
      offset = Gal_IntValue(kp->value);

    value = GalHUB_DBRetrieveFromHistory(session_id, kp->key, offset);
    if (value)
      Gal_SetProp(fr, kp->key, value);
  }
}
  
/* SAM 5/4/01: The token t will always be non-NULL. This function
   returns 1 if the message should be processed, 0 otherwise.
   It returns 0 in two circumstances: if there is no rule for the
   rule index given, or if the rule at the rule index is not
   expecting a return (in which case, it has a return because there
   was an unexpected return for a server which wasn't asked for one,
   and a following rule which expected a return, and that's the
   one we should continue from).

   I'm beginning to suspect, nowadays, that since singlethread is
   now the only flow of control option, if a rule index in the
   return message isn't greater than the index in the existing
   control info, it's a bad return. */

int HC_UpdateToken(TOKEN *t, SERVER_MESSAGE *msg)
{
  Gal_Frame frame = msg->message;
  RULE *rule = 0;
  int ridx, tidx;
  MIT_CONTROL *ctrl_info = NULL;
  
  ridx = Gal_IntValue(msg->opaque_script_info);
  tidx = t->tidx;
  
  if(t && (ctrl_info = getControlInfoFromToken(t)) != NULL) {
    rule = ctrl_info->prog->rules[ridx];
  }

  if (!rule) {
    return 0;
  }

  if (rule->control & GAL_CONTROL_ASYNCHRONOUS) {
    return 0;
  }
  
  if (ctrl_info->ridx > ridx)
    return 0;
  
  /* only update the SET, OUT, DEL and STORE parameters! */
  ctrl_info->ridx = ridx;
  
  /* store before it gets deleted!! */
  handle_store_vars(rule, frame, t->session_id);
  handle_del_vars(rule, t->state, ctrl_info->ridx, msg->namespace_array);
  
  /* now insert the new elements */
  /* If we have an ERROR: parameter and the message type is
     an error reply, we want to change the
     message type to a simple reply and run the ERROR: line.
     Otherwise, we run the OUT: line. */
  if ((msg->msg_type == GAL_ERROR_MSG_TYPE) && rule->catch_error) {
    msg->msg_type = GAL_REPLY_MSG_TYPE;
    handle_out_vars(frame, rule, ctrl_info->ridx, t->state,
		    t->session_id, t->tidx, rule->error_kps,
		    0, msg->namespace_array);
  } else {
    handle_out_vars(frame, rule, ctrl_info->ridx, t->state,
		    t->session_id, t->tidx, rule->out_kps,
		    0, msg->namespace_array);
  }
  handle_set_vars(rule, t->state, ctrl_info->ridx,
		  msg->namespace_array);

  GalHUB_LogfileUpdateServerMessageFromEntityPairs(rule->out_log_kps, msg,
						   msg->namespace_array);
  return 1;
}

/* singlethreaded programs allow multithreads on the condition that the first thread clearly dies */
/* this is an idea in progress!! */
static int 
is_single_threaded(TOKEN *t, int ridx)
{ 
  RULE *rule;
  MIT_CONTROL *ctrl_info;
  ctrl_info = getControlInfoFromToken(t);

  if (Gal_StringEq(ctrl_info->prog->mode, "singlethread"))
  { 
    
    rule = ctrl_info->prog->rules[ridx];
    if (rule && (rule->control & GAL_CONTROL_ASYNCHRONOUS))
      return 0;
    else
      return 1;
  }
  return(0);
}

/* Return 1 if there is a match, 0 otherwise */
static MIT_CONTROL *findProgramForToken(TOKEN *t) {
  MIT_CONTROL *ctrl_info;
  PROGRAM *p=NULL;

  p = findProgramFromName(global_progs, t->name);
  if (p) {
    if (!t->ctrl_info) {
      t->ctrl_info = (CONTROL_INFO *)newControlStruct();
      t->ctrl_info_free_fn = freeControlStruct;
    }
    ctrl_info = getControlInfoFromToken(t);
    ctrl_info->prog = p;
    return(ctrl_info);
  }
  return (MIT_CONTROL *)NULL;
}

static int __gal_hub_add_session_var(const char *key, Gal_Object val, void *caller_data)
{
  Gal_Frame target = (Gal_Frame) caller_data;
  char session_key[512];

  sprintf(session_key, ":hub_session_%s", key + 1);
  
  Gal_SetProp(target, session_key, Gal_CopyObject(val));
  return 1;
}

/* Returns whether or not program info was found. */

int HC_NextOperations(HUB *h, TOKEN *t, int debug)
{
  SERVER_MESSAGE *msg = NULL;
  int ridx = -1;
  MIT_CONTROL *ctrl_info;  
  Gal_Frame namespace_array[MAX_NAMESPACES];
  int i;
  Gal_Frame session_frame;
  SESSION *session = GalHUB_SessionLookupByFrame(t->state, 0);
  
  if (t->destroy)
    return 0;

  ctrl_info = getControlInfoFromToken(t);

  if (!ctrl_info && (ctrl_info = findProgramForToken(t)) == NULL) 
    return 0;

  ridx = ctrl_info->ridx;
  
  /* supplement t->state with all session vars */
  session_frame = Gal_CopyFrame(t->state);
  /* Remember, we need to add the :hub_session_ back in for
     the hub session variables. Grrrr. */
  if (session && session->session_vars) {
    Gal_DoProperties(session->session_vars,
		     __gal_hub_add_session_var,
		     (void *) session_frame);
  }

  /* Load up the namespaces. */
  for (i = 0; i < MAX_NAMESPACES; i++) {
    switch (i) {
    case GAL_SESSION_NAMESPACE:
      if (session)
	namespace_array[i] = session->session_vars;
      else
	namespace_array[i] = (Gal_Frame) NULL;
      break;
    case GAL_TOKEN_NAMESPACE:
      namespace_array[i] = session_frame;
      break;
    case GAL_GLOBAL_NAMESPACE:
      namespace_array[i] = Hub->globals;
      break;
    default:
      namespace_array[i] = (Gal_Frame) NULL;
    }
  }

  while((ridx = find_next_operation(t, ridx+1, namespace_array)) >= 0) {
    ctrl_info->ridx = ridx;
    if ((msg = HC_NextOperation(h, t, debug, namespace_array)) != NULL) {
      GalHUB_EnqueueServerMessage(h, t, msg);
      if (is_single_threaded(t,ridx)) break;
    }
  }
  Gal_FreeFrame(session_frame);
  return 1;
}

static SERVER_MESSAGE *HC_NextOperation(HUB *h, TOKEN *t, int debug, Gal_Frame *namespace_array)
{
  RULE *rule;
  char *op_name;
  SESSION *session;
  MIT_CONTROL *control_info;
  PROGRAM *prog=NULL;
  Gal_Frame result_frame = NULL;
  SERVER_MESSAGE *result = NULL;
  int hub_no_return = 0;
  int op_never_returns = 0;
  char *server_name = (char *) NULL;
  char *provider_id_name = (char *) NULL;
  int stype_found;
  int newly_created = 0;
  Gal_Object o = (Gal_Object) NULL;
  
  if (!t)
    return(result);

  control_info = getControlInfoFromToken(t);

  if (!control_info || (control_info = findProgramForToken(t)) == NULL) 
    return (result);
  
  prog = control_info->prog;
  /*print_session_locks();*/
  /* Abort handling was here */
  /* is there an abort pending for this utt in main? */
  if(Gal_StringEq(prog->name,"main")) { 
    if(Gal_GetObject(t->state,GAL_UTTERANCE_ID_FRAME_KEY) && Gal_GetObject(t->state,GAL_SESSION_ID_FRAME_KEY) &&
       (!Gal_GetObject(t->state,GAL_NO_ABORT_HUB_FRAME_KEY))) {
      if(GalHUB_SessionGetAbortUtt(Gal_GetInt(t->state,GAL_UTTERANCE_ID_FRAME_KEY), Gal_GetString(t->state,GAL_SESSION_ID_FRAME_KEY)))
      {
	GalUtil_CPInfo1(5,0,"\n-------Aborting [%3d]------------\n",t->tidx);
	GalHUB_OutlineFrame(t->state);
	GalUtil_CPInfo1(5,0,"------------------------------------\n\n");
	/* Wrong, should return abort notification */
	return result;
      }
    }
  }

  session = GalHUB_SessionLookupByFrame(t->state, 0);
  rule = prog->rules[control_info->ridx]; 
  op_name = rule->op_name;
  server_name = rule->server_name;

  /* First, find the provider value. */
  if (rule->provider) {
    o = Gal_GetProgramEntity(rule->provider, namespace_array,
			     &newly_created);
    if (o) {
      if (!Gal_Stringp(o)) {
	GalUtil_Warn("Found provider in rule which doesn't evaluate to string; ignoring");	
      } else {
	/* Use this name instead of the server_name. */
	provider_id_name = Gal_StringValue(o);
      }
    }
  }

  result_frame = Gal_MakeFrame(op_name, GAL_CLAUSE);
  
  result = GalHUB_NewServerMessage(result_frame, GAL_OUTGOING_MSG,
				   GAL_MESSAGE_MSG_TYPE, server_name,
				   provider_id_name,
				   (SERVICE_PROVIDER *) NULL,
				   rule->lock_mask, rule->lock_value,
				   -1, 0, Gal_IntObject(control_info->ridx),
				   0, t, session);

  if (o && newly_created)
    Gal_FreeObject(o);
	
  stype_found = GalHUB_FindServiceTypeForOperation(h, result, namespace_array);
  
  /* this should probably generate a system error */
  if (!stype_found) {
    if (server_name) 
      GalUtil_PInfo1("No available service provider for %s.%s!\n",
		     server_name, op_name);
    else
      GalUtil_PInfo1("No available service provider for %s!\n",
		     op_name);
    GalHUB_FreeServerMessage(result);
    Gal_FreeFrame(result_frame);
    return (SERVER_MESSAGE *) NULL;
  }

  /* SAM 3/11/02: I need to know if the builtin server was chosen.
     So I update this information in GalHUB_FindServiceTypeForOperation.
     I may end up having to do this test again in HC_PrepareOperation,
     if no server was yet chosen. */
  
  if (result->true_provider_spec.stype_name &&
      (!strcmp(result->true_provider_spec.stype_name,
	       BUILTIN_SERVER)) && op_name &&
      (list_member(op_name, GalHUB_AsynchronousBuiltins) != -1)) {
    /* Backward compatibility. */
    hub_no_return = 1;
    op_never_returns = 1;
  }

  /* Don't post the continuations yet, because we want to
     associate them with whatever server is chosen. These
     need to be copied before the continuation
     is enqueued. See HC_PrepareOperation. We also
     can't tell the Hub there's no actual return,
     because then the token will be destroyed. Somewhere,
     we need to tell the Hub NOT TO TELL THE SERVER TO
     SEND A REPLY. This probably needs to happen in
     HC_PrepareOperation, but it may only be a trick. */
  
  if ((rule->control & GAL_CONTROL_NO_RESULT) ||
      (rule->control & GAL_CONTROL_ASYNCHRONOUS)) {
    hub_no_return = 1;
  }

  /* If the operation won't return, but the rule isn't asynchronous,
     the Hub will hang waiting for a reply and the token will never
     go away. This is apparent in the rules, but not so apparent
     for the builtin function exceptions. So we destroy the token
     under those circumstances. */
  
  if ((op_never_returns && !(rule->control & GAL_CONTROL_ASYNCHRONOUS)) ||
      (rule->control & GAL_CONTROL_RETURN)) {
    /* Don't run any more rules */
    t->destroy = 1;
  }

  result->no_return = hub_no_return;

  GalHUB_PopulateServerMessageNamespaceArray(session, result, t);

  return (result);
}

/* This is called after the Hub has done all its bookkeeping,
   prepared the message as it sees fit, and chosen the
   service provider. */

void HC_PrepareOperation(SESSION *session, TOKEN *t, SERVER_MESSAGE *msg)
{
  int ridx = Gal_IntValue(msg->opaque_script_info);
  MIT_CONTROL *ctrl_info;
  RULE *rule = (RULE *) NULL;
  int op_never_returns = 0;

  /* This function currently only applies to new outgoing messages
     (that is, ones the scripting language came up with. */
  if (!((msg->direction == GAL_OUTGOING_MSG) &&
	(msg->msg_type == GAL_MESSAGE_MSG_TYPE)))
    return;
  
  if (t && (ctrl_info = getControlInfoFromToken(t)) != NULL) {
    rule = ctrl_info->prog->rules[ridx];
  }

  /* There had better be a rule, but if there isn't, skip. */
  if (!rule)
    return;

  /* Alarms can be handled anywhere, but they probably ought to
     be set as close to dispatch as possible. For instance,
     if the message is enqueued, it's not clear you want to
     have the alarm fire. */
  
  handle_alarm_vars(rule, session, ridx);
  handle_retrieve_vars(rule, msg->message, session->session_id);

  handle_param_vars(rule, msg->message, ridx, msg->namespace_array);
  handle_in_vars(rule, msg->message, ridx, t->state, msg->namespace_array);

  if (!strcmp(msg->stype->name, BUILTIN_SERVER)) {
    if (list_member(msg->bare_operation,
		    GalHUB_AsynchronousBuiltins) != -1) {
      op_never_returns = 1;
    }
  }
  
  /* Note that set/del happen before the call, instead of after.
     For dispatch_to_main/dispatch_token, this has no effect on
     the message sent, and the token updates may not make any
     difference, but things like :hub_session_ will happen.
     THIS IS A TERRIBLE THING TO RELY ON, and is only supported for
     backward compatibility.
  */
  
  if (op_never_returns || (rule->control & GAL_CONTROL_ASYNCHRONOUS)) {
    handle_set_vars(rule, t->state, ridx, msg->namespace_array);
    handle_del_vars(rule, t->state, ridx, msg->namespace_array);
  }

  GalHUB_LogfileUpdateServerMessageFromEntityPairs(rule->in_log_kps, msg,
						   msg->namespace_array);
  /* Finally, the continuations. */
  if (rule->reply_continuations || rule->error_continuations) {
    Gal_Frame *reply_frames = (Gal_Frame *) NULL;
    Gal_Frame *error_frames = (Gal_Frame *) NULL;
    int i;

    /* First, tell the Hub not to ask the server to provide
       a real reply. HC_PrepareOperation must be called before
       the opaque data frame is constructed. See
       writeMessageToServer in hub_process.c.
    */
    msg->no_return = 1;
    
    if (rule->reply_continuations) {
      for (i = 0; rule->reply_continuations[i]; i++);
      reply_frames = (Gal_Frame *) calloc(i + i, sizeof(Gal_Frame));
      for (i = 0; rule->reply_continuations[i]; i++) {
	reply_frames[i] = Gal_CopyFrame(rule->reply_continuations[i]);
      }
    }
    if (rule->error_continuations) {
      for (i = 0; rule->error_continuations[i]; i++);
      error_frames = (Gal_Frame *) calloc(i + i, sizeof(Gal_Frame));
      for (i = 0; rule->error_continuations[i]; i++) {
	error_frames[i] = Gal_CopyFrame(rule->error_continuations[i]);
      }
    }
    GalHUB_EnqueueHubContinuation(session, t->tidx,
				  Gal_CopyObject(msg->opaque_script_info),
				  /* Obviously, this is not scriptless. */
				  0, msg->true_provider_spec.stype_name,
				  reply_frames, error_frames,
				  (char *) NULL, msg->provider->id);
  }
}

static MIT_CONTROL *getControlInfoFromToken(TOKEN *t) {
  return (MIT_CONTROL *)t->ctrl_info;
}

static MIT_CONTROL *newControlStruct() {
  MIT_CONTROL *ctrl;
  ctrl = (void *) calloc(1,sizeof(MIT_CONTROL));
  ctrl->ridx = -1;
  return ctrl;
}

static void freeControlStruct(CONTROL_INFO *info)
{
  free(info);
}

/* Abort handling */

