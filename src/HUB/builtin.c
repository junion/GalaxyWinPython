/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include "galaxy/galaxy_all.h"
#include "hub_internal.h"
#include "builtin.h"

Gal_Frame *init_db()
{ 
  return GalHUB_DBInit();
}

Gal_Frame *
fetch_db_for_session(char *session_id)
{ 
  return GalHUB_SessionFetchDB(session_id);
}

/* find a possible prior reference to the switch domain */
int restore_switched_history(SESSION *session, char *switch_domain)
{ 
  return GalHUB_SessionRestoreSwitchedHistory(session, switch_domain);
}

/* we need to worry about restoring displays in gui server */
char *
scratch_that(Gal_Frame frame)
{ SESSION *session;
  Gal_Frame *db;
  int discourse_id;
  int num_no_parse;
  Gal_Object former_domain;
  session = GalHUB_SessionLookupByFrame(frame, 0);
  if (session)
  { num_no_parse = Gal_IntValue(GalHUB_SessionGetVar(session, ":num_no_parse"));
    if (num_no_parse > 0) return("scratched");
    if (session->discourse_id <= 0)
    { return("unscratched");
    }

/* in case the last query was a domain switch */
    if ((former_domain = GalHUB_DBRetrieveFromHistory(session->session_id, ":prior_domain", -1)))
    { restore_switched_history(session, Gal_StringValue(former_domain));
      return("scratched");
    }

    db = fetch_db_for_session(session->session_id);
    session->discourse_id--;
    discourse_id = session->discourse_id;
    if (discourse_id == -1) discourse_id = Max_History_Utts;
    if (db && (db[discourse_id]))
    { Gal_FreeFrame(db[discourse_id]);
      db[discourse_id] = NULL;
    }
    return("scratched");
  }
  return("unscratched");
}


Gal_Object
maybe_map_session_domain(Gal_Object domain, Gal_Object map)
{ char *smap, *sdomain, *tok, *x, *mymap;	
  char *map_domain = NULL;
  int i;
  if (!map) return (Gal_CopyObject(domain));
  sdomain = Gal_StringValue(domain);
  smap = Gal_StringValue(map);
  if (!strstr(smap, sdomain))
    return(Gal_CopyObject(domain));	
  tok =smap; 
  for (i=0;(x = strtok(tok, " \t\n"));i++)
  { mymap = strtok(NULL, " \t\n");
    if (!mymap) break;
    if (!strcmp(x, sdomain))
    { map_domain = _gal_strdup(mymap);
      break;
    }
    tok = NULL;
  }
  if (map_domain)
  { return(Gal_StringObject(map_domain));
  }
  return(Gal_CopyObject(domain));
}

char *call_me(Gal_Frame frame)
{ char *country_code, *area_code, *exchange, *core, callback_number[100];
  char *domain;
  SESSION *session = NULL;
  Gal_Frame parse_frame, number, reply_frame;
  Gal_Object map, session_domain;
  parse_frame = Gal_GetFrame(frame, ":parse_frame");	
  number = Gal_GetFrame(parse_frame, ":topic");
  if (!number)
  { GalUtil_Warn("Non-valid phone number!!. No topic number!");
    Gal_PPFrame(number);
  }
  callback_number[0] = '\0';
  core = Gal_GetString(number, ":core");
  if (!core)
  { GalUtil_Warn("Non-valid phone number!!. No core!!");
    Gal_PPFrame(number);
  }
  if ((country_code = Gal_GetString(number, ":country_code")))
  { strcpy(callback_number, country_code);
    strcat(callback_number, "-");
  }
  if ((area_code = Gal_GetString(number, ":area_code")))
  { strcat(callback_number, area_code);
    strcat(callback_number, "-");
  }
  if ((exchange = Gal_GetString(number, ":exchange")))
  { strcat(callback_number, exchange);
    strcat(callback_number, "-");
  }
  strcat(callback_number, core);
  reply_frame = Gal_MakeFrame("answer_phone", GAL_CLAUSE);
  if ((domain  = Gal_GetString(parse_frame, ":calling_domain")))
  { session = GalHUB_SessionLookupByFrame(frame, 0);
    if (session)
    { GalUtil_CPInfo1(5,0,"\n------- Domain %s Calling Back!!---------\n", domain);
      GalHUB_SessionSetVar(session,":domain", Gal_StringObject(domain));
      if((session_domain  = GalHUB_SessionGetVar(session, ":domain")))
      { map = Gal_GetObject(frame, ":domain_map");
	if (map)
	  Gal_SetProp(reply_frame, ":domain", maybe_map_session_domain(session_domain, map));
      }
    }
  }

  Gal_SetProp(frame,":callback_channel", Gal_StringObject("telephone"));
  Gal_SetProp(frame,":callback_address", Gal_StringObject(callback_number));

  Gal_SetProp(frame, ":reply_frame", Gal_FrameObject(reply_frame));
  return(NULL);
}

char *erase_all_history(Gal_Frame frame)
{
  GalHUB_DBEraseAllHistory(frame);
  return("history_cleared");
}

char *clear_history(Gal_Frame frame)
{
  GalHUB_DBClearHistory(frame);
  return("history_cleared");
}


/* saves away the current history under its domain.
  Restores a prior history under the new domain, if present.
*/
int reset_session_history(SESSION *session, char *current_domain, char *switch_domain)
{ 
  return GalHUB_SessionResetHistory(session, current_domain, switch_domain);
}


/* implicit domain switch */
/* sets the hub_session_domain to the new domain. */ 
Gal_Frame reset_history(Gal_Frame frame)
{ SESSION *session;
  char *session_id, *new_domain, *session_domain;

  session_id = Gal_GetString(frame, GAL_SESSION_ID_FRAME_KEY);
  session = GalHUB_SessionLookupByFrame(frame, 0);
  if (!session) return(frame);
  new_domain = Gal_GetString(frame, ":new_domain");
  session_domain = Gal_StringValue(GalHUB_SessionGetVar(session, ":domain"));
  if (new_domain && session_domain && (strcmp(new_domain, session_domain)))
  { reset_session_history(session, session_domain, new_domain);
  }
  return(frame);
}

void
print_DB(Gal_Frame *db) 
{
  GalHUB_DBPrint(db);
}

/*
Gal_Frame 
unknown_word_clause(Gal_Frame fr)
{
  Gal_Frame res;

  res = Gal_MakeFrame("reply", GAL_CLAUSE);
  add_local_reply_to_frame(res, NULL, fr);

  return(NULL);
}
*/

/* these were in Christine's version */
/*  reset_idle_music();
  res = Gal_MakeFrame("reply", GAL_CLAUSE);
  set_quit_after_response(1);
*/

char *
close_off(Gal_Frame fr)
{
  Gal_SetProp(fr,":close_off", Gal_IntObject(1));

  return("goodbye");
}

char *
greetings(Gal_Frame fr)
{
  Gal_SetProp(fr,":greetings", Gal_IntObject(1));
  return("greetings");
}

char *
praise(Gal_Frame fr)
{
/*   GalHUB_SessionFlushLogfile(); */
  Gal_SetProp(fr,":praise", Gal_IntObject(1));
  return("praise");
}

char *
vacuous(Gal_Frame fr)
{
  Gal_SetProp(fr,":vacuous", Gal_IntObject(1));
  return("vacuous");
}

char *fetch_param(Gal_Frame frame, char *param)
{
  char *value;

  value = Gal_GetString(frame, param);
  if (value)
    return(_gal_strdup(value));
  return(NULL);
}

static int string_exists(char *val){
  return (val && *val);
}

/* this is a mutual exchange -- I set in the session and I default from the session */
/* modifies frame in place */
Gal_Frame set_session_parameters(Gal_Frame frame, void *server_data)
{
  SESSION *session;
  char *param;
  HUB *h = GalHUB_GetHubFromLocalServerData(GalSS_EnvComm((GalSS_Environment *) server_data));

  if (0) GalUtil_PPFrame(GAL_PINFO1_LEVEL, frame);

  /* we are probably only called now for a normal token which has
     a session ID. We used to do something special for dispatch server
     reconnections, but those don't exist anymore. */

  if (Gal_GetObject(frame, GAL_SESSION_ID_FRAME_KEY)) {
    session = GalHUB_SessionLookupByFrame(frame, 1);

    /* no session, then this is probably a killed session! */
    if (!session) 
      return frame;
  } else {    
    /* no session, then this is probably a killed session! */
    GalUtil_Warn("No session found in set_session_parameters");
    session = GalHUB_SessionLookupByID(NULL,1); /* default session !! */  /* this is probably batch mode runs */
  }

  if ((param = fetch_param(frame, ":out_lang"))) {
    GalHUB_SessionSetVar(session, ":out_lang", Gal_StringObject(param));
  } else {
    if (!GalHUB_SessionGetVar(session, ":out_lang"))
      GalHUB_SessionSetVar(session, ":out_lang",
			   Gal_CopyObject(GalHUB_GetHubGlobal(h, "OUT_LANG:")));
    Gal_SetProp(frame, ":out_lang", Gal_CopyObject(GalHUB_SessionGetVar(session, ":out_lang")));
  }

  if ((param = fetch_param(frame, ":para_lang"))) {
    GalHUB_SessionSetVar(session, ":para_lang", Gal_StringObject(param));
  } else {
    if (!GalHUB_SessionGetVar(session, ":para_lang"))
      GalHUB_SessionSetVar(session, ":para_lang",
			   Gal_CopyObject(GalHUB_GetHubGlobal(h, "PARA_LANG:")));
    Gal_SetProp(frame, ":para_lang", Gal_CopyObject(GalHUB_SessionGetVar(session, ":para_lang")));
  }

  if ((param = fetch_param(frame, ":synth_lang"))) {
    GalHUB_SessionSetVar(session, ":synth_lang", Gal_StringObject(param));
  } else {
    if (!GalHUB_SessionGetVar(session, ":synth_lang"))
      GalHUB_SessionSetVar(session, ":synth_lang",
			   Gal_CopyObject(GalHUB_GetHubGlobal(h, "SYNTH_LANG:")));
    Gal_SetProp(frame, ":synth_lang", Gal_CopyObject(GalHUB_SessionGetVar(session, ":synth_lang")));
  }

  if ((param = fetch_param(frame, ":kv_lang"))) {
    GalHUB_SessionSetVar(session, ":kv_lang", Gal_StringObject(param));
  } else {
    if (!GalHUB_SessionGetVar(session, ":kv_lang"))
      GalHUB_SessionSetVar(session, ":kv_lang",
			   Gal_CopyObject(GalHUB_GetHubGlobal(h, "KV_LANG:")));
    Gal_SetProp(frame, ":kv_lang", Gal_CopyObject(GalHUB_SessionGetVar(session, ":kv_lang")));
  }

  {
    char *frame_session_domain = Gal_GetString(frame, ":hub_session_domain");
    char *frame_domain = Gal_GetString(frame, ":domain");

    if (!string_exists(frame_domain) && 
	string_exists(frame_session_domain)){
      /* :domain defaults to :hub_session_domain */
      Gal_SetProp(frame, ":domain", Gal_StringObject(frame_session_domain));
      frame_domain = Gal_GetString(frame, ":domain");
    }

    if (string_exists(frame_session_domain)
	&& strcmp("local", frame_session_domain)){
      /* Unless "local", push :hub_session_domain up to session if it exists */
      GalHUB_SessionSetVar(session, ":domain", 
			   Gal_StringObject(frame_session_domain));
    } else if (string_exists(frame_domain) && strcmp("local", frame_domain)){
      /* Otherwsie push :domain up to session if not "local" */
      GalHUB_SessionSetVar(session, ":domain", Gal_StringObject(frame_domain));
    }

    if (!GalHUB_SessionGetVar(session, ":domain"))
      /* Default the session variable if not set yet */
      GalHUB_SessionSetVar(session,":domain", 
			   Gal_CopyObject(GalHUB_GetHubDefaultDomain(h)));
    
    if (!string_exists(frame_domain)){
      /* If still no :domain in the frame, default it */
      Gal_SetProp(frame, ":domain",
		  Gal_CopyObject(GalHUB_SessionGetVar(session, ":domain")));
    }
  }
  
  Gal_SetProp(frame, GAL_SESSION_ID_FRAME_KEY, Gal_StringObject(session->session_id));
  if(!Gal_GetObject(frame,GAL_UTTERANCE_ID_FRAME_KEY) || Gal_GetInt(frame,GAL_UTTERANCE_ID_FRAME_KEY) < 0)
    Gal_SetProp(frame, GAL_UTTERANCE_ID_FRAME_KEY, Gal_IntObject(session->utterance_id));

  return(frame);
}

/* SAM 5/24/01: Note that there are a few dispatch functions
   which never return a value. The MIT scripting language wants to know
   this. I may put "destroy" on this list eventually as well, but
   for backward compatibility only dispatch_token and dispatch_to_main
   are on this list. */

char *GalHUB_AsynchronousBuiltins[] = {"dispatch_token", "dispatch_to_main", NULL};

Gal_Frame destroy(Gal_Frame frame, void *server_data)
{ 
  GalSS_EnvDestroyToken((GalSS_Environment *) server_data);
  return (Gal_Frame) NULL;
}

Gal_Frame debug_token(Gal_Frame frame, void *server_data)
{ 
  GalUtil_CPInfo1(5,0,"\n-------Debugging Token---------\n");
  Gal_PPFrame(frame);
  return frame;
}

Gal_Frame nop(Gal_Frame frame, void *server_data)
{ 
  return frame;
}

/* default dispatches to main */

Gal_Frame dispatch_token(Gal_Frame frame, void *server_data)
{ 
  char *program;

  program = Gal_GetString(frame, ":program");
  if (!program) program = "main";
  /* To keep from changing the incoming frame. */
  frame = Gal_CopyFrame(frame);
  Gal_SetFrameName(frame, program);
  Gal_DelProp(frame, GAL_TOKEN_INDEX_FRAME_KEY);
  Gal_DelProp(frame, GAL_HUB_OPAQUE_DATA_FRAME_KEY);
  Gal_DelProp(frame, ":program");

  frame = set_session_parameters(frame, server_data);
  /* nip it in the bud here */
  if(Gal_GetObject(frame,GAL_UTTERANCE_ID_FRAME_KEY) && Gal_GetObject(frame,GAL_SESSION_ID_FRAME_KEY) &&
     (!Gal_GetObject(frame,GAL_NO_ABORT_HUB_FRAME_KEY))) {

    if(GalHUB_SessionGetAbortUtt(Gal_GetInt(frame,GAL_UTTERANCE_ID_FRAME_KEY), Gal_GetString(frame,GAL_SESSION_ID_FRAME_KEY)))
    {
      GalUtil_CPInfo1(5,0,"\n-------Aborting dispatch---------\n");
      GalHUB_OutlineFrame(frame);
      GalUtil_CPInfo1(5,0,"------------------------------------\n\n");
      return 0;
    }
  }
  GalSS_EnvWriteFrame((GalSS_Environment *) server_data, frame, 0);
  /* Free after writing. The local server handling will
     ensure that it's copied. */
  Gal_FreeFrame(frame);
  return (Gal_Frame) NULL;
}

Gal_Frame dispatch_to_main(Gal_Frame frame, void *server_data)
{ 
  Gal_DelProp(frame, ":program");
  return(dispatch_token(frame, server_data));
}

/* A cleaner version that returns. */

static Gal_Frame __continue_call_program(Gal_Frame reply,
					 GalIO_MsgType msg_t,
					 GalSS_Environment *env,
					 void *continuation_state)
{
  /* This function is called when the Hub requires a reply. */
  if (!reply) {
    GalUtil_Warn("Didn't hear back from dispatch_token");
    return (Gal_Frame) NULL;
  }

  switch (msg_t) {
  case GAL_REPLY_MSG_TYPE:
    return reply;
  case GAL_ERROR_MSG_TYPE:
    GalSS_EnvError(env, Gal_GetString(reply, GAL_ERROR_DESCRIPTION_FRAME_KEY));
    return (Gal_Frame) NULL;
  default:
    return (Gal_Frame) NULL;
  }
}

Gal_Frame call_program(Gal_Frame fr, void *server_data)
{
  char *program = Gal_GetString(fr, ":program");
  Gal_Frame frame = Gal_CopyFrame(fr);

  if (!program) {
    GalSS_EnvError((GalSS_Environment *) server_data,
		   "no program");
    return (Gal_Frame) NULL;
  }
  
  Gal_SetFrameName(frame, program);
  Gal_DelProp(frame, GAL_TOKEN_INDEX_FRAME_KEY);
  Gal_DelProp(frame, GAL_HUB_OPAQUE_DATA_FRAME_KEY);
  Gal_DelProp(frame, ":program");
  
  if(Gal_GetInt(fr, ":inherit_timestamp"))
    GalSS_EnvInheritTokenTimestamp((GalSS_Environment *) server_data);

  if (((GalSS_Environment *) server_data)->return_required) {
    GalSS_EnvDispatchFrameWithContinuation((GalSS_Environment *) server_data,
					   frame, __continue_call_program,
					   (void *) NULL, NULL);
    Gal_FreeFrame(frame);
    return (Gal_Frame) NULL;
  } else {
    GalSS_EnvWriteFrame((GalSS_Environment *) server_data, frame, 0);
    /* Free after writing. The local server handling will
       ensure that it's copied. */
    Gal_FreeFrame(frame);
    return (Gal_Frame) NULL;
  }
}

Gal_Frame abort_main(Gal_Frame frame, void *server_data)
{ 
  Gal_DelProp(frame, GAL_TOKEN_INDEX_FRAME_KEY);
  Gal_DelProp(frame, GAL_HUB_OPAQUE_DATA_FRAME_KEY);
  frame = set_session_parameters(frame, server_data);
  GalHUB_SessionSetAbortUtt(Gal_GetInt(frame,GAL_UTTERANCE_ID_FRAME_KEY),Gal_GetString(frame,GAL_SESSION_ID_FRAME_KEY));
  return (0);
}

char *unknown_word(Gal_Frame frame)
{ Gal_SetProp(frame, ":reply_frame",  Gal_CopyObject(Gal_GetObject(frame, ":parse_frame")));
  return(NULL);
}


char *no_parse(Gal_Frame frame)
{ int index, num_no_parse;
  Gal_Frame reply_frame = NULL, parse_frame;
  Gal_Object map;
  char reject_message[128], *repeat_prompt = NULL;
  TObj session_domain;
  SESSION *session;	
  char *status = NULL;

  session = GalHUB_SessionLookupByFrame(frame, 0);
  status = Gal_GetString(frame, ":parse_status");
  if (status && (!strcmp(status, "EMPTY")))
  { reply_frame = Gal_MakeFrame("rejection_0", GAL_CLAUSE);	
  }
  else
  { /* migrate the repeat prompt, if it exists, as the reply frame */ 
    parse_frame = Gal_GetFrame(frame, ":parse_frame");
    if (parse_frame)
    { if ((repeat_prompt = Gal_GetString(parse_frame, ":repeat_prompt")))
      { reply_frame = Gal_MakeFrame(repeat_prompt, GAL_CLAUSE);	
      }
    }
    if (!reply_frame)
     /* Keep incrementing num_no_parse and cycle through rejection messages.
	 Hub script may want to reset whenever there's a successful parse.
     */
    { num_no_parse = Gal_IntValue(GalHUB_SessionGetVar(session, ":num_no_parse"));
      index = (num_no_parse%4) + 1;
      GalHUB_SessionSetVar(session, ":num_no_parse", Gal_IntObject(num_no_parse+1));
      sprintf(reject_message, "rejection_%d", index);
      reply_frame = Gal_MakeFrame(reject_message, GAL_CLAUSE);	
    }
  }
  Gal_SetProp(reply_frame, ":no_parse", Gal_IntObject(1));
  
  if((session_domain  = GalHUB_SessionGetVar(session, ":domain")))
  { map = Gal_GetObject(frame, ":domain_map");
    Gal_SetProp(reply_frame, ":domain", maybe_map_session_domain(session_domain, map));
  }
  Gal_SetProp(frame, ":reply_frame", Gal_FrameObject(reply_frame));
  return(NULL);
}

/* this now saves the current discourse history under the current domain name.
  It then clears out the session history (for the new domain)
  Unless it can find a prior history for this domain, in which case it restores it 
*/
char *domain_switch(Gal_Frame frame)
{ SESSION *session;
  char *domain = NULL, *session_id;
  Gal_Object session_domain = NULL, session_user =NULL;
  Gal_Frame topic, selected_frame, reply_frame = NULL;
  char *reply_name = "welcome_switch";

  session_id = Gal_GetString(frame, GAL_SESSION_ID_FRAME_KEY);
  session = GalHUB_SessionLookupByFrame(frame, 0);
  if (session)
    session_domain = Gal_CopyObject(GalHUB_SessionGetVar(session, ":domain"));
  selected_frame = Gal_GetFrame(frame, ":parse_frame");
  if (selected_frame)
  { domain = Gal_GetString(selected_frame, ":new_domain");
    if(!domain) 
    { topic = Gal_GetFrame(selected_frame, ":topic");
      if (topic) domain = Gal_GetString(topic, ":name");
    }
    if (domain && session) {
      /* Nonsensical switch */	
      if (domain && session_domain && (!strcmp(domain, Gal_StringValue(session_domain))))
	  reply_name = "rewelcome";
      else {
	/* possibly fetching a former instance of history for the new domain */
	/* if it found it from before, this is a "welcome_back" message */
	if (reset_session_history(session, Gal_StringValue(session_domain), domain)) {
	  reply_name = "welcome_back";
	  session_user = GalHUB_SessionGetVar(session, GAL_HUB_SESSION_USER_HUB_FRAME_KEY);
	}
	GalUtil_Print(-1, "Switching domain to %s\n", domain); 
	Gal_SetProp(frame,":new_domain", Gal_StringObject(domain));
      }
    } else { /* some kind of trouble here!! */
      if(session_domain) {
	Gal_Object map = Gal_GetObject(frame, ":domain_map");
	domain = Gal_StringValue(maybe_map_session_domain(session_domain, map));
      }
      reply_name = "rejection_0";
    }
  }

  if(session_domain) Gal_FreeObject(session_domain);
  reply_frame = Gal_MakeFrame(reply_name, GAL_CLAUSE);	
  Gal_SetProp(reply_frame, ":domain", Gal_StringObject(domain));
  if (domain)
    Gal_SetProp(reply_frame, ":domain", Gal_StringObject(domain));
  if (session_user)
  { Gal_SetProp(reply_frame, ":user_name", Gal_CopyObject(session_user));
  }
  if(!strcmp(reply_name, "rewelcome"))
    Gal_SetProp(frame, ":reply_frame", Gal_FrameObject(reply_frame));
  else
    Gal_SetProp(frame, ":welcome_frame", Gal_FrameObject(reply_frame));
  return(NULL);
}

void
rotate_db(SESSION *session, int max_utts, int min_utts)
{ int i, del_utts; 
  Gal_Frame *db;
  db = session->history;
  if (!db) return; 
  del_utts = max_utts - min_utts;
  for(i=0;i<del_utts;i++)
  { if (db[i])
    { Gal_FreeFrame(db[i]);
    }
    if (i < min_utts)
    { db[i] = db[del_utts+i];
      db[del_utts+i] = NULL;
    }
    else db[i] = NULL;
  }
}

/* also counts sequence of "no_parse" */
Gal_Frame increment_utterance(Gal_Frame frame, void *server_data)
{
  SESSION *session;
  HUB *h = GalHUB_GetHubFromLocalServerData(GalSS_EnvComm((GalSS_Environment *) server_data));
  
  session = GalHUB_SessionLookupByFrame(frame, 0);

  if (session)
  { 
    GalHUB_LogfileRecordUtteranceForSession(session);
    session->utterance_id++;
    GalHUB_SessionSetVar(session, GAL_HUB_SESSION_UTTERANCE_ID_HUB_FRAME_KEY, Gal_IntObject(session->utterance_id));
    h->logging_id++;

    /* ADDED !! 4/13/99 */ 
    if ((session->discourse_id <= 0) || (session->history && (session->history[session->discourse_id])))
      session->discourse_id++;

    if (session->discourse_id >= Max_History_Utts)
    { rotate_db(session, Max_History_Utts, Min_Utts);
      session->discourse_id = Min_Utts;
    }

    Gal_SetProp(frame,GAL_UTTERANCE_ID_FRAME_KEY, Gal_IntObject(session->utterance_id));

/*  Don't do this any more!! -- leave it to the hub script (optionally)
    domain = Gal_GetString(frame, ":domain"); 
    if (domain && (strcmp(domain, "local")))
    { session->num_no_parse = 0;
    }
*/
  }
  else
  {
    GalUtil_Warn("No session associated with frame!! Not incrementing utterance_id");
    GalUtil_PPFrame(GAL_PINFO1_LEVEL, frame);
  }
  return(frame);
}

Gal_Frame end_session(Gal_Frame frame, void *server_data)
{
  HUB *h = GalHUB_GetHubFromLocalServerData(GalSS_EnvComm((GalSS_Environment *) server_data));
  
  GalHUB_SessionEnd(h, GalHUB_SessionLookupByFrame(frame, 0));

  return frame;
}

Gal_Frame new_session(Gal_Frame frame, void *server_data)
{
  SESSION *s = GalHUB_SessionNew(frame);
  if (s) {
    GalSS_EnvUpdateSessionID((GalSS_Environment *) server_data,
			     s->session_id);
  }
  return frame;
}

Gal_Frame
hub_turn_management(Gal_Frame frame, void *server_data)
{ char *action = NULL, *act;
  char *(*fn)() = NULL;
  char *message = NULL;
  SESSION *session;
  int i;
  Gal_Frame reply_frame, request_frame;
  TObj to;

  action = Gal_FrameName(Gal_GetFrame(frame, ":parse_frame"));
  if (action)
  { for (i=0;(act = hub_function_map[i].name);i++)
    { if (Gal_StringEq(action, act))
      { fn = hub_function_map[i].fn;
	break;
      }
    }
  }

  if (fn)
  { message = (*fn)(frame, NULL, 0);
    if (message)
    {
      reply_frame = Gal_MakeFrame(message, GAL_CLAUSE);
      Gal_SetProp(frame, ":reply_frame", Gal_FrameObject(reply_frame));
    }
    else reply_frame = Gal_GetFrame(frame, ":reply_frame");
    if (reply_frame)
    { request_frame = Gal_GetFrame(frame, ":parse_frame");
      if (request_frame)
      {
	if ((!(Gal_GetObject(reply_frame, ":domain"))) && ((to = Gal_GetObject(request_frame, ":domain"))))
	  Gal_SetProp(reply_frame, ":domain", Gal_CopyObject(to));
      }
    }
  }
  else
  { GalUtil_Warn("Builtin functionality doesn't recognize the clause name %s!!\n", action);
  }
  session = GalHUB_SessionLookupByFrame(frame, 0);
  if (session->discourse_id > 0)
    session->discourse_id--; /* don't increment history counter for local utterances !! */
  return(frame);
}

Gal_Frame 
log_keys(Gal_Frame frame, void *server_data) 
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  HUB *h = GalHUB_GetHubFromLocalServerData(GalSS_EnvComm(env));
  SESSION *s;
  SERVICE_PROVIDER *server;
  SERVICE_TYPE *stype;

  s = GalHUB_SessionLookupByFrameInHUB(frame, 0);
  
  /* The builtin server is THIS SERVER. But we don't want
     a GalIO_CommStruct *, we want a SERVICE_PROVIDER *. */
  stype = GalHUB_FindServiceType(h, BUILTIN_SERVER);
  /* Use the first provider. */
  server = (SERVICE_PROVIDER *) Gal_PointerBufferNthElement(stype->providers, 0);

  GalHUB_LogfileLogMessage(s, server, frame,"log_keys",GalIO_MsgTypeToName(GAL_MESSAGE_MSG_TYPE));
  return (NULL);
  /* return frame; */
}

/* This function is called by GalSS_EnvSetSession(). It should
   override any locks and update the locks associated with the
   session. If there is no lock info, it just transfers the
   existing locks to the new session. This function can be called
   from the scripting language, of course, which means that LOCK:
   can't force an unlock, but --> builtin.set_session can. */

Gal_Frame set_session(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  SESSION *session = GalHUB_SessionLookupByFrame(f, 1);
  /* AND a global reference. Ouch. */
  int tidx = Gal_GetInt(env->hub_data, GAL_TOKEN_INDEX_FRAME_KEY);
  TOKEN *t = GalHUB_GetTokenFromIndex(tidx);
  Gal_Object lock_info_obj = Gal_GetObject(f, ":lock_info");
  int new_writes_only_to_session = 0;
  int new_reads_only_from_session = 0;
  int new_writes_only_to_server = 0;
  int lock_value = 0;
  char *previous_session_id = Gal_GetString(f, ":previous_session_id");
  SESSION *previous_session = (SESSION *) NULL;
 

  /* We don't need a server if there's no lock info. */
  if ((!session) || (!t) || (lock_info_obj && (!t->owner))) {
    GalUtil_Warn("Can't find session and server; not updating session info");
    return (Gal_Frame) NULL;
  }

  if (previous_session_id) {
    previous_session = GalHUB_SessionLookupByID(previous_session_id, 0);
  }
  if (!previous_session)
    /* To make sure that handle locks doesn't break. */
    previous_session = session;

  /* The update is a little tricky. I will only free locks if
     there's a previous session and the locks are for the
     previous session. Otherwise, out of luck. I'll try to free
     using the new session, but that's not likely to work
     if there are previous unfreed locks for
     GAL_SESSION_WRITES_ONLY_TO_SERVER. */

  if (lock_info_obj) {
    lock_value = Gal_IntValue(lock_info_obj);
    new_writes_only_to_session = lock_value & (GAL_SERVER_WRITES_ONLY_TO_SESSION | GAL_PERMANENT_LOCK);
    new_reads_only_from_session = lock_value & (GAL_SERVER_READS_ONLY_FROM_SESSION | GAL_PERMANENT_LOCK);
    new_writes_only_to_server = lock_value & (GAL_SESSION_WRITES_ONLY_TO_SERVER | GAL_PERMANENT_LOCK);
  } else {
    /* If there's no lock info, we need to figure out the
       existing lock info. Not the simplest thing in the world. */
    GalHUB_SessionDeduceLocks(previous_session, t->owner,
			      &new_writes_only_to_session,
			      &new_reads_only_from_session,
			      &new_writes_only_to_server);
  }
  /* Remove all the locks. */
  GalHUB_SessionHandleLocks(previous_session, t->owner,
			    t->tidx, 
			    GAL_SERVER_READS_ONLY_FROM_SESSION |
			    GAL_SERVER_WRITES_ONLY_TO_SESSION |
			    GAL_SESSION_WRITES_ONLY_TO_SERVER |
			    GAL_PERMANENT_LOCK,
			    0, 0, 1, 0);
  /* Update the locks. */
  if (new_writes_only_to_session)
    GalHUB_SessionHandleLocks(session, t->owner, t->tidx,
			      GAL_SERVER_WRITES_ONLY_TO_SESSION |
			      GAL_PERMANENT_LOCK,
			      new_writes_only_to_session, 0, 0, 0);
  if (new_reads_only_from_session)
    GalHUB_SessionHandleLocks(session, t->owner, t->tidx,
			      GAL_SERVER_READS_ONLY_FROM_SESSION |
			      GAL_PERMANENT_LOCK,
			      new_reads_only_from_session, 0, 0, 0);
  if (new_writes_only_to_server)
    GalHUB_SessionHandleLocks(session, t->owner, t->tidx,
			      GAL_SESSION_WRITES_ONLY_TO_SERVER |
			      GAL_PERMANENT_LOCK,
			      new_writes_only_to_server, 0, 0, 0);
  return (Gal_Frame) NULL;
}

/* Session and server property management. */

static SERVICE_PROVIDER *__hub_find_token_owner(GalSS_Environment *env);  

Gal_Frame get_properties(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  /* I shouldn't look inside the structure like this, but
     I'm not sure I want to add an API call. */
  SESSION *session;
  char *namespace = Gal_GetString(f, ":namespace");
  Gal_Frame source_f = (Gal_Frame) NULL;
  int num_keys, i;
  Gal_Object *keys = Gal_GetList(f, ":properties", &num_keys);
  SERVICE_PROVIDER *provider;

  if (!keys) {
    GalSS_EnvError(env, "no properties requested");
    return (Gal_Frame) NULL;
  } else if (!strcmp(namespace, "server")) {
    provider = __hub_find_token_owner(env);
    if (!provider) {
      GalSS_EnvError(env, "can't find server");
      return (Gal_Frame) NULL;
    } else {
      source_f = provider->properties;
    }
  } else if (!strcmp(namespace, "session")) {
    session = GalHUB_SessionLookupByFrame(f, 1);
    if (!session) {
      GalSS_EnvError(env, "can't create or find session");
      return (Gal_Frame) NULL;
    } else {
      source_f = session->session_vars;
    }
  }
  if (!source_f) {
    GalSS_EnvError(env, "can't find source frame");
    return (Gal_Frame) NULL;
  } else {
    Gal_Frame reply_frame = Gal_MakeClauseFrame("properties");
    
    for (i = 0; i < num_keys; i++) {
      char *s = Gal_StringValue(keys[i]);
      if (s) {
	Gal_Object o = Gal_GetObject(source_f, s);
	if (o) {
	  Gal_SetProp(reply_frame, s, Gal_CopyObject(o));
	}
      }
    }	  
    return reply_frame;
  }
  return (Gal_Frame) NULL;
}
    
/* Delete happens before set. See GalSS_EnvModifyServerProperties, for instance. */

Gal_Frame modify_properties(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  /* I shouldn't look inside the structure like this, but
     I'm not sure I want to add an API call. */
  SESSION *session;
  char *namespace = Gal_GetString(f, ":namespace");
  Gal_Frame target_f = (Gal_Frame) NULL;
  SERVICE_PROVIDER *provider;

  if (!strcmp(namespace, "server")) {
    provider = __hub_find_token_owner(env);
    if (!provider) {
      return (Gal_Frame) NULL;
    } else {
      target_f = provider->properties;
    }
  } else if (!strcmp(namespace, "session")) {
    session = GalHUB_SessionLookupByFrame(f, 1);
    if (!session) {
      return (Gal_Frame) NULL;
    } else {
      target_f = session->session_vars;
    }
  }
  if (!target_f) {
    return (Gal_Frame) NULL;
  } else {
    int num_keys, i;
    Gal_Object *keys = Gal_GetList(f, ":properties_to_delete", &num_keys);
    if (keys) {
      for (i = 0; i < num_keys; i++) {
	char *s = Gal_StringValue(keys[i]);
	if (s) {
	  Gal_DelProp(target_f, s);
	}
      }
    }      
    Gal_UpdateFrameProperties(target_f,
			      Gal_GetFrame(f, ":properties_to_set"),
			      (char **) NULL);
  }
  return (Gal_Frame) NULL;
}

/* This only works in the Hub. */

static SERVICE_PROVIDER *__hub_find_token_owner(GalSS_Environment *env)
{
  int tidx = Gal_GetInt(env->hub_data, GAL_TOKEN_INDEX_FRAME_KEY);
  TOKEN *t;

  /* AND a global reference. Ouch. */
  t = GalHUB_GetTokenFromIndex(tidx);
  if (t && t->owner)
    return t->owner;
  else
    return (SERVICE_PROVIDER *) NULL;
}

Gal_Frame hub_token_owner(Gal_Frame f, void *server_data)
{
  SERVICE_PROVIDER *p = __hub_find_token_owner((GalSS_Environment *) server_data);
  SERVICE_TYPE **stypes;

  if (p) {
    stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(p->stypes);

    /* We'll use the first stype as the name of the owner. */
    Gal_SetProp(f, ":owner", Gal_StringObject(stypes[0]->name));
    Gal_SetProp(f, ":owner_id",
		Gal_StringObject(GalHUB_ServiceProviderID(p)));
  }
  return f;
}  
  

/*
 *   NEW TEST AND DEBUG CODE
 */

/* This function checks the status of a specified server or type. */

Gal_Frame hub_ping(Gal_Frame f, void *server_data)
{
  char *provider_string = Gal_GetString(f, ":service_provider");
  char *stype_name = Gal_GetString(f, ":service_type");
  HUB *h = GalHUB_GetHubFromLocalServerData(GalSS_EnvComm((GalSS_Environment *) server_data));
  GalSS_ProviderSpec *spec;
  SERVICE_PROVIDER *p;
  int connected = 0;
  int known = 0;

  if (provider_string) {
    spec = GalHUB_SplitServerName(provider_string);

    if (spec) {
      p = GalHUB_ProviderSpecToProvider(h, spec);

      if (p) {
	known = 1;
	/* Check connected status. */
	if ((p->status == FREE) ||
	    (p->status == LOCAL)) {
	  connected = 1;
	}
      }
    }
  } else if (stype_name) {
    SERVICE_TYPE *stype = GalHUB_FindServiceType(h, stype_name);
    if (stype) {
      int num_providers = Gal_PointerBufferSize(stype->providers);
      SERVICE_PROVIDER **providers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(stype->providers);
      int j;
      for (j = 0; j < num_providers; j++) {
	known++;
	if ((providers[j]->status == FREE) ||
	    (providers[j]->status == LOCAL)) {
	  connected++;
	}
      }
    }    
  }
  Gal_SetProp(f, ":connected", Gal_IntObject(connected));
  Gal_SetProp(f, ":known", Gal_IntObject(known));
  return f;
}

Gal_Frame hub_available_servers(Gal_Frame f, void *server_data)
{
  HUB *h = GalHUB_GetHubFromLocalServerData(GalSS_EnvComm((GalSS_Environment *) server_data));
  int num_stypes = Gal_PointerBufferSize(h->stypes);
  int i;
  SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(h->stypes);
  Gal_Object res = Gal_CreateListObject((Gal_Object *) NULL, 0, _gal_free_object, 1);

  for (i = 0; i < num_stypes; i++) {
    Gal_ListObjectAdd(res, Gal_StringObject(stypes[i]->name));
  }
  Gal_SetProp(f, ":servers", res);
  return f;
}

/* this function prints out messages or namespaces.
   Also utterance databases or session locks. */

Gal_Frame hub_print(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  HUB *h = GalHUB_GetHubFromLocalServerData(GalSS_EnvComm(env));
  /* I shouldn't look inside the structure like this, but
     I'm not sure I want to add an API call. */
  int tidx;
  TOKEN *t;
  SESSION *session;
  char *namespace = Gal_GetString(f, ":namespace");
  Gal_Object verbosity_obj = Gal_GetObject(f, ":verbosity");
  char *message = Gal_GetString(f, ":message");
  int verbosity = -1;
  Gal_Frame target_f = (Gal_Frame) NULL;
  int print_locks = Gal_GetInt(f, ":session_locks");
  int print_db = Gal_GetInt(f, ":db");

  if (verbosity_obj && Gal_Intp(verbosity_obj))
    verbosity = Gal_IntValue(verbosity_obj);

  if ((verbosity == -1) || (GAL_VERBOSE >= verbosity)) {
    if (message) {
      GalUtil_Print(verbosity, message);
    }
    if (namespace) {
      Gal_NamespaceEntry *ne = Gal_FindNamespaceEntry(namespace, HubNamespaceTable);

      if (ne) {
	switch (ne->namespace_int) {
	case GAL_SESSION_NAMESPACE:
	  session = GalHUB_SessionLookupByFrame(f, 1);
	  if (session)
	    target_f = session->session_vars;
	  break;
	case GAL_UTTERANCE_DB_NAMESPACE:
	  break;
	case GAL_MESSAGE_NAMESPACE:
	  target_f = f;
	  break;
	case GAL_SERVER_NAMESPACE:
	  tidx = Gal_GetInt(env->hub_data, GAL_TOKEN_INDEX_FRAME_KEY);
	  t = GalHUB_GetTokenFromIndex(tidx);
	  if (t && t->owner)
	    target_f = t->owner->properties;
	  break;
	case GAL_TOKEN_NAMESPACE:	  
	  tidx = Gal_GetInt(env->hub_data, GAL_TOKEN_INDEX_FRAME_KEY);
	  t = GalHUB_GetTokenFromIndex(tidx);
	  if (t)
	    target_f = t->state;
	  break;
	case GAL_GLOBAL_NAMESPACE:
	  target_f = h->globals;
	  break;
	}
	if (target_f) {
	  Gal_PPFrame(target_f);
	}
      }
    }
    if (print_locks)
      print_session_locks(GAL_PINFO1_LEVEL, 0);
    if (print_db) {
      session = GalHUB_SessionLookupByFrame(f, 0);
      if (session) {
	print_DB(session->history);
      }
    }
  }
  return (Gal_Frame) NULL;  
}  
     
Gal_Frame hub_break(Gal_Frame f, void *server_data)
{
  int tidx = Gal_GetInt(((GalSS_Environment *) server_data)->hub_data, GAL_TOKEN_INDEX_FRAME_KEY);
  HUB *h = GalHUB_GetHubFromLocalServerData(GalSS_EnvComm((GalSS_Environment *) server_data));
  TOKEN *t = GalHUB_GetTokenFromIndex(tidx);
  SESSION *session = GalHUB_SessionLookupByFrame(f, 1);
  SERVICE_PROVIDER *sp = (SERVICE_PROVIDER *) NULL;

  if (t)
    sp = t->owner;
  
  if (!_gal_hub_break(h, t, sp, f, session)) {
    Gal_SetProp(f, ":disable_break", Gal_IntObject(1));
  }
  return f;
}

Gal_Frame hub_exit(Gal_Frame f, void *server_data)
{
  force_hub_exit();
  return (Gal_Frame) NULL;
}

Gal_Frame hub_raise_error(Gal_Frame f, void *server_data)
{
  char *error = Gal_GetString(f, ":error_description");

  if (error) {
    GalSS_EnvError((GalSS_Environment *) server_data, error);
  }
  return (Gal_Frame) NULL;
}

Gal_Frame hub_pause(Gal_Frame f, void *server_data)
{
  HUB *h = GalHUB_GetHubFromLocalServerData(GalSS_EnvComm((GalSS_Environment *) server_data));
  if (Gal_GetObject(f, ":milliseconds"))
    h->hub_pause = Gal_GetInt(f, ":milliseconds");
  return (Gal_Frame) NULL;
}

Gal_Frame hub_sleep(Gal_Frame f, void *server_data)
{
  if (Gal_GetObject(f, ":milliseconds"))
    GalUtil_MilliSleep(Gal_GetInt(f, ":milliseconds"));
  return (Gal_Frame) NULL;
}

Gal_Frame hub_gui_notify(Gal_Frame f, void *server_data)
{
  HUB *h = GalHUB_GetHubFromLocalServerData(GalSS_EnvComm((GalSS_Environment *) server_data));
  
  if (h->gui && h->gui->provider &&
      (h->gui->provider->status > DISCONNECTED)) {
    /* Get the message and send it. */
    int msg_len = -1;
    Gal_Object *msg_list = Gal_GetList(f, ":msgs", &msg_len);

    if (!msg_list) {
      Gal_Frame sub_f = Gal_GetFrame(f, ":msg");
      if (sub_f) {
	/* It's just a normal one. */
	GalIO_CommWriteFrame(h->gui->provider->gcomm, sub_f, 1);
      }
    } else {
      int i;

      for (i = 0; i < msg_len; i++) {
	GalIO_CommWriteFrame(h->gui->provider->gcomm,
			     Gal_FrameValue(msg_list[i]), 1);
      }
    }
  }
  return (Gal_Frame) NULL;
}

Gal_Frame hub_continue(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  int num_replies = 0;
  Gal_Object *reply_matches = Gal_GetList(f, ":reply_matches", &num_replies);
  int num_errors = 0;
  Gal_Object *error_matches = Gal_GetList(f, ":error_matches", &num_errors);
  char *stype = Gal_GetString(f, ":service_type");
  char *provider = Gal_GetString(f, ":service_provider");
  HUB *h = GalHUB_GetHubFromLocalServerData(GalSS_EnvComm(env));
  SESSION *session = GalHUB_SessionLookupByFrame(f, 1);
  Gal_Frame *reply_frames = (Gal_Frame *) NULL;
  Gal_Frame *error_frames = (Gal_Frame *) NULL;
  int i, j;
  SERVICE_PROVIDER *p = (SERVICE_PROVIDER *) NULL;
  char *provider_stype_name = (char *) NULL;
  int tidx = Gal_GetInt(env->hub_data, GAL_TOKEN_INDEX_FRAME_KEY);
  Gal_Object script_info = Gal_GetObject(env->hub_data, GAL_OPAQUE_SCRIPT_MODULE_HUB_FRAME_KEY);
  int scriptless = Gal_GetInt(env->hub_data, GAL_SCRIPT_STATUS_HUB_FRAME_KEY);
  int provider_id = -1;
  
  /* In order to record the continuation, we need to compute
     a bunch of things. First, we need to try to translate
     the provider name, if given, into an actual provider.
     Next, we need to put the reply and error matches into
     the appropriate array. */
  
  if (num_replies) {
    reply_frames = (Gal_Frame *) calloc(1 + num_replies, sizeof(Gal_Frame));
    j = 0;
    for (i = 0; i < num_replies; i++) {
      if (Gal_Framep(reply_matches[i])) {
	reply_frames[j++] = Gal_CopyFrame(Gal_FrameValue(reply_matches[i]));
      } else {
	GalUtil_WarnWithLocation(__FUNCTION__, "Not all elements in :reply_matches in continuation reply are frames");
      }
    }
  }

  if (num_errors) {
    error_frames = (Gal_Frame *) calloc(1 + num_errors, sizeof(Gal_Frame));
    j = 0;
    for (i = 0; i < num_errors; i++) {
      if (Gal_Framep(error_matches[i])) {
	error_frames[j++] = Gal_CopyFrame(Gal_FrameValue(error_matches[i]));
      } else {
	GalUtil_Warn("Not all elements in :error_matches in continuation reply are frames");
      }
    }
  }

  if (provider) {
    GalSS_ProviderSpec *spec = GalHUB_SplitServerName(provider);

    if (spec) {
      p = GalHUB_ProviderSpecToProvider(h, spec);
      if (spec->stype_name) {
	provider_stype_name = _gal_strdup(spec->stype_name);
      }
      if (p)
	provider_id = p->id;
      GalSS_FreeProviderSpec(spec);
    }
  }

  if (!provider_stype_name) {
    if (stype) {
      provider_stype_name = _gal_strdup(stype);
    }
  }  
  
  /* In this function, I will attempt to enqueue a
     Hub continuation. The continuation will consist of the
     current token ID, the error and reply match frames,
     and the possible service type and provider restrictions. */
  
  /* First, we postpone the reply. */
  GalSS_EnvPostponeReply(env);

  /* Next, we record the continuation. It will be up to the
     Hub itself to notify the scripting module that a continuation
     is at hand. */
  GalHUB_EnqueueHubContinuation(session,
				tidx, Gal_CopyObject(script_info),
				/* The scriptless setting can be
				   inherited, but the invoked stype
				   has already been lost. */
				scriptless, (char *) NULL,
				reply_frames, error_frames,
				provider_stype_name, provider_id);
  return (Gal_Frame) NULL;  
}

Gal_Frame hub_set_verbosity(Gal_Frame f, void *server_data)
{
  Gal_Object verbosity_obj = Gal_GetObject(f, ":verbosity");

  if ((!verbosity_obj) || (!Gal_Intp(verbosity_obj))) {
    GalUtil_Warn("No verbosity specified for hub_set_verbosity");
    return (Gal_Frame) NULL;
  }

  GalUtil_SetVerbose(Gal_IntValue(verbosity_obj));
  return (Gal_Frame) NULL;
}

Gal_Frame hub_token_timestamp(Gal_Frame f, void *server_data)
{ 
  int tidx = Gal_GetInt(((GalSS_Environment *) server_data)->hub_data, GAL_TOKEN_INDEX_FRAME_KEY);
  TOKEN *token = GalHUB_GetTokenFromIndex(tidx);
  double timestamp = token->timestamp;

  /* Convert the double representation of the timestamp into integer 
     seconds and milliseconds. */
  int secs = (int) timestamp;
  int msecs = (int) ((timestamp - ((double)secs)) * 1000);

  Gal_Frame token_timestamp_frame = Gal_MakeFrame("token_timestamp", GAL_CLAUSE);
  
  Gal_SetProp(token_timestamp_frame, ":seconds", Gal_IntObject(secs));
  Gal_SetProp(token_timestamp_frame, ":milliseconds", Gal_IntObject(msecs));
  return token_timestamp_frame;
}

Gal_Frame hub_gc_version(Gal_Frame f, void *server_data)
{
  /* Bit shifts and masks to create a list of three elements. */
  Gal_SetProp(f, ":major", Gal_IntObject((GC_VERSION >> 16) & 0xff));
  Gal_SetProp(f, ":minor", Gal_IntObject((GC_VERSION >> 8) & 0xff));
  Gal_SetProp(f, ":sub_minor", Gal_IntObject(GC_VERSION & 0xff));
  return f;
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
