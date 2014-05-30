/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include <time.h>
#include "galaxy/galaxy.h"
#include "hub_internal.h"
#include "galaxy/distinguished_keys.h"

extern struct SESSION *Sessions;
extern HUB *Hub;

void free_session_vars(SESSION *s);
void GalHUB_SessionSetVar(SESSION *s, char *var, Gal_Object value);
static void reset_alarms(SESSION *s);
static void __flush_hub_continuations(SESSION *s);

void free_session(SESSION *session)
{
  SESSION *prev;
  _GalHUB_ReportSessionStatus(Hub, GAL_PINFO1_LEVEL,
			      GAL_HUB_SESSION_DESTROYED, session, 0);
  if (session->aborted_utts)
    free(session->aborted_utts);
  free(session->session_id);
  GalHUB_DBEraseHistories(session, 1);
  free_logfile(session->log);
  free_session_vars(session);
  /* Locks were freed in GalHUB_SessionEnd. */
  Gal_FreePointerBuffer(session->lock_info);
  free(session->alarm_last_reset);
  free(session->alarm_secs_to_expiration);
  free(session->alarm_disabled);
  __flush_hub_continuations(session);

  /* now unlink us */
  if (Sessions == session) {
    Sessions = session->next;
    free(session);
  } else {
    prev = Sessions;
    while(prev && prev->next != session)
      prev = prev->next;
    if(!prev)
      GalUtil_Warn("Sessions linked list is in bad shape");
    else {
      prev->next = session->next;
      free(session);
    }
  }  
}

/* SAM 11/17/00: The system can no longer tell when it's
   got an I/O device - it used to assume that dispatch
   servers were exclusive I/O devices, but that's now wrong,
   even when translated into current terminology. So
   just bag this. */

#if 0
void reset_session_utterance_id(SESSION *session)
{
  if (session && session->utterance_id < 0) 
  {
    session->utterance_id = 0;
    GalHUB_SessionSetVar(session, GAL_HUB_SESSION_UTTERANCE_ID_HUB_FRAME_KEY, Gal_IntObject(session->utterance_id));
  }
}
#endif

SESSION *
initialize_session(char *session_id)
{
  SESSION *session;
  session = (SESSION *) calloc(1, sizeof(SESSION));
  session->session_id = _gal_strdup(session_id);
  session->utterance_id = -1;
  session->discourse_id = -1;
  session->alarm_last_reset = (time_t *)calloc(GalHUB_AlarmGetNum()?GalHUB_AlarmGetNum():1,sizeof(time_t));
  session->alarm_secs_to_expiration = (int *)calloc(GalHUB_AlarmGetNum()?GalHUB_AlarmGetNum():1,sizeof(int));
  session->alarm_disabled = (int *)calloc(GalHUB_AlarmGetNum()?GalHUB_AlarmGetNum():1,sizeof(int));
  session->next = NULL;
  session->lock_info = Gal_MakePointerBuffer(NULL, GAL_LOCK_INFO_PTYPE,
					     0, 0, 1, 1,
					     NULL, 10, 0);
  /* Use the name of the frame to stash the session ID. */
  session->session_vars = Gal_MakeFrame(session->session_id, GAL_CLAUSE);
  session->domain_histories = (Gal_Frame **) calloc(GalHUB_MaxDomains, sizeof(Gal_Frame *));
  GalHUB_SessionSetVar(session, GAL_HUB_SESSION_UTTERANCE_ID_HUB_FRAME_KEY, Gal_IntObject(session->utterance_id));
  _GalHUB_ReportSessionStatus(Hub, GAL_PINFO1_LEVEL,
			      GAL_HUB_SESSION_CREATED, session, 0);
  return(session);
}

SESSION *
GalHUB_SessionLookupByID(char *session_id, int create)
{ SESSION *session, *last_session = NULL; 
  char *match_session_id = NULL;

  if (session_id) match_session_id = session_id;
  else match_session_id = DEFAULT_SESSION_ID;

  for (session = Sessions;(session);session = session->next)
  { if (!strcmp(session->session_id, match_session_id))
      break;
    last_session = session;
  }

  if (!session && create)
  { session = initialize_session(match_session_id);
    if(!Sessions)
      Sessions = session;
    open_logfile(session);
    if (last_session) last_session->next = session;
  }
  return(session);
}

static SESSION *lookupSessionByFrame(Gal_Frame frame, int create, int inhub) 
{
  char *session_id = Gal_GetString(frame, GAL_SESSION_ID_FRAME_KEY);
  Gal_Object token_obj = Gal_GetObject(frame, GAL_TOKEN_INDEX_FRAME_KEY);

  if (!session_id && token_obj && inhub)
  {
    TOKEN *token = GalHUB_GetTokenFromIndex(Gal_IntValue(token_obj));
    if (token)
      session_id = token->session_id;
  }
  
  if (!session_id)
  { 
    if(0) 
	GalUtil_Warn("Failed to determine session_id", __FUNCTION__);
    session_id = DEFAULT_SESSION_ID;
    create = 1;			/* always have a Default */
  }
  return(GalHUB_SessionLookupByID(session_id,create));
} 

SESSION *GalHUB_SessionLookupByFrameInHUB(Nframe frame, int create) 
{ 
  return lookupSessionByFrame(frame,create,1);
}

SESSION *GalHUB_SessionLookupByFrame(Nframe frame, int create)
{
   return lookupSessionByFrame(frame,create,0);
}

void GalHUB_SessionSetVar(SESSION *s, char *var, Gal_Object value)
{
  if(!s || !s->session_vars)
    return;

  Gal_SetProp(s->session_vars, var, value);  
}

/* this does NOT return a copy... */
Gal_Object GalHUB_SessionGetVar(SESSION *s, char *var)
{
  if(!s || !s->session_vars)
    return NULL;

  return Gal_GetObject(s->session_vars, var);
}

void GalHUB_SessionDeleteVar(SESSION *s, char *var)
{
  Gal_DelProp(s->session_vars, var);
}

void free_frames(Gal_Frame *frames, int n)
{ 
  int i;
  for(i=0;i<n;i++) {
    if (frames[i]) Gal_FreeFrame(frames[i]);
    frames[i] = NULL;
  }
}

void free_session_vars(SESSION *s)
{
  if (!s || !s->session_vars)
    return;
  Gal_FreeFrame(s->session_vars);
}

/* SAM 8/4/00: It seems to me that if the log is
   already opened, we don't want to blindly set the
   utterance_id, since things may already have been
   logged (that is, this function can be called quite
   happily if the session has already been created). */

/* SAM 11/30/00: We now understand far more than we ever
   wanted to about what was happening here. Essentially,
   there was never a problem if you weren't reusing
   sessions; the first time through, most of the stuff
   in this function simply duplicates the session
   creation code. However, when you reused sessions, the
   code set log_already_opened to 0, even though the log
   was open, so that the NEXT time through, the log would
   be reopened. That is, the first call to new_session
   was redundant, and every subsequent call reopened
   the log. It's very hard to see how to do this
   otherwise, except to make sure the logic is explicit
   and well-documented; sessions were almost always
   created in advance of calling builtin.new_session
   (and now, they're guaranteed to be, since the Hub
   creates a session the instant it finds a new session ID
   in an incoming frame), and if you want to reuse
   a session ID, you don't want a tiny little log that
   captures whatever happened before new_session was called.
   So we'll introduce a flag on sessions which essentially
   keeps track of whether the session had this function
   called on it or not. */

SESSION *GalHUB_SessionNew(Gal_Frame frame)
{
  SESSION *session;
 
  session = GalHUB_SessionLookupByFrame(frame, 1);

  if (session) {
    int i;

    /* The first time through, the call to new_session
       is essentially superfluous, since it follows
       the creation of the session. However, on
       each subsequent call, we want at least to
       reopen the log. */
    if (session->new_session_called) {
      if (logfile_is_open(session))
	close_log_file(session);
      open_logfile(session);
    } else {
      session->new_session_called = 1;
    }
    
    session->utterance_id = -1;
    session->discourse_id = -1;

    GalHUB_SessionSetVar(session, GAL_HUB_SESSION_UTTERANCE_ID_HUB_FRAME_KEY, Gal_IntObject(session->utterance_id));
    session->num_system_errors = 0;
    
    GalHUB_SessionFlushLocks(session);
    reset_alarms(session);

    /* hmm -- who deleted the following????  It s/b there.  -RL */
    if(session->aborted_utts)
      for (i=0;i<Max_Abort_Utts;i++)
	session->aborted_utts[i] = -1;
  } else {
    GalUtil_Warn("No session associated with frame; not incrementing utterance_id");
    Gal_PPFrame(frame);
  }
  return session;
}

static void reset_alarms(SESSION *s)
{
  int i;
  for(i=0;i<GalHUB_AlarmGetNum();i++) {
    s->alarm_secs_to_expiration[i] = 0;
    s->alarm_disabled[i] = 0;
  }
}

Gal_Frame *GalHUB_SessionFetchDB(char *session_id)
{ SESSION *session;
  session = GalHUB_SessionLookupByID(session_id,0);
  if (!session->history)
    session->history = GalHUB_DBInit();
  return(session->history);
}

/* find a possible prior reference to the switch domain */
int GalHUB_SessionRestoreSwitchedHistory(SESSION *session, char *switch_domain)
{ int i, j;
  Gal_Frame *history, first_frame;
  Gal_Object session_user;
  char *its_domain = NULL;
  GalHUB_SessionSetVar(session,":domain", Gal_StringObject(switch_domain));
  GalHUB_SessionSetVar(session,":num_no_parse", Gal_IntObject(0));
  for (i=0;i<GalHUB_MaxDomains;i++)
  { if ((history = session->domain_histories[i]))
    { for(j=0;((!history[j]) && (j<GalHUB_MaxDomains));j++);
      first_frame = history[j];
      if (first_frame) its_domain = Gal_GetString(first_frame, ":switch_domain");
      if (its_domain && (!strcmp(its_domain, switch_domain)))
      { session->history = history;
	session->discourse_id = Gal_GetInt(first_frame, ":discourse_id");
	/* because we're about to decrement it by one !! */
	session->discourse_id++;
	session->domain_histories[i] = NULL;
	if((session_user = Gal_GetObject(first_frame, GAL_HUB_SESSION_USER_HUB_FRAME_KEY)))
	  GalHUB_SessionSetVar(session,GAL_HUB_SESSION_USER_HUB_FRAME_KEY, Gal_CopyObject(session_user));
	else GalHUB_SessionDeleteVar(session, GAL_HUB_SESSION_USER_HUB_FRAME_KEY);
	return(1);
      }
    }
  }

  if (!session->history)
  { session->history = GalHUB_DBInit();
  }
  session->discourse_id = 1;
  return(0);
}

int GalHUB_SessionResetHistory(SESSION *session, char *current_domain, char *switch_domain)
{ Gal_Frame first_frame;
  Gal_Object session_user;
  int i, previous_record;

  /* first prepare the current history for storing */
  if ((session->discourse_id > -1) && session->history)
  { for(i=0;((!session->history[i]) && (i<=session->discourse_id));i++);
    first_frame = session->history[i];
    if (first_frame)
    { Gal_SetProp(first_frame, ":discourse_id", Gal_IntObject(session->discourse_id));
      Gal_SetProp(first_frame, ":switch_domain", Gal_StringObject(current_domain));
      if ((session_user = GalHUB_SessionGetVar(session, GAL_HUB_SESSION_USER_HUB_FRAME_KEY)))
	Gal_SetProp(first_frame, GAL_HUB_SESSION_USER_HUB_FRAME_KEY, Gal_CopyObject(session_user));
      
      /* find a hole to put it into */
      for (i=0;i<GalHUB_MaxDomains;i++)
      { if (!session->domain_histories[i])
	{ session->domain_histories[i] = session->history;
	  session->history = NULL;
	  break;
	}
      }
    }
  }

  if (session->history) free_frames(session->history, session->discourse_id);
  
  previous_record = GalHUB_SessionRestoreSwitchedHistory(session, switch_domain);
  /* so we can remember this for "scratch that" opportunities */
/*   store_in_history(session->session_id, ":prior_domain", Gal_StringObject(current_domain)); */
  GalHUB_DBStoreInHistory(session->session_id, ":prior_domain", Gal_StringObject(current_domain));

  return(previous_record);
}

extern
void _GalIO_QueueApply(GalIO_PointerQueue *queue,
		       int (*fn)(void *data, void *caller_data),
		       void *caller_data);

/* The fn return 1 (continue), 0 (halt), -1 (dequeue). */

static int __remove_session(void *data, void *caller_data)
{
  SERVER_MESSAGE *msg = (SERVER_MESSAGE *) data;
  SESSION *s = (SESSION *) caller_data;

  if (msg->session == s)
    return -1;
  return 1;
}  

void GalHUB_SessionFlushQueues(HUB *h, SESSION *session)
{
  /* Remove everything which has this session. */
  _GalIO_QueueApply(h->message_queue, __remove_session, (void *) session);
}

void GalHUB_SessionEnd(HUB *h, SESSION *session)
{
  _GalHUB_SessionUnlockAndEnd(h, session, 0);
}

void _GalHUB_SessionUnlockAndEnd(HUB *h, SESSION *session, int force_unlock)
{
  int num_servers = Gal_PointerBufferSize(h->servers);
  SERVICE_PROVIDER **servers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(h->servers);
  int dont_free_session = 0;
  
  if (session) {
    int i;
    GalHUB_SessionFlushQueues(h, session);
    dont_free_session = _GalHUB_SessionUnlockAndFlushLocks(session, force_unlock);
    /* SAM 11/17/00: As we check for the write and read restrictions
       on particular servers, we should check to see what the lock
       status is. If it's permanent, then most likely the server
       provided the session ID when it contacted the Hub. Removing
       it would probably be a bad thing, unless you disconnected the
       server as well. Actually, we can keep track of whether
       we encounter one of these, and if we do, we should leave
       the lock in place and not free the session. */
    for (i = 0; i < num_servers; i++) {
      if (servers[i]->only_session_to_write_to &&
	  (servers[i]->only_session_to_write_to->session == session)) {
	if ((!force_unlock) &&
	    (servers[i]->only_session_to_write_to->value &
	     GAL_PERMANENT_LOCK)) {
	  dont_free_session = 1;
	} else {
	  free(servers[i]->only_session_to_write_to);
	  servers[i]->only_session_to_write_to = (SESSION_LOCK_INFO *) NULL;
	}
      }
      if (servers[i]->only_session_to_read_from &&
	  (servers[i]->only_session_to_read_from->session == session)) {
	if ((!force_unlock) &&
	    (servers[i]->only_session_to_read_from->value &
	     GAL_PERMANENT_LOCK)) {
	  dont_free_session = 1;
	} else {
	  free(servers[i]->only_session_to_read_from);
	  servers[i]->only_session_to_read_from = (SESSION_LOCK_INFO *) NULL;
	}
      }
    }
    close_log_file(session);
    reset_alarms(session);

    if (force_unlock ||
	(strcmp(session->session_id, DEFAULT_SESSION_ID) &&
	 !dont_free_session)) {
      free_session(session);
    } else {
      session->session_ended = 1;
    }
  }
}

/* SAM 11/30/00: I shouldn't flush permanent locks when
   I do this, because the permanent locks are our new
   reimplementation of the dispatch servers, which were
   never removed, and in fact blocked the deletion of a session. */

int GalHUB_SessionFlushLocks(SESSION *s)
{
  return _GalHUB_SessionUnlockAndFlushLocks(s, 0);
}

int _GalHUB_SessionUnlockAndFlushLocks(SESSION *s, int force_unlock)
{
  int i = 0;
  void *lock_info = Gal_PointerBufferNthElement(s->lock_info, i);
  int dont_free_session = 0;
  
  while (lock_info) {
    if (force_unlock ||
	!(((SESSION_LOCK_INFO *) lock_info)->value & GAL_PERMANENT_LOCK)) {
      Gal_PointerBufferRemove(s->lock_info, lock_info);
      free(lock_info);
      lock_info = Gal_PointerBufferNthElement(s->lock_info, i);
    } else {
      dont_free_session = 1;
      i++;
      lock_info = Gal_PointerBufferNthElement(s->lock_info, i);
    }
  }
  return dont_free_session;
}

void GalHUB_SessionFlushLogfile(SESSION *s)
{
  if(!s)
    return;

  GalHUB_LogfileRecordUtteranceForSession(s);
}

int GalHUB_SessionGetAbortUtt(int utterance_id, char *session_id)
{ 
  int i;
  SESSION *session;
  session = GalHUB_SessionLookupByID(session_id, 0);
  if(!session) {
    GalUtil_Warn("Aborting utt %d for session %s because session does not exist (already ended?)",
	     utterance_id, session_id ? session_id : DEFAULT_SESSION_ID);
    return 1;
  }
  if(!session->aborted_utts) return 0;	
  for (i=0;(session->aborted_utts[i] >=0);i++)
  { if (session->aborted_utts[i] == utterance_id) 
    { GalUtil_Warn("Abort set for utt %d for session %s", utterance_id, session_id);
      return (1);
    }
  }
  return(0);
}

void GalHUB_SessionSetAbortUtt(int utterance_id, char *session_id)
{ 
  int i, utt = -1;
  SESSION *session;
  session = GalHUB_SessionLookupByID(session_id, 0);
  if (!session->aborted_utts)
  { session->aborted_utts = (int *) calloc(Max_Abort_Utts, sizeof (int));
    for (i=0;i<Max_Abort_Utts;i++)
    { session->aborted_utts[i] = -1;
    }
  }
  for (i=0;((utt =session->aborted_utts[i]) >= 0);i++)
  { if (utt == utterance_id) return; /* already set!! */
  }
  session->aborted_utts[i] = utterance_id;
  GalUtil_CPInfo1WithLocation(__FUNCTION__, 4,0,"Aborting utt %d for session %s", utterance_id,session_id);
  if (i == Max_Abort_Utts-1)
  { rotate_abort_utts(session->aborted_utts, Max_Abort_Utts, Min_Utts);
  }
}

char *
GalHUB_SessionCreateID()
{
  char ds[16], ts[16], buf[1000];
  make_timestamps (ds, ts);
  sprintf (buf, "session-%s-%s", ts, ds);
  return(_gal_strdup(buf));
}

void
GalHUB_SessionAlarmEnable(SESSION *s, char *key)
{
  int a = GalHUB_AlarmNameToIndex(key);
  s->alarm_disabled[a] = 0;
  _GalHUB_CReportSessionAlarmStatus(Hub, GAL_PINFO2_LEVEL,
				    6, 0, GAL_HUB_SESSION_ALARM_ENABLED,
				    key, NULL, 0, 0);
  s->alarm_last_reset[a] = time(NULL);
  s->alarm_secs_to_expiration[a] = 0;
  GalHUB_LogfileLogAlarmEvent(s,key,GalHUB_AlarmGetEnableKey());
}

void 
GalHUB_SessionAlarmDisable(SESSION *s, char *key)
{
  int a = GalHUB_AlarmNameToIndex(key);
  s->alarm_disabled[a] = 1;
  s->alarm_secs_to_expiration[a] = 0;
  _GalHUB_CReportSessionAlarmStatus(Hub, GAL_PINFO2_LEVEL, 6, 0,
				    GAL_HUB_SESSION_ALARM_DISABLED,
				    key, NULL, 0, 0);
  GalHUB_LogfileLogAlarmEvent(s, key,GalHUB_AlarmGetDisableKey());
}

void
GalHUB_SessionAlarmResetTo(SESSION *session, char *key, int secs)
{
  char s[20];
  int a;
  
  a = GalHUB_AlarmNameToIndex(key);
  sprintf(s,"set=%ds",secs);
  session->alarm_last_reset[a] = time(NULL);
  session->alarm_secs_to_expiration[a] = secs;
  _GalHUB_CReportSessionAlarmStatus(Hub, GAL_PINFO2_LEVEL, 6, 0,
				    GAL_HUB_SESSION_ALARM_RESET,
				    key, NULL, secs, 0);
  GalHUB_LogfileLogAlarmEvent(session,key,s);
}

void GalHUB_SessionHandleLocks(SESSION *s, SERVICE_PROVIDER *server,
			       int tidx, int lock_mask, int lock_value,
			       int log_locks, int force_unlock,
			       int via_listener)
{
  SESSION_LOCK_INFO *sli;
  int i = 0;
  int num_lock_info;
  SESSION_LOCK_INFO **lock_info;
  
  if (!server || !s)
    return;

  /* I'll use a session lock info structure both for the
     case where the sessions must write to servers, and where
     the servers must read from and write to specific sessions.
     I'll store the latter info on the server, and the former
     info on the session. */

  if (lock_mask & GAL_SESSION_WRITES_ONLY_TO_SERVER) {
    num_lock_info = Gal_PointerBufferSize(s->lock_info);
    lock_info = (SESSION_LOCK_INFO **) Gal_PointerBufferPointers(s->lock_info);
    /* First, try to find the element. */
    for (i = 0; i < num_lock_info; i++) {
      sli = lock_info[i];
      if (sli->provider == server) {
	/* If it's present, it's gotta mean that it's
	   one of these locks. So first we check to see if
	   it's locked, and if it isn't, we either save a new one
	   or we remove it. */
	if ((!force_unlock) && (sli->value & GAL_PERMANENT_LOCK)) {
	  GalUtil_Warn("Provider %s wants to release or capture output from session %s, but previous setting is permanent\n", server->pname, s->session_id);
	} else if (lock_value & GAL_SESSION_WRITES_ONLY_TO_SERVER) {
	  /* We're setting, but it's already set. */
	  GalHUB_LogfileLogGetSessionLock(s,server,tidx);
	  _GalHUB_CReportSessionLockStatus(Hub, GAL_PINFO1_LEVEL, 3, 0, GAL_HUB_SESSION_LOCK_SET, NULL, GAL_SESSION_WRITES_ONLY_TO_SERVER, s->session_id, 0, "Provider %s already captures output from session %s\n", server->pname, s->session_id);
	} else {
	  /* We're releasing, so we should free this element. */
	  GalHUB_LogfileLogReleaseSessionLock(s,server,tidx);
	  _GalHUB_CReportSessionLockStatus(Hub, GAL_PINFO1_LEVEL, 3, 0, GAL_HUB_SESSION_LOCK_RELEASED, NULL, GAL_SESSION_WRITES_ONLY_TO_SERVER, s->session_id, 0, "Provider %s releasing output from session %s\n", server->pname, s->session_id);
	  Gal_PointerBufferRemove(s->lock_info, (void *) sli);
	  free(sli);
	}
	break;
      }
    }
    if (i == num_lock_info) {
      if (lock_value & GAL_SESSION_WRITES_ONLY_TO_SERVER) {
	/* We're setting. */
	_GalHUB_CReportSessionLockStatus(Hub, GAL_PINFO1_LEVEL, 3, 0, GAL_HUB_SESSION_LOCK_RELEASED, NULL, GAL_SESSION_WRITES_ONLY_TO_SERVER, s->session_id, 0, "Provider %s capturing output from session %s\n", server->pname, s->session_id);
	sli = (SESSION_LOCK_INFO *) calloc(1, sizeof(SESSION_LOCK_INFO));
	sli->provider = server;
	sli->session = s;
	sli->value = lock_value;
	sli->via_listener = via_listener;
	Gal_PointerBufferAdd(s->lock_info, (void *) sli);
	GalHUB_LogfileLogGetSessionLock(s,server,tidx);
      } else {
	/* We're releasing, even though it wasn't set. */
	GalHUB_LogfileLogReleaseSessionLock(s,server,tidx);
	_GalHUB_CReportSessionLockStatus(Hub, GAL_PINFO1_LEVEL, 3, 0, GAL_HUB_SESSION_LOCK_RELEASED, NULL, GAL_SESSION_WRITES_ONLY_TO_SERVER, s->session_id, 0, "Provider %s releasing output from session %s\n", server->pname, s->session_id);
      }
    }
  }

  if (lock_mask & GAL_SERVER_READS_ONLY_FROM_SESSION) {
    /* Now, we look at the server. */
    if (server->only_session_to_read_from &&
	((!force_unlock) && (server->only_session_to_read_from->value & GAL_PERMANENT_LOCK))) {      
      GalUtil_Warn("Session %s wants to reserve or unreserve provider %s for its exclusive use, but previous setting is permanent\n", s->session_id, server->pname);
    } else if (lock_value & GAL_SERVER_READS_ONLY_FROM_SESSION) {
      /* We're setting. */
      if (server->only_session_to_read_from) {
	_GalHUB_CReportSessionLockStatus(Hub, GAL_PINFO1_LEVEL, 3, 0, GAL_HUB_SESSION_LOCK_RELEASED, NULL, GAL_SERVER_READS_ONLY_FROM_SESSION, s->session_id, 0, "Releasing session reservation of provider %s\n", server->pname);
	free(server->only_session_to_read_from);
      }
      sli = (SESSION_LOCK_INFO *) calloc(1, sizeof(SESSION_LOCK_INFO));
      sli->provider = server;
      sli->session = s;
      sli->value = lock_value;
      sli->via_listener = via_listener;
      server->only_session_to_read_from = sli;      
      _GalHUB_CReportSessionLockStatus(Hub, GAL_PINFO1_LEVEL, 3, 0, GAL_HUB_SESSION_LOCK_SET, NULL, GAL_SERVER_READS_ONLY_FROM_SESSION, s->session_id, 0, "Session %s reserving provider %s for its exclusive use\n", s->session_id, server->pname);
      GalHUB_LogfileLogServeThisSessionOnly(s,server,tidx);
    } else {
      /* We're releasing. */
      if (server->only_session_to_read_from) {
	_GalHUB_CReportSessionLockStatus(Hub, GAL_PINFO1_LEVEL, 3, 0, GAL_HUB_SESSION_LOCK_RELEASED, NULL, GAL_SERVER_READS_ONLY_FROM_SESSION, s->session_id, 0, "Releasing session reservation of provider %s\n", server->pname);
	free(server->only_session_to_read_from);
      }
      GalHUB_LogfileLogServeAnySession(s,server,tidx);
      server->only_session_to_read_from = (SESSION_LOCK_INFO *) NULL;
    }
  }

  /* These are currently not logged. */
  if (lock_mask & GAL_SERVER_WRITES_ONLY_TO_SESSION) {
    /* Now, we look at the server. */
    if (server->only_session_to_write_to &&
	((!force_unlock) && (server->only_session_to_write_to->value & GAL_PERMANENT_LOCK))) {            
      GalUtil_Warn("Session %s wants to capture or release all messages from provider %s, but previous setting is permanent\n", s->session_id, server->pname);
    } else if (lock_value & GAL_SERVER_WRITES_ONLY_TO_SESSION) {
      /* We're setting. */
      if (server->only_session_to_write_to) {
	_GalHUB_CReportSessionLockStatus(Hub, GAL_PINFO1_LEVEL, 3, 0, GAL_HUB_SESSION_LOCK_RELEASED, NULL, GAL_SERVER_WRITES_ONLY_TO_SESSION, s->session_id, 0, "Releasing message capture from provider %s\n", server->pname);
	free(server->only_session_to_write_to);
      }
      sli = (SESSION_LOCK_INFO *) calloc(1, sizeof(SESSION_LOCK_INFO));
      sli->provider = server;
      sli->session = s;
      sli->value = lock_value;
      sli->via_listener = via_listener;
      _GalHUB_CReportSessionLockStatus(Hub, GAL_PINFO1_LEVEL, 3, 0, GAL_HUB_SESSION_LOCK_SET, NULL, GAL_SERVER_WRITES_ONLY_TO_SESSION, s->session_id, 0, "Session %s capturing all messages from provider %s\n", s->session_id, server->pname);
      server->only_session_to_write_to = sli;
    } else {
      /* We're releasing. */
      if (server->only_session_to_write_to) {
	_GalHUB_CReportSessionLockStatus(Hub, GAL_PINFO1_LEVEL, 3, 0, GAL_HUB_SESSION_LOCK_RELEASED, NULL, GAL_SERVER_WRITES_ONLY_TO_SESSION, s->session_id, 0, "Releasing message capture from provider %s\n", server->pname);
	free(server->only_session_to_write_to);
      }
      server->only_session_to_write_to = (SESSION_LOCK_INFO *) NULL;
    }
  }
}

void GalHUB_SessionDeduceLocks(SESSION *session, SERVICE_PROVIDER *s,
			       int *server_writes_to_session,
			       int *server_reads_from_session,
			       int *session_writes_to_server)
{
  int num_lock_info = Gal_PointerBufferSize(session->lock_info);
  SESSION_LOCK_INFO **lock_info = (SESSION_LOCK_INFO **) Gal_PointerBufferPointers(session->lock_info);
  int i;
  SESSION_LOCK_INFO *sli;

  *server_writes_to_session = 0;
  *server_reads_from_session = 0;
  *session_writes_to_server = 0;
  for (i = 0; i < num_lock_info; i++) {
    sli = lock_info[i];
    if (sli->provider == s) {
      *session_writes_to_server = sli->value & (GAL_SESSION_WRITES_ONLY_TO_SERVER | GAL_PERMANENT_LOCK);
      break;
    }
  }
  if (s && s->only_session_to_write_to) {
    *server_writes_to_session = s->only_session_to_write_to->value & (GAL_SERVER_WRITES_ONLY_TO_SESSION | GAL_PERMANENT_LOCK);
  }
  if (s && s->only_session_to_read_from) {
    *server_reads_from_session = s->only_session_to_read_from->value & (GAL_SERVER_READS_ONLY_FROM_SESSION | GAL_PERMANENT_LOCK);
  }
}  

void
rotate_abort_utts(int *abort_utt_list, int max_utts, int min_utts)
{ int i, del_utts; 
  del_utts = max_utts - min_utts;
  for(i=0;i<del_utts;i++)
  { if (i < min_utts)
    { abort_utt_list[i] = abort_utt_list[del_utts+i];
      abort_utt_list[del_utts+i] = -1;
    }
    else abort_utt_list[i] = -1;
  }
}

/* Hub continuations. These are hosted on the session.
   They need to be handled differently from the server
   continuations, although I can't exactly articulate why
   at the moment. */

typedef struct __GalHub_ContinuationMatch {
  Gal_Frame *reply_matches;
  Gal_Frame *error_matches;
  int continuation_tidx;
  char *stype_name;
  int provider_id;
  /* Info which is stored in the opaque
     hub_data which we need to reestablish. */  
  Gal_Object script_info;
  int scriptless;
  char *invoked_stype;
} __GalHub_ContinuationMatch;

void GalHUB_EnqueueHubContinuation(SESSION *s,
				   int tidx,
				   Gal_Object script_info,
				   int scriptless,
				   char *invoked_stype,
				   Gal_Frame *reply_matches,
				   Gal_Frame *error_matches,
				   char *stype_name,
				   int provider_id)
{
  __GalHub_ContinuationMatch *m;

  /* Make sure at least one is present. */
  if ((!reply_matches) && (!error_matches))
    return;

  if (!s)
    return;

  if (!s->continuation_buffer) {    
    s->continuation_buffer = Gal_MakePointerBuffer(NULL,
						   GAL_HUB_CONTINUATION_PTYPE,
						   0, 0, 1, 1,
						   NULL, 10, 0);
  }
  m = (__GalHub_ContinuationMatch *) calloc(1, sizeof(__GalHub_ContinuationMatch));

  m->reply_matches = reply_matches;
  m->error_matches = error_matches;
  m->continuation_tidx = tidx;
  m->script_info = script_info;
  if (invoked_stype) {
    m->invoked_stype = _gal_strdup(invoked_stype);
  }
  m->scriptless = scriptless;
  /* Now, stash away requirements for the service
     type or the provider. */
  m->stype_name = stype_name;
  /* Find the appropriate provider. */
  m->provider_id = provider_id;
  /* Now, add it to the pointer buffer. */
  Gal_PointerBufferAdd(s->continuation_buffer, (void *) m);
}

static void __free_continuation_match(__GalHub_ContinuationMatch *m)
{
  int j;
  
  if (m->reply_matches) {
    for (j = 0; m->reply_matches[j]; j++)
      Gal_FreeFrame(m->reply_matches[j]);
    free(m->reply_matches);
  }
  if (m->error_matches) {
    for (j = 0; m->error_matches[j]; j++)
      Gal_FreeFrame(m->error_matches[j]);
    free(m->error_matches);
  }
  if (m->stype_name)
    free(m->stype_name);
  if (m->invoked_stype)
    free(m->invoked_stype);
  if (m->script_info)
    Gal_FreeObject(m->script_info);
  free(m);
}

static void __GalHUB_RemoveContinuation(SESSION *session,
					__GalHub_ContinuationMatch *m)
{
  /* And now I remove the current continuation match from the
     pointer buffer. */
  if (session->continuation_buffer)
    Gal_PointerBufferRemove(session->continuation_buffer, (void *) m);
  /* And now, I free the continuation match. */
  __free_continuation_match(m);
}

static void __flush_hub_continuations(SESSION *s)
{
  if (s->continuation_buffer) {
    int num_continuations = Gal_PointerBufferSize(s->continuation_buffer);
    __GalHub_ContinuationMatch **continuations = (__GalHub_ContinuationMatch **) Gal_PointerBufferPointers(s->continuation_buffer);
    int i;

    for (i = 0; i < num_continuations; i++) {
      __free_continuation_match(continuations[i]);
    }
    Gal_FreePointerBuffer(s->continuation_buffer);
    s->continuation_buffer = (Gal_PointerBuffer *) NULL;
  }
}

int GalHUB_HubContinuationMatched(HUB *h, SERVER_MESSAGE *msg)
{
  __GalHub_ContinuationMatch *m;
  Gal_Frame frame = msg->message;
  SERVICE_PROVIDER *s = msg->provider;
  SESSION *session = msg->session;
  int i, j;
  __GalHub_ContinuationMatch **c_b;

  if (!session)
    return 0;

  if (!session->continuation_buffer)
    return 0;
  
  c_b = (__GalHub_ContinuationMatch **) Gal_PointerBufferPointers(session->continuation_buffer);
  i = 0;
  /* We need to check the size each time through, since the
     loop can remove elements. Also, we only increment i when
     we don't remove the element. */
  while (i < Gal_PointerBufferSize(session->continuation_buffer)) {
    GalIO_MsgType mtype = (GalIO_MsgType) -1;
    TOKEN *t;
    
    m = c_b[i];    
    /* If there's a provider ID and the IDs don't match, continue. */
    if ((m->provider_id != -1) && (m->provider_id != s->id)) {
      i++;
      continue;
    }
    /* If there's an stype and the stype doesn't match, continue. */
    if (m->stype_name) {
      int num_stypes = Gal_PointerBufferSize(s->stypes);
      SERVICE_TYPE **stypes = (SERVICE_TYPE **) Gal_PointerBufferPointers(s->stypes);
      int j;
      int found = 0;

      for (j = 0; j < num_stypes; j++) {
	if (!strcmp(stypes[j]->name, m->stype_name)) {
	  found = 1;
	  break;
	}
      }
      if (!found) {
	i++;
	continue;
      }
    }
    t = getToken(h, m->continuation_tidx);
    if (!t) {
      /* Remove the element, don't increment. */
      __GalHUB_RemoveContinuation(session, m);
      continue;
    }
    /* Now that we've checked all that stuff, if either the error or
       reply elements match the frame, claim it.
       Remember, Gal_MatchFrame(super, sub) */
    if (m->reply_matches) {
      for (j = 0; m->reply_matches[j]; j++) {
	if (Gal_MatchFrame(frame, m->reply_matches[j])) {
	  mtype = GAL_REPLY_MSG_TYPE;
	  break;
	}
      }
    }
    
    if ((mtype == (GalIO_MsgType) -1) && m->error_matches) {
      for (j = 0; m->error_matches[j]; j++) {
	if (Gal_MatchFrame(frame, m->error_matches[j])) {
	  mtype = GAL_ERROR_MSG_TYPE;
	  break;
	}
      }
    }
    if (mtype != (GalIO_MsgType) -1) {
      /* This message should now look like a reply.
	 So we should populate the message structure
	 with the appropriate information. */
      Gal_SetProp(frame, GAL_TOKEN_INDEX_FRAME_KEY,
		  Gal_IntObject(m->continuation_tidx));
      if (msg->opaque_script_info)
	Gal_FreeObject(msg->opaque_script_info);
      if (m->script_info)
	msg->opaque_script_info = Gal_CopyObject(m->script_info);
      msg->scriptless = m->scriptless;
      if (msg->true_provider_spec.stype_name)
	free(msg->true_provider_spec.stype_name);
      if (m->invoked_stype)
	msg->true_provider_spec.stype_name = _gal_strdup(m->invoked_stype);
      else
	msg->true_provider_spec.stype_name = (char *) NULL;	  
      msg->msg_type = mtype;
      __GalHUB_RemoveContinuation(session, m);
      return 1;
    } else {
      i++;
    }
  }
  return 0;
}				   

