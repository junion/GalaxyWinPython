/*
  This file (c) Copyright 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <stdarg.h>

#include "galaxy/galaxy_all.h"
#include "galaxy/sysdep.h"
#include "hub.h"
#include "hub_internal.h"

extern GalUtil_PrintPkg GalUtil_DefaultPrintPkg;

extern
void _GalUtil_VPkgPrint(GalUtil_PrintPkg *pkg, int level, char *fmt, 
			va_list args);

extern
void _GalUtil_VPkgCPrint(GalUtil_PrintPkg *pkg, int level, int fore, int back,
			 char *fmt, va_list args);

static void __GalHUB_SendHubStatusMessage(HUB *h, Gal_Frame f)
{
  GalIO_CommWriteMessage(h->gui->provider->gcomm, f, GAL_MESSAGE_MSG_TYPE, 1);
}

static void __GalHUB_SendSessionAlarmStatusMessage(HUB *h, int action, 
						   char* key, char* session_id,
						   int expiration_secs,
						   int remaining_secs)
{ 
  Gal_Frame f = Gal_MakeFrame("session_alarm_status", GAL_CLAUSE);
  Gal_SetProp(f, ":action", Gal_IntObject(action));
  Gal_SetProp(f, ":alarm_key", Gal_StringObject(key));
  if(session_id)
    Gal_SetProp(f, ":session_id", Gal_StringObject(key));
  if(expiration_secs)
    Gal_SetProp(f, ":expiration_secs", Gal_IntObject(expiration_secs));
  if(remaining_secs)
    Gal_SetProp(f, ":remaining_secs", Gal_IntObject(remaining_secs));

  __GalHUB_SendHubStatusMessage(h, f);
  Gal_FreeFrame(f);
}

static void __GalHUB_ReportSessionAlarmStatus(HUB *h, int level, int fore, 
					      int back, int action, char* key,
					      char* session_id,
					      int expiration_secs,
					      int remaining_secs)
{
   if (GAL_VERBOSE >= level) {
     if (h && h->gui && h->gui->provider &&
	 (h->gui->provider->status > DISCONNECTED)) {
       __GalHUB_SendSessionAlarmStatusMessage(h, action, key, session_id,
					      expiration_secs, remaining_secs);
    } else {
      /* else display in terminal*/ 
      switch(action) {
      case GAL_HUB_SESSION_ALARM_STATUS:
	if(fore > 0 && back > 0)
	  GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, 
			    "Alarm %s enabled: session %s,  %d secs, %d secs remaining", 
			    key, session_id, expiration_secs, remaining_secs);
	else
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, 
			   "Alarm %s enabled: session %s,  %d secs, %d secs remaining", 
			   key, session_id, expiration_secs, remaining_secs);
	break;
      case GAL_HUB_SESSION_ALARM_ENABLED:
	if(fore > 0 && back > 0)
	  GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, 
			    "Enabling alarm %s", key);
	else
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, 
			   "Enabling alarm %s", key);
	break;
      case GAL_HUB_SESSION_ALARM_DISABLED:
	if(fore > 0 && back > 0)
	  GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back,
			    "Disabling alarm %s", key);
	else
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, 
			   "Disabling alarm %s", key);
	break;
      case GAL_HUB_SESSION_ALARM_RESET:
	if(fore > 0 && back > 0)
	  GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back,
			    "Resetting alarm %s to %d", key, 
			    expiration_secs);
	else 
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, 
			   "Resetting alarm %s to %d", key, 
			   expiration_secs);
	break;
      }
    }
   } 
}

void _GalHUB_ReportSessionAlarmStatus(HUB *h, int level, int action, char* key,
				      char* session_id, int expiration_secs,
				      int remaining_secs)
{
  __GalHUB_ReportSessionAlarmStatus(h, level, -1, -1, action, key, session_id,
				    expiration_secs, remaining_secs);
}

void _GalHUB_CReportSessionAlarmStatus(HUB *h, int level, int fore, int back, 
				       int action, char* key, char* session_id, 
				       int expiration_secs, int remaining_secs)
{
  __GalHUB_ReportSessionAlarmStatus(h, level, fore, back, action, key, 
				    session_id, expiration_secs, 
				    remaining_secs);
}

static void __GalHUB_SendSessionLockStatusMessage(HUB *h, int action,
						  SESSION *sessions,
						  int lock_type, 
						  char *session_id,
						  int debug)
{ 
  SESSION *session;
  int i, num_lock_info, num_servers;
  SESSION_LOCK_INFO **lock_info, *sli;
  SERVICE_PROVIDER **servers, *s;
  Gal_Object providers_sessions_list = NULL;
  Gal_Object providers_sessions_readers_list = NULL;
  Gal_Object providers_sessions_writers_list = NULL;
  Gal_Object list_element = NULL;

  Gal_Frame f = Gal_MakeFrame("session_lock_status", GAL_CLAUSE);
  
  if(debug)
    Gal_SetProp(f, ":debug", Gal_IntObject(1));
  Gal_SetProp(f, ":action", Gal_IntObject(action));
  switch(action) {
  case GAL_HUB_SESSION_LOCK_STATUS:
    
    for (session = sessions;(session);session = session->next) {
      num_lock_info = Gal_PointerBufferSize(session->lock_info);
      lock_info = (SESSION_LOCK_INFO **) Gal_PointerBufferPointers(session->lock_info);
      if(!providers_sessions_list && num_lock_info > 0)
	providers_sessions_list = Gal_CreateListObject((Gal_Object*) NULL, 0, _gal_free_object, 1);
      
      for (i = 0; i < num_lock_info; i++) {
	sli = lock_info[i]; 
	list_element = Gal_ListObjectFromElements(2, Gal_StringObject(GalHUB_ServiceProviderID(sli->provider)),  Gal_StringObject(session->session_id));
	Gal_ListObjectAdd(providers_sessions_list, list_element);
      }
    }
    
    if(providers_sessions_list)
      Gal_SetProp(f, ":providers_sessions", providers_sessions_list);
    
    num_servers = Gal_PointerBufferSize(h->servers);
    servers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(h->servers);
    for (i = 0; i < num_servers; i++) {
      s = servers[i];
      if (s->only_session_to_read_from) {
	if(!providers_sessions_readers_list)
	  providers_sessions_readers_list = Gal_CreateListObject((Gal_Object*) NULL, 0, _gal_free_object, 1);
	list_element = Gal_ListObjectFromElements(2, Gal_StringObject(GalHUB_ServiceProviderID(s)),  Gal_StringObject(s->only_session_to_read_from->session->session_id));
	Gal_ListObjectAdd(providers_sessions_readers_list, list_element);
      }
      
      if (s->only_session_to_write_to) {
	if(!providers_sessions_writers_list)
	  providers_sessions_writers_list = Gal_CreateListObject((Gal_Object*) NULL, 0, _gal_free_object, 1);
	list_element = Gal_ListObjectFromElements(2, Gal_StringObject(GalHUB_ServiceProviderID(s)),  Gal_StringObject(s->only_session_to_write_to->session->session_id));
	Gal_ListObjectAdd(providers_sessions_writers_list, list_element);
      }
    } 

    if(providers_sessions_readers_list)
	Gal_SetProp(f, ":providers_sessions_readers", providers_sessions_readers_list);

    if(providers_sessions_writers_list)
	Gal_SetProp(f, ":providers_sessions_writers", providers_sessions_writers_list);
    break;
  case GAL_HUB_SESSION_LOCK_SET:
  case GAL_HUB_SESSION_LOCK_RELEASED:
    Gal_SetProp(f, ":lock_type", Gal_IntObject(lock_type));
    Gal_SetProp(f, ":session_id", Gal_StringObject(session_id));
    break;
  default:
	break;
  }

  __GalHUB_SendHubStatusMessage(h, f);
  Gal_FreeFrame(f);
}

static void __GalHUB_ReportSessionLockStatus(HUB *h, int level, int fore, 
					     int back, int action, 
					     SESSION *sessions, int lock_type, 
					     char *session_id, int debug,
					     char *fmt, va_list args)
{ 
  SESSION *session;
  int i, num_lock_info, num_servers;
  SESSION_LOCK_INFO **lock_info, *sli;
  SERVICE_PROVIDER **servers, *s;
  
  if (GAL_VERBOSE >= level) {
    if (h && h->gui && h->gui->provider &&
	(h->gui->provider->status > DISCONNECTED)) {
      __GalHUB_SendSessionLockStatusMessage(h, action, sessions, lock_type, 
					    session_id, debug);
    } else {
      /* else display in terminal*/
      switch(action) {
      case GAL_HUB_SESSION_LOCK_STATUS:
	for (session = sessions;(session);session = session->next) {
	  num_lock_info = Gal_PointerBufferSize(session->lock_info);
	  lock_info = (SESSION_LOCK_INFO **) Gal_PointerBufferPointers(session->lock_info);
	  for (i = 0; i < num_lock_info; i++) {
	    sli = lock_info[i];
	    if(fore > 0 && back > 0)
	      GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Session %s can only write to provider %s for the relevant services\n", session->session_id, sli->provider->pname);
	    else 
	      GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Session %s can only write to provider %s for the relevant services\n", session->session_id, sli->provider->pname);        
	  }
	}
	
	num_servers = Gal_PointerBufferSize(h->servers);
	servers = (SERVICE_PROVIDER **) Gal_PointerBufferPointers(h->servers);
	for (i = 0; i < num_servers; i++) {
	  s = servers[i];
	  if (s->only_session_to_read_from) {
	    if(fore > 0 && back > 0)
	      GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Provider %s only reading messages from session %s\n", s->pname, s->only_session_to_read_from->session->session_id);
	    else
	      GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Provider %s only reading messages from session %s\n", s->pname, s->only_session_to_read_from->session->session_id);
	  }
	  
	  if (s->only_session_to_write_to) {
	    if(fore > 0 && back > 0)
	      GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Provider %s only writing messages to session %s\n", s->pname, s->only_session_to_write_to->session->session_id);
	    else
	      GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Provider %s only writing messages to session %s\n", s->pname, s->only_session_to_write_to->session->session_id);
	  }
	}
	break;
      case GAL_HUB_SESSION_LOCK_SET:
      case GAL_HUB_SESSION_LOCK_RELEASED:
	if(fore > 0 && back > 0)
	  _GalUtil_VPkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back,
			      fmt, args);
	else
	  _GalUtil_VPkgPrint(&GalUtil_DefaultPrintPkg, level, 
			     fmt, args);
	break;
      default:
		break;
      }
    }
  }
}

void _GalHUB_ReportSessionLockStatus(HUB *h, int level,
				     int action, SESSION *sessions, 
				     int lock_type, char *session_id, 
				     int debug, char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  __GalHUB_ReportSessionLockStatus(h, level, -1, -1, action, sessions, 
				   lock_type, session_id, debug, fmt, args);
  va_end(args);
}

void _GalHUB_CReportSessionLockStatus(HUB *h, int level, int fore, int back, 
				      int action, SESSION *sessions, 
				      int lock_type, char *session_id,
				      int debug, char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  __GalHUB_ReportSessionLockStatus(h, level, fore, back, action, sessions, 
				   lock_type, session_id, debug, fmt, args);
  va_end(args);
}

static void __GalHUB_SendTokenStatusMessage(HUB *h, int action, TOKEN *t, int debug)
{ 
  Gal_Frame f = Gal_MakeFrame("token_status", GAL_CLAUSE);
  Gal_SetProp(f, ":action", Gal_IntObject(action));
  if(debug)
    Gal_SetProp(f, ":debug", Gal_IntObject(1));
  switch(action) {
  case GAL_HUB_TOKEN_STATUS:
    if(t) {
      Gal_SetProp(f, ":state", Gal_FrameObject(t->state));
    } 
    break;
  case GAL_HUB_TOKEN_CREATED:
  case GAL_HUB_TOKEN_DESTROYED:
    Gal_SetProp(f, ":tidx", Gal_IntObject(t->tidx));
    break;
  default:
	break;
  }
 
  __GalHUB_SendHubStatusMessage(h, f);
  /* Remove the properties before freeing the frame, so we don't have
     to copy the frame. */
  Gal_RemProp(f, ":state");
  Gal_FreeFrame(f);
}

static void __GalHUB_ReportTokenStatus(HUB *h, int level, int fore, int back, 
				       int action, TOKEN *t, int debug)
{
  if (GAL_VERBOSE >= level) {
    if (h && h->gui && h->gui->provider &&
	(h->gui->provider->status > DISCONNECTED)) {
      __GalHUB_SendTokenStatusMessage(h, action, t, debug);
    } else {
      switch(action) {
      case GAL_HUB_TOKEN_STATUS:
	if(fore > 0 && back > 0) {
	  if(t) {
	    GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "\n----------------[%3d]----------------------\n", t->tidx);
	    GalHUB_OutlineFrame(t->state);
	    GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "--------------------------------------------\n\n");
	  } else {
	    GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Null token\n");
	  }
	} else {
	  if(t) {
	    GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "\n----------------[%3d]----------------------\n", t->tidx);
	    GalHUB_OutlineFrame(t->state);
	    GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "--------------------------------------------\n\n");
	  } else {
	    GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Null token\n");
	  }
	}
	break;
      case GAL_HUB_TOKEN_CREATED:
	if(fore > 0 && back > 0) {
	  GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Created token %d\n", t->tidx);
	} else {
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Created token %d\n", t->tidx);
	}
	break;
      case GAL_HUB_TOKEN_DESTROYED:
	if(fore > 0 && back > 0) {
	  GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Destroying token %d\n", t->tidx);
	} else {
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Destroying token %d\n", t->tidx);
	}
	break;
      default:
		  break;
      }
    }
  }
}

void _GalHUB_ReportTokenStatus(HUB *h, int level, int action, TOKEN *t, int debug)
{
  __GalHUB_ReportTokenStatus(h, level, -1, -1, action, t, debug);
}

void _GalHUB_CReportTokenStatus(HUB *h, int level, int fore, int back, int action, TOKEN *t, int debug)
{
  __GalHUB_ReportTokenStatus(h, level, fore, back, action, t, debug);
}

static void __GalHUB_SendSessionStatusMessage(HUB *h, int action, SESSION *session, int debug)
{ 
  Gal_Frame f = Gal_MakeFrame("session_status", GAL_CLAUSE);
  Gal_SetProp(f, ":action", Gal_IntObject(action));
  if(debug)
    Gal_SetProp(f, ":debug", Gal_IntObject(1));
  if(session)
    Gal_SetProp(f, ":session_id", Gal_StringObject(session->session_id));
  switch(action) {
  case GAL_HUB_SESSION_SETTINGS:
    if(session)
      Gal_SetProp(f, ":settings", Gal_FrameObject(session->session_vars));
    break;
  case GAL_HUB_SESSION_CREATED:
    break;
  case GAL_HUB_SESSION_DESTROYED:
    break;
  case GAL_HUB_SESSION_HISTORY:
    if(session && session->history)
      Gal_SetProp(f, ":history", Gal_FrameObject(*(session->history)));
    break;
  default:
	  break;
  }
 
  __GalHUB_SendHubStatusMessage(h, f);
  /* Remove the properties before freeing the frame, so we don't have
     to copy the frame. */
  Gal_RemProp(f, ":history");
  Gal_RemProp(f, ":settings");
  Gal_FreeFrame(f);
}

static void __GalHUB_ReportSessionStatus(HUB *h, int level, int fore, int back,
					 int action, SESSION *session, 
					 int debug)
{
  if (GAL_VERBOSE >= level) {
    if (h && h->gui && h->gui->provider &&
	(h->gui->provider->status > DISCONNECTED)) {
      __GalHUB_SendSessionStatusMessage(h, action, session, debug);
    } else {
      switch(action) {
      case GAL_HUB_SESSION_SETTINGS:
	if(fore > 0 && back > 0) {
	  if(session) {
	    GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Session settings:\n");
	    Gal_PPFrame(session->session_vars);
	  } else {
	    GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "No session found.\n");
	  }
	} else {
	  if(session) {
	    GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Session settings:\n"); 
	    Gal_PPFrame(session->session_vars);
	  } else {
	    GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "No session found.\n");
	  }
	}
	break;
      case GAL_HUB_SESSION_CREATED:
	if(fore > 0 && back > 0) {
	  GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Session %s created\n", session->session_id);  
	} else {
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Session %s created\n", session->session_id);
	}
	break;
      case GAL_HUB_SESSION_DESTROYED:
	if(fore > 0 && back > 0) {
	  GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Session %s destroyed\n", session->session_id);  
	} else {
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Session %s destroyed\n", session->session_id);
	}
	break;
      case GAL_HUB_SESSION_HISTORY:
	if(fore > 0 && back > 0) {
	  if(session) {
	    GalHUB_DBPrint(session->history);
	  } else {
	    GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "No session found.\n");
	  }
	} else {
	  if(session) {
	    GalHUB_DBPrint(session->history);
	  } else {
	    GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "No session found.\n");
	  }
	}
	break;
      default:
		  break;
      }
    }
  }
}

void _GalHUB_ReportSessionStatus(HUB *h, int level, int action, SESSION *session, int debug)
{
  __GalHUB_ReportSessionStatus(h, level, -1, -1, action, session, debug);
}

void _GalHUB_CReportSessionStatus(HUB *h, int level, int fore, int back, int action, SESSION *session, int debug)
{
  __GalHUB_ReportSessionStatus(h, level, fore, back, action, session, debug);
}

static void __GalHUB_SendListenerStatusMessage(HUB *h, int action, int port, int reused)
{ 
  Gal_Frame f = Gal_MakeFrame("listener_status", GAL_CLAUSE);
  Gal_SetProp(f, ":action", Gal_IntObject(action));
  if(port > 0) {
    Gal_SetProp(f, ":port", Gal_IntObject(port));
    Gal_SetProp(f, ":reused", Gal_IntObject(reused));
  }

  __GalHUB_SendHubStatusMessage(h, f);
  Gal_FreeFrame(f);
}

static void __GalHUB_ReportListenerStatus(HUB *h, int level, int fore, int back, 
					  int action, int port, int reused)
{
  if (GAL_VERBOSE >= level) {
    if (h && h->gui && h->gui->provider &&
	(h->gui->provider->status > DISCONNECTED)) {
      __GalHUB_SendListenerStatusMessage(h, action, port, reused);
    } else {
      switch(action) {
      case GAL_HUB_LISTENER_INITIALIZED:
	if(fore > 0 && back > 0) {
	  if(reused) {
	    GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Using existing listener on port %d\n", port);
	  } else {
	    GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Using listener on port %d\n", port);
	  }
	} else {
	  if(reused) {
	    GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Using existing listener on port %d\n", port);
	  } else {
	    GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Using listener on port %d\n", port);
	  }
	}
	break;
      case GAL_HUB_LISTENER_INITIALIZATION_ERROR:
	if(fore > 0 && back > 0) {
	  GalUtil_PkgCPrint(&GalUtil_DefaultPrintPkg, level, fore, back, "Could not initialize Hub listener\n");
	} else {
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, "Could not initialize Hub listener\n");
	}
	break;
      default:
		  break;
      }
    }
  }
}

void _GalHUB_ReportListenerStatus(HUB *h, int level, int action, int port, int reused)
{
  __GalHUB_ReportListenerStatus(h, level, -1, -1, action, port, reused);
}

void _GalHUB_CReportListenerStatus(HUB *h, int level, int fore, int back, int action, int port, int reused)
{
  __GalHUB_ReportListenerStatus(h, level, fore, back, action, port, reused);
}

/* Return 1 to continue, 0 to halt, -1 to dequeue. */

typedef struct __queue_info {
  int msg_index;
  int *tidx;
} __queue_info;

int __get_tidx(void *data, void *caller_data)
{
  SERVER_MESSAGE *msg = (SERVER_MESSAGE *) data;
  __queue_info *qip = (__queue_info *) caller_data;
  
  qip->tidx[qip->msg_index] = msg->tidx;
  qip->msg_index = qip->msg_index + 1;
  return 1;
}

extern
void _GalIO_QueueApply(GalIO_PointerQueue *queue,
		       int (*fn)(void *data, void *caller_data),
		       void *caller_data);

static void __get_current_token_indices(HUB *h, __queue_info *qip)
{
  if (h->msg_queue_length > 0) {
    qip->tidx = (int*) malloc(sizeof(int) * h->msg_queue_length);
    qip->msg_index = 0;
    _GalIO_QueueApply(h->message_queue, __get_tidx, (void *) qip);
  } else {
    qip->tidx = (int *) NULL;
    qip->msg_index = 0;
  }
}

static void __GalHUB_SendMessageQueueStatusMessage(HUB *h, int action, 
						   int *tidx)
{ 
  Gal_Frame f = Gal_MakeFrame("message_queue_status", GAL_CLAUSE);

  Gal_SetProp(f, ":action", Gal_IntObject(action));
  Gal_SetProp(f, ":queue", Gal_CreateInt32Object((void*) tidx,
						 h->msg_queue_length, 1));

  __GalHUB_SendHubStatusMessage(h, f);
  Gal_FreeFrame(f);
}

static void __GalHUB_ReportMessageQueueStatus(HUB *h, int level, int action,
					      int *tidx)
{
  int i;
  if (GAL_VERBOSE >= level) {
    if (h && h->gui && h->gui->provider &&
	(h->gui->provider->status > DISCONNECTED)) {
      __GalHUB_SendMessageQueueStatusMessage(h, action, tidx);
    } else {
      int queue_length = h->msg_queue_length;
      switch(action) {
      case GAL_HUB_MESSAGE_QUEUE_CONTENTS:
	if (queue_length == 0) {
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, 
			   "Message queue length is 0.\n");
	} else {
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, 
			   "Message queue length is %d. Token indices are: ", 
			   queue_length);
	  for(i=0; i<queue_length; ++i)
	    GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, 
			     "%d ", tidx[i]); 
	  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, level, 
			   "\n");
	}
	break;
      default:
		  break;
      }
    }
  }
}

void _GalHUB_ReportMessageQueueStatus(HUB *h, int level, int action)
{
  __queue_info qi;
  
  __get_current_token_indices(h, &qi);
  __GalHUB_ReportMessageQueueStatus(h, level, action, qi.tidx);
  if (qi.tidx) free(qi.tidx);
}




