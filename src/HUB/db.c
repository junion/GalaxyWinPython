/*
  This file (c) Copyright 1998 - 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdlib.h>
#include "hub.h"
#include "hub_internal.h"

#ifdef WIN32
#include "galaxy/sysdep.h"
#endif

/* #define Max_History_Utts 50  */
static void free_frames(Gal_Frame *frames, int n);

extern GalUtil_PrintPkg GalUtil_DefaultPrintPkg;

void GalHUB_DBErase(Gal_Frame *db)
{
  int i;

  for(i=0;i<=Max_History_Utts;i++) {
    if (db[i]) 
    { Gal_FreeFrame(db[i]);
      db[i] = NULL;
    }
  }
}

void GalHUB_DBFree(Gal_Frame *db)
{
  GalHUB_DBErase(db);
  free(db);
}

Gal_Frame *GalHUB_DBInit() {
  Gal_Frame *db;
  /* The (-01) utterance history is tucked away in the overshoot-by-one space allocated specially */
  db = (Gal_Frame *) calloc(Max_History_Utts+1, sizeof(Gal_Frame));
  GalUtil_CPInfo1(4,0,"%s: Creating local database for builtin functions Store and Retrieve\n", __FUNCTION__);
  return(db);
}/* store each non-mandatory key by discourse_id */

void
GalHUB_DBStoreInHistory(char *session_id, char *key, Gal_Object value)
{ Gal_Frame *db;
  SESSION *session;
  int discourse_id;
  
  session = GalHUB_SessionLookupByID(session_id, 0);
  if (!session) 
  { GalUtil_Warn("Can't store in DB for non-existent session %s", session_id);
    return;
  }
  if ((discourse_id = session->discourse_id) < -1)
  { GalUtil_Warn("Negative discourse_id");	
    session->discourse_id = 0;
    discourse_id = session->discourse_id;
  }
  if(discourse_id >= Max_History_Utts)
    GalUtil_Fatal("Can't store in DB (utterance id too large)");
  /* The (-01) utterance history is tucked away in the overshoot-by-one space allocated specially */
  if(discourse_id == -1) 
  { discourse_id = Max_History_Utts;
  }
  if (!(db = session->history))
  { session->history = GalHUB_DBInit();
    db = session->history;
  }
  if (!db[discourse_id]) db[discourse_id] = Gal_MakeFrame("db", GAL_CLAUSE);
  if (value) {
    Gal_SetProp(db[discourse_id], key, Gal_CopyObject(value));
  }
}

Gal_Object
GalHUB_DBRetrieveFromHistory(char *session_id, char *key, int offset)
{ 
  Nframe history;
  Nframe *db;
  int index;
  SESSION *session;

  session = GalHUB_SessionLookupByID(session_id, 0);
  if (!session) {
    GalUtil_Warn("Can't retrieve from DB for non-existent session %s",
		 session_id);
    return(NULL);
  }
  index = session->discourse_id + offset;
  /* if (index < 0) return(NULL); */
  if (index < -1) return(NULL);  
  if(index >= Max_History_Utts)
    GalUtil_Fatal("Can't retrieve from DB (utterance id too large)");

  /* The (-01) utterance history is tucked away in the overshoot-by-one space allocated specially */
  if (index == -1) index = Max_History_Utts;
  
  if (!(db = session->history)) return (NULL);
  history = db[index];
  if (!history) return(NULL);

  return(Gal_CopyObject(Gal_GetObject(history, key)));
}
void GalHUB_DBClearHistory(Nframe frame)
{ SESSION *session;	
  Nframe *db;
  session = GalHUB_SessionLookupByFrame(frame, 0);
  if (session && (session->discourse_id > 0))
  {	
    db = GalHUB_SessionFetchDB(session->session_id);
    if (db) free_frames(db, session->discourse_id);
    session->discourse_id = 0;
  }
}

void GalHUB_DBEraseHistories(SESSION *session, int free_db)
{
  int i;
  Gal_Frame *history;
  
  if (session->history) {
    GalHUB_DBErase(session->history);
    if (free_db)
      free(session->history);
  }
  if (session->domain_histories) {
    for (i=0;i<GalHUB_MaxDomains;i++) { 
      if ((history = session->domain_histories[i])) {
	GalHUB_DBErase(history);
	if (free_db)
	  free(history);
      }
    }
    if (free_db)
      free(session->domain_histories);
  }
}

void GalHUB_DBEraseAllHistory(Gal_Frame frame)
{ 
  SESSION *session;
  session = GalHUB_SessionLookupByFrame(frame, 0);

  GalHUB_DBEraseHistories(session, 0);
  session->discourse_id = -1;
  session->utterance_id = 0;
}

static void
free_frames(Gal_Frame *frames, int n)
{ int i;
  for(i=0;i<n;i++)
  { if (frames[i]) Gal_FreeFrame(frames[i]);
    frames[i] = NULL;
  }
}

void
GalHUB_DBPrint(Nframe *db) 
{
  int i;
  if (db) {
    for(i=0;i<=Max_History_Utts;i++) {
      if (db[i]) Gal_PPFrame(db[i]);
    }
  }
  GalUtil_PkgPrint(&GalUtil_DefaultPrintPkg, -1, "- - - - - - - - - - - - -\n");
  return;
}
