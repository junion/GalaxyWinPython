/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <galaxy/util.h>
#include <galaxy/galaxy.h>
#include <galaxy/generic-server.h>
#include <domain_svr/domain_svr.h>

static char *paraphrase_frame_as_key_value(GAL_DOMAIN_SVR *dc, Gal_Frame frame, char *language);

/* defaults to "db_query" */
/* assumes the domain is defined in the frame */
/* can send an intermediate reply to entertain the user while waiting for the database */
/* The intermediate reply is interruptable if it has a value for the ":interrupt" key */
Gal_Frame
GalSS_DispatchQueryToIServer(GalSS_Environment *server_data, char *para_lang, Gal_Frame query_frame, Gal_Frame intermediate_reply)
{ Gal_Frame dispatch, response;
  char *domain = NULL;
  Gal_Object interrupt;

  dispatch = Gal_MakeFrame("db_query", GAL_CLAUSE);
  if (query_frame) 
  { domain = Gal_GetString(query_frame, ":domain");
  }

  if(domain) Gal_SetProp(dispatch, ":domain", Gal_StringObject(domain));
  else GalUtil_Warn("No Domain specified in frame!!!");

  if (query_frame) 
  { if (para_lang)
      Gal_SetProp(query_frame, ":out_lang", Gal_StringObject(para_lang));
    else 
      GalUtil_Warn("No paraphrase language defined for DispatchQueryToIServer");
    Gal_SetProp(dispatch, ":db_request", Gal_FrameObject(query_frame));
    if(Gal_GetObject(query_frame,":paraphrase_string"))
      Gal_SetProp(dispatch,":paraphrase_string",Gal_CopyObject(Gal_GetObject(query_frame,":paraphrase_string")));
    if(Gal_GetObject(query_frame,":genesis_version"))
      Gal_SetProp(dispatch,":genesis_version",Gal_CopyObject(Gal_GetObject(query_frame,":genesis_version")));
  }

  if (intermediate_reply)
  { Gal_SetProp(dispatch, ":intermediate_reply", Gal_FrameObject(intermediate_reply));
    if ((interrupt = Gal_RemProp(intermediate_reply, ":interrupt")))
    { Gal_SetProp(dispatch, ":interrupt", interrupt);
    }
  }

  GalUtil_PPFrame(GAL_PINFO1_LEVEL, dispatch);
  response = GalSS_EnvDispatchFrame(server_data, dispatch, NULL);

  if (response) {
    GalUtil_PPFrame(GAL_PINFO1_LEVEL, response);
    return(Gal_GetFrame(response, ":db_result"));
  }
  else
    GalUtil_Warn(":db_result from GalSS_EnvDispatchFrame == NULL");
  return(NULL);
}

Gal_Frame
GalSS_RefillDialogueState(GAL_DOMAIN_SVR *dc, Gal_Frame request_frame, char *para_lang, int clear)
{ dc->dc->para = paraphrase_frame_as_key_value(dc, request_frame, para_lang);
  if(dc->dc->para)
  { return(Gal_FillDialogueState(dc->dc, clear));
  }
  return(NULL);
}

/*
 *  after the input frame has been altered in some way,
 *  reparaphrase it, by sending it back to genesis via the hub.
 */

static char *
paraphrase_frame_as_key_value(GAL_DOMAIN_SVR *dc, 
			      Gal_Frame frame, 
			      char *language)
{ Gal_Frame dispatch, response;

  if (!frame) return(NULL); 
  dispatch = Gal_MakeFrame("key_value", GAL_CLAUSE);
  Gal_SetProp(frame, ":out_lang", Gal_StringObject(language));
  Gal_SetProp(dispatch, ":paraphrase_frame", Gal_FrameObject(frame));

  GalUtil_PPFrame(GAL_PINFO1_LEVEL, dispatch);

  response = GalSS_EnvDispatchFrame(dc->server_data, dispatch, NULL);
  if (response) 
  {
    GalUtil_PPFrame(GAL_PINFO1_LEVEL, response);
    return(Gal_GetString(response, ":paraphrase_string"));
  }
  else
    GalUtil_Warn(":paraphrase_string from GalSS_EnvDispatchFrame == NULL");
  return(NULL);
}

/* 
 *  for Emacs...
 *  Local Variables:
 *  mode: c
 *  fill-column: 110
 *  comment-column: 80
 *  c-tab-always-indent: nil
 *  c-indent-level: 2
 *  c-continued-statement-offset: 2
 *  c-brace-offset: -2
 *  c-argdecl-indent: 2
 *  c-label-offset: -2
 *  End:
 */
