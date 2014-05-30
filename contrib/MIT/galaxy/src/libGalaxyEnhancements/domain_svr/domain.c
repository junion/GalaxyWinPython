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
#include <domain_svr/domain_svr.h>

static char *oas[] = {
  "-dialogue_script name", "script for dialogue control in domain server", NULL,
  NULL
};

GAL_DOMAIN_SVR *Domain_Svr = NULL;

#define DEFAULT_KV_LANG "dialogue"
#define CURRENT_VERSION "v1"
#define NA			"N/A"

#define NCOMMENTS 10

static int Something_Else_count = 0;
static char Something_Else[128];

char *increment_something_else()
{
  if (++Something_Else_count > 8)
  { Something_Else_count = 1;
  }
  sprintf(Something_Else, "something_else%d", Something_Else_count);
  return(Something_Else);
}


/* **************************************************************** */
/* used by the help messages. */

char Help_Message[128];

char *increment_help_message(GAL_DOMAIN_SVR *dc, int max_help)
{ int help_count = 0;
  help_count = Gal_GetInt(dc->prev_state, ":help_count");
  if (++help_count > max_help)
  { help_count = 1;
  }
  sprintf(Help_Message, "help_message%d", help_count);
  Gal_SetProp(dc->prev_state, ":help_count", Gal_IntObject(help_count));
  return(Help_Message);
}

/* **************************************************************** */
/* parenthetical comments to user */
void
Gal_AddComment(GAL_DOMAIN_SVR *dc,Gal_Frame clause)
{
  if (dc->comment_array[dc->ncomments]) Gal_FreeObject(dc->comment_array[dc->ncomments]);
  dc->comment_array[dc->ncomments] = Gal_FrameObject(Gal_CopyFrame(clause));
  dc->ncomments++;
}

Gal_Frame
Gal_FillParaphraseKeys(char *clause_name, Gal_Frame frame, char *domain, char *language)
{ Gal_Frame out_frame;
  out_frame = Gal_CopyFrame(frame);
  if (domain) Gal_SetProp(out_frame, ":domain", Gal_StringObject(domain)); 
  if (language) Gal_SetProp(out_frame, ":out_lang", Gal_StringObject(language));
  Gal_SetFrameName(out_frame, clause_name);
  Gal_SetProp(out_frame, ":prefill", Gal_IntObject(1));
  return(out_frame);
}

/* presumes "welcome" domain local by default */
Gal_Frame
Gal_RepeatResponse(Gal_Frame prev_state)
{ Gal_Frame prev_reply;
  if (prev_state && (prev_reply = Gal_GetFrame(prev_state, ":prev_response")))
    return(Gal_CopyFrame(prev_reply));
  prev_reply = Gal_MakeFrame("welcome", GAL_CLAUSE);
  Gal_SetProp(prev_reply, ":domain", Gal_StringObject("local"));
  return(prev_reply);
}

void
Gal_InitializeContents(Gal_Object *array, int nmax)
{ int i;
  for (i=0;(i<nmax && (array[i]));i++)
  { if(array[i])
      Gal_FreeObject(array[i]);
    array[i] = NULL;
  }
}

/* finds and returns a pred in nframe that has the same name as predicate */
Gal_Frame Gal_FetchPredNamed (char *name, Gal_Frame nframe)
{
  return(Gal_GetPredByName(nframe, name));
}

/* finds a match to match_string, rejecting if it contains reject_string,
  and requiring an exact match if "exact" is set */
Gal_Frame Gal_GrovelForPredLike(Gal_Frame ref_frame, char *match_string, char *reject_string, int exact)
{ Gal_Frame pred = NULL, topic = NULL;
  int num_preds, i;
  char *pred_name = NULL;
  Gal_Frame next_pred;

  if (ref_frame == NULL) return(NULL);

  num_preds = Gal_NumPreds(ref_frame);

  for(i=0;i<num_preds;i++)
  {
    pred = Gal_GetPred(ref_frame, i);
    if(pred) pred_name = Gal_FrameName(pred);
    if(pred_name) 
      if (match_string && ((exact && Gal_StringEq(pred_name, match_string))
			   || (strstr(pred_name, match_string)))
	  && (!reject_string || (!strstr(pred_name, reject_string))))
	return(pred);
    if ((next_pred = Gal_GrovelForPredLike(pred, match_string, reject_string, exact)))
      return(next_pred);
  }
  if (Gal_Framep(Gal_GetObject(ref_frame, ":topic")))
  { topic = Gal_GetTopicFrame(ref_frame, ":topic");
    return(Gal_GrovelForPredLike(topic, match_string, reject_string, exact));
  }
  return(NULL);
}

Gal_Frame Gal_GrovelForPred(Gal_Frame ref_frame, char *pred_name)
{
  return(Gal_GrovelForPredLike(ref_frame, pred_name, NULL, 1));
}

Gal_Frame Gal_GrovelForTopic(Gal_Frame ref_frame, char *topic_name)
{
  Gal_Frame nframe, pred, pred_topic, value;
  Gal_Object frame_topic;
  int npreds, i, nkeys;
  char **keys, *key;

  frame_topic = NULL;
  pred_topic = NULL;

  if(ref_frame)
    {
    if ((Gal_TopicFramep(ref_frame)) && (Gal_FrameNameEq(ref_frame, topic_name)))
       return(ref_frame);
    frame_topic = Gal_GetObject(ref_frame, ":topic");
    if(Gal_Framep(frame_topic))
    {
      if((nframe = Gal_GrovelForTopic(Gal_FrameValue(frame_topic), topic_name)))
	return(nframe);
    }
    npreds = Gal_NumPreds(ref_frame);
    for(i=0;i<npreds;i++)
    { pred = Gal_GetPred(ref_frame, i);
      if(pred && (pred_topic = Gal_GrovelForTopic(pred, topic_name)))
	return(pred_topic);
    }
    keys = Gal_GetProperties(ref_frame, &nkeys);
    for (i=0;i<nkeys;i++)
    { key = keys[i];
      if (Gal_Framep(Gal_GetObject(ref_frame, key)))
      { value = Gal_GetFrame(ref_frame, key);
        if ((nframe = Gal_GrovelForTopic(value, topic_name))) {
	  if (keys) free(keys);
	  return(nframe);
	}
      }
    }
    if (keys) free(keys);
  }
  return(NULL);
}

/* this copies the tarray into the filter list */
Gal_Frame
Gal_DeclareFilterList(char *Ftype, char *Fcategory, char *Fkey,Gal_Object *tarray, int n)
{ Gal_Frame flist;	
  int i;
  Gal_Object *new_array;
  if (!strcmp(Fcategory, "ignore")) return(NULL);  
  flist = Gal_MakeFrame("filter_list", GAL_CLAUSE);

  if (tarray && (n>0))
  { new_array = (Gal_Object *) calloc(n+1, sizeof(Gal_Object));
    for (i=0;i<n;i++)
    { new_array[i] = Gal_CopyObject(tarray[i]);
    }
    Gal_SetProp(flist, ":Flist", Gal_ListObject(new_array, n));
    free(new_array);
  }
 
  if (Fcategory) Gal_SetProp(flist, ":Fcategory", Gal_StringObject(Fcategory));
  if (Ftype) Gal_SetProp(flist, ":Ftype",  Gal_StringObject(Ftype));
  if (Fkey) Gal_SetProp(flist, ":Fkey",  Gal_StringObject(Fkey));
  return(flist);
}


/* this fetches any prior filter list for n-best selection filtering on categories (e.g., flight_number) */

Gal_Frame Gal_FetchFilterList(Gal_Frame prev_state)
{ 
  if (!prev_state) return(NULL);

  return(Gal_GetFrame(prev_state, ":filter_list"));
}

/* did a frame of this name show up in the "thin" frame? */
Gal_Frame
Gal_IsNew(Gal_Frame frame, Gal_Frame thin_frame)
{
  if (Gal_TopicFramep(frame))
  { return(Gal_GrovelForTopic(thin_frame, Gal_FrameName(frame)));
  }
  else if (Gal_PredFramep(frame))
  { return(Gal_GrovelForPred(thin_frame, Gal_FrameName(frame)));
  }
  return(NULL);
}

void
print_comments(GAL_DOMAIN_SVR *dc)
{ int i;
  for(i=0;i<dc->ncomments;i++)
  {
    GalUtil_PPFrame(GAL_PINFO1_LEVEL, Gal_FrameValue(dc->comment_array[i]));
  }
}

/* put something into the Domain_Down_File  to disable system 
  when it's "broken" 
*/
int
domain_down(char *domain)
{ FILE *fp;
  char down_file[1000];

  if (!domain) return (0);

  strcpy(down_file, domain);
  strcat(down_file, "_down");
  fp = fopen(down_file, "r");
  if (fp)
  {
    fclose(fp);
    return(1);
  }
  return (0);
}

/*
new res:
  :filter_list
  :discourse_frame  :discourse_update, :system_initiative
  :reply_frame 

Optional res:
  :response_list
  :response_map
  :response_image

dc->prev_state:   :prev_response, :filter_list
*/

/* if the reply_frame is a clause REPLY, assume it's really a "res" and we're done */
/* this is for "please repeat that!! */
void
Gal_FillResponse(GAL_DOMAIN_SVR *dc, Gal_Frame res)
{ Gal_Frame reply_frame;
  char *continuant, *status;
  Gal_Object domain;
  reply_frame = dc->reply_frame;

  if (reply_frame)
  { if (dc->domain && (!Gal_GetString(reply_frame, ":domain")))
      Gal_SetProp(reply_frame, ":domain", Gal_StringObject(dc->domain));
    GalUtil_PPFrame(GAL_PINFO1_LEVEL, reply_frame);
    if (dc->default_reply_name && (Gal_GetObject(reply_frame, "unused")))
    { Gal_SetFrameName(reply_frame, dc->default_reply_name);
    }

    if (dc->ncomments > 0)
    { int i;
      Gal_SetProp(reply_frame, ":comment_tlist",
		  Gal_ListObject(dc->comment_array, dc->ncomments));
      for(i=0;i<dc->ncomments;i++)
      { dc->comment_array[i] = NULL;
      }
    }
    if (!Gal_GetObject(reply_frame, ":continuant"))
    { continuant = Gal_GetString(dc->dialogue_state, ":continuant");
      if (!continuant) 
	continuant = increment_something_else();
      Gal_SetProp(reply_frame, ":continuant", 
		  Gal_FrameObject(Gal_MakeFrame(continuant, GAL_CLAUSE)));
    }
    
    if (Gal_VarGetValue(dc->dialogue_state, ":fill_db") && (dc->num_db_tlist > 0))
    { int i;
      Gal_SetProp(reply_frame, ":db_tlist", Gal_ListObject(dc->db_tlist, dc->num_db_tlist));
      /* reply_frame will be deleted as part of returning the result, so
       * make sure that the objects in db_tlist don't get freed again
       */
      for(i=0; i<dc->num_db_tlist; i++)
	dc->db_tlist[i] = NULL;
      dc->num_db_tlist = 0;
    }

    Gal_SetProp(res, ":reply_frame", Gal_FrameObject(Gal_CopyFrame(reply_frame)));
    Gal_SetProp(dc->prev_state, ":prev_response", Gal_FrameObject(Gal_CopyFrame(reply_frame)));
  }

  /* save it in the prev_state as well */
  if (dc->filter_list)
  { Gal_SetProp(dc->prev_state, ":filter_list", Gal_FrameObject(Gal_CopyFrame(dc->filter_list)));  
    Gal_SetProp(res, ":filter_list", Gal_FrameObject(Gal_CopyFrame(dc->filter_list)));
  }
  else
  { Gal_DelProp(dc->prev_state, ":filter_list");
    Gal_DelProp(res, ":filter_list");
  }

  if(dc->html)
  { Gal_SetProp(res, ":html", Gal_CopyObject(dc->html));
  }	

  if(dc->response_list)
    Gal_SetProp(res, ":list", Gal_CopyObject(dc->response_list));
  if(dc->response_map)
    Gal_SetProp(res, ":map", Gal_CopyObject(dc->response_map));
  if(dc->response_image)
    Gal_SetProp(res, ":image", Gal_CopyObject(dc->response_image));

  if(dc->prev_state)
  { Gal_SetProp(res, ":tm_state", Gal_FrameObject(Gal_CopyFrame(dc->prev_state)));
  }


  if (dc->discourse_update)
  { 
    /* make sure it has a domain!! */
    if ((!(domain = Gal_GetObject(dc->discourse_update, ":domain"))) && (dc->domain))
    { Gal_SetProp(dc->discourse_update, ":domain", Gal_StringObject(dc->domain));
    }
    Gal_SetProp(res, ":discourse_update", Gal_FrameObject(Gal_CopyFrame(dc->discourse_update)));
  }
  if (dc->system_initiative)
    Gal_SetProp(res, ":system_initiative", Gal_FrameObject(Gal_CopyFrame(dc->system_initiative)));
  if ((status = Gal_GetString(dc->dialogue_state, ":tm_status")))
  { Gal_SetProp(res, ":tm_status", Gal_StringObject(status));
  }
}

void
Gal_AddSystemInitiative(GAL_DOMAIN_SVR *dc, char *pred_name, Gal_Frame frame)
{ 
  Gal_Frame si_frame, si_pred;
  Gal_Frame former_initiative = NULL;
  Gal_Object list_of_inits[10], *init_list = NULL;
  int nlist = 0, i;

  if ((former_initiative = dc->system_initiative))
  { if ((init_list = Gal_GetList(former_initiative, ":sys_init_list", &nlist)))
    { for(i=0;i<nlist;i++)
      { list_of_inits[i] = init_list[i];
      }
    }
    else
    { list_of_inits[0] = Gal_FrameObject(former_initiative);
      dc->system_initiative = Gal_MakeFrame("list", GAL_CLAUSE);

      nlist = 1;
    }
  }

  si_frame = Gal_MakeFrame("system_initiative", GAL_CLAUSE);
  if (frame)
    Gal_SetProp(si_frame,":initiative_frame", Gal_FrameObject(Gal_CopyFrame(frame)));
  else
  { si_pred = Gal_MakeFrame(pred_name, GAL_PRED);
    Gal_SetProp(si_frame,":initiative_frame", Gal_FrameObject(si_pred));
  }
  Gal_SetProp(si_frame,":initiative_name", Gal_StringObject(pred_name));
  if (nlist == 0)
    dc->system_initiative = si_frame;
  else
  { list_of_inits[nlist++] = Gal_FrameObject(si_frame);
    Gal_SetProp(dc->system_initiative, ":sys_init_list", Gal_ListObject(list_of_inits, nlist));
  }
}

void
Gal_InitDService(Gal_Frame big_frame, GAL_DOMAIN_SVR *domain)
{ if (domain->token) Gal_FreeFrame(domain->token);
  domain->token = Gal_CopyFrame(big_frame);

  Gal_FreeFrame(domain->prev_state);
  if (Gal_GetObject(big_frame, ":tm_state"))
  { domain->prev_state = Gal_CopyFrame(Gal_GetFrame(big_frame, ":tm_state"));
  }
  else domain->prev_state = Gal_MakeFrame("prev_state", GAL_CLAUSE);

  domain->ncomments = 0;
  if (!domain->comment_array)
    domain->comment_array = (Gal_Object *) calloc(NCOMMENTS, sizeof(Gal_Object));
  Gal_InitializeContents(domain->comment_array, NCOMMENTS);

  domain->parse_frame = Gal_GetFrame(big_frame, ":parse_frame");
  domain->request_frame = Gal_GetFrame(big_frame, ":request_frame");
  domain->key_value = Gal_GetString(big_frame, ":key_value");

  if(domain->system_initiative) Gal_FreeFrame(domain->system_initiative);
  domain->system_initiative = NULL;

  if (domain->filter_list) Gal_FreeFrame(domain->filter_list);
  domain->filter_list = Gal_CopyFrame(Gal_GetFrame(big_frame, ":filter_list"));

  if (domain->html) Gal_FreeObject(domain->html);
  domain->html = NULL;

  if (domain->discourse_update) Gal_FreeFrame(domain->discourse_update);
  domain->discourse_update = NULL;

  if (Gal_GetObject(big_frame, ":kv_lang"))
    strcpy(domain->kv_lang, Gal_GetString(big_frame,":kv_lang"));
  else strcpy(domain->kv_lang, DEFAULT_KV_LANG);

  if (!domain->domain) domain->domain = (char *) calloc(1000, sizeof(char));
  if (Gal_GetObject(domain->request_frame, ":domain"))
    strcpy(domain->domain,  Gal_GetString(domain->request_frame, ":domain"));
  else if (Gal_GetObject(domain->token, ":domain"))
    strcpy(domain->domain,  Gal_GetString(domain->token, ":domain"));

  if (domain->html) Gal_FreeObject(domain->html);
  domain->html = NULL;

  if (domain->reply_frame) Gal_FreeFrame(domain->reply_frame);
  domain->reply_frame = NULL;
  
  if (domain->response_list) Gal_FreeObject(domain->response_list);
  domain->response_list = NULL;

  if(domain->response_map) Gal_FreeObject(domain->response_map);
  domain->response_map = NULL;
  
  if (domain->response_image) Gal_FreeObject(domain->response_image);
  domain->response_image = NULL;
}

Gal_Frame turn_management(Gal_Frame fr, void *server_data)
{ 
  return 
    domain_turn_management(Domain_Svr, (GalSS_Environment*)server_data, fr);
}

/* If the rule specifies a ":key_value" then we assume we should convert that into a dialogue_state.
   Otherwise, we expect there to be a ":dialogue_state" already in the fr, which we adopt.
*/ 
Gal_Frame domain_turn_management(GAL_DOMAIN_SVR *domain, 
				 GalSS_Environment *server_data,
				 Gal_Frame fr)
{ 
  char *status = NULL;
  char *kv;

  printf("entering turn_management\n");

  domain->server_data = server_data;
  domain->exit_function = 0;

  if (!fr)
    return(NULL);

  if (!domain->dc)
    GalUtil_Fatal("domain->dc == NULL!");

  if (domain->dc->para)
  { free(domain->dc->para);
  }

  if((kv = Gal_GetString(fr,":key_value")))
    domain->dc->para = strdup(kv);
  else domain->dc->para = NULL;
  Gal_InitDService(fr, domain);

  if (domain->dialogue_state) Gal_FreeFrame(domain->dialogue_state);
  domain->dialogue_state = domain->dc->dialogue_state = NULL;

  if (!domain->dc->para)	
    domain->dialogue_state = domain->dc->dialogue_state 
      = Gal_CopyFrame(Gal_GetFrame(fr, ":dialogue_state"));
  if (!domain->dialogue_state)
    domain->dialogue_state = Gal_FillDialogueState(domain->dc,1); 

/* a new addition for pegasus to communicate a reparse with self-referencial filtering */
  if ((status = Gal_GetString(fr, ":tm_status")))
  { Gal_SetProp(domain->dialogue_state, ":tm_status", Gal_StringObject(status));
  }

  /* put something into the file "jupiter_down" to disable jupiter in a panic */
  if (domain_down(domain->domain))
    domain->reply_frame = Gal_MakeFrame("system_down", GAL_CLAUSE);
  else 
    Gal_DialogueLoop(domain->dc, domain);

  if(domain->exit_function) {
    fr = (*((Gal_dc_exit_function)domain->exit_function))(domain,fr);
  }
  Gal_FillResponse(domain,fr);

  return(fr);
}


GAL_DOMAIN_SVR *Gal_InitTM(int argc, char **argv)
{
  GAL_DOMAIN_SVR *domain;
  char   *dialogue_script = NULL;

  printf("\tInitializing GAL_DOMAIN_SVR module\n");

  domain = (GAL_DOMAIN_SVR*)calloc(1,sizeof(GAL_DOMAIN_SVR));
  GalUtil_OAExtract(argc, argv, oas, "-dialogue_script", GAL_OA_STRING, &dialogue_script); 
  if (!(domain->dc = Gal_InstantiateSystem(dialogue_script)))
      GalUtil_Fatal("Gal_InitTM: problems instantiating system");
  return(domain);
}

/* ________________________________________________ */

/*
 *  server-specific usage and initialization functions
 *  called by initialize_server in ServerStub/generic_server.c
 */

void Gal_DialoguePrintUsage(int argc, char **argv)
{
  GalUtil_OAPrintOptions(argc, argv, oas, "  Dialogue options:");
  printf("\n");
}

void *Gal_DialogueInitServer(char *server_name, int argc, char **argv)
{
  if (GalUtil_OACheckUsage(argc, argv, oas, NULL) == 0)
    exit(1);

  Domain_Svr = Gal_InitTM(argc, argv);
  if (Domain_Svr)
  {
    if (server_name)
      Domain_Svr->domain = strdup(server_name);
    else
      GalUtil_Warn("%s: No server name specified!", __FUNCTION__);
  }
  return(Domain_Svr);
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

