/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _DOMAIN_SVR_H
#define _DOMAIN_SVR_H

#include <galaxy/galaxy.h>
#include <galaxy/program.h>

/*** from dialogue.c ***/
#define DIALOGUE_STOP          0
#define DIALOGUE_CONTINUE      1
#define DIALOGUE_RESTART      -1
#define DIALOGUE_FAILED_AND    2

typedef struct _gal_dialog_control
{ 
  char *dialog_control_file_name;
  Gal_ConditionStruct **conditions;
  int num_conditions;

  Gal_Frame dialogue_state;

  char *eform_to_semantic_file;
  char *para;    /* used to store the key/value paraphrase */
} GAL_DIALOG_CONTROL;

typedef struct _gal_db
{
  Gal_Frame dialogue_state;
  GAL_DIALOG_CONTROL *dc;
  Gal_Frame result;
  GalSS_Environment *server_data;
  char *tclfile;
} GAL_DB;

typedef struct _gal_domain_svr
{
  char *domain;
  Gal_Frame token;
  char *default_reply_name;

  GAL_DIALOG_CONTROL *dc;
  Gal_Frame dialogue_state;

  char *key_value;
  char kv_lang[1000];
  Gal_Frame parse_frame;
  Gal_Frame request_frame;
  Gal_Frame reply_frame;

  Gal_Object response_list;
  Gal_Object response_map;
  Gal_Object response_image;
  Gal_Object  html;			/* this appears on the gui display as html formatted string */

  Gal_Object *comment_array; 
  int ncomments;

  Gal_Frame db_query;
  Gal_Object *result; 
  Gal_Object *db_tlist; 
  int  num_db_tlist;

  Gal_Frame discourse_update;
  Gal_Frame system_initiative;
  Gal_Frame filter_list;

  /* this is historical -- it's retained across user queries */
  Gal_Frame prev_state;

  /* these are for jupiter */
  /* fold these into a scratch pad structure LATER */

  Gal_Object *city_array; /* the cities (states, countries,etc.) to be spoken */
  char *selected_airport;
  Gal_Frame topic;
  Gal_Frame its_date;
  Gal_Frame city_frame;
  Gal_Frame city_topic;

  GalSS_Environment *server_data;
  
  void *exit_function;

} GAL_DOMAIN_SVR;

typedef Gal_Frame (*Gal_dc_exit_function)(GAL_DOMAIN_SVR *dc, Gal_Frame prevres);

typedef void *Gal_Data;

/* Functions protos for file dialogue.c */
void Gal_InitializeDialogueDefaults(GAL_DIALOGUE_FUNCTION_MAP *function_map);
Gal_IntFnPtr Gal_GetDialogueFunction(char *fn_name);

GAL_DIALOG_CONTROL *Gal_InstantiateSystem(char *dialogscriptfilename);
void Gal_FreeSystem(GAL_DIALOG_CONTROL *dc);
Gal_Frame Gal_FillDialogueState(GAL_DIALOG_CONTROL *dc, int clear);
char *Gal_VarGetValue(Gal_Frame dialogue_state, char *key);
int   Gal_DialogueLoop(GAL_DIALOG_CONTROL *dc, Gal_Data data);
int   Gal_VarIsSet(Gal_Frame dialogue_state, char *key);
int   Gal_VarUnsetValue(Gal_Frame dialogue_state, char *key);
int   Gal_VarMatchValue(Gal_Frame dialogue_state, char *key, 
			char *match_value);
void  Gal_VarSetValue(Gal_Frame dialogue_state, char *key, 
		      char *value, int int_value);
void  Gal_VarAugmentValue(Gal_Frame dialogue_state, char *key, 
			  char *value);       
void  Gal_Para2Fact(Gal_Frame dc, char *paraphrase);
Gal_Object Gal_CreateConditionObject(Gal_TestClause *tests);
Gal_Object Gal_LookupConditionsKey(Gal_Frame conditions, char *key_name);

/* Function protos for file domain.c */
Gal_Frame Gal_DeclareFilterList(char *Ftype, char *Fcategory, char *Fkey, Gal_Object *tarray, int n);
Gal_Frame Gal_FetchFilterList(Gal_Frame prev_state);
Gal_Frame Gal_FillParaphraseKeys(char *clause_name, Gal_Frame frame, 
				 char *domain, char *language);
Gal_Frame Gal_GrovelForPredLike(Gal_Frame ref_frame, char *match_string, char *reject_string, int exact);
Gal_Frame Gal_GrovelForPred(Gal_Frame ref_frame, char *pred_name);
Gal_Frame Gal_GrovelForTopic(Gal_Frame ref_frame, char *topic_name);
Gal_Frame Gal_IsNew(Gal_Frame frame, Gal_Frame thin_frame);
void Gal_AddComment(GAL_DOMAIN_SVR *dc,Gal_Frame clause);
void Gal_AddSystemInitiative(GAL_DOMAIN_SVR *dc, char *pred_name, Gal_Frame frame);
void Gal_FillResponse(GAL_DOMAIN_SVR *dc, Gal_Frame res);
void Gal_InitDService(Gal_Frame big_frame, GAL_DOMAIN_SVR *domain_svr);
void Gal_InitializeContents(Gal_Object *array, int nmax);
Gal_Frame Gal_RepeatResponse(Gal_Frame prev_state);

/* turn manager functions */
GAL_DOMAIN_SVR *Gal_InitTM(int argc, char **argv);
void *Gal_DialogueInitServer(char *server_name, int argc, char **argv);
void  Gal_DialoguePrintUsage(int argc, char **argv);
Gal_Frame turn_management(Gal_Frame fr, void *server_data);
Gal_Frame domain_turn_management(GAL_DOMAIN_SVR * domain_svr, 
				 GalSS_Environment *server_data,
				 Gal_Frame fr);

/* hub-dispatch.c */
Gal_Frame GalSS_DispatchQueryToIServer(GalSS_Environment *server_data, char *par_lang, Gal_Frame query_frame, Gal_Frame eform);
Gal_Frame GalSS_RefillDialogueState(GAL_DOMAIN_SVR *dc, Gal_Frame request_frame, char *para_lang, int clear);

/* dctl_file.c */

Gal_ConditionStruct **Gal_LoadDialogueControl(char *filename, int *num_conditions_ptr);
void Gal_PrintDialogueControl(FILE *fp, Gal_ConditionStruct **conditions, int num_conditions);

#include "name_barrier.h"

#endif  /* _DOMAIN_SVR_H */
