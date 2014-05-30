/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <galaxy/galaxy.h>
#include <domain_svr/domain_svr.h>

/* Wrappers for dialogue.c */
int var_is_set(Gal_Frame dialogue_state, char *key)
{
  return(Gal_VarIsSet(dialogue_state, key));
}

void var_set_value(Gal_Frame dialogue_state, char *key, char *value, int int_value)
{
  Gal_VarSetValue(dialogue_state, key, value, int_value);
}

char *var_get_value(Gal_Frame dialogue_state, char *key)
{
  return(Gal_VarGetValue(dialogue_state, key));
}

void var_augment_value(Gal_Frame dialogue_state, char *key, char *value)
{
  Gal_VarAugmentValue(dialogue_state, key, value);
}

int var_get_int_value(Gal_Frame dialogue_state, char *key)
{
  return(Gal_GetInt(dialogue_state, key));
}

int var_unset_value(Gal_Frame dialogue_state, char *key)
{
  return(Gal_VarUnsetValue(dialogue_state, key));
}

int var_match_value(Gal_Frame dialogue_state, char *key, char *match_value)
{
  return(Gal_VarMatchValue(dialogue_state, key, match_value));
}

/* Wrappers for domain.c */

void *default_init_server(char *server_name, int argc, char **argv)
{
  return(Gal_DialogueInitServer(server_name, argc, argv));
}

Gal_Frame declare_Flist(char *Ftype, char *Fcategory, char *Fkey, Gal_Object *tarray, int n)
{
  return(Gal_DeclareFilterList(Ftype, Fcategory, Fkey, tarray, n));
}

Gal_Frame fill_paraphrase_keys(char *clause_name, Gal_Frame frame, char *domain,
			       char *language)
{
  return(Gal_FillParaphraseKeys(clause_name, frame, domain, language));
}

void fill_response(GAL_DOMAIN_SVR *dc, Gal_Frame res)
{
  Gal_FillResponse(dc, res);
}

void init_dservice(Gal_Frame big_frame, GAL_DOMAIN_SVR *domain_svr)
{
  Gal_InitDService(big_frame, domain_svr);
}

void initialize_contents(Gal_Object *array, int nmax)
{
  Gal_InitializeContents(array, nmax);
}
