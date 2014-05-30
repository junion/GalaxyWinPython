/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _DOMAIN_SVR_NAME_BARRIER_H
#define _DOMAIN_SVR_NAME_BARRIER_H

/* wrappers defined in server_name_barrier.c */

/* dialogue.c */
int   var_is_set(Nframe dialogue_state, char *key);
void  var_set_value(Nframe dialogue_state, char *key, char *value, int int_value);
char *var_get_value(Nframe dialogue_state, char *key);
void  var_augment_value(Nframe dialogue_state, char *key, char *value);	
int   var_get_int_value(Nframe dialogue_state, char *key);
int   var_unset_value(Nframe dialogue_state, char *key);
int   var_match_value(Nframe dialogue_state, char *key, char *match_value);

/* domain.c */
void *default_init_server(char *server_name, int argc, char **argv);
Nframe declare_Flist(char *Ftype, char *Fcategory, char *Fkey, TObj *tarray, int n);
Nframe fill_paraphrase_keys(char *clause_name, Nframe frame, char *domain, 
			    char *language);
void   fill_response(GAL_DOMAIN_SVR *dc, Nframe res);
void   init_dservice(Nframe big_frame, GAL_DOMAIN_SVR *domain_svr);
void   initialize_contents(TObj *array, int nmax);

/* For backward compatibility. */

typedef GAL_DIALOGUE_FUNCTION_MAP FUNCTION_MAP;
typedef GAL_DOMAIN_SVR DOMAIN_SVR;

#endif  /* _DOMAIN_SVR_NAME_BARRIER_H */
