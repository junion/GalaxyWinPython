/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _GalHUB_PROGRAM_H
#define _GalHUB_PROGRAM_H

#include "galaxy/program.h"
#include "mit_control.h"

#define MAX_PROGRAMS 1024
#define MAX_RULES 1024

/* Default tags which hub_program_tags.h and read_hub_program.c use. */

#define DEFAULT_USER_ID_KEY "USER_ID:"
#define DEFAULT_PARA_LANG_KEY "PARA_LANG:"
#define DEFAULT_OUT_LANG_KEY "OUT_LANG:"
#define DEFAULT_KV_LANG_KEY "KV_LANG:"
#define DEFAULT_SYNTH_LANG_KEY "SYNTH_LANG:"

#define GAL_TAG(x,y) ,y
typedef enum
{
  GAL_START_GalHUB_PROGRAM_TAGS = 100
#include "hub_program_tags.h"
} Gal_HubProgramTag;

typedef enum
{
  GAL_START_GalHUB_ERRORS = 100
#include "hub_error_tags.h"
} Gal_HubErrorTag;
#undef GAL_TAG

enum
{
  GAL_CONTROL_ASYNCHRONOUS_POS = 0,
  GAL_CONTROL_RETURN_POS = 1,
  GAL_CONTROL_NO_RESULT_POS = 2
};

enum
{
  GAL_CONTROL_ASYNCHRONOUS = 1<<GAL_CONTROL_ASYNCHRONOUS_POS,
  GAL_CONTROL_RETURN =       1<<GAL_CONTROL_RETURN_POS,
  GAL_CONTROL_NO_RESULT =    1<<GAL_CONTROL_NO_RESULT_POS
};

/* It's important when you construct the namespace
   array of frames that they be loaded in enum order.
   There's also no checking if the frame array is
   large enough; make sure it is. */

typedef struct hub_control_struct 
{
  PROGRAM **programs;
  PROGRAM **messages;
  HUB *hub;
  char **operations_to_ignore;
  char *mode;
  char **timestamps;
  Gal_PointerBuffer *active_servers;
  /* And here are some keys we need to track. */
  char *domain_key;
  char *initial_reply_key;
  char *initial_token_key;
  char *kv_lang_key;
  char *para_lang_key;
  char *synth_lang_key;
  char *out_lang_key;
  char *log_dir_key;
  char *log_version_key;
  char *user_id_key;
  char *session_id_key;
} HubControlStruct;

/* read_hub_program.c */
KeyPair **read_key_value_string(char *str);
HubControlStruct *load_hub_control(HUB *h, char *filename,
				   int *error_ptr, int flags);
void free_hub_control(PROGRAM **global_progs);
void free_HubControlStruct(HubControlStruct *hc);

/* print_hub_program.c */
void print_hub_program_error(Gal_FileStruct *fs, int error);
void print_hub_control(FILE *fp, HubControlStruct *hub);
void print_server(FILE *fp, SERVICE_TYPE *server);
void print_rule(FILE *fp, RULE *rule);
void print_program(FILE *fp, char *header, PROGRAM *program);
void print_location(FILE *fp, SERVICE_PROVIDER *loc);

#endif  /* _GalHUB_PROGRAM_H */
