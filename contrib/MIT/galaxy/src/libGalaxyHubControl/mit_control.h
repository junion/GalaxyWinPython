/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _MIT_CONTROL_H
#define _MIT_CONTROL_H

#include "galaxy/program.h"

typedef struct RULE
{
  char *server_name;
  char   *op_name;
  int     ridx;
  EntityPair **in_kps;
  EntityPair **in_log_kps;
  EntityPair **out_kps;
  EntityPair **error_kps;
  int catch_error;
  EntityPair **out_log_kps;
  KeyPair **retrieve_kps;
  char **store_vars;
  Gal_ProgramEntity **del_vars;
  EntityPair **param_kps;
  EntityPair **set_kps;
  KeyPair **alarm_kps;
  int lock_mask;
  int lock_value;
  Gal_TestClause *tests;
  int control;
  Gal_Frame *reply_continuations;
  Gal_Frame *error_continuations;
  int extended_syntax;
  Gal_ProgramEntity *provider;
} RULE;

typedef struct PROGRAM
{
  char         *name;
  char 	       *mode; 		/* currently one of: "multithread", "singlethread" */
  int           num_rules;
  RULE        **rules;
  EntityPair **log_in;
  EntityPair **log_out;
} PROGRAM;

typedef struct MIT_CONTROL
{
  PROGRAM *prog;
  int      ridx;
} MIT_CONTROL;

#endif /* _MIT_CONTROL_H */
