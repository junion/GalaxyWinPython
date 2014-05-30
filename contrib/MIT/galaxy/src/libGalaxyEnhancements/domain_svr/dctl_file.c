/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <stdlib.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <string.h>
#include <galaxy/util.h>
#include <galaxy/program.h>
#include <domain_svr/domain_svr.h>

Gal_ConditionStruct **_Gal_IReadDialogueControl(Gal_FileStruct *fs, int *num_conditions_ptr, int *error_ptr, int control_flags)
{
  char line[GAL_LINE_LENGTH];
  ProgramObject *objects;
  Gal_ConditionStruct *condition;
  Gal_ConditionStruct **conditions;
  int num_conditions = 0;
  int line_count = 0;
  int done = 0;
  Gal_ProgramParser pp;
  
  if (!fs)
  {
    *error_ptr = GAL_NO_FILE;
    return NULL;
  }

  while (fgets(line, GAL_LINE_LENGTH, fs->fp))
  {
    switch(line[0])
    {
    case '\n': case ';':
      break;
    default:
      line_count++;
    }
  }
  rewind(fs->fp);

  Gal_InitializeProgramParser(&pp, Gal_MakeFileInputStream(fs->fp),
			      fs, GAL_PGM_RULE, control_flags);
  
  conditions = (Gal_ConditionStruct **)calloc(line_count, sizeof(Gal_ConditionStruct *));

  if (conditions == NULL)
  {
    *error_ptr = GAL_ALLOCATION_FAILED;
    return NULL;
  }

  while (!done && !pp.error && (objects = Gal_ReadProgramLine(&pp, NULL)))
  {
    int tag = Gal_GetProgramObjectTag(objects, 0);

    objects->index = 1;
    
    switch(tag)
    {
    case GAL_PGM_COMMENT_LINE:
    case GAL_PGM_PGM_SYNTAX:
    case GAL_PGM_BLANK_LINE:
      break;
    case GAL_PGM_EOF:
      done = 1;
      break;
    case GAL_PGM_RULE:
      if ((condition = Gal_NewCondition(objects, &pp, &(pp.error))))
      {
	if (control_flags & GAL_PROGRAM_VERIFY) {
	  conditions[num_conditions++] = condition;
	} else {
	  if ((condition->fn_ptr = Gal_GetDialogueFunction(condition->operation)))
	    conditions[num_conditions++] = condition;
	  else
	    pp.error = GAL_INVALID_DIALOGUE_FUNCTION;
	}
      }
      break;
    default:
      pp.error = GAL_INVALID_DCTL_TAG;
    }
  }
  free(pp.gs);

  if (!done && !pp.error)
    pp.error = GAL_STOPPED_BEFORE_EOF;

  *error_ptr = pp.error;
  *num_conditions_ptr = num_conditions;
  return(conditions);
}


Gal_ConditionStruct **Gal_ReadDialogueControl(Gal_FileStruct *fs, int *num_conditions_ptr, int *error_ptr)
{
  return _Gal_IReadDialogueControl(fs, num_conditions_ptr, error_ptr, 0);
}

/* print conditions as a dialogue control file */

void Gal_PrintDialogueControl(FILE *fp, Gal_ConditionStruct **conditions, int num_conditions)
{
  int i;

  for (i=0; i<num_conditions; i++)
    Gal_PrintCondition(fp, conditions[i], NULL, 0);
}

Gal_ConditionStruct **Gal_LoadDialogueControl(char *filename, int *num_conditions_ptr)
{
  Gal_FileStruct *fs;
  Gal_ConditionStruct **conditions = NULL;
  int error = GAL_NO_ERROR;

  if ((fs = Gal_OpenControlFile(filename, NULL)))
  {
    conditions = Gal_ReadDialogueControl(fs, num_conditions_ptr, &error);
    if (error != GAL_NO_ERROR)
      Gal_PrintProgramError(fs, error);

    Gal_CloseControlFile(fs);

    if (error == GAL_NO_ERROR)
      return(conditions);
  }
  return(NULL);
}

