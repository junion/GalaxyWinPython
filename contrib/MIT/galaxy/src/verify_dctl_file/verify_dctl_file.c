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
#include <galaxy/galaxy.h>
#include <domain_svr/domain_svr.h>
#include <galaxy/program.h>

int Debug_Mode = 0;

static char *oas[] =
{
  "-in filename", "dctl file to verify", NULL,
  "-out filename", "regenerate dctl to file (- is stdout)", NULL,
  "-debug", "verbose mode",
  "-color", "enable color output",
  NULL,
};

extern Gal_ConditionStruct **_Gal_IReadDialogueControl(Gal_FileStruct *fs, int *num_conditions_ptr, int *error_ptr, int control_flags);

int main(int argc, char **argv)
{
  Gal_ConditionStruct **conditions = NULL;
  int num_conditions = 0;
  char *in_pgm = NULL, *out_pgm = NULL;
  int color = 0;
  FILE *fp;
  int control_flags = GAL_PROGRAM_VERIFY;

  Gal_InitializeStatics();

  /* process command line arguments */
  if (GalUtil_OACheckUsage(argc, argv, oas, NULL) == 0) {
    GalUtil_Fatal("Command line argument processing failed!");
  }

  GalUtil_OAExtract(argc, argv, oas, "-in", GAL_OA_STRING, &in_pgm);
  GalUtil_OAExtract(argc, argv, oas, "-out", GAL_OA_STRING, &out_pgm);
  GalUtil_OAExtract(argc, argv, oas, "-color", GAL_OA_INT, &color);
  GalUtil_OAExtract(argc, argv, oas, "-debug", GAL_OA_INT, &Debug_Mode);

  if (color)
    GalUtil_VerboseUseColor();
  else
    GalUtil_VerboseUseBW();

  if (Debug_Mode)
    control_flags = control_flags | GAL_PROGRAM_DEBUG;

  if (in_pgm)
  {
    Gal_FileStruct *fs;
    int error = GAL_NO_ERROR;
    
    GalUtil_PInfo1("\nVerifying control file %s\n", in_pgm);
    
    if ((fs = Gal_OpenControlFile(in_pgm, NULL)))
    {
      conditions = _Gal_IReadDialogueControl(fs, &num_conditions, &error, control_flags);
      if (error != GAL_NO_ERROR) {
	Gal_PrintProgramError(fs, error); 
	conditions = NULL;
      }
      Gal_CloseControlFile(fs);
    }
  }
  else
  {
    GalUtil_OAPrintUsage(argc, argv, oas);
    exit(1);
  }

  if (out_pgm)
  {
    if (!strcmp(out_pgm, "-"))
      Gal_PrintDialogueControl(stdout, conditions, num_conditions);
    else if ((fp = fopen(out_pgm, "w")))
    {
      GalUtil_PInfo1("\nWriting control file %s\n", out_pgm);
      Gal_PrintDialogueControl(fp, conditions, num_conditions);
      fclose(fp);
    }
    else
    {
      GalUtil_Warn("Failed to open output file %s", out_pgm);
      exit(1);
    }
  }

  exit(0);
}

/* 
 *  for Emacs...
 *  Local Variables:
 *  mode: c
 *  fill-column: 110
 *  comment-column: 80
 *  c-indent-level: 2
 *  c-continued-statement-offset: 2
 *  c-brace-offset: -2
 *  c-argdecl-indent: 2
 *  c-label-offset: -2
 *  End:
 */
