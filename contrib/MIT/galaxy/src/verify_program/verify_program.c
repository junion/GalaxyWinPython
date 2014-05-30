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
#include "galaxy/util.h"
#include "galaxy/galaxy.h"
#include "hub.h"
#include "hub_program.h"

int Debug_Mode = 0;

static char *oas[] =
{
  "-in filename", "program file to verify", NULL,
  "-out filename", "regenerate program to file (- is stdout)", NULL,
  "-debug", "verbose mode",
  "-color", "enable color output",
  NULL,
};

int main(int argc, char **argv)
{
  HubControlStruct *hub_control = NULL;
  char *in_pgm = NULL, *out_pgm = NULL;
  int color = 0, error = 0;
  int control_flags = GAL_PROGRAM_VERIFY;  
  FILE *fp;

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

  if (in_pgm) {
    /* SAM 4/3/02: Due to changes in the program file parsing,
       the HUB structure is now directly populated. So we need
       to instantiate a small one to do the verification.
       This code duplicates a portion of the new_hub() function
       in hub_init.c, but trying to set up the links with
       that file would be a nightmare. */
    HUB *h =   (HUB *) calloc(1, sizeof(HUB));
    
    h->globals = Gal_MakeFrame("global", GAL_CLAUSE);
    GalHUB_SetHubDefaultDomain(h, "Unspecified", "DOMAIN:");
    GalHUB_InstantiateLogRecord(h);
    
    GalUtil_PInfo1("\nVerifying control file %s\n", in_pgm);
    hub_control = load_hub_control(h, in_pgm, &error, control_flags);
  }
  else
  {
    GalUtil_OAPrintUsage(argc, argv, oas);
    exit(1);
  }

  if (out_pgm)
  {
    if (!strcmp(out_pgm, "-")) {
      /* print to stdout. */
      print_hub_control(stdout, hub_control);
    } else if ((fp = fopen(out_pgm, "w")))
    {
      GalUtil_PInfo1("\nWriting control file %s\n", out_pgm);
      print_hub_control(fp, hub_control);
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
