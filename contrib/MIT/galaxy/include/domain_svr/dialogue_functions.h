/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _DIALOGUE_FUNCTIONS_H
#define _DIALOGUE_FUNCTIONS_H

#ifdef DIALOGUE_FUNCTIONS_INCLUDE

#ifndef DIALOGUE_FUNCTION_ARG_TYPE
#define DIALOGUE_FUNCTION_ARG_TYPE GAL_DOMAIN_SVR
#endif

/* define the function prototypes */
#define GAL_DIALOGUE_FUNCTION(__fn__) int __fn__(DIALOGUE_FUNCTION_ARG_TYPE *);
#include DIALOGUE_FUNCTIONS_INCLUDE
#undef GAL_DIALOGUE_FUNCTION

/* define the dialogue function map */
#define GAL_DIALOGUE_FUNCTION(__fn__) {#__fn__, __fn__},
static GAL_DIALOGUE_FUNCTION_MAP Dialogue_Function_Map[] =
{
#include DIALOGUE_FUNCTIONS_INCLUDE
  {NULL, NULL}
};
#undef GAL_DIALOGUE_FUNCTION

/* SAM 9/23/99: This is the function which allows the dialogue
   stuff to be loaded by the server initialization header. */

extern void Gal_SetServerName(char *name);

void _GalSS_configure_MITDialogue(GalIO_ServerStruct *s)
{
  if (s) {
    Gal_SetServerName(GalIO_GetServerName(s));
    Gal_InitializeDialogueDefaults(Dialogue_Function_Map);
  }
}

#endif /* #ifdef DIALOGUE_FUNCTIONS_INCLUDE */

#endif /* #ifndef _DIALOGUE_FUNCTIONS_H */
