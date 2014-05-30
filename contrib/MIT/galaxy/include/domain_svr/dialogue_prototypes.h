/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _DIALOGUE_PROTOTYPES_H
#define _DIALOGUE_PROTOTYPES_H


#ifdef DIALOGUE_FUNCTIONS_INCLUDE

/* define the dialogue function prototypes */
#define GAL_DIALOGUE_FUNCTION(__fn__) int __fn__(DIALOGUE_FUNCTION_ARG_TYPE *);
#include DIALOGUE_FUNCTIONS_INCLUDE
#undef GAL_DIALOGUE_FUNCTION

#endif /* #ifdef DIALOGUE_FUNCTIONS_INCLUDE */

#endif /* #ifndef _DIALOGUE_PROTOTYPES_H */
