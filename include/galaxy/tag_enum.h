/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _TAG_ENUM_H
#define _TAG_ENUM_H

#define GAL_TAG(x,y) ,y
typedef enum
{
  GAL_NO_PROGRAM_TAG = 0
#include "galaxy/program_tags.h"
} Gal_ProgramTag;

typedef enum
{
  GAL_NO_ERROR = 0
#include "galaxy/error_tags.h"
} Gal_ErrorTag;
#undef GAL_TAG

#endif
