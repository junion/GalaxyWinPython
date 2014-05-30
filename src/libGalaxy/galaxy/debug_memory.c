/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define DEFINING_GAL_DEBUG_MEMORY
#include "galaxy/debug_memory.h"
#include "galaxy/sysdep.h"
#include "galaxy/util.h"

static int enable_printing = 0;
static void *print_address = NULL;

void gal_debug_memory_enable_printing(int val)
{
  enable_printing = val;
}

void gal_debug_memory_print_address(void *val)
{
  print_address = val;
}

void gal_debug_free(void *ptr, char *fn)
{
  if (enable_printing || ptr == print_address)
    GalUtil_Print(-1,"%s: free 0x0%x\n", fn, (unsigned int)ptr);
  free(ptr);
}

void *gal_debug_malloc(size_t size, char *fn)
{
  void *ptr = malloc(size);
  if (enable_printing || ptr == print_address)
    GalUtil_Print(-1,"%s: malloc 0x0%x %d\n", fn, (unsigned int)ptr, size);
  return(ptr);
}

void *gal_debug_calloc(size_t nmemb, size_t size, char *fn)
{
  void *ptr = calloc(nmemb, size);
  if (enable_printing || ptr == print_address)
    GalUtil_Print(-1,"%s: calloc 0x0%x %d\n", fn, (unsigned int)ptr, nmemb * size);
  return(ptr);
}

char *gal_debug_strdup(char *str, char *fn)
{
  char *ptr = _gal_strdup(str);
  if (enable_printing || ptr == print_address)
    GalUtil_Print(-1,"%s: strdup 0x0%x (%s)\n", fn, (unsigned int)ptr, str);
  return(ptr);
}
