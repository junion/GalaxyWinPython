/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef DEFINING_GAL_DEBUG_MEMORY
#define free(x) gal_debug_free(x, __FUNCTION__)
#define malloc(x) gal_debug_malloc(x, __FUNCTION__)
#define calloc(x,y) gal_debug_calloc(x, y, __FUNCTION__)
#define strdup(x) gal_debug_strdup(x, __FUNCTION__)
#endif

void gal_debug_memory_enable_printing(int val);
void gal_debug_memory_print_address(void *val);
void gal_debug_free(void *ptr, char *fn);
void *gal_debug_malloc(size_t size, char *fn);
void *gal_debug_calloc(size_t nmemb, size_t size, char *fn);
char *gal_debug_strdup(char *str, char *fn);
