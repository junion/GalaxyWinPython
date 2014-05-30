/*
  This file (c) Copyright 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <limits.h>
#include "galaxy/sysdep.h"
#include <errno.h>

#ifdef WIN32
#include <direct.h>

#define MAXPATH _MAX_PATH
typedef int mode_t;

#else

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#define MAXPATH PATH_MAX

#endif

#include "galaxy/util.h"

/* Like mkdir -p pathname
 * Returns 0 on success, -1 on failure (errno will be set)
 * all created directories will be created with mode
 * NOTE: / and \ are both considered to directory seperators
 */
int GalUtil_Mkdirp(const char *pathname, mode_t mode){
  int len;
  int i = 0;
  char *dirpath = _gal_strdup(pathname);
  if (0 == dirpath){
    return -1;
  }
  len = strlen(dirpath);
  if (dirpath[len-1] == '/' ||
      dirpath[len-1] == '\\'){
    len--;
    dirpath[len] = '\0';
  }
  for(i=1;i<=len;i++){
    if ('/' == dirpath[i] ||
	'\\' == dirpath[i] ||
	'\0' == dirpath[i]){
      dirpath[i] = '\0';
#ifndef WIN32
    RETRY:
#endif

#ifdef WIN32
      if (0 != _mkdir(dirpath
#else
      if (0 != mkdir(dirpath
		  ,mode
#endif
		  )){
	switch(errno){
	case EEXIST:
	  break;
#ifndef WIN32
	case EINTR:
	  goto RETRY;
#endif
	default:
	  free(dirpath);
	  return -1;
	}
      }
      /* Windows is documented to work with / */
      dirpath[i] = '/';
    }
  }
  free(dirpath);
  return 0;  
}

