/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include <stdarg.h>
#include <math.h>

#include "galaxy/util.h"

#include "galaxy/gthread.h"

/* SAM 9/14/99: I'll just use the same mutex for both. */

static int usage_lock_initialized = 0;
static GalUtil_LocalMutex usage_lock;

/* This must only ever be called from one thread until it has initialized */
static void init_usage_lock(void)
{
  if (!usage_lock_initialized){
    GalUtil_InitLocalMutex(&usage_lock);
    usage_lock_initialized = 1;
  }
}

static int usage_called = 0;
static int ui_usage_called = 0;

static void
oa_i_check_buf_alloc(char *str, char **oa_buf, int *oa_buf_len);
static int oa_i_find_key(const char *key, char **oas, int *num_arg);
static void oa_i_set_arg(void *arg, int tpe, char *value);
static int oa_i_check_oas(char **oas, char **rest_descr, int *num_oas);

/*
   This function looks through the oa string, checking if the user has called the program properly, and also
   checking if the oa string is properly formatted.  It sets first_real_arg, if non-null, to be the argc of
   the first non-key argument.  The function returns 1 if usage is correct, 0 on error.

   If the special key -REST (added for GalUtil_OASplitArgs) appears, first_real_arg will be set to the first
   argument following the key.
*/

int
GalUtil_OACheckUsage(int argc, char **argv, char **oas, int *first_real_arg)
{
  int i, j, bad, loc, num_arg, num_oas, first_arg;
  char *key, *last_key, *tag, *rest;

  /* First, verify oas string is well formatted */
  if (oa_i_check_oas(oas, &rest, &num_oas) == 0)
    return(0);

  GalUtil_LockLocalMutex(&usage_lock);
  usage_called = 1;
  GalUtil_UnlockLocalMutex(&usage_lock);


  /* If argc or argv is empty, return. */
  if ((!argc) || (!argv))
    return 1;
  
  tag = (char *) calloc(num_oas, sizeof(char));
  /* Now check user -- move forwards insisting on proper key */
  /* syntax until the end of the key list */
  bad = 0;
  first_arg = -1;
  for(i=1;i<argc;) {
    if (*argv[i] != '-') {
      if (!bad) first_arg = i;
      bad = 1;
      i++;
    }
    else {
      /* See if this is one of our keys */
      if (!strcmp(argv[i], "-help")) {
	GalUtil_OAPrintUsage(argc, argv, oas);
	return(0);
      }

      loc = oa_i_find_key(argv[i], oas, &num_arg);
      
      if (loc != -1) {

	if (tag[loc]) {
	  if (num_arg > 0)
	    GalUtil_Warn("Key `%s' specified twice -- will use first occurrence", argv[i]);
	}

	tag[loc] = 1;

 	key = argv[i];

	if (bad) {
	  if (i-first_arg == 1)
	    GalUtil_Warn("Unknown keys (%s) -- check usage",
		     argv[first_arg]);
	  else if (i-first_arg == 2)
	    GalUtil_Warn("Unknown keys (%s %s) -- check usage",
		     argv[first_arg], argv[first_arg+1]);
	  else if (i-first_arg == 3)
	    GalUtil_Warn("Unknown keys (%s %s %s) -- check usage",
		     argv[first_arg], argv[first_arg+1], argv[first_arg+2]);
	  else
	    GalUtil_Warn("Unknown keys (starting with %s) -- check usage",
		     argv[first_arg]);

	  GalUtil_OAPrintUsage(argc, argv, oas);
	  return(0);
	}

						  /* Advance across the number of args for this key */
	for(j=0;j<num_arg;j++) {
	  i++;

	  if (i == argc) {
	    GalUtil_Warn("Final key `%s' doesn't have enough arguments", key);
	    GalUtil_OAPrintUsage(argc, argv, oas);
	    return(0);
	  }
	}

	last_key = key;
						  /* Advance to next entry */
	i++;
      }
      else if (!strcmp(argv[i], "-REST") && rest) {
	first_arg = i+1;
	break;
      }
      else {
	if (!bad)
	  first_arg = i;
	bad = 1;
	i++;
      }
    }
  }

  if (rest == NULL && first_arg != -1) {
    GalUtil_Warn("Unknown keys at the end of the options (starting with %s)",
	     argv[first_arg]);
    GalUtil_OAPrintUsage(argc, argv, oas);
    return(0);
  }

  if (first_real_arg != NULL)
    *first_real_arg = first_arg;

  free(tag);
  return(1);
}


/*
  Just a version for which the first argument starts at 0, not 1.
*/

int
GalUtil_OAUiCheckUsage(const char *progname, int argc, char **argv, char **oas, int *first_real_arg)
{
  int i, j, bad, loc, num_arg, num_oas, first_arg;
  char *key, *last_key, *tag, *rest;
  
						  /* First, verify oas string is well formatted */
  if (oa_i_check_oas(oas, &rest, &num_oas) == 0)
    return(0);

  GalUtil_LockLocalMutex(&usage_lock);
  ui_usage_called = 1;
  GalUtil_UnlockLocalMutex(&usage_lock);

  tag = (char *) calloc(num_oas, sizeof(char));
						  /* Now check user -- move forwards insisting on proper key */
						  /* syntax until the end of the key list */
  bad = 0;
  first_arg = -1;
  for(i=0;i<argc;) {
    if (*argv[i] != '-') {
      if (!bad) first_arg = i;
      bad = 1;
      i++;
    } else {					  /* See if this is one of our keys */
      if (!strcmp(argv[i], "-help")) {
	GalUtil_OAUiPrintUsage(progname, argc, argv, oas);
	return(0);
      }

      loc = oa_i_find_key(argv[i], oas, &num_arg);
      
      if (loc != -1) {

	if (tag[loc]) {
	  if (num_arg > 0)
	    GalUtil_Warn("Key `%s' specified twice -- using first occurrence", argv[i]);
	}

	tag[loc] = 1;

 	key = argv[i];

	if (bad) {
	  if (i-first_arg == 1)
	    GalUtil_Warn("Unknown keys (%s) -- check usage",
		     argv[first_arg]);
	  else if (i-first_arg == 2)
	    GalUtil_Warn("Unknown keys (%s %s) -- check usage",
		     argv[first_arg], argv[first_arg+1]);
	  else if (i-first_arg == 3)
	    GalUtil_Warn("Unknown keys (%s %s %s) -- check usage",
		     argv[first_arg], argv[first_arg+1], argv[first_arg+2]);
	  else
	    GalUtil_Warn("Unknown keys (starting with %s) -- check usage",
		     argv[first_arg]);

	  GalUtil_OAUiPrintUsage(progname, argc, argv, oas);
	  return(0);
	}

						  /* Advance across the number of args for this key */
	for(j=0;j<num_arg;j++) {
	  i++;

	  if (i == argc) {
	    GalUtil_Warn("Final key `%s' doesn't have enough arguments", key);
	    GalUtil_OAUiPrintUsage(progname, argc, argv, oas);
	    return(0);
	  }
	}

	last_key = key;
						  /* Advance to next entry */
	i++;
      }
      else {
	if (!bad)
	  first_arg = i;
	bad = 1;
	i++;
      }
    }
  }

  if (rest == NULL && first_arg != -1) {
    GalUtil_Warn("Unknown keys at the end of the options (starting with %s)",
	     argv[first_arg]);
    GalUtil_OAUiPrintUsage(progname, argc, argv, oas);
    return(0);
  }

  if (first_real_arg != NULL)
    *first_real_arg = first_arg;

  free(tag);
  return(1);
}

/*
   This prints out the usage line, specifying the defaults and descriptions.
*/

void
GalUtil_OAPrintUsage(int argc, char **argv, char **oas)
{
  char *ptr, *rest;


  if (!oa_i_check_oas(oas, &rest, NULL))
    return;

  ptr = strrchr(argv[0], '/');
  if (ptr == NULL) ptr = argv[0];
  else ptr++;

  GalUtil_Print(-1,"\n");

  if (rest != NULL)
    GalUtil_Print(-1,"Usage: %s [Options] [%s]\n\n", ptr, rest);
  else
    GalUtil_Print(-1,"Usage: %s [Options]\n\n", ptr);

  GalUtil_OAPrintOptions(argc, argv, oas, "  Options:");

  GalUtil_Print(-1,"    -help: print this usage line\n\n");

  return;
}

/*
   This prints out the options, preceded by an optional tag.
*/

void
GalUtil_OAPrintOptions(int argc, char **argv, char **oas, char *tag)
{
  int num_arg, loc, found, i;
  char *ptr, *rest;
  char *oa_buf = (char *) NULL;
  int oa_buf_len = 0;
  char *tok_lasts;

  if (!oa_i_check_oas(oas, &rest, NULL))
    return;

  if (tag)
    GalUtil_Print(-1,"%s\n", tag);
  else
  {
    ptr = strrchr(argv[0], '/');
    if (ptr == NULL)
      ptr = argv[0];
    else
      ptr++;

    GalUtil_Print(-1,"  %s options:\n", ptr);
  }

  loc = 0;
  while(oas[loc] != NULL) {
    oa_i_check_buf_alloc(oas[loc], &oa_buf, &oa_buf_len);
    strcpy(oa_buf, oas[loc]);
    _gal_strtok_r(oa_buf, " ", &tok_lasts);

    loc = oa_i_find_key(oa_buf, oas, &num_arg); /* Just to get num_arg */

    if (!strcmp(oas[loc], "REST")) {
      loc++;
      loc++;
      continue;
    }

    GalUtil_Print(-1,"    %s: %s", oas[loc], oas[loc+1]);

    loc += 2;

    found = 0;
    for(i=0;i<num_arg;i++) {
      if (oas[loc+i] != NULL && strlen(oas[loc+i])>0)
	found = 1;
    }

    if (found) {
      if (num_arg > 1)
	GalUtil_Print(-1," (defaults ");
      else
	GalUtil_Print(-1," (default ");

      for(i=0;i<num_arg;i++) {
	if (i > 0) GalUtil_Print(-1,", ");

	if (oas[loc+i] != NULL && strlen(oas[loc+i])>0)
	  GalUtil_Print(-1,"%s", oas[loc+i]);
	else
	  GalUtil_Print(-1,"<none>");
      }

      GalUtil_Print(-1,")");
    }

    GalUtil_Print(-1,"\n");

    loc += num_arg;
  }
  free(oa_buf);
  return;
}


/*
   This just prints out the usage line, specifying the defaults and descriptions.
*/

void
GalUtil_OAUiPrintUsage(const char *progname, int argc, char **argv, char **oas)
{
  int num_arg, loc, found, i;
  const char *ptr;
  char *rest;
  char *oa_buf = (char *) NULL;
  int oa_buf_len = 0;
  char *tok_lasts;

  if (!oa_i_check_oas(oas, &rest, NULL))
    return;

  ptr = strrchr(progname, '/');
  if (ptr == NULL) ptr = progname;
  else ptr++;

  GalUtil_Print(-1,"\n");

  if (rest != NULL)
    GalUtil_Print(-1,"Usage: %s [Options] [%s]\n\n  Options:\n", ptr, rest);
  else
    GalUtil_Print(-1,"Usage: %s [Options]\n\n  Options:\n", ptr);

  loc = 0;
  while(oas[loc] != NULL) {
    oa_i_check_buf_alloc(oas[loc], &oa_buf, &oa_buf_len);
    strcpy(oa_buf, oas[loc]);
    _gal_strtok_r(oa_buf, " ", &tok_lasts);

    loc = oa_i_find_key(oa_buf, oas, &num_arg); /* Just to get num_arg */

    if (!strcmp(oas[loc], "REST")) {
      loc++;
      loc++;
      continue;
    }

    GalUtil_Print(-1,"    %s: %s", oas[loc], oas[loc+1]);

    loc += 2;

    found = 0;
    for(i=0;i<num_arg;i++) {
      if (oas[loc+i] != NULL && strlen(oas[loc+i])>0)
	found = 1;
    }

    if (found) {
      if (num_arg > 1)
	GalUtil_Print(-1," (defaults ");
      else
	GalUtil_Print(-1," (default ");

      for(i=0;i<num_arg;i++) {
	if (i > 0) GalUtil_Print(-1,", ");

	if (oas[loc+i] != NULL && strlen(oas[loc+i])>0)
	  GalUtil_Print(-1,"%s", oas[loc+i]);
	else
	  GalUtil_Print(-1,"<none>");
      }

      GalUtil_Print(-1,")");
    }

    GalUtil_Print(-1,"\n");

    loc += num_arg;
  }
  GalUtil_Print(-1,"    -help: print this usage line\n");

  GalUtil_Print(-1,"\n");
  free(oa_buf);
  return;
}


/*  This function creates in_argc/argv containing only those arguments
    specified in the oas and returns the remainder of the arguments in
    ex_argc/argv.  It checks for syntax errors like GalUtil_OACheckUsage for
    the keys in the oas.

    If it finds the -help key it adds it to both in_argc/argv and
    ex_argc/argv and calls GalUtil_OAPrintUsage but does not return an
    error.  

    It relies on the special key -REST in the argument list to set
    first_real_arg.  If the -REST key is found and REST appears in the
    oas, the remaining arguments (excluding the -REST key) will be
    appended to in_argc/argv.

    The function returns 1 if all arguments are consumed and 0
    if there is an error.

 */

int GalUtil_OASplitArgs(int argc, char **argv, char **oas, int *first_real_arg,
		  int *in_argc, char ***in_argv,
		  int *ex_argc, char ***ex_argv)
{
  int i, j, loc, num_arg, num_oas;
  int in_count = 0, ex_count = 0;
  char *key, *tag, *rest;
  
  /* First, verify oas string is well formatted */
  if (oa_i_check_oas(oas, &rest, &num_oas) == 0)
    return(0);

  /* If argc or argv is empty, return. */
  if ((!argc) || (!argv))
    return 1;

  /* allocate and initialize the new argc/argv */
  in_count = 1;
  (*in_argv) = calloc((argc+1), sizeof(char *));
  (*in_argv)[0] = _gal_strdup(argv[0]);
  ex_count = 1;
  (*ex_argv) = calloc((argc+1), sizeof(char *));
  (*ex_argv)[0] = _gal_strdup(argv[0]);

  /* set default value of first_real_arg */
  if (first_real_arg)
    *first_real_arg = -1;

  /* keep track of which keys have appeared in argc/argv */
  tag = (char *) calloc(num_oas, sizeof(char));

  /*  Now check usage and split the keys */
  for(i=1;i<argc;)
  {
    key = argv[i];

    if (key[0] != '-')
    {
      if (rest)
	GalUtil_Warn("Must use -REST key to indicate the start of non-oas args");
      else
	GalUtil_Warn("Invalid key %s", key);
      GalUtil_OAPrintUsage(argc, argv, oas);
      free(tag);
      return(0);
    }

    /* The help key should be included in both in_argv and ex_argv */
    if (!strcmp(key, "-help"))
    {
      (*in_argv)[in_count++] = _gal_strdup(key);
      (*ex_argv)[ex_count++] = _gal_strdup(key);
      i++;
    }
    else if (!strcmp(key, "-REST") && rest)
    {
      /* skip over the -REST key */
      i++;

      /* set the index of the first non-oas argument */
      if (first_real_arg)
	*first_real_arg = in_count + 1;

      /* add the remaining arguments to in_argv and return */
      while(i<argc)
	(*in_argv)[in_count++] = _gal_strdup(argv[i++]);
      *in_argc = in_count;
      *ex_argc = ex_count;
      free(tag);
      return(1);
    }
    else
    {
      /* look for the key in the oas */
      loc = oa_i_find_key(key, oas, &num_arg);
      if (loc != -1)
      {
	/* if the key is found, add the key and its arguments to in_argv */
	(*in_argv)[in_count++] = _gal_strdup(argv[i++]);

	for(j=0;j<num_arg && i<argc;j++)
	  (*in_argv)[in_count++] = _gal_strdup(argv[i++]);

	if (j < num_arg)
	{
	  GalUtil_Warn("Final key `%s' doesn't have enough arguments", key);
	  GalUtil_OAPrintUsage(argc, argv, oas);
	  free(tag);
	  return(0);
	}

	/* print warning if key has appeared before */
	if (tag[loc])
	  if (num_arg > 0)
	    GalUtil_Warn("Key `%s' specified more than once, will extract first occurrence", key);
	tag[loc] = 1;
      }
      else
      {
	/* if the key is not found add it (and its arguments) to ex_argv */
	do {
	  (*ex_argv)[ex_count++] = _gal_strdup(argv[i++]);
	} while ((i < argc) && (argv[i][0] != '-'));
      }
    }
  }
  *in_argc = in_count;
  *ex_argc = ex_count;
  free(tag);
  return(1);
}



/*
   This function will actually extract the arguments from the online arguments specified by the user.  Key is
   which argument needs to be extracted (and has to be defined in oas).  Oas is the description array of the
   online arguments.

   If the specified online argument has no arguments, then this function will set the argument it is called
   with to the boolean value true or false as to whether the online argument was actually specified.

   It assumes GalUtil_OACheckUsage has been called.
*/

extern void _gal_unlock_mutex(void *mutex);

int
GalUtil_OAExtract(int argc, char **argv, char **oas, const char *key, ...)
{
  va_list ap;
  int i, j, loc, found, num_arg, tpe, tmp_n_arg;
  void *arg;

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) &usage_lock);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) &usage_lock);
#endif
  GalUtil_LockLocalMutex(&usage_lock);
  
  if (!usage_called) {
    oa_i_check_oas(oas, NULL, NULL);
    usage_called = 1;
  }
  
  GalUtil_UnlockLocalMutex(&usage_lock);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif

  GalUtil_Assert(*key == '-', "Key must start with `-'");

  loc = oa_i_find_key(key, oas, &num_arg);

  GalUtil_Assert(loc != -1, "No such key `%s'", key);

  /* If argc or argv is empty, return. */
  if ((!argc) || (!argv))
    return 0;

  /* Carefully look for this key in the online arguments */
  found = 0;
  for(i=1;i<argc;) {
    /* We've reached the end of the keys */
    if (*argv[i] != '-' || oa_i_find_key(argv[i], oas, &tmp_n_arg) == -1) {
      found = 0;
      break;
    }
    else {
      /* We've found our key */
      if (*argv[i] == '-' && strcmp(key, argv[i]) == 0) {
	found = 1;
	break;
      }
      else {
	/* Advance to the next key */
	i += tmp_n_arg+1;
      }
    }
  }

  va_start(ap, key);
  
  if (num_arg > 0) {
    if (!found) {
      /* Fill in with default values */
      loc += 2;
      for(j=loc;j<loc+num_arg;j++) {
	tpe = va_arg(ap, int);
	arg = va_arg(ap, void *);

	/* Set value only if non-null */
	if (oas[j] != NULL)
	  oa_i_set_arg(arg, tpe, oas[j]);
      }
    }
    else {
      loc += 2;

      i++;

      for(j=loc;j<loc+num_arg;j++) {
	tpe = va_arg(ap, int);
	arg = va_arg(ap, void *);

	oa_i_set_arg(arg, tpe, argv[i]);
	
	i++;
      }
    }
  }
  else {					  /* Handle the zero-argument case differently: return a */
    tpe = va_arg(ap, int);			  /* boolean value as the value of the argument */
    arg = va_arg(ap, void *);

    if (!found)
      oa_i_set_arg(arg, tpe, "0");
    else
      oa_i_set_arg(arg, tpe, "1");
  }

  va_end(ap);
  return(found);
}



/*
  Simple argument extract -- doesn't use the oas (char *) array.  And argc/argv DO NOT start with the program
  (so it will start with argv[0] looking for the key).
*/

int
GalUtil_SAExtract(int argc, char **argv, const char *key, int num, ...)
{
  va_list ap;
  int i, j, tpe, res;
  void *arg;


  GalUtil_Assert(*key == '-', "Key must start with `-'");


						  /* Carefully look for this key in the online arguments */
  for(i=0;i<argc;i++) {
    if (!strcmp(argv[i], key))
      break;
  }
  
  res = 0;

  if (i < argc) {

    i++;

    va_start(ap, num);

    for(j=0;j<num;i++,j++) {
      tpe = va_arg(ap, int);
      arg = va_arg(ap, void *);

      if (tpe == 0) {
	break;
      } else {
	if (i == argc) {
	  GalUtil_Warn("Expected more arguments for key `%s'", key);
	  res = -1;
	  break;
	}
      }
						  /* Set value only if non-null */
      oa_i_set_arg(arg, tpe, argv[i]);
    }
  
    if (res == 0)
      res = 1;
  }
  va_end(ap);

  return(res);
}

/*
  This is just like the previous function, but meant to be used to extract arguments from a ui structure (the
  only difference is argv[0] is the first argument, not argv[1]).
*/

int
GalUtil_OAUiExtract(int argc, char **argv, char **oas, const char *key, ...)
{
  va_list ap;
  int i, j, loc, found, num_arg, tpe, tmp_n_arg;
  void *arg;

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) &usage_lock);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) &usage_lock);
#endif
  GalUtil_LockLocalMutex(&usage_lock);
  
  if (!ui_usage_called) {
    oa_i_check_oas(oas, NULL, NULL);
    ui_usage_called = 1;
  }
  
  GalUtil_UnlockLocalMutex(&usage_lock);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
  
  GalUtil_Assert(*key == '-', "Key must start with `-'");

  loc = oa_i_find_key(key, oas, &num_arg);

  GalUtil_Assert(loc != -1, "No such key `%s'", key);


						  /* Carefully look for this key in the online arguments */
  found = 0;
  for(i=0;i<argc;) {				  /* We've reached the end of the keys */
    if (*argv[i] != '-' || oa_i_find_key(argv[i], oas, &tmp_n_arg) == -1) {
      found = 0;
      break;
    }
    else {					  /* We've found our key */
      if (*argv[i] == '-' && strcmp(key, argv[i]) == 0) {
	found = 1;
	break;
      }
      else {					  /* Advance to the next key */
	i += tmp_n_arg+1;
      }
    }
  }

  va_start(ap, key);
  
  if (num_arg > 0) {
    if (!found) {				  /* Fill in with default values */
      loc += 2;
      for(j=loc;j<loc+num_arg;j++) {
	tpe = va_arg(ap, int);
	arg = va_arg(ap, void *);
						  /* Set value only if non-null */
	if (oas[j] != NULL)
	  oa_i_set_arg(arg, tpe, oas[j]);
      }
    }
    else {
      loc += 2;

      i++;

      for(j=loc;j<loc+num_arg;j++) {
	tpe = va_arg(ap, int);
	arg = va_arg(ap, void *);

	oa_i_set_arg(arg, tpe, argv[i]);
	
	i++;
      }
    }
  } else {					  /* Handle the zero-argument case differently: return a */
    tpe = va_arg(ap, int);			  /* boolean value as the value of the argument */
    arg = va_arg(ap, void *);

    if (!found)
      oa_i_set_arg(arg, tpe, "0");
    else
      oa_i_set_arg(arg, tpe, "1");
  }

  va_end(ap);

  return(found);
}



/*
   This is just like the previous one, except it asserts that the argument be specified.  It has no
   return value because if the argument was not specified, it prints the usage and exits.
*/

void
GalUtil_OAExtractAsserting(int argc, char **argv, char **oas, const char *key, ...)
{
  va_list ap;
  int i, j, loc, found, num_arg, tpe, tmp_n_arg;
  void *arg;

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) &usage_lock);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) &usage_lock);
#endif
  GalUtil_LockLocalMutex(&usage_lock);
  
  if (!usage_called) {
    oa_i_check_oas(oas, NULL, NULL);
    usage_called = 1;
  }
  
  GalUtil_UnlockLocalMutex(&usage_lock);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif

  GalUtil_Assert(*key == '-', "Key must start with `-'");

  loc = oa_i_find_key(key, oas, &num_arg);

  GalUtil_Assert(loc != -1, "No such key `%s'", key);


						  /* Carefully look for this key in the online arguments */
  found = 0;
  for(i=1;i<argc;) {				  /* We've reached the end of the keys */
    if (*argv[i] != '-' || oa_i_find_key(argv[i], oas, &tmp_n_arg) == -1) {
      found = 0;
      break;
    }
    else {					  /* We've found our key */
      if (*argv[i] == '-' && strcmp(key, argv[i]) == 0) {
	found = 1;
	break;
      }
      else {					  /* Advance to the next key */
	i += tmp_n_arg+1;
      }
    }
  }

  if (!found) {
    GalUtil_Print(-1,"  (must specify %s)\n", key);
    GalUtil_OAPrintUsage(argc, argv, oas);
    exit(-1);
  }

  va_start(ap, key);
  
  if (num_arg > 0) {
    if (!found) {				  /* Fill in with default values */
      loc += 2;
      for(j=loc;j<loc+num_arg;j++) {
	tpe = va_arg(ap, int);
	arg = va_arg(ap, void *);
						  /* Set value only if non-null */
	if (oas[j] != NULL)
	  oa_i_set_arg(arg, tpe, oas[j]);
      }
    }
    else {
      loc += 2;

      i++;

      for(j=loc;j<loc+num_arg;j++) {
	tpe = va_arg(ap, int);
	arg = va_arg(ap, void *);

	oa_i_set_arg(arg, tpe, argv[i]);
	
	i++;
      }
    }
  }
  else {					  /* Handle the zero-argument case differently: return a */
    tpe = va_arg(ap, int);			  /* boolean value as the value of the argument */
    arg = va_arg(ap, void *);

    if (!found)
      oa_i_set_arg(arg, tpe, "0");
    else
      oa_i_set_arg(arg, tpe, "1");
  }

  va_end(ap);
}


void
GalUtil_OAUiExtractAsserting(int argc, char **argv, char **oas, const char *key, ...)
{
  va_list ap;
  int i, j, loc, found, num_arg, tpe, tmp_n_arg;
  void *arg;

#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex, (void *) &usage_lock);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex, (void *) &usage_lock);
#endif
  GalUtil_LockLocalMutex(&usage_lock);
  
  if (!ui_usage_called) {
    oa_i_check_oas(oas, NULL, NULL);
    ui_usage_called = 1;
  }
  
  GalUtil_UnlockLocalMutex(&usage_lock);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif

  GalUtil_Assert(*key == '-', "Key must start with `-'");

  loc = oa_i_find_key(key, oas, &num_arg);

  GalUtil_Assert(loc != -1, "No such key `%s'", key);


						  /* Carefully look for this key in the online arguments */
  found = 0;
  for(i=0;i<argc;) {				  /* We've reached the end of the keys */
    if (*argv[i] != '-' || oa_i_find_key(argv[i], oas, &tmp_n_arg) == -1) {
      found = 0;
      break;
    }
    else {					  /* We've found our key */
      if (*argv[i] == '-' && strcmp(key, argv[i]) == 0) {
	found = 1;
	break;
      }
      else {					  /* Advance to the next key */
	i += tmp_n_arg+1;
      }
    }
  }

  if (!found) {
    GalUtil_Print(-1,"  (must specify %s)\n", key);
    GalUtil_OAPrintUsage(argc, argv, oas);
    exit(-1);
  }

  va_start(ap, key);
  
  if (num_arg > 0) {
    if (!found) {				  /* Fill in with default values */
      loc += 2;
      for(j=loc;j<loc+num_arg;j++) {
	tpe = va_arg(ap, int);
	arg = va_arg(ap, void *);
						  /* Set value only if non-null */
	if (oas[j] != NULL)
	  oa_i_set_arg(arg, tpe, oas[j]);
      }
    }
    else {
      loc += 2;

      i++;

      for(j=loc;j<loc+num_arg;j++) {
	tpe = va_arg(ap, int);
	arg = va_arg(ap, void *);

	oa_i_set_arg(arg, tpe, argv[i]);
	
	i++;
      }
    }
  }
  else {					  /* Handle the zero-argument case differently: return a */
    tpe = va_arg(ap, int);			  /* boolean value as the value of the argument */
    arg = va_arg(ap, void *);

    if (!found)
      oa_i_set_arg(arg, tpe, "0");
    else
      oa_i_set_arg(arg, tpe, "1");
  }

  va_end(ap);
}




/*
   This looks up the specified key in the oas string, and returns the location of its entry.  It assumes that
   oa_i_check_oas has been called and returned 1.
*/

static int
oa_i_find_key(const char *key, char **oas, int *num_arg)
{
  int loc, found;
  char *ptr;
  char *oa_buf = (char *) NULL;
  int oa_buf_len = 0;
  char *tok_lasts;
  
  loc = 0;
  found = -1;
  while(oas[loc] != NULL) {
    oa_i_check_buf_alloc(oas[loc], &oa_buf, &oa_buf_len);
    strcpy(oa_buf, oas[loc]);
    ptr = _gal_strtok_r(oa_buf, " ", &tok_lasts);

    if (!strcmp(ptr, key))
      found = loc;

    ptr = _gal_strtok_r(NULL, " ", &tok_lasts);

    loc++;					  /* Advance to the description */
    loc++;					  /* Advance to the first default string */

						  /* Advance beyond the rest of the default strings */
    (*num_arg) = 0;
    while(ptr != NULL) {
      loc++;
      (*num_arg)++;
      ptr = _gal_strtok_r(NULL, " ", &tok_lasts);
    }

    if (found != -1) {
      free(oa_buf);
      return(found);
    }
  }
  free(oa_buf);
  return(-1);
}




/*
   This verifies that the oas string is well formatted.  It will also set rest_descr to be the description of
   REST if it is there.
*/


static int
oa_i_check_oas(char **oas, char **rest_descr, int *num_oas)
{
  int upto, len, num_arg, i;
  char *ptr, *key;
  char *oa_buf = (char *) NULL;
  int oa_buf_len = 0;
  char *tok_lasts;

  init_usage_lock();
						  /* First check programmer: is the oas string formatted */
						  /* corrrectly? */
  if (rest_descr != NULL)
    *rest_descr = NULL;

  upto = 0;
  while(oas[upto] != NULL) {
						  /* This should be a -key entry -- parse it into the # of */
						  /* arguments we are expecting */

    key = oas[upto];

/*
    GalUtil_Print(-1,"  (key %s)\n", key);
*/

    oa_i_check_buf_alloc(oas[upto], &oa_buf, &oa_buf_len);
    strcpy(oa_buf, oas[upto]);

    len = strlen(oa_buf);
						  /* Key length cannot be zero */
    if (len == 0) {
      GalUtil_Warn("Online argument string has a zero-length key");
      free(oa_buf);
      return(0);
    }

    if (!strcmp(oa_buf, "REST")) {
      if (rest_descr != NULL) {
	if (oas[upto+1] == NULL) {
	  GalUtil_Warn("Description string for REST is NULL");
	  free(oa_buf);
	  return(0);
	}

	*rest_descr = oas[upto+1];
      }

      upto++;
      upto++;
      continue;
    }

    if (oa_buf[0] != '-') {
      GalUtil_Warn("Key `%s' in online argument string does not start with `-'", oa_buf);
      free(oa_buf);
      return(0);
    }

						  /* Count how many args this key has */
    num_arg = 0;
    ptr = _gal_strtok_r(oa_buf, " \t", &tok_lasts);
    ptr = _gal_strtok_r(NULL, " \t", &tok_lasts);
    while(ptr != NULL) {
      num_arg++;
      ptr = _gal_strtok_r(NULL, " \t", &tok_lasts);
    }

    upto++;
    if (oas[upto] == NULL) {			  /* Up to the description of this key */
      GalUtil_Warn("Premature ending of online argument string, after key `%s'", key);
      free(oa_buf);
      return(0);
    }

    for(i=0;i<num_arg;i++)
      upto++;

    upto++;
  }

  if (num_oas != NULL)
    *num_oas = upto;
  
  free(oa_buf);
  return(1);
}

/* SAM 7/18/00: I need a clean way of "turning off"
   arguments, so that people can use the built-in
   server argument parsing but also make sure their own
   restrictions are respected (always contact the Hub,
   always assert, etc.). So I'll copy and read the oas,
   and keep track of which arguments are where. */

static GalUtil_OASDescription *__GalUtil_NewOASDescription(char **oas, int num_oas, GalUtil_OASEntry *entries)
{
  GalUtil_OASDescription *new_descr = (GalUtil_OASDescription *) malloc(sizeof(GalUtil_OASDescription));
  int i;
  
  new_descr->entries = (GalUtil_OASEntry *) entries;
  new_descr->num_oas = num_oas;

  /* Need i + 1 entries. */
  new_descr->oas = (char **) calloc(num_oas + 1, sizeof(char *));
  /* No need to copy the strings. */
  for (i = 0; i < num_oas; i++) {
    new_descr->oas[i] = oas[i];
  }
  return new_descr;
}

static GalUtil_OASEntry *__GalUtil_AddOASEntry(GalUtil_OASEntry *entry,
					       char *key,
					       int start_index,
					       int end_index)
{
  GalUtil_OASEntry *new_entry = (GalUtil_OASEntry *) malloc(sizeof(GalUtil_OASEntry));

  new_entry->next = (GalUtil_OASEntry *) NULL;
  new_entry->start_index = start_index;
  new_entry->end_index = end_index;
  new_entry->flag_string = key;  

  /* Put it at the end. */
  if (!entry) {
    return new_entry;
  } else {
    GalUtil_OASEntry *temp = entry;
    while (temp->next) {
      temp = temp->next;
    }
    temp->next = new_entry;
    return entry;
  }
}

void _GalUtil_FreeOASEntry(GalUtil_OASEntry *entry)
{
  GalUtil_OASEntry *temp1, *temp2;

  temp1 = entry;
  while (temp1) {
    temp2 = temp1->next;
    free(temp1->flag_string);
    free(temp1);
    temp1 = temp2;
  }
}

void _GalUtil_FreeOASDescription(GalUtil_OASDescription *descr)
{
  _GalUtil_FreeOASEntry(descr->entries);
  if (descr->oas)
    free(descr->oas);
  free(descr);
}

/* This function digests the OAS by copying it and making
   a record of where the indices are. The indices that are
   reported are 0-based, and the final index is INCLUSIVE. So
   { "-foo", "desc", "-bar foo", "desc", NULL, NULL }
   will say "-foo" from 0 to 1, "-bar" from 2 to 4. */

GalUtil_OASDescription *GalUtil_OADigest(char **oas)
{
  int upto, len, num_arg, i;
  char *ptr, *key;
  char *oa_buf = (char *) NULL;
  int oa_buf_len = 0;
  int key_index;
  char *tok_lasts;
  GalUtil_OASEntry *entry = (GalUtil_OASEntry *) NULL;

  init_usage_lock();
  
  upto = 0;
  while(oas[upto] != NULL) {
    /* This should be a -key entry -- parse it into the # of */
    /* arguments we are expecting */

    key = oas[upto];
    key_index = upto;

    oa_i_check_buf_alloc(oas[upto], &oa_buf, &oa_buf_len);
    strcpy(oa_buf, oas[upto]);

    len = strlen(oa_buf);
    /* Key length cannot be zero */
    if (len == 0) {
      GalUtil_Warn("Online argument string has a zero-length key");      
      free(oa_buf);
      _GalUtil_FreeOASEntry(entry);
      return((GalUtil_OASDescription *) NULL);
    }

    if (!strcmp(oa_buf, "REST")) {
      upto++;
      upto++;
      continue;
    }

    if (oa_buf[0] != '-') {
      GalUtil_Warn("Key `%s' in online argument string does not start with `-'", oa_buf);
      free(oa_buf);
      _GalUtil_FreeOASEntry(entry);
      return((GalUtil_OASDescription *) NULL);
    }

    /* Count how many args this key has */
    num_arg = 0;
    ptr = _gal_strtok_r(oa_buf, " \t", &tok_lasts);
    ptr = _gal_strtok_r(NULL, " \t", &tok_lasts);
    while(ptr != NULL) {
      num_arg++;
      ptr = _gal_strtok_r(NULL, " \t", &tok_lasts);
    }

    upto++;
    if (oas[upto] == NULL) {
      /* Up to the description of this key */
      GalUtil_Warn("Premature ending of online argument string, after key `%s'", key);
      free(oa_buf);
      _GalUtil_FreeOASEntry(entry);
      return((GalUtil_OASDescription *) NULL);
    }

    for(i=0;i<num_arg;i++)
      upto++;

    upto++;
    /* At this point, we should be able to add an entry. */
    entry = __GalUtil_AddOASEntry(entry, _gal_strdup(oa_buf), key_index, upto - 1);
  }
  free(oa_buf);
  /* upto is the index of the NULL element. */
  return __GalUtil_NewOASDescription(oas, upto, entry);
}

/* This function removes a key from the OAS. The indices that are
   recorded are 0-based, and the final index is INCLUSIVE. So
   { "-foo", "desc", "-bar foo", "desc", NULL, NULL }
   will say "-foo" from 0 to 1, "-bar" from 2 to 4. This
   function updates the record, which will be in order. This
   function returns 1 if it found an argument to remove, 0 otherwise. */

int GalUtil_RemoveOASArg(GalUtil_OASDescription *descr, const char *key)
{
  GalUtil_OASEntry *entry = descr->entries;
  GalUtil_OASEntry *previous = (GalUtil_OASEntry *) NULL;
  GalUtil_OASEntry *temp;

  while (entry) {
    if (!strcmp(key, entry->flag_string)) {
      /* This is the entry to remove. The entries
	 will be in order, so we can just loop through
	 the subsequent entries and subtract the difference. */
      GalUtil_OASEntry *afters = entry->next;
      int i = 1 + (entry->end_index - entry->start_index);
      int j;

      while (afters) {
	afters->start_index -= i;
	afters->end_index -= i;
	afters = afters->next;
      }

      /* Don't update previous in this case, just
	 previous' next pointer. */
      if (previous) {
	previous->next = entry->next;
      } else {
	descr->entries = entry->next;
      }

      /* Shorten OAS. */
      for (j = 0; j < (descr->num_oas - entry->end_index); j++) {
	descr->oas[j + entry->start_index] = descr->oas[j + i + entry->start_index];
      }
      for (j = 1 + (descr->num_oas - i) ; j < descr->num_oas; j++) {
	descr->oas[j] = (char *) NULL;
      }
      descr->num_oas -= i;

      /* Update loop elements. */
      temp = entry;
      entry = entry->next;
      temp->next = (GalUtil_OASEntry *) NULL;
      _GalUtil_FreeOASEntry(temp);
      return 1;
    } else {
      previous = entry;
      entry = entry->next;
    }
  }
  return 0;
}

/*
   This sets the argument according to the type specification.
*/

static void
oa_i_set_arg(void *arg, int tpe, char *value)
{

  if (arg == NULL)
    return;

  switch(tpe) {

  case GAL_OA_CHAR:
    *((char *) arg) = value[0];
    break;

  case GAL_OA_SHORT:
    *((short *) arg) = (short) atoi(value);
    break;

  case GAL_OA_INT:
    *((int *) arg) = atoi(value);
    break;

  case GAL_OA_STRING:
    /* SAM 2/28/01: This is the only one which would point
       directly into the newly allocated argv (because we
       call OASplitArgs in generic-server-toplevel.c. I want
       to free that allocated memory. So yes, we should strdup().
       Everything else is a number computed from a string. */
    *((char **) arg) = _gal_strdup(value);
    break;

  case GAL_OA_FLOAT:
    *((float *) arg) = (float) atof(value);
    break;

  case GAL_OA_DOUBLE:
    *((double *) arg) = (double) atof(value);
    break;

  default:
    GalUtil_Fatal("Unknown type %d", tpe);
  }

  return;
}




/*
   This just ensures that oa_buf is allocated to at least len bytes.
*/

static void
oa_i_check_buf_alloc(char *str, char **oa_buf, int *oa_buf_len)
{
  int len;

  len = strlen(str)+1;

  if ((*oa_buf_len) < len) {
    if (*oa_buf_len == 0) {
      *oa_buf = (char *) malloc(len*sizeof(char));
    }
    else {
      *oa_buf = (char *) realloc(*oa_buf, len*sizeof(char));
    }

    *oa_buf_len = len;
  }
}

