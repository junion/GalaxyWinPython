/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* PURPOSE:								      
 *
 *   This file implements the sls message printing facility. The idea is to
 *   provide run time control over the verbosity of messages. This is 
 *   accomplished by creating levels or classes of message printing functions
 *   (e.g. error, warning, debug, etc). Developers using this facility decide
 *   at which level a message is printed by calling the function corresponding
 *   to that level. (e.g. GalUtil_Error(), GalUtil_Warn(), sls_debug(), etc).
 *   Whether or not the message passed to that function gets printed is 
 *   determined at runtime by the user's environment variable defined by
 *   "GAL_VERBOSE". This allows the user to have run time control over the 
 *   number and nature of messages which get printed. Below is a table showing
 *   what gets printed for a given value of the environment variable. Note that
 *   as you increase levels (increase the value of the variable), the printing
 *   is cumulative. i.e. all messages up to and including that level are 
 *   printed.
 *
 *   verbose  functions	      purpose
 *   value    invoked
 *   =======================================================================
 *   0        none	      quiet mode
 *   1        GalUtil_Fatal   connect/disconnect info, fatal errors
 *            GalUtil_Error    
 *            GalUtil_Assert                   
 *   2        GalUtil_Warn    non-fatal errors
 *   3        GalUtil_PInfo1  traffic summary
 *            GalUtil_CPInfo1 
 *   4        GalUtil_PInfo2  verbose traffic info
 *            GalUtil_CPInfo2 
 *   5        GalUtil_Debug1  start/end of threads and timed tasks
 *   6        GalUtil_Debug2  raw transport layer
 *
 *   In addition, it provides another level of control by allowing the 
 *   developer to decide how the messages get displayed or processed. It does
 *   this by using the GalUtil_Warn(), etc, functions as wrappers which only 
 *   call other functions to do the real work. Each sls verbose function has an
 *   associated function pointer, which it dereferences to call the "work"
 *   function. By default, these function pointers are stored in 
 *   GalUtil_DefaultPrintPkg. For example, GalUtil_Warn dereferences 
 *   GalUtil_DefaultPrintPkg.warn_func, which is assigned a function which 
 *   looks at the GAL_VERBOSE variable to decide if anything should get 
 *   printed. If so, it calls printf(). Below is a table which shows the
 *   mapping of gal_verbose function to function pointer variable.
 *	
 *	GalUtil_Fatal   -> GalUtil_DefaultPrintPkg.fatal_func
 *      GalUtil_Assert  -> GalUtil_DefaultPrintPkg.fatal_func
 * 	GalUtil_Error   -> GalUtil_DefaultPrintPkg.error_func
 *	GalUtil_Warn    -> GalUtil_DefaultPrintPkg.warn_func
 *	GalUtil_Print   -> GalUtil_DefaultPrintPkg.level_func
 *	GalUtil_CPrint  -> GalUtil_DefaultPrintPkg.clevel_func
 *	GalUtil_PInfo1  -> GalUtil_DefaultPrintPkg.pinfo1_func
 *	GalUtil_PInfo2  -> GalUtil_DefaultPrintPkg.pinfo2_func
 *	GalUtil_CPInfo1 -> GalUtil_DefaultPrintPkg.cpinfo1_func
 *	GalUtil_CPInfo2 -> GalUtil_DefaultPrintPkg.cpinfo2_func
 *	GalUtil_Debug1  -> GalUtil_DefaultPrintPkg.debug1_func
 *	GalUtil_Debug2  -> GalUtil_DefaultPrintPkg.debug2_func
 * 
 *   A custom print package, GalUtil_CurrentPrintPkg, is also availabe. One
 *   can use GalUtil_CreatePrintPkg to create a new print package and then
 *   simply assign it to GalUtil_CurrentPrintPkg. This becomes the new
 *   print package used by the system. GalUtil_CreatePrintPkg allows for the
 *   specification of void *client_data, which can be used to provide
 *   some arbitrary common data to all print functions in 
 *   GalUtil_CurrentPrintPkg (e.g., a reference to the Hub).
 */

#include <stdio.h>
#include "galaxy/sysdep.h"
#include <signal.h>
#include <stdarg.h>

#include "galaxy/util.h"

/* 
 * The macro GAL_VERBOSE, defined in <sls/util.h>, ultimately expands to
 * a check of the global int galutil_verbose.  Initially, galutil_verbose = -1.
 * The first time GAL_VERBOSE is evaluated, it will assume the value of the
 * user's environment variable "GAL_VERBOSE".
 */
int galutil_verbose = -1;

/* Declare the default functions to be called by the sls_ functions. Make */
/* these static since we don't want people calling these functions directly */
static void printf_fatal(const char *fmt, va_list args, void *client_data);
static void printf_error(const char *fmt, va_list args, void *client_data);
static void printf_warn(const char *fmt, va_list args, void *client_data);
static void printf_level(int level, const char *fmt, va_list args, 
			 void *client_data);
static void printf_clevel(int level, int fore, int back, const char *fmt, 
			  va_list args, void *client_data);
static void printf_info(const char *fmt, va_list args, void *client_data);
static void printf_color_info(int fore, int back, const char *fmt, va_list args, 
			      void *client_data);

/* Define function pointers and initialize them to default functions. */
/* GalVPrintfFunc and GalVCPrintfFunc are typedef'd in <sls/util.h> */

GalUtil_PrintPkg GalUtil_DefaultPrintPkg = {
  printf_fatal,
  printf_error,
  printf_warn,
  printf_level,
  printf_clevel,
  printf_info,
  printf_info,
  printf_color_info,
  printf_color_info,
  printf_info,
  printf_info,
  NULL
};

GalUtil_PrintPkg *GalUtil_CurrentPrintPkg = &GalUtil_DefaultPrintPkg;


int user_initialized_print_package = 0;

/* Assign the function pointers to functions defined below */
void GalUtil_VerboseUseBW(void)
{
  GalUtil_DefaultPrintPkg.fatal_func = printf_fatal;
  GalUtil_DefaultPrintPkg.error_func = printf_error;
  GalUtil_DefaultPrintPkg.warn_func  = printf_warn;
  GalUtil_DefaultPrintPkg.level_func = printf_level;
  GalUtil_DefaultPrintPkg.clevel_func  = printf_clevel;
  GalUtil_DefaultPrintPkg.pinfo1_func  = printf_info;
  GalUtil_DefaultPrintPkg.pinfo2_func  = printf_info;
  GalUtil_DefaultPrintPkg.cpinfo1_func = printf_color_info;
  GalUtil_DefaultPrintPkg.cpinfo2_func = printf_color_info;
  GalUtil_DefaultPrintPkg.debug1_func  = printf_info;
  GalUtil_DefaultPrintPkg.debug2_func  = printf_info;
  GalUtil_DefaultPrintPkg.client_data = (void *) NULL;
  GalUtil_CurrentPrintPkg = &GalUtil_DefaultPrintPkg;
  user_initialized_print_package = 1;
}

GalUtil_PrintPkg *GalUtil_CreatePrintPkg(GalVPrintfFunc fatal_func,
					 GalVPrintfFunc error_func,
					 GalVPrintfFunc warn_func,
					 GalVLevelFunc level_func,
					 GalVCLevelFunc clevel_func,
					 GalVPrintfFunc pinfo1_func,
					 GalVPrintfFunc pinfo2_func,
					 GalVCPrintfFunc cpinfo1_func,
					 GalVCPrintfFunc cpinfo2_func,
					 GalVPrintfFunc debug1_func,
					 GalVPrintfFunc debug2_func, 
					 void *client_data)
{
  GalUtil_PrintPkg *new_pkg = (GalUtil_PrintPkg *) calloc(1, sizeof(GalUtil_PrintPkg));

  if (fatal_func)
    new_pkg->fatal_func = fatal_func;
  else
    new_pkg->fatal_func = printf_fatal;

  if (error_func)
    new_pkg->error_func = error_func;
  else
    new_pkg->error_func = printf_error;

  if (warn_func)
    new_pkg->warn_func  = warn_func;
  else
    new_pkg->warn_func  = printf_warn;

  if (level_func)
    new_pkg->level_func = level_func;
  else
    new_pkg->level_func = printf_level;

  if (clevel_func)
    new_pkg->clevel_func  = clevel_func;
  else
    new_pkg->clevel_func  = printf_clevel;

  if (pinfo1_func)
    new_pkg->pinfo1_func  = pinfo1_func;
  else
    new_pkg->pinfo1_func  = printf_info;

  if (pinfo2_func)
    new_pkg->pinfo2_func  = pinfo2_func;
  else
    new_pkg->pinfo2_func  = printf_info;

  if (cpinfo1_func)
    new_pkg->cpinfo1_func = cpinfo1_func;
  else
    new_pkg->cpinfo1_func = printf_color_info;

  if (cpinfo2_func)
    new_pkg->cpinfo2_func = cpinfo2_func;
  else
    new_pkg->cpinfo2_func = printf_color_info;

  if (debug1_func)
    new_pkg->debug1_func  = debug1_func;
  else
    new_pkg->debug1_func  = printf_info;

  if (debug2_func)
    new_pkg->debug2_func  = debug2_func;
  else
    new_pkg->debug2_func  = printf_info;

  new_pkg->client_data = client_data;
  
  return new_pkg;
}

int GalUtil_SetVerbose(int verbose_level)
{
  galutil_verbose = verbose_level;
  
  if (galutil_verbose < 0)
    /* galutil_verbose = 9999; */
    galutil_verbose = 3;
  
  if (!user_initialized_print_package)
#ifdef SLS_VERBOSE_DEFAULT_BW
    GalUtil_VerboseUseBW();
#else
    GalUtil_VerboseUseColor();
#endif
  return galutil_verbose;
}

int GalUtil_InitVerbose()
{
  char *valstr = getenv("GAL_VERBOSE");
  int verbose_int = -1;

  if (valstr)
    verbose_int = atoi(valstr);
  
  return GalUtil_SetVerbose(verbose_int);
}	

/*-----------------------------------------------------------------------------
 *      
 * GAL_VERBOSE FUNCTIONS WHICH SIMPLY DEREFERENCE A POINTER TO ANOTHER FUNCTION
 * 
 ----------------------------------------------------------------------------*/
/*
 *   Call this to report a fatal error and exit.
 */
static void __GalUtil_VPkgFatal(GalUtil_PrintPkg *pkg, const char *fmt, va_list args)
{
  if (GAL_VERBOSE >= GAL_FATAL_LEVEL) {
    (*pkg->fatal_func)(fmt, args, pkg->client_data);
  }

#ifndef WIN32
  kill(_gal_getpid(), SIGQUIT);
#else
  exit(-1);
#endif
}

void GalUtil_PkgFatal(GalUtil_PrintPkg *pkg, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgFatal(pkg, fmt, args);
  va_end(args);
}

void GalUtil_Fatal(const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgFatal(GalUtil_CurrentPrintPkg, fmt, args);
  va_end(args);
}

/*
 * Call this to report a fatal error and exit if the condition fails 
 * (returns 0).
 */
void GalUtil_PkgAssert(GalUtil_PrintPkg *pkg, int truth, const char *fmt, ...)
{
  va_list args;

  if (truth) 
    return;
  else {
    va_start(args, fmt);
    __GalUtil_VPkgFatal(pkg, fmt, args);
    va_end(args);
  }
}

void GalUtil_Assert(int truth, const char *fmt, ...)
{
  va_list args;

  if (truth) 
    return;
  else {
    va_start(args, fmt);
    __GalUtil_VPkgFatal(GalUtil_CurrentPrintPkg, fmt, args);
    va_end(args);
  }
}

/*
 *   Call this to report a serious error, but not to exit.
 */
static void __GalUtil_VPkgError(GalUtil_PrintPkg *pkg, const char *fmt, va_list args)
{
  if (GAL_VERBOSE >= GAL_FATAL_LEVEL) {
    (*pkg->error_func)(fmt, args, pkg->client_data);
  }
}

void GalUtil_PkgError(GalUtil_PrintPkg *pkg, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgError(pkg, fmt, args);
  va_end(args);
}

void GalUtil_Error(const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
   __GalUtil_VPkgError(GalUtil_CurrentPrintPkg, fmt, args);
  va_end(args);
}

/*
 *   Call this to report a warning.
 */

static void __GalUtil_VPkgWarn(GalUtil_PrintPkg *pkg, const char *fmt, va_list args)
{	      
  if (GAL_VERBOSE >= GAL_WARNING_LEVEL) {
    (*pkg->warn_func)(fmt, args, pkg->client_data);
  }
}

static void __GalUtil_VPkgWarnLevel(GalUtil_PrintPkg *pkg, int level,
				    const char *fmt, va_list args)
{	      
  if (GAL_VERBOSE >= level) {
    (*pkg->warn_func)(fmt, args, pkg->client_data);
  }
}

void GalUtil_PkgWarn(GalUtil_PrintPkg *pkg, const char *fmt, ...)
{	      
  va_list args;

  va_start(args, fmt);	
  __GalUtil_VPkgWarn(pkg, fmt, args);
  va_end(args);
}

void GalUtil_Warn(const char *fmt, ...)
{	      
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgWarn(GalUtil_CurrentPrintPkg, fmt, args);	
  va_end(args);
}

void GalUtil_WarnWithLocation(const char *fn, const char *fmt, ...)
{
  va_list args;
  
  va_start(args, fmt);
  if (GAL_VERBOSE >= GAL_PINFO2_LEVEL) {
    char *expanded_fmt = (char *) calloc(strlen(fmt) + strlen(fn) + 4,
					 sizeof(char));

    sprintf(expanded_fmt, "%s: %s", fn, fmt);
    __GalUtil_VPkgWarn(GalUtil_CurrentPrintPkg, expanded_fmt, args);
    free(expanded_fmt);
  } else {
    __GalUtil_VPkgWarn(GalUtil_CurrentPrintPkg, fmt, args);
  }
  va_end(args);
}

void GalUtil_WarnLevelWithLocation(int level, const char *fn, const char *fmt, ...)
{
  va_list args;
  
  va_start(args, fmt);
  if (GAL_VERBOSE >= GAL_PINFO2_LEVEL) {
    char *expanded_fmt = (char *) calloc(strlen(fmt) + strlen(fn) + 4,
					 sizeof(char));

    sprintf(expanded_fmt, "%s: %s", fn, fmt);
    __GalUtil_VPkgWarnLevel(GalUtil_CurrentPrintPkg, level,
			    expanded_fmt, args);
    free(expanded_fmt);
  } else {
    __GalUtil_VPkgWarnLevel(GalUtil_CurrentPrintPkg, level, fmt, args);
  }
  va_end(args);
}

/*
 *   Call this to print at a specified GAL_VERBOSE level
 */
void _GalUtil_VPkgPrint(GalUtil_PrintPkg *pkg, int level, const char *fmt, 
			va_list args)
{
  if (GAL_VERBOSE >= level) {
    (*pkg->level_func)(level, fmt, args, pkg->client_data);
  }
}

void GalUtil_PkgPrint(GalUtil_PrintPkg *pkg, int level, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  _GalUtil_VPkgPrint(pkg, level, fmt, args);
  va_end(args);
}

void GalUtil_Print(int level, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  _GalUtil_VPkgPrint(GalUtil_CurrentPrintPkg, level, fmt, args);
  va_end(args);
}

void GalUtil_PrintWithLocation(int level, const char *fn, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  if (GAL_VERBOSE >= GAL_PINFO2_LEVEL) {
    char *expanded_fmt = (char *) calloc(strlen(fmt) + strlen(fn) + 4,
					 sizeof(char));

    sprintf(expanded_fmt, "%s: %s", fn, fmt);
    _GalUtil_VPkgPrint(GalUtil_CurrentPrintPkg, level,
		       expanded_fmt, args);
    free(expanded_fmt);
  } else {
    _GalUtil_VPkgPrint(GalUtil_CurrentPrintPkg, level, fmt, args);
  }
  va_end(args);
}


/*
 *   Call this to print at a specified GAL_VERBOSE level
 */
void _GalUtil_VPkgCPrint(GalUtil_PrintPkg *pkg, int level, int fore, 
			 int back, const char *fmt, va_list args)
{
  if (GAL_VERBOSE >= level) {
    (*pkg->clevel_func)(level, fore, back, fmt, args, pkg->client_data);
  }
}

void GalUtil_PkgCPrint(GalUtil_PrintPkg *pkg, int level, int fore, int back, 
		       const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  _GalUtil_VPkgCPrint(pkg, level, fore, back, fmt, args);
  va_end(args);
}

void GalUtil_CPrint(int level, int fore, int back, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  _GalUtil_VPkgCPrint(GalUtil_CurrentPrintPkg, level, fore, back, fmt, args);
  va_end(args);
}

/*
 *   Call this to print simple information -- such as computing mfsc's, 
 *   loading this, etc.
 */
static void __GalUtil_VPkgPInfo1(GalUtil_PrintPkg *pkg, const char *fmt,
				 va_list args)
{
  if (GAL_VERBOSE >= GAL_PINFO1_LEVEL) {
    (*pkg->pinfo1_func)(fmt, args, pkg->client_data);
  }
}

void _GalUtil_PkgPInfo1(GalUtil_PrintPkg *pkg, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgPInfo1(pkg, fmt, args);
  va_end(args);
}

void GalUtil_PInfo1(const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgPInfo1(GalUtil_CurrentPrintPkg, fmt, args);
  va_end(args);
}

void GalUtil_PInfo1WithLocation(const char *fn, const char *fmt, ...)
{
  va_list args;
  
  va_start(args, fmt);
  if (GAL_VERBOSE >= GAL_PINFO2_LEVEL) {
    char *expanded_fmt = (char *) calloc(strlen(fmt) + strlen(fn) + 4,
					 sizeof(char));

    sprintf(expanded_fmt, "%s: %s", fn, fmt);
    __GalUtil_VPkgPInfo1(GalUtil_CurrentPrintPkg, expanded_fmt, args);
    free(expanded_fmt);
  } else {
    __GalUtil_VPkgPInfo1(GalUtil_CurrentPrintPkg, fmt, args);
  }
  va_end(args);
}


/*
 *   Call this to print simple information -- such as computing mfsc's, 
 *   loading this, etc.
 */
static void __GalUtil_VPkgPInfo2(GalUtil_PrintPkg *pkg, const char *fmt,
				 va_list args)
{
  if (GAL_VERBOSE >= GAL_PINFO2_LEVEL) {
    (*pkg->pinfo2_func)(fmt, args, pkg->client_data);
  }
}

void _GalUtil_PkgPInfo2(GalUtil_PrintPkg *pkg, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgPInfo2(pkg, fmt, args);
  va_end(args);
}

void GalUtil_PInfo2(const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgPInfo2(GalUtil_CurrentPrintPkg, fmt, args);
  va_end(args);
}

/*
 *   Call this to print simple information with colors -- such as computing 
 *   mfsc's, loading this, etc.
 */
static void __GalUtil_VPkgCPInfo1(GalUtil_PrintPkg *pkg, int fore, int back, 
				  const char *fmt, va_list args)
{
  if (GAL_VERBOSE >= GAL_PINFO1_LEVEL) {
    (*pkg->cpinfo1_func)(fore, back, fmt, args, pkg->client_data);
  }
}

void GalUtil_PkgCPInfo1(GalUtil_PrintPkg *pkg, int fore, int back, 	
			const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgCPInfo1(pkg, fore, back, fmt, args);
  va_end(args);
}

void GalUtil_CPInfo1(int fore, int back, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgCPInfo1(GalUtil_CurrentPrintPkg, fore, back, fmt, args);
  va_end(args);
}

void GalUtil_CPInfo1WithLocation(const char *fn, int fore, int back, const char *fmt, ...)
{
  va_list args;
  
  va_start(args, fmt);
  if (GAL_VERBOSE >= GAL_PINFO2_LEVEL) {
    char *expanded_fmt = (char *) calloc(strlen(fmt) + strlen(fn) + 4,
					 sizeof(char));

    sprintf(expanded_fmt, "%s: %s", fn, fmt);
    __GalUtil_VPkgCPInfo1(GalUtil_CurrentPrintPkg, fore, back,
			  expanded_fmt, args);
    free(expanded_fmt);
  } else {
    __GalUtil_VPkgCPInfo1(GalUtil_CurrentPrintPkg, fore, back,
			  fmt, args);
  }
  va_end(args);
}

/*
 *   Call this to print simple information with colors -- such as computing 
 *   mfsc's, loading this, etc.
 */
static void __GalUtil_VPkgCPInfo2(GalUtil_PrintPkg *pkg, int fore, int back, 
				  const char *fmt, va_list args)
{
  if (GAL_VERBOSE >= GAL_PINFO2_LEVEL) {
    (*pkg->cpinfo2_func)(fore, back, fmt, args, pkg->client_data);
  }
}

void GalUtil_PkgCPInfo2(GalUtil_PrintPkg *pkg, int fore, int back, 	
			const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgCPInfo2(pkg, fore, back, fmt, args);
  va_end(args);
}

void GalUtil_CPInfo2(int fore, int back, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgCPInfo2(GalUtil_CurrentPrintPkg, fore, back, fmt, args);
  va_end(args);
}

/*
 *   Call this to print simple debug information (ie, not too much output).
 */
static void __GalUtil_VPkgDebug1(GalUtil_PrintPkg *pkg, const char *fmt, 
  va_list args)
{
  if (GAL_VERBOSE >= GAL_DEBUG1_LEVEL) {
     (*pkg->debug1_func)(fmt, args, pkg->client_data);	
  }
}

void GalUtil_PkgDebug1(GalUtil_PrintPkg *pkg, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgDebug1(pkg, fmt, args);
  va_end(args);
}

void GalUtil_Debug1(const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgDebug1(GalUtil_CurrentPrintPkg, fmt, args);
  va_end(args);
}

/*
 *   Call this for more detailed debug information -- nfreq %d, ntime %d, 
 *   rate %d, etc.
 */
static void __GalUtil_VPkgDebug2(GalUtil_PrintPkg *pkg, const char *fmt, 
				 va_list args)
{
  if (GAL_VERBOSE >= GAL_DEBUG2_LEVEL) {
     (*pkg->debug2_func)(fmt, args, pkg->client_data);	
  }
}

void GalUtil_PkgDebug2(GalUtil_PrintPkg *pkg, const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgDebug2(pkg, fmt, args);
  va_end(args);
}

void GalUtil_Debug2(const char *fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  __GalUtil_VPkgDebug2(GalUtil_CurrentPrintPkg, fmt, args);
  va_end(args);
}

/*-----------------------------------------------------------------------------
 *      
 * DEFAULT FUNCTIONS WHICH GET CALLED (VIA DEREFERENCE) BY CORRESPONDING
 * GAL_VERBOSE FUNCTIONS
 * 
 ----------------------------------------------------------------------------*/

/*
 *   for GalUtil_Fatal
 */
static void printf_fatal(const char *format, va_list args, void *client_data)
{

  fprintf(stderr, "  (FATAL ERROR: ");
  vfprintf(stderr, format, args);
  fprintf(stderr, ")\n");
  fflush(stderr);
}

/*
 *   for GalUtil_Error
 */
static void printf_error(const char *format, va_list args, void *client_data)
{

  fprintf(stderr, "  (ERROR: ");
  vfprintf(stderr, format, args);
  fprintf(stderr, ")\n");
  fflush(stderr);
}

/*
 *   for GalUtil_Warn
 */
static void printf_warn(const char *format, va_list args, void *client_data)
{

  fprintf(stderr, "  (WARNING: ");
  vfprintf(stderr, format, args);
  fprintf(stderr, ")\n");
  fflush(stderr);
}

/*
 *   for GalUtil_Print
 */
static void printf_level(int level, const char *format, va_list args, 
			 void *client_data)
{

  vprintf(format, args);
  fflush(stdout);
}

/*
 *   for GalUtil_CPrint
 */
static void printf_clevel(int level, int fore, int back, const char *format, 
			  va_list args, void *client_data)
{

  vprintf(format, args);
  fflush(stdout);
}

/*
 *   for GalUtil_PInfo1, GalUtil_PInfo2, GalUtil_Debug1, GalUtil_Debug2
 */
static void printf_info(const char *format, va_list args, void *client_data)
{
  vprintf(format, args);
  fflush(stdout);
}

/*
 *   for GalUtil_CPInfo1, GalUtil_CPInfo2
 */
static void printf_color_info(int fore, int back, const char *format, 
			      va_list args, void *client_data)
{

  vprintf(format, args);
  fflush(stdout);
}

void GalUtil_fprintf(FILE *fp, const char *fmt, ...)
{ 
  va_list args;
  va_start(args, fmt);
  if(fp == stdout || fp == stderr) {
    _GalUtil_VPkgPrint(GalUtil_CurrentPrintPkg, -1, fmt, args);
  } else {
    vfprintf(fp, fmt, args);
    fflush(fp);	
  }		
  va_end(args);	
}		
