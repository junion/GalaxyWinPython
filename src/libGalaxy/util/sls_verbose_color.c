/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* PURPOSE:
 *  
 *   Allows the GAL_VERBOSE facility to use color with cxterm.
 *   Provides functions to replace all default GAL_VERBOSE functions. In order
 *   to use this, simply call GalUtil_VerboseUseColor().
 *
 * AUTHOR: Ed Hurley
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "galaxy/util.h"

/* Declare the function pointers which we need to override */
extern GalUtil_PrintPkg GalUtil_DefaultPrintPkg;

/* Declare the function prototypes which should only be */
/* visible to this module */
static void color_fatal(const char *format, va_list args, void *client_data);
static void color_error(const char *format, va_list args, void *client_data);
static void color_warn(const char *format, va_list args, void *client_data);
static void color_level(int level, const char *format, va_list args, void *client_data);
static void color_clevel(int level, int fore, int back, const char *format, va_list args, void *client_data);
static void color_pinfo(const char *format, va_list args, void *client_data);
static void color_cpinfo(int fore, int back, const char *fmt, va_list args, void *client_data);

/* Assign the function pointers to functions defined below */
void GalUtil_VerboseUseColor(void)
{
  GalUtil_DefaultPrintPkg.fatal_func   = color_fatal;
  GalUtil_DefaultPrintPkg.error_func   = color_error;
  GalUtil_DefaultPrintPkg.warn_func    = color_warn;
  GalUtil_DefaultPrintPkg.level_func   = color_level;
  GalUtil_DefaultPrintPkg.clevel_func  = color_clevel;
  GalUtil_DefaultPrintPkg.pinfo1_func  = color_pinfo;
  GalUtil_DefaultPrintPkg.pinfo2_func  = color_pinfo;
  GalUtil_DefaultPrintPkg.cpinfo1_func = color_cpinfo;
  GalUtil_DefaultPrintPkg.cpinfo2_func = color_cpinfo;
  GalUtil_DefaultPrintPkg.debug1_func  = color_pinfo;
  GalUtil_DefaultPrintPkg.debug2_func  = color_pinfo;
  GalUtil_DefaultPrintPkg.client_data = (void *) NULL;
  GalUtil_CurrentPrintPkg = &GalUtil_DefaultPrintPkg;
}

/*
 *   Used to actually set colors.
 */
static void gal_verbose_set_colors(FILE* fp, int fore, int back)
{
  if (fore < 0 && back < 0)
    fprintf(fp, "\033[0m");
  else if (fore < 0)
    fprintf(fp, "\033[4%dm", back);
  else if (back < 0)
    fprintf(fp, "\033[3%dm", fore);
  else
    fprintf(fp, "\033[3%dm\033[4%dm", fore, back);
}

/*
 *   Call this if when a fatal error occurs.
 *   GalUtil_Fatal will exit.
 */
static void color_fatal(const char *format, va_list args, void *client_data)
{
  gal_verbose_set_colors(stderr, GAL_FATAL_COLOR, -1);
  fprintf(stderr, "  (FATAL ERROR: ");
  vfprintf(stderr, format, args);
  fprintf(stderr, ")\n");
  gal_verbose_set_colors(stderr, -1, -1);
  fflush(stderr);
}

/*
 *   Call this when reporting a serious error, but not exiting.
 */
static void color_error(const char *format, va_list args, void *client_data)
{
  gal_verbose_set_colors(stderr, GAL_ERROR_COLOR, -1);
  fprintf(stderr, "  (ERROR: ");
  vfprintf(stderr, format, args);
  fprintf(stderr, ")\n");
  gal_verbose_set_colors(stderr, -1, -1);
  fflush(stderr);
}

/*
 *   Call this when reporting a warning.
 */
static void color_warn(const char *format, va_list args, void *client_data)
{
  gal_verbose_set_colors(stderr, GAL_WARNING_COLOR, -1);
  fprintf(stderr, "  (WARNING: ");
  vfprintf(stderr, format, args);
  fprintf(stderr, ")\n");
  gal_verbose_set_colors(stderr, -1, -1);
  fflush(stderr);
}

/*
 *   for GalUtil_Print
 */
static void color_level(int level, const char *format, va_list args, void *client_data)
{

  switch(level)
  {
  case GAL_FATAL_LEVEL:
    gal_verbose_set_colors(stderr, GAL_FATAL_COLOR, -1);
    vfprintf(stderr, format, args);
    gal_verbose_set_colors(stderr, -1, -1);
    fflush(stderr);
    break;

  case GAL_WARNING_LEVEL:
    gal_verbose_set_colors(stderr, GAL_WARNING_COLOR, -1);
    vfprintf(stderr, format, args);
    gal_verbose_set_colors(stderr, -1, -1);
    fflush(stderr);
    break;

  case GAL_PINFO1_LEVEL: case GAL_PINFO2_LEVEL:
    gal_verbose_set_colors(stderr, GAL_PINFO_COLOR, -1);
    vfprintf(stderr, format, args);
    gal_verbose_set_colors(stderr, -1, -1);
    fflush(stderr);
    break;

  case GAL_DEBUG1_LEVEL: case GAL_DEBUG2_LEVEL:
    gal_verbose_set_colors(stderr, GAL_DEBUG_COLOR, -1);
    vfprintf(stderr, format, args);
    gal_verbose_set_colors(stderr, -1, -1);
    fflush(stderr);
    break;

  default:
    vprintf(format, args);
    fflush(stdout);
    break;
  }
}

/*
 *   for GalUtil_CPrint
 */
static void color_clevel(int level, int fore, int back, const char *format, va_list args, void *client_data)
{

  switch(level)
  {
  case GAL_FATAL_LEVEL: case GAL_WARNING_LEVEL:
    gal_verbose_set_colors(stderr, fore, back);
    vfprintf(stderr, format, args);
    gal_verbose_set_colors(stderr, -1, -1);
    fflush(stderr);
    break;

  default:
    gal_verbose_set_colors(stdout, fore, back);
    vprintf(format, args);
    gal_verbose_set_colors(stdout, -1, -1);
    fflush(stdout);
    break;
  }
}

/*
 *   for GalUtil_PInfo1, GalUtil_PInfo2, GalUtil_Debug1, GalUtil_Debug2
 */
static void color_pinfo(const char *format, va_list args, void *client_data)
{

  vprintf(format, args);
  fflush(stdout);
}

/*
 *   for GalUtil_CPInfo1, GalUtil_CPInfo2
 */
static void color_cpinfo(int fore, int back, const char *format, va_list args, void *client_data)
{
  if (fore >= 0) fore &= 7;
  if (back >= 0) back &= 7;
  gal_verbose_set_colors(stdout, fore, back);
  vprintf(format, args);
  gal_verbose_set_colors(stdout, -1, -1);
  fflush(stdout);
}

/* 
  for Emacs...
  Local Variables:
  mode: c
  fill-column: 110
  comment-column: 80
  c-tab-always-indent: nil
  c-indent-level: 2
  c-continued-statement-offset: 2
  c-brace-offset: -2
  c-argdecl-indent: 2
  c-label-offset: -2
  End:
*/
