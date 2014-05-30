/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* SAM 8/24/99: Copied and modified for open source. Removed all
   header information except for oa.c and sls_verbose.c. Added header
   mapping include file to "move the symbols out of the way."
   Changed names here in order to be able to import both these
   headers and SLS headers. */

#ifndef I_GAL_UTIL
#define I_GAL_UTIL

#include <stdio.h>
#include <stdarg.h>
#include <sys/types.h>
#ifndef WIN32
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#else
#include <winsock2.h>
#endif

#include "galaxy/socket.h"

/* Colors for cprintf, sls_cpinfo */
#define GAL_BLACK 0
#define GAL_RED 1
#define GAL_GREEN 2
#define GAL_YELLOW 3
#define GAL_BLUE 4
#define GAL_MAGENTA 5
#define GAL_CYAN 6
#define GAL_WHITE 7
#define GAL_DEFAULT -1
#define GAL_FATAL_COLOR GAL_YELLOW
#define GAL_WARNING_COLOR GAL_MAGENTA
#define GAL_ERROR_COLOR GAL_RED
#define GAL_DEBUG_COLOR GAL_GREEN
#define GAL_PINFO_COLOR GAL_BLUE
#define GAL_FATAL_LEVEL 1
#define GAL_WARNING_LEVEL 2
#define GAL_PINFO1_LEVEL 3
#define GAL_PINFO2_LEVEL 4
#define GAL_DEBUG1_LEVEL 5
#define GAL_DEBUG2_LEVEL 6

#define GAL_CONNECTION_VERBOSITY_LEVEL GAL_FATAL_LEVEL
#define GAL_ERROR_VERBOSITY_LEVEL GAL_FATAL_LEVEL
#define GAL_FATAL_VERBOSITY_LEVEL GAL_FATAL_LEVEL
    
#define GAL_WARNING_VERBOSITY_LEVEL GAL_WARNING_LEVEL

#define GAL_TRAFFIC_SUMMARY_VERBOSITY_LEVEL GAL_PINFO1_LEVEL

#define GAL_TRAFFIC_DETAILS_VERBOSITY_LEVEL GAL_PINFO2_LEVEL
#define GAL_ERROR_DETAILS_VERBOSITY_LEVEL GAL_PINFO2_LEVEL
#define GAL_FATAL_DETAILS_VERBOSITY_LEVEL GAL_PINFO2_LEVEL
#define GAL_INITIALIZATION_DETAILS_VERBOSITY_LEVEL GAL_PINFO2_LEVEL

#define GAL_THREAD_ACTIVITY_VERBOSITY_LEVEL GAL_DEBUG1_LEVEL
#define GAL_TRANSPORT_SUMMARY_VERBOSITY_LEVEL GAL_DEBUG1_LEVEL

#define GAL_TRANSPORT_DETAILS_VERBOSITY_LEVEL GAL_DEBUG2_LEVEL

#define GAL_VERBOSE ((galutil_verbose >= 0) ? galutil_verbose : GalUtil_InitVerbose())
typedef void (*GalVLevelFunc)(int level, const char *fmt, va_list args, void *client_data);
typedef void (*GalVCLevelFunc)(int level, int fore, int back, const char *fmt, va_list args, void *client_data);
typedef void (*GalVPrintfFunc)(const char *fmt, va_list args, void *client_data);
typedef void (*GalVCPrintfFunc)(int fore, int back, const char *fmt, va_list args, void *client_data);

typedef struct __GalUtil_PrintPkg {
  GalVPrintfFunc fatal_func;
  GalVPrintfFunc error_func;
  GalVPrintfFunc warn_func;
  GalVLevelFunc level_func;
  GalVCLevelFunc clevel_func;
  GalVPrintfFunc pinfo1_func;
  GalVPrintfFunc pinfo2_func;
  GalVCPrintfFunc cpinfo1_func;
  GalVCPrintfFunc cpinfo2_func;
  GalVPrintfFunc debug1_func;
  GalVPrintfFunc debug2_func; 
  void *client_data;
} GalUtil_PrintPkg;

extern GalUtil_PrintPkg *GalUtil_CurrentPrintPkg;

int GalUtil_InitVerbose();
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
					 void *client_data);
int GalUtil_SetVerbose(int verbose_level);
extern int galutil_verbose;

/* sls_verbose.c */
void  GalUtil_fprintf(FILE *fp, const char *fmt, ...);
void  GalUtil_Fatal(const char *format, ...);
void  GalUtil_Warn(const char *format, ...);
void  GalUtil_WarnWithLocation(const char *fn, const char *fmt, ...);
void  GalUtil_WarnLevelWithLocation(int level, const char *fn, const char *fmt, ...);
void  GalUtil_Error(const char *format, ...);
void  GalUtil_Print(int level, const char *format, ...);
void  GalUtil_PrintWithLocation(int level, const char *fn, const char *fmt, ...);
void  GalUtil_CPrint(int level, int fore, int back, const char *format, ...);
void  GalUtil_PInfo1(const char *format, ...);
void  GalUtil_PInfo1WithLocation(const char *fn, const char *fmt, ...);
void  GalUtil_PInfo2(const char *format, ...);
void  GalUtil_CPInfo1(int fore, int back, const char *format, ...);
void  GalUtil_CPInfo2(int fore, int back, const char *format, ...);
void  GalUtil_Debug1(const char *format, ...);
void  GalUtil_Debug2(const char *format, ...);
void  GalUtil_Assert(int truth, const char *format, ...);

void  GalUtil_PkgFatal(GalUtil_PrintPkg *pkg, const char *format, ...);
void  GalUtil_PkgWarn(GalUtil_PrintPkg *pkg, const char *format, ...);
void  GalUtil_PkgError(GalUtil_PrintPkg *pkg, const char *format, ...);
void  GalUtil_PkgPrint(GalUtil_PrintPkg *pkg, int level, const char *format, ...);
void  GalUtil_PkgCPrint(GalUtil_PrintPkg *pkg, int level, int fore, int back, const char *format, ...);
void  GalUtil_PkgPInfo1(GalUtil_PrintPkg *pkg, const char *format, ...);
void  GalUtil_PkgPInfo2(GalUtil_PrintPkg *pkg, const char *format, ...);
void  GalUtil_PkgCPInfo1(GalUtil_PrintPkg *pkg, int fore, int back, const char *format, ...);
void  GalUtil_CPInfo1WithLocation(const char *fn, int fore, int back, const char *fmt, ...);
void  GalUtil_PkgCPInfo2(GalUtil_PrintPkg *pkg, int fore, int back, const char *format, ...);
void  GalUtil_PkgDebug1(GalUtil_PrintPkg *pkg, const char *format, ...);
void  GalUtil_PkgDebug2(GalUtil_PrintPkg *pkg, const char *format, ...);
void  GalUtil_PkgAssert(GalUtil_PrintPkg *pkg, int truth, const char *format, ...);

void  GalUtil_VerboseUseBW(void);

/* sls_verbose_color.c */

void  GalUtil_VerboseUseColor(void);

/* oa.c (online arg function stuff) */
#define GAL_OA_CHAR 5
#define GAL_OA_SHORT 6
#define GAL_OA_INT 7

#define GAL_OA_STRING 8
#define GAL_OA_FLOAT 9
#define GAL_OA_DOUBLE 10

typedef struct __GalUtil_OASEntry {
  char *flag_string;
  int start_index;
  int end_index;
  struct __GalUtil_OASEntry *next;
} GalUtil_OASEntry;

typedef struct __GalUtil_OASDescription {
  char **oas;
  int num_oas;
  GalUtil_OASEntry *entries;
} GalUtil_OASDescription;

void _GalUtil_FreeOASDescription(GalUtil_OASDescription *descr);
GalUtil_OASDescription *GalUtil_OADigest(char **oas);
int GalUtil_RemoveOASArg(GalUtil_OASDescription *descr, const char *key);

int  GalUtil_OACheckUsage(int argc, char **argv, char **oas, int *first_real_arg);
void GalUtil_OAPrintUsage(int argc, char **argv, char **oas);
void GalUtil_OAPrintOptions(int argc, char **argv, char **oas, char *tag);
int  GalUtil_OASplitArgs(int argc, char **argv, char **oas, int *first_real_arg,
		   int *in_argc, char ***in_argv, int *ex_argc, char ***ex_argv);
int  GalUtil_OAExtract(int argc, char **argv, char **oas, const char *key, ...);
void GalUtil_OAExtractAsserting(int argc, char **argv, char **oas, const char *key, ...);

int  GalUtil_OAUiCheckUsage(const char *progname, int argc, char **argv, char **oas, int *first_real_arg);
void GalUtil_OAUiPrintUsage(const char *progname, int argc, char **argv, char **oas);
int  GalUtil_OAUiExtract(int argc, char **argv, char **oas, const char *key, ...);
void GalUtil_OAUiExtractAsserting(int argc, char **argv, char **oas, const char *key, ...);

int GalUtil_SAExtract(int argc, char **argv, const char *key, int num, ...);

/* mkdirp.c */
#ifdef WIN32
typedef int mode_t;
#endif

int GalUtil_Mkdirp(const char *pathname, mode_t mode);

#endif /* I_GAL_UTIL */

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

