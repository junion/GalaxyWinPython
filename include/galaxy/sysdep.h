/*
  This file (c) Copyright 1995 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef I_GAL_SYSDEP
#define I_GAL_SYSDEP

/* SAM 8/24/99: Made modifications to symbols to negotiate potential
   symbol clashes with libsls_util. Checked to make sure that declaring
   variable types and function signatures more than once didn't
   cause a problem. Modified all #defines which correspond to a
   function definition in sysdep.c.

   sysdep.c is a little different, since it should handle all function
   calls which are "standard" but missing. So while it's possible that
   the other files might diverge, it shouldn't matter which symbols are
   being used here (the SLS versions or the GalaxyCommunicator versions).
   So while I still need to map the symbol names out of the way, so
   the libraries don't collide, I need to make sure that anyone who
   calls strerror(), for instance, gets something that corresponds to
   strerror(). In other words, the mappings need to be in this header
   file, unlike the others.

   Lee and I have decided to allow the linker to handle the
   conflicts. We've split up sysdep into individual files, one for
   each function. We'll also guaranteed that no #defines are done
   without an #undef. We intend to use almost exactly the same header
   file for both distributions.

*/

/* SAM 5/20/01: Following a recommendation by Scott Cyphers, I've
   redefined all the replacement definitions to have a unique
   prefix, and #define'd them in the headers. So there is no chance
   for a symbol conflict. Unfortunately, this means that I need to
   make sure that all the values are set one way or the other for
   all the platforms, and I would like to do that in configure,
   but that would mean that I'd have to include GC_config.h everywhere
   we reference these symbols or use sysdep.h. That isn't such
   a good idea.

   11/15/01: changed my mind. It's the only way to do it. */

/* We're going to try to use configure for everything which
   isn't on Windows. */
#include "GC_config.h"

#ifndef WIN32
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
/* bcopy is here, on most platforms */
#include <strings.h>
/* stat is here */
#include <sys/stat.h>

#define _gal_random random
#define _gal_srandom srandom
#define _gal_strerror strerror
#define _gal_strdup strdup
#define _gal_gethostname gethostname
#ifdef HAVE_STRTOK_R
#define _gal_strtok_r strtok_r
#else
char *_gal_strtok_r(char *s1, const char *s2, char **lasts);
#endif
#define _gal_bcopy bcopy
#define _gal_close close
#define _gal_fileno fileno
#define _gal_getpid getpid
#define _gal_read read
#define _gal_sleep sleep
#define _gal_stat stat
#define _gal_strcasecmp strcasecmp
#define _gal_gettimeofday(p) gettimeofday(p, NULL)

/*---------------------------------------------------------------------------
  Sun - specific 4.1.3 support removed (pre 5.7, too)
  ---------------------------------------------------------------------------*/

/*-----------------------------
  HP-UX - specific support removed
  -----------------------------*/

/*------------------------------
  DEC Alpha OSF - specific support removed
  ------------------------------*/

/*-------------------------------
  NeXT - specific support removed
  -------------------------------*/
 
/*--------------------------
  Linux
  --------------------------*/

/*----------------------------
  SGI Irix
  ----------------------------*/

#else /* ifndef WIN32 */

/*------------------------------
  Windows/Windows NT
  ------------------------------*/

/*
 * Define some compatibility macros for WIN32/NT
 */

#undef WORDS_BIGENDIAN

/* These are for printing out the file location. */

#undef str
#undef estr
#undef __FUNCTION__
#define str(x) # x
#define estr(x) str(x)
#define __FUNCTION__ __FILE__":"estr(__LINE__)

/* implemented in compat/win32 */

#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <float.h>

#include <sys/timeb.h>
#include <sys/types.h>

/** SPC Quote from MSDN Docs
 * You have to be careful when using Sleep and code that directly 
 * or indirectly creates windows. If a thread creates any windows, 
 * it must process messages. Message broadcasts are sent to all windows 
 * in the system. If you have a thread that uses Sleep with infinite 
 * delay, the system will deadlock. Two examples of code that indirectly
 * creates windows are DDE and COM CoInitialize. Therefore, if you have
 * a thread that creates windows, use MsgWaitForMultipleObjects or 
 * MsgWaitForMultipleObjectsEx, rather than Sleep.
 */

#include <io.h>
#include <process.h>
#include <winsock2.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

/* #define _gal_random random */
long _gal_random();
/* #define _gal_srandom srandom */
void _gal_srandom(int seed);
#define _gal_strerror strerror
#ifdef __STDC__
#define _gal_strdup _strdup
#define _gal_stat _stat
#define _gal_read _read
#define _gal_fileno _fileno
#define _gal_getpid _getpid
#define _gal_close _close
#else
#define _gal_strdup strdup
#define _gal_stat stat
#define _gal_read read
#define _gal_fileno fileno
#define _gal_getpid getpid
#define _gal_close close
#endif
#define _gal_gethostname gethostname
/* #define _gal_strtok_r strtok_r */
char *_gal_strtok_r(char *s1, const char *s2, char **lasts);
/* #define _gal_bcopy bcopy */
void _gal_bcopy(const void *s1, void *s2, size_t n);
/* #define _gal_gettimeofday(p) gettimeofday(p, NULL) */
int _gal_gettimeofday(struct timeval *tp);
/* #define _gal_sleep sleep */
unsigned _gal_sleep(unsigned seconds);
#define _gal_strcasecmp _stricmp

typedef unsigned char u_char;

#ifndef PURIFY
#undef purify_is_running
#undef purify_set_pool_id
#undef purify_map_pool
#define purify_is_running() GAL_FALSE
#define purify_set_pool_id(x, y)	/* stubbed out for now */
#define purify_map_pool(x, y)	/* stubbed out for now */
#endif

#endif	/* WIN32 */

#endif /* I_GAL_SYSDEP */

/* 
  for Emacs...
  Local Variables:
  mode: C
  comment-column: 50
  fill-column: 110
  c-indent-level: 2
  c-continued-statement-offset: 2
  c-brace-offset: -2
  c-argdecl-indent: 2
  c-label-offset: -2
  End:
*/

