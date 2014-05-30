/*
  This file (c) Copyright 1994 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* 9/3/99: Extracted from sysdep.c by Sam Bayer (MITRE) following
   a suggestion by Lee Heatherington. */

/* This file provided by MIT to handle the absence of sleep()
   in Win32. */

#include "GC_config.h"
#include "galaxy/util.h"

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

unsigned _gal_sleep(unsigned sec)
{
  Sleep(1000*sec);
  return 0;
}

