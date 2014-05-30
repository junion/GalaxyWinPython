/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef __SIMPLE_MAINLOOP_H__
#define __SIMPLE_MAINLOOP_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "galaxy/util.h"

typedef void (*LooperFn)(void *client_data);

struct __Looper;

typedef struct __FDCallback {
  void *client_data;
  GAL_SOCKET fd;
  LooperFn fn;
  struct __Looper *looper;
  struct __FDCallback *next;
} FDCallback;

typedef struct __TimerCallback {
  void *client_data;
  int ticks;
  int cur_ticks;
  LooperFn fn;
  struct __Looper *looper;
  struct __TimerCallback *next;
} TimerCallback;

typedef struct __Looper {
  FDCallback *fd_callback_list;
  TimerCallback *timer_callback_list;
  int done;
} Looper;

void SM_AddFDCallback(Looper *l, GAL_SOCKET fd, LooperFn fn, void *client_data);
void SM_RemoveFDCallback(Looper *l, GAL_SOCKET fd);
void SM_RemoveAllFDCallbacks(Looper *l);
int SM_NumFDCallbacks(Looper *l);
int SM_NumTimerCallbacks(Looper *l);
TimerCallback *SM_AddTimerCallback(Looper *l, int ticks,
				   LooperFn fn, void *client_data);
void SM_RemoveTimerCallback(TimerCallback *t);
void SM_RemoveAllTimerCallbacks(Looper *l);
Looper *SM_NewLooper();
void SM_Mainloop(Looper *l);
void SM_LooperExit(Looper *l);

#ifdef __cplusplus
}
#endif

#endif /* __SIMPLE_MAINLOOP_H__ */
