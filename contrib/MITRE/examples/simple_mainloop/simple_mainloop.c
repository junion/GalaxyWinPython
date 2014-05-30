/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* Here I write a simple mainloop tool for use in demonstrating
   how Galaxy interacts appropriately with timers and file handlers. */

#include <stdlib.h>
/* Needed on MacOS X for bzero definition for FD_. */
#include <string.h>
#ifdef WIN32
#include <time.h>
#include <winsock2.h>
#else
#include <sys/time.h>
#include <unistd.h>
#endif
#include <stdio.h>
#include <sys/types.h>

#include "simple_mainloop.h"
#include "galaxy/gthread.h"

#ifndef max
#define max(a,b)  ((a)>(b) ? (a):(b))
#endif

/* File handlers */

void SM_AddFDCallback(Looper *l, GAL_SOCKET fd, LooperFn fn, void *client_data)
{
  FDCallback *new_cb = (FDCallback *) malloc(sizeof(FDCallback));

  new_cb->client_data = client_data;
  new_cb->fd = fd;
  new_cb->fn = fn;
  new_cb->looper = l;
  new_cb->next = (FDCallback *) NULL;

  if (!l->fd_callback_list) {
    l->fd_callback_list = new_cb;
  } else {
    new_cb->next = l->fd_callback_list;
    l->fd_callback_list = new_cb;
  }
}

void SM_RemoveFDCallback(Looper *l, GAL_SOCKET fd)
{
  FDCallback *cb = l->fd_callback_list;
  FDCallback *prev_cb = (FDCallback *) NULL;

  while (cb) {
    if (cb->fd == fd) {
      if (cb == l->fd_callback_list) {
	l->fd_callback_list = cb->next;
	free(cb);
      } else {
	prev_cb->next = cb->next;
	free(cb);
      }
      break;
    }
    prev_cb = cb;
    cb = cb->next;
  }
}

void SM_RemoveAllFDCallbacks(Looper *l)
{
  FDCallback *cb = l->fd_callback_list;
  FDCallback *prev_cb = (FDCallback *) NULL;

  while (cb) {
    prev_cb = cb;
    cb = cb->next;
    free(prev_cb);
  }
  l->fd_callback_list = (FDCallback *) NULL;
}

static void SM_DoFDCallbacks(Looper *l)
{
  fd_set readfd;
  int num_ready, max_fd = 0;
  static struct timeval timeout;
  FDCallback *cb;
  
  if (l->fd_callback_list) {
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;  

    /* We poll all connections. */
  
    FD_ZERO(&readfd);     /* clear read mask */

    /* Set the servers */
    cb = l->fd_callback_list;
    while (cb) {
      FD_SET(cb->fd, &readfd);
#ifndef WIN32
	  /* This parameter is ignored on Windows. */
      max_fd = max(max_fd, cb->fd);
#endif
      cb = cb->next;
    }
  
    num_ready = select(max_fd + 1, &readfd, (fd_set *) 0, (fd_set *) 0,
		       &timeout);

    /* process the results */
    if (num_ready > 0) {
      FDCallback *next_cb;
      cb = l->fd_callback_list;
      while (cb) {
	/* Just in case the cb is destroyed while
	   it's being executed. */
	next_cb = cb->next;
	if (FD_ISSET(cb->fd, &readfd)) {
	  (*(cb->fn))(cb->client_data);
	}
	cb = next_cb;
      }
    }
  } else {
    /* usleep is microseconds */
    GalUtil_MilliSleep(100);
  }
}

/* Timer handlers */

TimerCallback *SM_AddTimerCallback(Looper *l, int ticks, LooperFn fn, void *client_data)
{
  TimerCallback *new_cb = (TimerCallback *) malloc(sizeof(TimerCallback));

  new_cb->client_data = client_data;
  new_cb->ticks = ticks;
  new_cb->cur_ticks = ticks;
  new_cb->fn = fn;
  new_cb->looper = l;
  new_cb->next = (TimerCallback *) NULL;

  if (!l->timer_callback_list) {
    l->timer_callback_list = new_cb;
  } else {
    new_cb->next = l->timer_callback_list;
    l->timer_callback_list = new_cb;
  }
  return new_cb;
}

void SM_RemoveTimerCallback(TimerCallback *t)
{
  Looper *l = t->looper;
  TimerCallback *cb = l->timer_callback_list;
  TimerCallback *prev_cb = (TimerCallback *) NULL;

  while (cb) {
    if (cb == t) {
      if (cb == l->timer_callback_list) {
	l->timer_callback_list = cb->next;
	free(cb);
      } else {
	prev_cb->next = cb->next;
	free(cb);
      }
      break;
    }
    prev_cb = cb;
    cb = cb->next;
  }
}

void SM_RemoveAllTimerCallbacks(Looper *l)
{
  TimerCallback *cb = l->timer_callback_list;
  TimerCallback *prev_cb;

  while (cb) {
    prev_cb = cb;
    cb = cb->next;
    free(prev_cb);
  }
  l->timer_callback_list = (TimerCallback *) NULL;
}

static void SM_DoTimerCallbacks(Looper *l)
{
  TimerCallback *cb = l->timer_callback_list;

  while (cb) {
    cb->cur_ticks--;
    if (!cb->cur_ticks) {
      (*(cb->fn))(cb->client_data);
      cb->cur_ticks = cb->ticks;
    }
    cb = cb->next;
  }
}

/* Looper creation */

Looper *SM_NewLooper()
{
  Looper *l = (Looper *) malloc(sizeof(Looper));

  l->fd_callback_list = (FDCallback *) NULL;
  l->timer_callback_list = (TimerCallback *) NULL;
  l->done = 0;
  return l;
}

/* Main loop */

void SM_LooperExit(Looper *l)
{
  l->done = 1;
}

int SM_NumFDCallbacks(Looper *l)
{
  int i = 0;
  FDCallback *cb = l->fd_callback_list;

  while (cb) {
    i++;
    cb = cb->next;
  }
  return i;
}

int SM_NumTimerCallbacks(Looper *l)
{
  int i = 0;
  TimerCallback *cb = l->timer_callback_list;

  while (cb) {
    i++;
    cb = cb->next;
  }
  return i;
}     

void SM_Mainloop(Looper *l)
{
  /* Each time through, I either find fds to execute or sleep
     for one second. Then I go through each of the timers, and
     reduce the cur_ticks, and if the ticks reach 0, I execute
     and reset. */

  while (1) {
    SM_DoFDCallbacks(l);
    SM_DoTimerCallbacks(l);
    if (l->done) {
      break;
    }
  }
}
