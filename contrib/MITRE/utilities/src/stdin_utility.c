/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <time.h>
#include <stdio.h>
#include <sys/types.h>

/* SAM 7/30/02: On Windows, the way we want to do this is
   spawn a thread and let it do a blocking read. I'll enable
   this by forcing the thread stuff to be loaded on
   Windows (it's always available there). We have to do
   this before we load the GC headers. I want to use
   the GC headers because they make more sense, frankly. */

#ifdef WIN32
#ifndef GAL_THREADS
#define GAL_THREADS
#endif
#endif

#include "galaxy/sysdep.h"
#include "galaxy/galaxy_all.h"

/* This utility is a timed task which you can use to add a stdin
   poll to your server. */

#include "MITRE_galaxy.h"

#define BUF_INC 128
#define BUF_PAD 32

#define MG_DEBUG 0

#ifdef WIN32

static void __MGal_Win32TerminateStdinThread(MGal_StdinPoll *poll_struct)
{
  GalUtil_LockLocalMutex(&(poll_struct->mutex));
  SetEvent(poll_struct->termination_event);
  GalUtil_UnlockLocalMutex(&(poll_struct->mutex));
}

/* SAM 8/5/02: Here's the problem. We need to wait on two things: first, the 
   termination event, and second, input from stdin. The problem is that
   stdin isn't always a console; under the Windows equivalent of dup/fork/exec,'
   it's a pipe. So the console waiter won't work on it; it's always in a 
   signaled state. What this means is that if the input is a pipe, I want
   to do something different than if it's a console. */

enum {MG_STDIN_CONSOLE, MG_STDIN_PIPE, MG_STDIN_OTHER}; 

static void *__MGal_Win32StdinThread(void *arg)
{
  MGal_StdinPoll *poll_struct = (MGal_StdinPoll *) arg;
  char one_char_string[2];
  HANDLE both_handles[2];
  int records_read = 0;
  INPUT_RECORD irec[1];
  int res;
  int stdin_type = MG_STDIN_CONSOLE;

  both_handles[0] = poll_struct->termination_event;
  both_handles[1] = GetStdHandle(STD_INPUT_HANDLE);

  /* First, let's try to find out if it's a pipe or a console. Since
     there's no way to check to see what kind of handle we have, directly,
     first we call PeekConsoleInput, and if it's not a console input, then
     it may be file input (if we're doing a redirect). In the first case,
     we wait on both handles; in the second case, we wait with 0 on the
     termination event and peek the console; in the third case, we wait 
     with 0 on the termination event and then do our blocking read, since
     there will always be input available. */

  res = PeekConsoleInput(both_handles[1], irec, 1, &records_read);
  if (res != 0) {
    if (MG_DEBUG) {
      printf("stdin is console\n"); fflush(stdout);
    }
    stdin_type = MG_STDIN_CONSOLE;
    SetConsoleMode(both_handles[1], 
                   ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
    /* Start clean. */
    FlushConsoleInputBuffer(both_handles[1]);
  } else if (GetLastError() == ERROR_INVALID_HANDLE) {
    /* Not a console, maybe a pipe. */
    res = PeekNamedPipe(both_handles[1], NULL, 0, NULL, &records_read, NULL);
    if (res != 0) {
      if (MG_DEBUG) {
        printf("stdin is pipe\n"); fflush(stdout);
      }
      stdin_type = MG_STDIN_PIPE;
    } else {
      if (MG_DEBUG) {
        printf("stdin is probably other\n"); fflush(stdout);
      }
      stdin_type = MG_STDIN_OTHER;
    }
  } else {
    /* Wild guess, probably wrong. */
    if (MG_DEBUG) {
      printf("stdin is probably other\n"); fflush(stdout);
    }
    stdin_type = MG_STDIN_OTHER;
  }

  one_char_string[1] = '\0';
  
  while (1) {
    int done = 0;
    int input_available = 0;

    switch (stdin_type) {
    case MG_STDIN_CONSOLE:
      if (MG_DEBUG) {
        printf("About to wait on console\n"); fflush(stdout);
      }
      /* Wait for both events. */
      res = WaitForMultipleObjects(2, both_handles, FALSE, INFINITE);
      if (res == WAIT_OBJECT_0) {
        /* Index is WAIT_OBJECT_0 - WAIT_OBJECT_0, i.e., 0 */
        /* Termination. */
        done = 1;
      } else if (res == WAIT_OBJECT_0 + 1) {
        /* For reasons that I don't understand, the std handle can be 
           signaled when you click on ANOTHER window, and also for 
           some reason under the process monitor, it's always signaled. 
           So I'm going to be really crude and peek the console input, to
           see if what's queued is really a keyboard event. */
        res = PeekConsoleInput(both_handles[1], irec, 1, &records_read);
        if (res == 0) {
          done = 1;
        } else if (records_read > 0) {
          if (irec[0].EventType != KEY_EVENT) {
            if (MG_DEBUG) {
              if (irec[0].EventType == MOUSE_EVENT)
                printf("Got mouse event, ignoring\n");
              else if (irec[0].EventType == WINDOW_BUFFER_SIZE_EVENT)
                printf("Got window buffer size event, ignoring\n");
              else if (irec[0].EventType == MENU_EVENT)
                printf("Got menu event, ignoring\n");
              else if (irec[0].EventType == FOCUS_EVENT)
                printf("Got focus event, ignoring\n");
              fflush(stdout);
            }
            /* Read and discard. */
            ReadConsoleInput(both_handles[1], irec, 1, &records_read);
          } else {
            input_available = 1;
          }
        }
      } else {
        /* Something is really wrong. */
        done = 1;
      }
      break;
    case MG_STDIN_PIPE:
      /* Check for the termination, and peek the pipe. */
      if (MG_DEBUG) {
        printf("About to wait on pipe\n"); fflush(stdout);
      }
      res = WaitForSingleObject(both_handles[0], 0);
      if (res == WAIT_OBJECT_0) {
        /* Termination. */
        done = 1;
      } else if (res == WAIT_TIMEOUT) {
        /* Nothing there, so peek the pipe. */
        char buf[1];
        int bytes_available = 0;
        if (MG_DEBUG) {
          printf("Peeking the pipe\n"); fflush(stdout);
        }
        res = PeekNamedPipe(both_handles[1], buf, 1, &records_read, &bytes_available, NULL);
        if (res != 0) {
          if (records_read > 0) {
            input_available = 1;
          } else {
            /* We don't want a tight loop if there's no input. */
            Sleep(10);
          }
        } else {
          /*  Something is badly wrong. */
          done = 1;
        }
      } else if (res == WAIT_FAILED) {
        /* Something is badly wrong. */
        done = 1;
      } else {
        if (MG_DEBUG) {
          printf("Mysterious pipe peek result %d\n", res); fflush(stdout);
        }
      }
      break;
    case MG_STDIN_OTHER:
      if (MG_DEBUG) {
        printf("About to wait on other\n"); fflush(stdout);
      }
      /* Check for the termination, and otherwise assume input available. */
      res = WaitForSingleObject(both_handles[0], 0);
      if (res == WAIT_OBJECT_0) {
        /* Termination. */
        done = 1;
      } else if (res == WAIT_TIMEOUT) {
        /* Nothing there, so peek the pipe. */
        input_available = 1;
      } else if (res == WAIT_FAILED) {
        /* Something is badly wrong. */
        done = 1;
      }
      break;
    }

    if (input_available) {
      if (MG_DEBUG) {
        printf("Input is available\n"); fflush(stdout);
      }
      while (1) {
        /* SAM 8/2/02: For some bizarre reason, ReadConsole after a Wait
           mangles the first character. getc works, though. Had the same
           problem in a minimal example, also with ReadFile, and also on
           W2K. Strange. */
        /* read_res = ReadConsole(both_handles[1], one_char_string, 1, &chars_read, NULL); */
	if (MG_DEBUG) {
	  printf("Blocking read\n"); fflush(stdout);
	}
        one_char_string[0] = getc(stdin);
	
        if (one_char_string[0] == EOF) {
          /* It could happen. In that case, I want to wait until the termination
	     is signaled, because I don't want to exit this loop (and thus
	     free the pipe) until we're ready to exit. */
	  WaitForSingleObject(both_handles[0], INFINITE);
          done = 1;
          break;
        } else if (one_char_string[0] != '\r') {
	  if (MG_DEBUG) {
	    printf("Read '%c' (%d)\n", one_char_string[0], (int) one_char_string[0]);
	    fflush(stdout);
	  }
          /* Add it to the stdin pipe. */
          GalUtil_LockLocalMutex(&(poll_struct->mutex));
          Gal_StringBufferWrite(poll_struct->stdin_pipe, 1, one_char_string);
          GalUtil_UnlockLocalMutex(&(poll_struct->mutex));
          if (one_char_string[0] == '\n') {
            break;
          }
        }
      }
    }
        
    /* Check AFTER we do input_available, in case it reads EOF. */
    if (done) {
      CloseHandle(both_handles[0]);
      if (MG_DEBUG) {
	printf("Input is done\n"); fflush(stdout);
      }
      break;
    }
  }


  /* Freeing it here, rather than in the main thread,
     because then I don't need to know when this thread ends. */
  if (poll_struct->stdin_pipe)
    Gal_FreeByteBuffer(poll_struct->stdin_pipe);
  return (void *) NULL;
}
#endif

static void __MGal_FreePoll(void *data)
{
  MGal_StdinPoll *poll_struct = (MGal_StdinPoll *) data;
  Gal_FreeByteBuffer(poll_struct->input_buf); 
  free(poll_struct->prompt);
  if (poll_struct->env)
    GalSS_EnvUnlock(poll_struct->env);
  free(poll_struct);
}

static void stdin_poll(Gal_TaskPkg *p)
{
  MGal_StdinPoll *poll_struct = (MGal_StdinPoll *) Gal_TaskPkgData(p);

  switch (MGal_PollStdin(poll_struct)) {
  case 1:
    /* Successfully polled stdin */
    /* Don't reactivate! */
    /* Which means we need to remove the poll. */
    poll_struct->task = (Gal_TaskPkg *) NULL;
    break;
  case 0:
    /* No data yet. If the ms are set, refire poll. */
    if (!poll_struct->active) {
      poll_struct->active = 1;
      if (poll_struct->ms > 0) {
#ifdef WIN32
        Gal_ReAddTask(poll_struct->task,
                      (void *) poll_struct,
                      poll_struct->ms, 0, NULL);
#else
        Gal_ReAddTaskWithFileIO(poll_struct->task,
                                (void *) poll_struct,
                                poll_struct->ms, 1,
                                stdin, (FILE *) NULL, NULL);
#endif
      }
    }
    break;
  }
}

/* We have to respect blocking in here. How do we do a
   nonblocking read from stdin? Probably do a select. */

/*
  Return values:
  1 means input was found and the callback was executed.
  0 means no input was found.
*/

static int __MGal_ReadStdinLine(MGal_StdinPoll *poll_struct)
{
  /* If there's no input, return 0. Remember, this task will
     be fired a an interval whether or not there's input. */
  
#ifdef WIN32
  /* We read from the stdin pipe, and shift everything down.
     If there's no line in the input, we punt. */
  char *found_newline;
  int len_length;
  char *stdin_pipe_buf;
  
  GalUtil_LockLocalMutex(&(poll_struct->mutex));
  stdin_pipe_buf = Gal_ByteBufferBytes(poll_struct->stdin_pipe);
  if (!stdin_pipe_buf) {
    GalUtil_UnlockLocalMutex(&(poll_struct->mutex));
    return 0;
  }

  found_newline = strchr(stdin_pipe_buf, '\n');
  if (!found_newline) {
    GalUtil_UnlockLocalMutex(&(poll_struct->mutex));
    return 0;
  }

  /* Otherwise, still holding the mutex, and we know
     where the newline is. Null it out, write the string
     to the input buffer, copy the memory down, reset the bufsize,
     unlock the mutex, exit. */
  found_newline[0] = '\0';
    /* Reset the byte buffer. */
  poll_struct->input_buf->bufpos = 0;
  Gal_StringBufferWrite(poll_struct->input_buf, 0, "");
  Gal_StringBufferWriteString(poll_struct->input_buf,
                              stdin_pipe_buf);
  /* This is how many bytes we need to copy. */
  len_length = strlen(found_newline + 1);
  memmove(stdin_pipe_buf, found_newline + 1, len_length);
  stdin_pipe_buf[len_length] = '\0';
  poll_struct->stdin_pipe->bufpos = len_length;
  GalUtil_UnlockLocalMutex(&(poll_struct->mutex));
  return 1;
#else  
  unsigned int fd = (unsigned int) _gal_fileno(stdin);
  int res;
  fd_set readfd;
  struct timeval tv;
  char one_char_string[2];

  tv.tv_sec = 0;
  tv.tv_usec = 0;      
  FD_ZERO(&readfd);
  FD_SET(fd, &readfd);
  res = select(fd + 1, &readfd, (fd_set *) NULL, (fd_set *) NULL, &tv);
  if (res <= 0) {
    /* Either error or no input. */
    return 0;
  }

  /* There will be input. It will be terminated by a newline, because
     you have to do very complicated things to get stdin to respond
     immediately on a keypress. */

  /* Terminate the string array. */
  one_char_string[1] = '\0';

  /* Reset the byte buffer. */
  poll_struct->input_buf->bufpos = 0;
  Gal_StringBufferWrite(poll_struct->input_buf, 0, "");

  while (1) {
    one_char_string[0] = getc(stdin);

    if (one_char_string[0] == '\n')
      break;
    
    Gal_StringBufferWrite(poll_struct->input_buf, 1, one_char_string);
  }
  /* input_buf will be a null_terminated line of text. */
  return 1;
#endif
}

int MGal_PollStdin(MGal_StdinPoll *poll_struct)
{
  Gal_Frame fr;
  int res;

  /* Mark the task as inactive. The user may choose to
     activate it again. Active != "prompt has been fired". */
  poll_struct->active = 0;

  res = __MGal_ReadStdinLine(poll_struct);

  if (res == 0) {
    /* No input. */
    return 0;
  }

  /* We now have the input string. It may be of length 0,
     but it's a string. We'll now (possibly) send a message
     to the Hub, if the frame creation function returns a frame.
     If the user reactivates calls MGal_ActivateStdinPoll in
     the frame creation function, the poll will restart immediately.
     Otherwise, the server has to call it explicitly. */
  /* First, make sure that it's clear the prompt hasn't been fired. */
  poll_struct->prompt_fired = 0;
  fr = (*poll_struct->fn)(poll_struct->input_buf->buf, poll_struct);
  /* If the user chooses not to return a frame, nothing changes. */
  if (fr) {
    if (poll_struct->env) {
      GalSS_EnvWriteFrame(poll_struct->env, fr, 0);
    } else {
      GalIO_CommWriteFrame(poll_struct->gcomm, fr, 0);
    }
    Gal_FreeFrame(fr);
    return 1;
  } else {
    return 0;
  }
}

static MGal_StdinPoll *__MGal_CreateStdinPoll(char *prompt,
                                              GalIO_CommStruct *gcomm,
                                              GalSS_Environment *env,
                                              MGal_StdinFrameCreator fn,
                                              int ms, int activate)
{
  MGal_StdinPoll *poll_struct = (MGal_StdinPoll *) calloc(1, sizeof(MGal_StdinPoll));

  poll_struct->prompt = _gal_strdup(prompt);
  poll_struct->fn = fn;
  if (gcomm)
    poll_struct->gcomm = gcomm;
  if (env) {
    GalSS_EnvLock(env);
    poll_struct->env = env;
  }
  poll_struct->input_buf = Gal_MakeByteBuffer((char *) NULL, 0, 0,
                                              1, 1, BUF_INC, BUF_PAD);
  /* The byte buffer MAY BE A REUSE. This would mean that
     there may be an actual string in there. Better null it out. */
  Gal_StringBufferWrite(poll_struct->input_buf, 0, "");
  if (ms == 0) ms = 100;
  poll_struct->ms = ms;
  poll_struct->active = 0;
  poll_struct->poll_data = (void *) NULL;
  
#ifdef WIN32
  poll_struct->termination_event = CreateEvent(NULL, TRUE, FALSE, NULL);
        /* Set up the thread and mutex. */
        GalUtil_InitLocalMutex(&(poll_struct->mutex));
        /* Make the pipe. */
        poll_struct->stdin_pipe = Gal_MakeByteBuffer((char *) NULL, 0, 0,
                                                     1, 1, BUF_INC, BUF_PAD);
        GalUtil_ThreadCreate(&(poll_struct->thread_id),
                             __MGal_Win32StdinThread,
                             (void *) poll_struct);
#endif

  if (activate) {
    MGal_ActivateStdinPoll(poll_struct);
  }
  return poll_struct;
}

MGal_StdinPoll *MGalIO_CreateStdinPoll(char *prompt,
                                       GalIO_CommStruct *gcomm,
                                       MGal_StdinFrameCreator fn,
                                       int ms, int activate)
{
  return __MGal_CreateStdinPoll(prompt, gcomm, (GalSS_Environment *) NULL,
                                fn, ms, activate);
}

MGal_StdinPoll *MGalSS_EnvCreateStdinPoll(char *prompt,
                                          GalSS_Environment *env,
                                          MGal_StdinFrameCreator fn,
                                          int ms, int activate)
{
  return __MGal_CreateStdinPoll(prompt, (GalIO_CommStruct *) NULL, env,
                                fn, ms, activate);
}

/* The poll can need the prompt fired without having an active task. */

void MGal_ActivateStdinPoll(MGal_StdinPoll *poll_struct)
{
  if (!poll_struct->prompt_fired) {
    GalUtil_Print(-1, poll_struct->prompt);
    poll_struct->prompt_fired = 1;
  }
  if (!poll_struct->active) {
    poll_struct->active = 1;
    if (poll_struct->ms > 0) {
#ifdef WIN32
      /* No stdin poll via select on Windows. Using a thread instead. */
      if (poll_struct->task) {
        Gal_ReAddTask(poll_struct->task,
                      (void *) poll_struct,
                      poll_struct->ms, 0, NULL);
      } else {
        poll_struct->task = Gal_AddTask(stdin_poll,
                                                  (void *) poll_struct,
                                                  poll_struct->ms, 0, NULL);
      }
#else
      if (poll_struct->task) {
        Gal_ReAddTaskWithFileIO(poll_struct->task,
                                (void *) poll_struct,
                                poll_struct->ms, 1,
                                stdin, (FILE *) NULL, NULL);
      } else {
        poll_struct->task = Gal_AddTaskWithFileIO(stdin_poll,
                                                  (void *) poll_struct,
                                                  poll_struct->ms, 1,
                                                  stdin, (FILE *) NULL, NULL);
      }
#endif
    }
  }
}

void MGal_FreeStdinPoll(MGal_StdinPoll *poll_struct)
{
  if (poll_struct->task) {
    Gal_RemoveTask(poll_struct->task);
    poll_struct->task = (Gal_TaskPkg *) NULL;
#ifdef WIN32
    /* Shut down the thread. */
    __MGal_Win32TerminateStdinThread(poll_struct);
#endif
  }
  __MGal_FreePoll(poll_struct);
}

void MGal_SetStdinPollData(MGal_StdinPoll *poll_struct, void *data)
{
  poll_struct->poll_data = data;
}

void *MGal_GetStdinPollData(MGal_StdinPoll *poll_struct)
{
  return poll_struct->poll_data;
}

void MGal_SetStdinPollPrompt(MGal_StdinPoll *poll_struct, char *prompt)
{
  if (poll_struct->prompt)
    free(poll_struct->prompt);
  poll_struct->prompt_fired = 0;
  poll_struct->prompt = _gal_strdup(prompt);
}
