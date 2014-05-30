/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include "gal_internal.h"

enum
{
  GAL_FILE_STREAM = 609,
  GAL_STRING_STREAM = 1920,
};

static int Gal_StringNextChar(Gal_InputStream gs);
static void Gal_StringRewind(Gal_InputStream gs, int count);

static Gal_InputFnPkg __Gal_StringInputFns = {Gal_StringNextChar, Gal_StringRewind};

static int Gal_FileNextChar(Gal_InputStream gs);
static void Gal_FileRewind(Gal_InputStream gs, int count);

static Gal_InputFnPkg __Gal_FileInputFns = {Gal_FileNextChar, Gal_FileRewind};

int Gal_InputStreamNextChar(Gal_InputStream gs)
{
  return (*gs->fn_pkg->next_char_fn)(gs);
}

void Gal_InputStreamRewind(Gal_InputStream gs, int count)
{
  (*gs->fn_pkg->rewind_fn)(gs, count);
}

/********************************************************************************
 * String stream functions for Gal_ReadToken
 ********************************************************************************/

/* The code will not change this string, but we have
   to recast it because we can't make the same guarantee for
   file pointers. */

Gal_InputStream Gal_MakeStringInputStream(const char *sp)
{
  Gal_InputStream gs = NULL;

  if (sp)
    gs = (Gal_InputStream)malloc(sizeof(GAL_INPUT_STREAM));

  if (gs)
  {
    gs->type = GAL_STRING_STREAM;
    gs->position = 0L;
    gs->stream = (void *) sp;
    gs->fn_pkg = &__Gal_StringInputFns;
  }
  return(gs);
}

static int Gal_StringNextChar(Gal_InputStream gs)
{
  if (gs && gs->type == GAL_STRING_STREAM)
  {
    unsigned char *sp;
    int next_char;

    sp = (unsigned char *)gs->stream;
    if (sp)
    {
      next_char = sp[gs->position];
      if (next_char)
	gs->position++;
      return(next_char);
    }
  }
  return(-1);
}

static void Gal_StringRewind(Gal_InputStream gs, int count)
{
  if (gs && gs->type == GAL_STRING_STREAM)
  {
    if ((count >= 0) && (gs->position > count))
      gs->position -= count;
    else
      gs->position = 0;
  }
}

char *Gal_StringInputStreamString(Gal_InputStream gs)
{
  if (gs && gs->type == GAL_STRING_STREAM)
  {
    char *sp = (char *)gs->stream;
    if (sp)
      return(sp + gs->position);
  }
  return(NULL);
}


/********************************************************************************
 * FILE stream functions for Gal_ReadToken
 ********************************************************************************/

Gal_InputStream Gal_MakeFileInputStream(FILE *fp)
{
  Gal_InputStream gs = NULL;

  if (fp)
    gs = (Gal_InputStream)malloc(sizeof(GAL_INPUT_STREAM));

  if (gs)
  {
    gs->type = GAL_FILE_STREAM;
    gs->position = ftell(fp);
    gs->stream = fp;
    gs->fn_pkg = &__Gal_FileInputFns;
  }
  return(gs);
}

/* read the next char and update the stream position */

static int Gal_FileNextChar(Gal_InputStream gs)
{
  if (gs && gs->type == GAL_FILE_STREAM && gs->position >= 0)
  {
    FILE *fp;
    int next_char;

    fp = (FILE *)gs->stream;
    next_char = fgetc(fp);
    if (next_char != EOF)
      gs->position=ftell(fp);
    return(next_char);
  }
  return(-1);
}

/* read the next line and update the stream position */

char *Gal_FileNextLine(Gal_InputStream gs, char *line, int max)
{
  if (gs && gs->type == GAL_FILE_STREAM && gs->position >= 0)
  {
    FILE *fp;
    char *res;

    fp = (FILE *)gs->stream;
    res = fgets(line, max, fp);
#ifdef WIN32
    if (res) {
      int len = strlen(line);
      if ((len > 1) && (line[len - 1] == '\n') &&
	  (line[len - 2] == '\r')) {
	/* Remove the \r. */
	line[len - 2] = '\n';
	line[len - 1] = '\0';
      }
    }
#endif
    gs->position = ftell(fp);
    return(res);
  }
  return(NULL);
}

/*  Read the rest of the current line, then reset the stream
 *  to the original position.  Returns NULL if the reset fails.
 */

/* SAM 8/5/02: On Windows, we don't want the line that's 
   retrieved to contain a \r. Because we're reading in binary
   mode, it might. */

char *Gal_FileCurrentLine(Gal_InputStream gs, char *line, int max)
{
  if (gs && gs->type == GAL_FILE_STREAM && gs->position >= 0)
  {
    FILE *fp;
    char *res;
    
    fp = (FILE *)gs->stream;
    res = fgets(line, max, fp);
#ifdef WIN32
    if (res) {
      int len = strlen(line);
      if ((len > 1) && (line[len - 1] == '\n') &&
	  (line[len - 2] == '\r')) {
	/* Remove the \r. */
	line[len - 2] = '\n';
	line[len - 1] = '\0';
      }
    }
#endif
    if (fseek(fp, gs->position, SEEK_SET))
    {
      gs->position = -1;
      return(NULL);
    }
    return(res);
  }
  return(NULL);
}

/*  Back up count chars and update stream position.
 *  Rewind to beginning of file for count < 0.
 */

static void Gal_FileRewind(Gal_InputStream gs, int count)
{
  if (gs && gs->type == GAL_FILE_STREAM && gs->position >= 0)
  {
    FILE *fp;

    fp = (FILE *)gs->stream;
    if (fp)
    {
      if ((count >= 0) && (gs->position > count))
	fseek(fp, gs->position - count, SEEK_SET);
      else
	fseek(fp, 0, SEEK_SET);
      gs->position = ftell(fp);
    }
  }
}

Gal_OutputStream Gal_MakeStringOutputStream(char *buf, int bufsize)
{
  Gal_OutputStream gs = (Gal_OutputStream) malloc(sizeof(GAL_OUTPUT_STREAM));

  if (gs) {
    gs->type = GAL_STRING_STREAM;
    gs->stream = Gal_MakeStringBuffer(buf, bufsize);
  }
  return gs;
}

void Gal_FreeStringOutputStream(Gal_OutputStream gs)
{
  if (gs->type == GAL_STRING_STREAM) {
    Gal_FreeStringBuffer((Gal_StringBuffer *) gs->stream);
    free(gs);
  }
}

/* Note carefully: no strdup() is done here. If you want to use the
   string before you free the object, do your own strdup() (unless you
   passed in a fixed buffer, in which case this function isn't needed). */

char *Gal_StringOutputStreamString(Gal_OutputStream gs)
{
  if (gs->type == GAL_STRING_STREAM) {
    return (((Gal_StringBuffer *) gs->stream)->buf);
  } else {
    return (char *) NULL;
  }
}

/* If size is -1, compute the increment size from the input string. If
   you can't expand the buffer, fail. */

int Gal_StringStreamWriteString(Gal_OutputStream gs, int increment, const char *s, ...)
{
  va_list args;
  if (gs->type != GAL_STRING_STREAM) {
    return 0;
  } else {
    int res;
    va_start(args, s);
    res = Gal_StringBufferWrite((Gal_StringBuffer *) gs->stream, increment, s, args);
    va_end(args);
    return res;
  }
}
