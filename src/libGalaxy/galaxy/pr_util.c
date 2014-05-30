/*
  Portions of this file (c) Copyright 1998 - 2000 M.I.T.
  Portions of this file (c) Copyright 2002 The MITRE Corporation.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"
#include <stdio.h>

#include "gal_internal.h"

/* SAM 1/29/02: I want to clean up the outline stuff so that it
   uses exactly the same mechanisms as the other printing, and
   now I'm seeing that the warn_buf is used more locally than
   necessary. So perhaps I should package all this stuff up and
   pass just a reference to the __print_data around. Internally. */

typedef struct __print_data {
  int how_print;
  int kindent;
  Gal_StringBuffer *warn_buf;
  int failure;
  Gal_StringBuffer *buf;
  int buf_allocated;
} __print_data;

/* Function prototypes */

static int __gal_print_string(char *string, Gal_StringBuffer *buf);
static int __gal_print_object(Gal_Object to, __print_data *print_data);
static int __gal_print_list(Gal_Object *tlist, int len,
			    __print_data *print_data);
static int __gal_print_frame(Gal_Frame fr, __print_data *print_data);
static int __gal_print_binary(Gal_Object to, __print_data *print_data);

/* Utilities. */

static void __gal_initialize_print_data(__print_data *d, int how_print,
					Gal_StringBuffer *buf)
{
  d->warn_buf = (Gal_StringBuffer *) NULL;
  d->how_print = how_print;
  d->kindent = 0;
  d->failure = 0;
  if (buf) {
    d->buf = buf;
    d->buf_allocated = 0;
  } else {
    d->buf = Gal_MakeStringBuffer((char *) NULL, 0);
    d->buf_allocated = 1;
  }
}

static void __gal_free_print_data(__print_data *d)
{
  if (d->warn_buf)
    Gal_FreeStringBuffer(d->warn_buf);
  if (d->buf_allocated && d->buf)
    Gal_FreeStringBuffer(d->buf);
}

/*
 *  Gal_Frame and Gal_Object print routines
 */

/* This is a little subtle. The string buffer should be
   RETRIEVED WITHOUT COPYING; the callers of this function
   expect to continue to reuse the buffer. It must be allocated
   not from the static store, but unlike other statically
   allocated string buffers, the buffer is expandable. */

/* SAM 1/29/02: We'll convert this to use the print_data stuff
   too, but it won't be as general as the subsequent functions. */

#define BUF_INC (10 * 1024)
#define BUF_TRIGGER 1024

char *Gal_PrintFrameToString(Gal_Frame fr, char *pr_buf, int *bufsizeptr, int how)
{
  Gal_StringBuffer *buf;
  int res;
  int bufsize;
  __print_data print_data;
  
  if ((how != GAL_PP_PRINT) && (how != GAL_PR_PRINT) &&
      (how != GAL_PP_TRUNC_PRINT)) {
    return (char *) NULL;
  }

  if (bufsizeptr) {
    bufsize = *bufsizeptr;
    if (bufsize < 0) {
      bufsize = 0;
      pr_buf = (char *) NULL;
    } 
  } else {
    bufsize = 0;
    pr_buf = (char *) NULL;
  }

  buf = Gal_MakeByteBuffer(pr_buf, 0, bufsize, 0, 1, BUF_INC, BUF_TRIGGER);

  __gal_initialize_print_data(&print_data, how, buf);
  
  res = __gal_print_frame(fr, &print_data);
  
  if (res == 0) {
    if (bufsizeptr)
      *bufsizeptr = 0;
    free(buf->buf);
    pr_buf = _gal_strdup("");
  } else {
    pr_buf = buf->buf;
    if (bufsizeptr)
      *bufsizeptr = buf->bufsize;
  }
  __gal_free_print_data(&print_data);
  return(pr_buf);
}

/* Pretty print a frame to a string */

char *Gal_PPFrameToString(Gal_Frame fr, char *pr_buf, int *bufsizeptr)
{
  return Gal_PrintFrameToString(fr, pr_buf, bufsizeptr, GAL_PP_PRINT);
}

/* Print a frame to a string (convert frame to raw print rep) */

char *Gal_PrFrameToString(Gal_Frame fr, char *pr_buf, int *bufsizeptr)
{
  return Gal_PrintFrameToString(fr, pr_buf, bufsizeptr, GAL_PR_PRINT);
}

/* Print a frame to a file */

void Gal_PrintFrameToFile(Gal_Frame fr, FILE *fp, int how)
{
  __print_data print_data;

  if ((how != GAL_PP_PRINT) && (how != GAL_PR_PRINT) &&
      (how != GAL_PP_TRUNC_PRINT)) {
    return;
  }
  
  if (fp) {
    __gal_initialize_print_data(&print_data, how, (Gal_StringBuffer *) NULL);
    if (__gal_print_frame(fr, &print_data)) {
      GalUtil_fprintf(fp, "%s\n", Gal_StringBufferString(print_data.buf));
    }
    __gal_free_print_data(&print_data);
  }
}

void Gal_PrFrameToFile(Gal_Frame fr, FILE *fp)
{
  Gal_PrintFrameToFile(fr, fp, GAL_PR_PRINT);
}

/* Pretty print a frame to a file */
void Gal_PPFrameToFile(Gal_Frame fr, FILE *fp)
{
  Gal_PrintFrameToFile(fr, fp, GAL_PP_PRINT);
}

/* Print a frame to stdout */
void Gal_PrFrame(Gal_Frame fr)
{
  Gal_PrFrameToFile(fr, stdout);
}

/* Pretty print a frame to stdout */
void Gal_PPFrame(Gal_Frame fr)
{
  Gal_PPFrameToFile(fr, stdout);
}

/* Pretty print a frame using gal_verbose */
void GalUtil_PPFrame(int gal_verbose_level, Gal_Frame fr)
{
  __print_data print_data;

  if (GAL_VERBOSE >= gal_verbose_level) {
    __gal_initialize_print_data(&print_data, GAL_PP_PRINT,
				(Gal_StringBuffer *) NULL);

    if (__gal_print_frame(fr, &print_data)) {    
      GalUtil_Print(-1, "%s\n",
		    Gal_StringBufferString(print_data.buf));
    }
    __gal_free_print_data(&print_data);
  }
}

/* Pretty print a frame in color using gal_verbose */
void GalUtil_CPPFrame(int gal_verbose_level, int fore, int back, Gal_Frame fr)
{
  __print_data print_data;

  if (GAL_VERBOSE >= gal_verbose_level) {
    __gal_initialize_print_data(&print_data, GAL_PP_PRINT,
				(Gal_StringBuffer *) NULL);

    if (__gal_print_frame(fr, &print_data)) {
      GalUtil_CPrint(-1, fore, back, "%s",
		     Gal_StringBufferString(print_data.buf));
      GalUtil_Print(-1, "\n");
    }
    __gal_free_print_data(&print_data);
  }
}

/* Pretty print a frame outline */

void Gal_OutlineFrame(Gal_Frame fr, int gal_verbose_level)
{
  if (GAL_VERBOSE >= GAL_PINFO2_LEVEL) {
    Gal_PPFrame(fr);
  } else if (GAL_VERBOSE >= gal_verbose_level) {
    __print_data print_data;
    
    __gal_initialize_print_data(&print_data, GAL_PP_TRUNC_PRINT,
				(Gal_StringBuffer *) NULL);

    if (__gal_print_frame(fr, &print_data)) {
      GalUtil_Print(-1, "%s\n",
		    Gal_StringBufferString(print_data.buf));
    }
    __gal_free_print_data(&print_data);
  }
}

/* Print an object using gal_verbose */

void GalUtil_PrintObject(int gal_verbose_level, Gal_Object to, int how)
{
  __print_data print_data;

  if ((how != GAL_PP_PRINT) && (how != GAL_PR_PRINT) &&
      (how != GAL_PP_TRUNC_PRINT)) {
    return;
  }
  if (GAL_VERBOSE >= gal_verbose_level) {
    __gal_initialize_print_data(&print_data, how, (Gal_StringBuffer *) NULL);
    
    if (__gal_print_object(to, &print_data)) {
      GalUtil_Print(-1, "%s", Gal_StringBufferString(print_data.buf));
    }
    __gal_free_print_data(&print_data);
  }
}

void GalUtil_PrObject(int gal_verbose_level, Gal_Object to)
{
  GalUtil_PrintObject(gal_verbose_level, to, GAL_PR_PRINT);
}

/* Pretty print an object using gal_verbose */

void GalUtil_PPObject(int gal_verbose_level, Gal_Object to)
{
  GalUtil_PrintObject(gal_verbose_level, to, GAL_PP_PRINT);
}

/* Pretty print an object in color using gal_verbose */

void GalUtil_CPPObject(int gal_verbose_level, int fore, int back,
		       Gal_Object to)
{
  __print_data print_data;

  if (GAL_VERBOSE >= gal_verbose_level) {
    __gal_initialize_print_data(&print_data, GAL_PP_PRINT,
				(Gal_StringBuffer *) NULL);
    
    if (__gal_print_object(to, &print_data)) {
      GalUtil_CPrint(-1, fore, back, "%s",
		     Gal_StringBufferString(print_data.buf));
      GalUtil_Print(-1, "\n");
    }
    __gal_free_print_data(&print_data);
  }
}
  
/* Pretty print an object outline */

void Gal_OutlineObject(Gal_Object to, int gal_verbose_level)
{
  if (Gal_Framep(to)) {
    Gal_OutlineFrame(Gal_FrameValue(to), gal_verbose_level);
  } else {
    __print_data print_data;
    
    if (GAL_VERBOSE >= gal_verbose_level) {
      __gal_initialize_print_data(&print_data, GAL_PP_TRUNC_PRINT,
				  (Gal_StringBuffer *) NULL);
    
      if (__gal_print_object(to, &print_data)) {
	GalUtil_Print(-1, "%s\n", Gal_StringBufferString(print_data.buf));
      }
      __gal_free_print_data(&print_data);
    }
  }
}

/* returns a newly allocated string */
char *Gal_ObjectToString(Gal_Object to)
{
  __print_data print_data;

  __gal_initialize_print_data(&print_data, GAL_PR_PRINT,
			      (Gal_StringBuffer *) NULL);
  
  if (__gal_print_object(to, &print_data)) {
    char *new_buf = _gal_strdup(Gal_StringBufferString(print_data.buf));
    __gal_free_print_data(&print_data);
    return new_buf;
  } else {
    __gal_free_print_data(&print_data);
    return (char *) NULL;
  }
}

/* SAM 9/17/99: This is a problem. We don't want to copy the
   string, because this is used almost exclusively in printouts
   and it would be a memory leak. How to handle this?
   Pass the buffer in from outside, and don't copy. The
   caller is responsible for freeing the string buffer. */

char *Gal_ObjectString(Gal_Object to, Gal_StringBuffer **bufptr)
{
  Gal_StringBuffer *buf = *bufptr;
  __print_data print_data;
  int res;
  
  if (Gal_Keywordp(to) || Gal_Symbolp(to) || Gal_Tagp(to))
    return(Gal_KeywordValue(to));

  __gal_initialize_print_data(&print_data, GAL_PR_PRINT, buf);

  if (print_data.buf_allocated) {
    *bufptr = print_data.buf;
  } else {
    /* Rewind! */
    buf->bufpos = 0;
  }
  
  res = __gal_print_object(to, &print_data);

  /* "Steal" the possibly allocated buffer. */
  print_data.buf_allocated = 0;

  if (res) {
    return Gal_StringBufferString(print_data.buf);
  } else return "";
}

/* Here's another version, which is called for side effect. */

void Gal_ObjectStringToBuffer(Gal_Object to, Gal_StringBuffer *buf)
{
  __print_data print_data;
  int res;

  __gal_initialize_print_data(&print_data, GAL_PR_PRINT, buf);
  
  res = __gal_print_object(to, &print_data);

  if (res == 0) {
    Gal_StringBufferWrite(buf, -1, "[unformattable object]");
  }
}


/* Print a Gal_Object to a file */

static void __gal_pr_object_internal(Gal_Object to, FILE *fp, int how)
{
  __print_data print_data;

  __gal_initialize_print_data(&print_data, how,
			      (Gal_StringBuffer *) NULL);
  if (__gal_print_object(to, &print_data)) {
    GalUtil_fprintf(fp,"%s\n", Gal_StringBufferString(print_data.buf));
  }
  __gal_free_print_data(&print_data);
}

void Gal_PrObjectToFile(Gal_Object to, FILE *fp)
{
  __gal_pr_object_internal(to, fp, GAL_PR_PRINT);
}

/* Pretty print a Gal_Object to a file */
void Gal_PPObjectToFile(Gal_Object to, FILE *fp)
{
  __gal_pr_object_internal(to, fp, GAL_PP_PRINT);
}

/* Print a Gal_Object to stdout */
void Gal_PrObject(Gal_Object to)
{
  Gal_PrObjectToFile(to, stdout);
}

/* Pretty print a Gal_Object to stdout */
void Gal_PPObject(Gal_Object to)
{
  Gal_PPObjectToFile(to, stdout);
}

/* print a string in raw print format
   delimiter "
   escape char \
*/

static int __gal_print_string(char *string, Gal_StringBuffer *buf)
{
  int len;
  char *cp, *bp;

  if (string)
    len = strlen(string);
  else
    len = 0;

  /* First, expand the buffer. Give it an increment but no string. */
  
  if (!Gal_StringBufferWrite(buf, (len * 2) + 3, ""))
    return 0;

  /* Now, lay your hands on the actual buffer. Ugh. */
  bp = buf->buf + buf->bufpos;
  *bp++ = '"';
  buf->bufpos++;
  if (string)
  {
    cp = string;
    while (*cp)
    {
      /* escape special characters " and \ */
      if ((*cp == '"') ||
	  (*cp == '\\'))
      {
	*bp++ = '\\';
	buf->bufpos++;
      }
      *bp++ = *cp++;
      buf->bufpos++;
    }
  }
  *bp++ = '"';
  buf->bufpos++;
  *bp = '\0';
  return 1;
}

/*
 *  Gal_Object printing utilities
 */

static int __gal_print_object(Gal_Object to, __print_data *print_data)
{
  Gal_StringBuffer *buf = print_data->buf;
  int how_print = print_data->how_print;
  float *fp;
  int increment;
  int length = 0;
  Gal_Object *objs;
  GalSS_BrokerProxy *bp;
  int res;
  char *tstring;

  if (to && ((to->vtype == GAL_STRING) || (to->vtype == GAL_TOKEN)) && to->value)
    increment = (2 * strlen((char *)to->value)) + 2;
  else
    increment = 128;

  if (to == NULL) {
    return Gal_StringBufferWrite(buf, increment, "NULLObject");
  } else {
    switch (to->vtype) {
    case GAL_STRING:
      switch (how_print) {
      case GAL_PR_PRINT:
      case GAL_PP_PRINT:
	res = __gal_print_string((char *)to->value, buf);
#ifdef _GAL_DEBUG_MEMORY
	if (res && (how_print == GAL_PP_PRINT)) {
	  res = Gal_StringBufferWrite(buf, 32, " (0x0%x)", to->value);
	}
#endif
	return res;
      case GAL_PP_TRUNC_PRINT:
	if (to->value) {
	  char *s = (char *) to->value;
	  char sbuf[256];
	  
	  if (strlen(s) >= 60) {
	    s = (char *) to->value;
	    
	  } else {
	    strncpy(sbuf, s, 50);
	    strcpy(sbuf + 50, " ...");
	    s = sbuf;
	  }
	  return __gal_print_string(s, buf);
	} else {
	  return __gal_print_string("", buf);
	}
	break;
      default:
	return 0;
      }
      break;
    case GAL_INT:
      return Gal_StringBufferWrite(buf, increment, "%d", (int)to->value);
    case GAL_FLOAT:
      fp = (float *)(to->value);
      return Gal_StringBufferWrite(buf, increment, "%e", *fp);
    case GAL_SYMBOL:
      return Gal_StringBufferWrite(buf, increment, "%s", sym_name(to->value));
    case GAL_KEYWORD:
      return Gal_StringBufferWrite(buf, increment, "%s", sym_name(to->value));
    case GAL_TAG:
      return Gal_StringBufferWrite(buf, increment, "%s", sym_name(to->value));
    case GAL_TOKEN:
      return Gal_StringBufferWrite(buf, increment, "%s", (char *)to->value);
    case GAL_FRAME:
      switch (how_print) {
      case GAL_PR_PRINT:
      case GAL_PP_PRINT:
      case GAL_PP_TRUNC_PRINT:
	return __gal_print_frame((Gal_Frame) to->value, print_data);
      default:
	return 0;
      }
    case GAL_LIST:
      switch (how_print) {
      case GAL_PR_PRINT:
      case GAL_PP_PRINT:
      case GAL_PP_TRUNC_PRINT:
	objs = Gal_ListValue(to, &length);	
	return __gal_print_list(objs, length, print_data);
      default:
	return 0;
      }
    case GAL_BINARY:
      switch (how_print) {
      case GAL_PR_PRINT:
      case GAL_PP_PRINT:
	return __gal_print_binary(to, print_data);
      case GAL_PP_TRUNC_PRINT:
	return Gal_StringBufferWrite(buf, increment, 
				     "%% %d %s object",
				     Gal_ByteBufferSize((Gal_StringBuffer *) to->value),
				     Gal_ObjectTypeString(to->vtype));
      default:
	return 0;
      }
    case GAL_INT_16:
    case GAL_INT_32:
    case GAL_INT_64:
    case GAL_FLOAT_32:
    case GAL_FLOAT_64:
      switch (how_print) {
      case GAL_PR_PRINT:
	/* Encoding as a string. Tough tooties. */
	return Gal_StringBufferWrite(buf, increment, "\"[non-readable frame element: array of type %s]\"", Gal_ObjectTypeString(to->vtype));
      case GAL_PP_PRINT:
      case GAL_PP_TRUNC_PRINT:
	return Gal_StringBufferWrite(buf, increment, "\"[array: type %s, %d bytes]\"", Gal_ObjectTypeString(to->vtype), Gal_ByteBufferSize((Gal_StringBuffer *) to->value));
      default:
	return 0;
      }
    case GAL_PROXY:
      bp = (GalSS_BrokerProxy *) to->value;
      if (bp->object_type == (Gal_ObjectType) -1)
	tstring = "any";
      else
	tstring = Gal_ObjectTypeString(bp->object_type);
      /* Encoded as a non-readable frame element */
      switch (how_print) {
      case GAL_PR_PRINT:
	return Gal_StringBufferWrite(buf, increment,
				     "\"[non-readable frame element: broker proxy, call ID %s, host %s, port %d, type %s]\"",
				     bp->call_id, bp->host, bp->port,
				     tstring);
      case GAL_PP_PRINT:
      case GAL_PP_TRUNC_PRINT:
	return Gal_StringBufferWrite(buf, increment,
				     "\"[broker proxy: call ID %s, host %s, port %d, type %s]\"",
				     bp->call_id, bp->host, bp->port,
				     tstring);
      default:
	return 0;
      }
    default:
      GalUtil_WarnWithLocation(__FUNCTION__, "Cannot print Gal_Object of type %s", Gal_ObjectTypeString(to->vtype));
      return Gal_StringBufferWrite(buf, increment, "NULLObject");
    }
  }
}

/*
 *  List printing utilities
 */

/* Print a list to a string */

static int __gal_print_list(Gal_Object *tlist, int len, __print_data *print_data)
{
  Gal_StringBuffer *buf = print_data->buf;
  int how_print = print_data->how_print;
  int indent = print_data->kindent;
  int i, lindent;

  if (!Gal_StringBufferWrite(buf, -1, "( "))
    return 0;
  
  if (len && tlist)
  {
    switch (how_print) {
    case GAL_PP_PRINT:
    case GAL_PP_TRUNC_PRINT:
      lindent = indent + 2;
      break;
    default:
      lindent = indent;
      break;
    }
    
    for (i=0; i<len; i++)
    {
      if (i > 0) {
	switch (how_print) {
	case GAL_PP_PRINT:
	case GAL_PP_TRUNC_PRINT:
	  /* -*s means pad the string on the right in the specified field width */
	  if (!Gal_StringBufferWrite(buf, lindent + 1, "\n%-*s", lindent, " "))
	    return 0;
	  break; 
	case GAL_PR_PRINT:
	  if (!Gal_StringBufferWrite(buf, 1, " "))
	    return 0;
	  break;
	default:
	  break;
	}
	if (how_print == GAL_PP_TRUNC_PRINT) {
	  /* If we have more than 1 element in the list,
	     print ellipses and exit for the truncated version. */
	  return Gal_StringBufferWrite(buf, 5, "... )");
	}
      }
      print_data->kindent = lindent;
      if (!__gal_print_object(tlist[i], print_data)) {
	print_data->kindent = indent;
	return 0;
      } else {
	print_data->kindent = indent;
      }
    }
  }
  return Gal_StringBufferWrite(buf, 2, " )");
}

/*
 *  Frame printing utilities
 */


/* Print a frame to a string */
/* Pretty print a frame to a string */

static int __gal_print_properties(const char *key, Gal_Object value,
				  void *caller_data)
{
  __print_data *print_data = (__print_data *) caller_data;
  int vindent;
  int kindent = print_data->kindent;
  int how_print = print_data->how_print;
  Gal_StringBuffer *buf = print_data->buf;
  
  switch (how_print) {
  case GAL_PP_PRINT:
  case GAL_PP_TRUNC_PRINT:
    vindent = kindent + strlen(key) + 1;
    break;
  default:
    vindent = kindent;
    break;
  }

  /* SAM 1/29/02: For some reason, the truncated stuff
     doesn't check this case. */
  
  if (how_print != GAL_PP_TRUNC_PRINT) {
    /* some objects are not allowed as values */
    if (Gal_Keywordp(value) || Gal_Tagp(value) ||
	Gal_Tokenp(value) || Gal_Pointerp(value)) {
      GalUtil_WarnWithLocation(__FUNCTION__, "Found illegal object type %s (%s) for key %s while printing frame properties",
		   Gal_ObjectTypeString(value->vtype),
		   Gal_ObjectString(value, &print_data->warn_buf), key);
      return 1;
    }
  }

  switch (how_print) {
  case GAL_PP_PRINT:
  case GAL_PP_TRUNC_PRINT:
    /* -*s means pad the string on the right in the specified field width */
    if (!Gal_StringBufferWrite(buf, kindent + strlen(key) + 2,
			       "\n%-*s%s ", kindent, " ", key)) {
      print_data->failure = 1;
      return 0;
    }
    break;
  case GAL_PR_PRINT:
    if (!Gal_StringBufferWrite(buf, strlen(key) + 2, " %s ", key)) {
      print_data->failure = 1;
      return 0;
    }
    break;
  default:
    break;
  }

  print_data->kindent = vindent;
  if (!__gal_print_object(value, print_data)) {
    print_data->kindent = kindent;
    print_data->failure = 1;
    return 0;
  }
  print_data->kindent = kindent;
  return 1;
}

static int __gal_print_predicates(Gal_Object pred,
				      void *caller_data)
{
  return __gal_print_properties(":pred", pred, caller_data);
}

static int __gal_print_frame(Gal_Frame fr, __print_data *print_data)
{
  Gal_StringBuffer *buf = print_data->buf;
  int how_print = print_data->how_print;
  int indent = print_data->kindent;
  int kindent;
  char *ftype;
  char *name = (char *) NULL;

  /* print frame type; how_print doesn't matter */
  if (fr == NULL) {
    return Gal_StringBufferWrite(buf, -1, "NULLFrame");
  }
  if (fr == (Gal_Frame)-1) {
    return Gal_StringBufferWrite(buf, -1, "BadFrame");
  }

  switch (fr->ftype) {
  case GAL_TOPIC:
    ftype = "{q ";
    break;
  case GAL_CLAUSE:
    ftype = "{c ";
    break;
  case GAL_PRED:
    ftype = "{p ";
    break;
  default:
    ftype = "{  ";
    break;
  }
  if (!Gal_StringBufferWrite(buf, 3, ftype))
    return 0;

  /* print frame name */
  if (fr->name != NULL)
    name = sym_name(fr->name);
  else 
    name = "NO_NAME";

  if (!Gal_StringBufferWriteString(buf, name))
    return 0;

  if ((how_print == GAL_PP_TRUNC_PRINT) && (indent > 0)) {
    return Gal_StringBufferWrite(buf, 6, " ... }");
  } else {  
    /* print key value pairs */
    switch (how_print) {
    case GAL_PP_PRINT:
    case GAL_PP_TRUNC_PRINT:
      kindent = indent + 3;
      break;
    default:
      kindent = indent;
      break;
    }

    print_data->failure = 0;
    print_data->kindent = kindent;
  
    Gal_DoProperties(fr, __gal_print_properties, (void *) print_data);
    if (print_data->failure) {
      print_data->kindent = indent;
      return 0;
    }

    print_data->failure = 0;
  
    Gal_DoPreds(fr, __gal_print_predicates, (void *) print_data);
  
    print_data->kindent = indent;
    if (print_data->failure) {
      return 0;
    }
    return Gal_StringBufferWrite(buf, 2, " }");
  }
}

/*
 *  Binary printing utilities
 */

/* Print a binary object to a string */
/* Pretty print a binary object to a string */

static int __gal_print_binary(Gal_Object to, __print_data *print_data)
{
  Gal_StringBuffer *buf = print_data->buf;
  int how_print = print_data->how_print;
  char *fmt_string, *data;
  int datalen, increment, extra, res;

  switch (how_print) {
  case GAL_PP_PRINT:
    fmt_string = "%% %d %d %s\n";
    extra = 1;
    break;
  default:
    fmt_string = "%% %d %d %s";
    extra = 0;
    break;
  }

  data = _gal_uuencode(Gal_ByteBufferBytes((Gal_StringBuffer *) to->value),
		       Gal_ByteBufferSize((Gal_StringBuffer *) to->value));
  if (data) {
    datalen = strlen(data);
  } else {
    datalen = 0;
    data = "";
  }
  /* data plus spaces plus % + two integers + optional newline */
  increment = datalen + 4 + 64 + extra;
  res = Gal_StringBufferWrite(buf, increment, fmt_string, Gal_ByteBufferSize((Gal_StringBuffer *) to->value), datalen, data);
  if (datalen) free(data);
  return res;
}

/* 
 *  for Emacs...
 *  Local Variables:
 *  mode: c
 *  fill-column: 110
 *  comment-column: 80
 *  c-indent-level: 2
 *  c-continued-statement-offset: 2
 *  c-brace-offset: -2
 *  c-argdecl-indent: 2
 *  c-label-offset: -2
 *  End:
 */
