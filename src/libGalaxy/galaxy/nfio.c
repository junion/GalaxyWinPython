/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <ctype.h>
#include "galaxy/sysdep.h"
#include <stdarg.h>
#include "gal_internal.h"
#include "galaxy/program.h"

#define LIST_MAX 1024
#define TOKEN_MAX 1024

/*
 *  IRP form is {<type> <name> [<keyword> <value>]* [:pred <pred>]* }
 *
 *  Quirks of the IRP protocol:
 *    special characters:
 *    	{ {q {c {p	begin frame
 *    	}		end frame (each close brace must be a separate space delimited token)
 *    	:		a sym that begins with a colon is assumed to be a keyword
 *    	(		begin list
 *    	)		end list (each close parenthesis must be a separate space delimited token)
 *    	"		delimit string
 *    	\		escape within string
 *    	%		begin binary data
 *    floats are transmitted in %e format
 *    frames without names will be transmitted with the name "NO_NAME"
 *
 *  Rules for constructing frames for IRP transmission:
 *
 *    The use of syms as IRP values is deprecated.  If they are used:
 *        syms that are IRP values must begin with alphabetic characters or '$'
 *        syms that are IRP values may not contain spaces
 *    frame names may not begin with any of the special characters
 *    frame names may not contain spaces
 *    keywords may not begin with any of the special characters other than ':'
 *    maximum of 1024 characters for an int, float, or sym value
 *    maximum of 1024 characters for a keyword or frame name
 *
 *  Breaking these rules may result in transmission failures and/or seg faults
 */

static int debugnull = 0;
static int warnunknownvalue = 0;

/* SAM 9/17/99: Passing around global hash tables is very thread-unsafe.
   In order to import the tags for reading programs, we should
   either refer to the hash functions in place or copy the
   contents of the hash table. */

#include "galaxy/gthread.h"

static GalUtil_LocalMutex tag_mutex;

void _Gal_init_nfio(void)
{
  GalUtil_InitLocalMutex(&tag_mutex);
}

static Gal_HashTable TagObjectHash = NULL;

/* SAM 4/18/02: Adding or removing elements from hash tables
   is not thread-cancellation-safe, due to possible calls to
   Gal_FreeFrame. */

extern void _gal_unlock_mutex(void *mutex);

void Gal_SetTagObjectHash(Gal_HashTable ht)
{
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			    (void *) &tag_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex,
		       (void *) &tag_mutex);
#endif
  GalUtil_LockLocalMutex(&tag_mutex);

  if (TagObjectHash)
    Gal_FreeHash(TagObjectHash);
  if (ht)
    TagObjectHash = Gal_CopyHash(ht);
  else
    TagObjectHash = (Gal_HashTable) NULL;

  GalUtil_UnlockLocalMutex(&tag_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif  
}

/* SAM: This is currently not used, but in case it is, I
   copy the hash table. See also Gal_GetProgramTagHash(). */

/* 4/18/02: Not cancellation-safe. */

Gal_HashTable Gal_GetTagObjectHash(void)
{
  Gal_HashTable ht;
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			    (void *) &tag_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex,
		       (void *) &tag_mutex);
#endif
  GalUtil_LockLocalMutex(&tag_mutex);
  
  ht = Gal_CopyHash(TagObjectHash);

  GalUtil_UnlockLocalMutex(&tag_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif
  
  return ht;
}

Gal_Object Gal_GetTagObject(const char *str)
{
  Gal_Object obj;
  GalUtil_LockLocalMutex(&tag_mutex);
  obj = Gal_GetHash(str, TagObjectHash);
  GalUtil_UnlockLocalMutex(&tag_mutex);
  return obj;
}

/* 4/18/02: Not cancellation-safe. */

void Gal_SetTagObject(const char *key, Gal_Object value)
{
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPushCleanup(_gal_unlock_mutex,
			    (void *) &tag_mutex);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_push(_gal_unlock_mutex,
		       (void *) &tag_mutex);
#endif
  GalUtil_LockLocalMutex(&tag_mutex);
  
  if (TagObjectHash == NULL)
  {
    TagObjectHash = Gal_MakeHash(10000);
  }
  Gal_SetHash(key, Gal_CopyObject(value), TagObjectHash);
  
  GalUtil_UnlockLocalMutex(&tag_mutex);
#ifdef GAL_WIN32_THREADS
  GalUtil_ThreadPopCleanup(0);
#endif
#ifdef GAL_PTHREADS
  pthread_cleanup_pop(0);
#endif  
}

/*
 *  Read an object from a stream.  See string_util for next_char functions.
 *  In case of error, last (a buffer of size TOKEN_MAX) is set to the last
 *  character or token.
 */

/* SAM 9/17/99: tags can be read by Gal_ReadObject, but they're not
   part of the frame transmission protocol, and they require a mutex,
   as implemented above. We really ought to set it up so that
   Gal_ReadObject() only admits tokens when it's reading for a
   program file (Gal_ReadObjectLine() or direct call in read_program.c),
   for efficiency reasons, because mutexes are expensive.
   I'm going to change the API of Gal_ReadObject() - it looks like
   I have control over all its callers anyway. */

/* These are flags which control the behavior of the reader.
   The reader is local, as far as I'm concerned. */

static Gal_Object __Gal_ReadList(Gal_InputStream gs, int flags,
				 Gal_SpecialTokenizer special_tokenizer);
static char *__Gal_ReadString(Gal_InputStream gs);
static Gal_Object __Gal_ReadBinary(Gal_InputStream gs);
static Gal_Frame __Gal_ReadFrame(Gal_InputStream gs, int flags);

#define GAL_CONSIDER_TAGS 1
#define GAL_ALLOW_VARS 2

static Gal_Object __Gal_ReadObject(Gal_InputStream gs, char *last,
				   int flags,
				   Gal_SpecialTokenizer special_tokenizer)
{
  int next_char;
  char token[TOKEN_MAX];
  char *string;
  Gal_Frame frame;
  Gal_Object obj;
  int next_index;

  if (last)
    *last = '\0';

  while (isspace((next_char = (*gs->fn_pkg->next_char_fn)(gs))));
  if (next_char <= 0) return (NULL);

  switch(next_char) {

  case '"':
    /* read string */
    (*gs->fn_pkg->rewind_fn)(gs, 1);
    string = __Gal_ReadString(gs);
    obj = Gal_StringObject(string);
    free(string);
    return(obj);

  case '{':
    /* read frame */
    (*gs->fn_pkg->rewind_fn)(gs, 1);
    frame = __Gal_ReadFrame(gs, flags);
    return(Gal_FrameObject(frame));

  case '}':
    /* end frame */
    if (last)
      strcpy(last,"}");
    return(NULL);

  case '(':
    /* read list */
    (*gs->fn_pkg->rewind_fn)(gs, 1);
    return(__Gal_ReadList(gs, flags, special_tokenizer));

  case ')':
    /* end list */
    if (last)
      strcpy(last,")");
    return(NULL);

  default:
    /* read int, float, keyword, tag, or sym */
    (*gs->fn_pkg->rewind_fn)(gs, 1);
    if (Gal_ReadToken(gs, token, TOKEN_MAX, "}){(\"", 1) == NULL)
      return(NULL);

    next_index = -1;
    /* Now we run the special tokenizer. */
    if (special_tokenizer && (obj = (*special_tokenizer)(gs, token)))
      return obj;
    

    else if ((flags & GAL_CONSIDER_TAGS) && Gal_GetTagObject(token))
    {
      /* read tag */
      return(Gal_TagObject(token));
    }
    if (token[0] == ':')
    {
      /* read keyword */
      return(Gal_KeywordObject(token));
    }
    else if (Gal_StringEq(token, "%"))
    {
      /* read binary */
      (*gs->fn_pkg->rewind_fn)(gs, 1);
      return(__Gal_ReadBinary(gs));
    }
    else if (Gal_StringEq(token,"NULLObject") || Gal_StringEq(token,"NULLFrame"))
    {
      if (last)
	strcpy(last, token);
      if (debugnull) GalUtil_WarnWithLocation(__FUNCTION__, "Read %s", token);
      return (NULL);
    }
    else if (Gal_StringEq(token,"..."))
    {
      if (last)
	strcpy(last, token);
      GalUtil_WarnWithLocation(__FUNCTION__, "Got token '%s', reading outline frame?", token);
      return (NULL);
    }
    else if (Gal_DigitStringp(token))
    {
      return(Gal_IntObject(atoi(token)));
    }
    else if (Gal_FloatStringp(token))
    {
      return(Gal_FloatObject((float)atof(token)));
    }
    else if ((isalpha((int) token[0])) || (token[0] == '$'))
    {
      return(Gal_SymbolObject(token));
    }
    else
    {
      if (warnunknownvalue)
	GalUtil_WarnWithLocation(__FUNCTION__, "Read unknown value '%s'", token);
      return(Gal_TokenObject(token));
    }
  }
  return(NULL);
}

/*
 *  Read a GAL_FRAME from a stream
 */

static Gal_Frame __Gal_ReadFrame(Gal_InputStream gs, int flags)
{
  char token[TOKEN_MAX];
  char key[TOKEN_MAX];
  char *kp = NULL;
  char *name;
  Gal_Frame frame = NULL;
  Gal_FrameType ftype = GAL_CLAUSE;
  int done = 0;

  if (Gal_ReadToken(gs, token, TOKEN_MAX, "}", 1) == NULL)
  {
    GalUtil_WarnWithLocation(__FUNCTION__, "Failed to read frame\n");
    return(NULL);
  }

  if (strcmp(token,"{") == 0)
  {
    ftype = GAL_CLAUSE;
  }
  else if (strcmp(token,"{q") == 0)
  {
    ftype = GAL_TOPIC;
  }
  else if (strcmp(token,"{c") == 0)
  {
    ftype = GAL_CLAUSE;
  }
  else if (strcmp(token,"{p") == 0)
  {
    ftype = GAL_PRED;
  }
  else if ((strlen(token) != 2) || token[0] != '{')
  {
    GalUtil_WarnWithLocation(__FUNCTION__, "Invalid frame '%s . . .'", token);
    return(NULL);
  }
 
  if (Gal_ReadToken(gs, token, TOKEN_MAX, "}){(\"", 1) == NULL)
  {
    GalUtil_WarnWithLocation(__FUNCTION__, "Failed to read frame name");
    return(NULL);
  }

  switch (token[0]) {
  case '{': case '(': case ')': case '"': case '\\': case '%':
    GalUtil_WarnWithLocation(__FUNCTION__, "Invalid frame name '%s'", token);
    return(NULL);

  case ':':
    frame = Gal_MakeFrame(NULL, ftype);
    strcpy(key, token);
    kp = key;
    break;

  case '}':
    frame = Gal_MakeFrame(NULL, ftype);
    return(frame);

  default:
    frame = Gal_MakeFrame(token, ftype);
  }

  while(kp || Gal_ReadToken(gs, key, TOKEN_MAX, "}){(\"", 1))
  {
    Gal_Object obj;
    char last[TOKEN_MAX];

    if (kp)
      kp = NULL;

    if (key[0] == '}')
    {
      done = 1;
      break;
    }
    else if (Gal_StringEq(key, "..."))
    {
      GalUtil_WarnWithLocation(__FUNCTION__, "Got token '%s', reading outline frame?", key);
      continue;
    }
    else if (strchr("{()\"\\%", key[0]))
    {
      GalUtil_WarnWithLocation(__FUNCTION__, "Read invalid keyword '%s'", key);
      break;
    }

    obj = __Gal_ReadObject(gs, last, flags, NULL);
    if (Gal_Tokenp(obj))
    {
      GalUtil_WarnWithLocation(__FUNCTION__, "Read illegal value '%s' for key '%s'", Gal_StringValue(obj), key);
      break;
    }
    if (obj == NULL)
    {
      if (Gal_StringEq(last, "NULLObject"))
	continue;
      else if (Gal_StringEq(last, "NULLFrame"))
	continue;
      else if (Gal_StringEq(last, "..."))
	continue;
      else if (last[0] == '}')
      {
	GalUtil_WarnWithLocation(__FUNCTION__, "Read no value for key '%s'!", key);
	done = 1;
	break;
      }
      else if (last[0])
      {
	GalUtil_WarnWithLocation(__FUNCTION__, "Read illegal value '%s' for key '%s'", last, key);
	break;
      }
      else
      {
	GalUtil_WarnWithLocation(__FUNCTION__, "Read no value for key '%s'", key);
	break;
      }
    }
    else if (Gal_Keywordp(obj))
    {
      GalUtil_WarnWithLocation(__FUNCTION__, "Read no value for key '%s'", key);
      strcpy(key, Gal_KeywordValue(obj));
      Gal_FreeObject(obj);
      kp = key;
      continue;
    }
    else if (Gal_Symbolp(obj)) {
      name = Gal_KeywordValue(obj);
      if ((!name) || (strlen(name) == 0) ||
	  (name[0] != '$') || (!(flags & GAL_ALLOW_VARS))) {
	GalUtil_WarnWithLocation(__FUNCTION__, "Read unexpected symbol as value for key '%s'", key);
      }
    }

    if (strcmp(key,":pred") == 0)
      fr_add_pred(frame,obj);
    else if (strcmp(key,":PRED") == 0)
      fr_add_pred(frame,obj);
    else
      Gal_SetProp(frame,key,obj);
  }

  if (!done)
    GalUtil_WarnWithLocation(__FUNCTION__, "Frame terminated abnormally while reading");

  return(frame);
}

/*
 *  Read a GAL_STRING object from a stream
 */

static char *__Gal_ReadString(Gal_InputStream gs)
{
  char *string_buf = NULL;
  int string_max = 0;
  int string_len = 0;
  char *new_buf;
  int next_char;
  int esc = 0;
  int done = 0;

  next_char = (*gs->fn_pkg->next_char_fn)(gs);

  if (next_char != '"')
  {
    if (next_char > 0)
      GalUtil_WarnWithLocation(__FUNCTION__, "Read invalid string: does not start with \"");
    return NULL;
  }

  while (!done)
  {
    /* allocate space for string_buf */
    if (string_len == string_max)
    {
      if (string_buf)
	new_buf = (char *)realloc(string_buf, string_max + TOKEN_MAX*5);
      else
	new_buf = (char *)malloc(string_max + TOKEN_MAX*5);

      if (new_buf)
      {
	string_buf = new_buf;
	string_max += TOKEN_MAX*5;
      }
      else
      {
	GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't allocate space for string while reading");
	return(NULL);
      }
    }

    next_char = (*gs->fn_pkg->next_char_fn)(gs);

    if (next_char <= 0)
    {
      if (string_len < string_max)
	string_buf[string_len] = '\0';
      else
	string_buf[string_max - 1] = '\0';

      GalUtil_WarnWithLocation(__FUNCTION__, "String \"%s\" terminated unexpectedly while reading", string_buf);
      return string_buf;
    }
    else if (esc)
    {
      if (string_len < string_max)
	string_buf[string_len++] = next_char;
      esc = 0;
    }
    else if (next_char == '\\')
    {
      esc = 1;
    }
    else if (next_char == '"')
    {
      if (string_len < string_max)
	string_buf[string_len] = '\0';
      else
      {
	string_buf[string_max - 1] = '\0';
	GalUtil_WarnWithLocation(__FUNCTION__, "String \"%s\" truncated while reading", string_buf);
      }
      return string_buf;
    }
    else
    {
      if (string_len < string_max)
	string_buf[string_len++] = next_char;
    }
  }
  return NULL;
}      

/*
 *  Read a GAL_LIST object from a stream
 *  Note: lists preserve null objects
 */

static Gal_Object __Gal_ReadList(Gal_InputStream gs, int flags,
				 Gal_SpecialTokenizer special_tokenizer)
{
  Gal_Object *obj_array = NULL;
  Gal_Object res_list = NULL;
  int list_max = 0;
  int list_len = 0;
  int done = 0;
  int next_char;

  next_char = (*gs->fn_pkg->next_char_fn)(gs);

  if (next_char != '(')
  {
    if (next_char > 0)
      GalUtil_WarnWithLocation(__FUNCTION__, "Read invalid list: does not start with (");
    return NULL;
  }

  while (!done)
  {
    Gal_Object obj;
    char last[TOKEN_MAX];

    /* allocate space for obj_array */
    if (list_len == list_max)
    {
      Gal_Object *new_list = NULL;

      if (obj_array)
	new_list = (Gal_Object *)realloc(obj_array, (list_max + LIST_MAX) * sizeof(Gal_Object));
      else
	new_list = (Gal_Object *)malloc((list_max + LIST_MAX) * sizeof(Gal_Object));

      if (new_list)
      {
	obj_array = new_list;
	list_max += LIST_MAX;
      }
    }

    obj = __Gal_ReadObject(gs, last, flags, special_tokenizer);

    if (obj == NULL)
    {
      if (last[0] == ')')
      {
	done = 1;
	break;
      }
      else if (Gal_StringEq(last, "NULLObject"))
	obj_array[list_len++] = NULL;
      else if (Gal_StringEq(last, "NULLFrame"))
	obj_array[list_len++] = NULL;
      else if (Gal_StringEq(last, "..."))
	continue;
      else if (last[0])
      {
	GalUtil_WarnWithLocation(__FUNCTION__, "Read illegal object '%s'", last);
	break;
      }
      else
	break;
    }
    else
    {
      if (list_len < list_max)
	obj_array[list_len++] = obj;
      else
      {
	/* this will only happen if allocation fails */
	list_len++;
	Gal_FreeObject(obj);
      }
    }
  }

  if (list_len > list_max)
  {
    /* this will only happen if allocation fails */
    GalUtil_WarnWithLocation(__FUNCTION__, "List length exceeded list_max while reading (%d > %d), truncated", list_len, list_max);
    list_len = list_max;
  }

  if (!done)
    GalUtil_WarnWithLocation(__FUNCTION__, "List terminated abnormally while reading");

  res_list = Gal_ListObject(obj_array, list_len);
  free(obj_array);
  return(res_list);
}

/*
 *  Read a GAL_BINARY object from a stream
 */

static Gal_Object __Gal_ReadBinary(Gal_InputStream gs)
{
  Gal_Object obj = NULL;
  char token[TOKEN_MAX];
  char *encode_buf, *decode_buf;
  int encode_len, decode_len;
  char next_char;

  next_char = (*gs->fn_pkg->next_char_fn)(gs);

  if (next_char != '%')
  {
    if (next_char > 0)
      GalUtil_WarnWithLocation(__FUNCTION__, "Read invalid binary object: does not start with %%");
    return (NULL);
  }

  /* Read the size of the decoded data */
  Gal_ReadToken(gs, token, TOKEN_MAX, NULL, 1);
  if (!Gal_DigitStringp(token) || (decode_len = atoi(token)) < 0)
  {
    GalUtil_WarnWithLocation(__FUNCTION__, "Read invalid decode length %s for binary object", token);
    return(NULL);
  }

  /* Read the size of the encoded data */
  /* do_rewind = 0 to consume the space that marks the end of the token */
  Gal_ReadToken(gs, token, TOKEN_MAX, NULL, 0);
  if (!Gal_DigitStringp(token) || (encode_len = atoi(token)) < 0)
  {
    GalUtil_WarnWithLocation(__FUNCTION__, "Read invalid encode length %s for binary object", token);
    return(NULL);
  }

  encode_buf = malloc(encode_len + 1);

  if (encode_buf)
  {
    int i = 0;

    while(i < encode_len)
    {
      if ((next_char = (*gs->fn_pkg->next_char_fn)(gs)) > 0)
	encode_buf[i++] = next_char;
      else
      {
	free(encode_buf);
	return(NULL);
      }
    }
    encode_buf[i] = '\0';
    decode_buf = _gal_uudecode(encode_buf, decode_len);
    obj = Gal_BinaryObject(decode_buf, decode_len);
    free(decode_buf);
    free(encode_buf);
  }
  return(obj);
}

/* Main external calls */

Gal_Frame Gal_ReadFrameFromString(const char *buf)
{
  Gal_InputStream gs = Gal_MakeStringInputStream(buf);
  Gal_Frame frame = NULL;

  if (gs)
  {
    frame = __Gal_ReadFrame(gs, 0);
    free(gs);
  }
  return frame;
}

Gal_Object Gal_ReadObjectFromString(const char *buf)
{
  Gal_InputStream gs = Gal_MakeStringInputStream(buf);
  Gal_Object obj = NULL;

  if (gs)
  {
    obj = __Gal_ReadObject(gs, NULL, 0, NULL);
    free(gs);
  }
  return(obj);
}

Gal_Object _Gal_ReadObjectWithTags(Gal_InputStream gs, 
				   Gal_SpecialTokenizer tokenizer)
{
  return __Gal_ReadObject(gs, (char *) NULL, GAL_CONSIDER_TAGS, tokenizer);
}

/*
 *  Read a frame from a file.  If the next non-whitespace
 *  character is not a left curly bracket, return NULL.
 */
Gal_Frame Gal_ReadFrameFromFile(FILE *fp)
{
  Gal_InputStream gs = Gal_MakeFileInputStream(fp);
  Gal_Frame frame = NULL;
  int next_char;

  if (gs)
  {
    while ((next_char = (*gs->fn_pkg->next_char_fn)(gs)) > 0)
    {
      if (next_char == '{')
      {
	(*gs->fn_pkg->rewind_fn)(gs, 1);
	break;
      }
      else if (isspace(next_char))
	continue;
      else
      {
	(*gs->fn_pkg->rewind_fn)(gs, 1);
	free(gs);
	return NULL;
      }
    }

    if (next_char > 0)
      frame = __Gal_ReadFrame(gs, 0);

    free(gs);
  }
  return(frame);
}

/*
 *  Read the next frame from a file.  If beginning_of_line != 0
 *  the frame must begin at the beginning of a line. This allows
 *  a pretty-printed frame to be commented out with a single
 *  comment character at the beginning of the frame.
 */
Gal_Frame Gal_ReadNextFrameFromFile(FILE *fp, int bol)
{
  Gal_InputStream gs = Gal_MakeFileInputStream(fp);
  Gal_Frame frame = NULL;
  int newline = 1;
  int comment = 0;
  int next_char;

  if (gs)
  {
    while ((next_char = (*gs->fn_pkg->next_char_fn)(gs)) > 0)
    {
      if (comment)
      {
	if (next_char == '\n')
	{
	  comment = 0;
	  newline = 1;
	}
      }
      else if (next_char == '{')
      {
	if (!bol || newline)
	{
	  (*gs->fn_pkg->rewind_fn)(gs, 1);
	  break;
	}
      }
      else if (next_char == '\n')
	newline = 1;
    }

    if (next_char > 0)
      frame = __Gal_ReadFrame(gs, 0);
    free(gs);
  }
  return(frame);
}

Gal_Object Gal_ReadObjectFromFile(FILE *fp)
{
  Gal_InputStream gs = Gal_MakeFileInputStream(fp);
  Gal_Object obj = NULL;

  if (gs)
  {
    obj = __Gal_ReadObject(gs, NULL, 0, NULL);
    free(gs);
  }
  return(obj);
}

/* Variable substitution. We create a string with tokens
   as values, whose name starts with "$". Then we provide
   mappings from strings to Gal_Objects and do the substitutions.
   We'd better make sure we copy when we insert, because
   otherwise some bad things might happen. */

static Gal_VarMapping *__gal_create_var_mapping(int num_pairs, va_list args)
{
  Gal_VarMapping *map;
  int i;

  map = (Gal_VarMapping *) calloc(num_pairs + 1, sizeof(Gal_VarMapping));
  for (i = 0; i < num_pairs; i++) {
    map[i].var = va_arg(args, char *);
    map[i].value = va_arg(args, Gal_Object);
  }
  return map;
}

Gal_VarMapping *Gal_CreateVarMapping(int num_pairs, ...)
{
  va_list args;
  Gal_VarMapping *map;

  va_start(args, num_pairs);
  map = __gal_create_var_mapping(num_pairs, args);
  va_end(args);
  return map;
}

Gal_Object _Gal_FindVarMapping(char *name, Gal_VarMapping *map)
{
  int i;

  for (i = 0; map[i].var ; i++) {
    if (!strcmp(map[i].var, name)) {
      return map[i].value;
    }
  }
  return (Gal_Object) NULL;
}

typedef struct __gal_frame_and_map {
  Gal_VarMapping *map;
  Gal_Frame fr;
} __gal_frame_and_map;

void _Gal_SubstMappingsIntoFrame(Gal_Frame f, Gal_VarMapping *map);

Gal_Object _Gal_SubstMappingsIntoObject(Gal_Object value, Gal_VarMapping *map)
{
  char *name;
  Gal_Object subst_val = value;
  
  if (Gal_Symbolp(value)) {
    name = Gal_KeywordValue(value);
    if (name && (strlen(name) > 0) &&
	name[0] == '$') {
      /* We've got a possibility. */
      subst_val = Gal_CopyObject(_Gal_FindVarMapping(name, map));
    }
  } else if (Gal_Listp(value)) {
    /* SAM 5/25/02: this should really be recursive, so make
       it recursive. */
    int len, i;
    Gal_Object *objs = Gal_ListValue(value, &len);
    Gal_Object repl;
    
    for (i = 0; i < len; i++) {
      repl = _Gal_SubstMappingsIntoObject(objs[i], map);
      if (repl != objs[i]) {
	/* We're really working with the actual buffer
	   in the list object. So if we replace an element,
	   we need to see if the list object's buffer
	   has a free function. Actually, we'll just set the
	   element using a new API function. */
	Gal_SetListObject(value, i, repl);
      }
    }
  } else if (Gal_Framep(value)) {
    _Gal_SubstMappingsIntoFrame(Gal_FrameValue(value), map);
  }
  return subst_val;
}

static int __gal_subst_key_mapping(const char *key, Gal_Object value,
				   void *caller_data)
{
  __gal_frame_and_map *fnm = (__gal_frame_and_map *) caller_data;  
  Gal_Object subst_val;
  
  subst_val = _Gal_SubstMappingsIntoObject(value, fnm->map);
  if (subst_val != value) {
    Gal_SetProp(fnm->fr, key, subst_val);
  }      
  return 1;
}

static int __gal_subst_in_preds(Gal_Object pred, void *caller_data)
{
  Gal_VarMapping *map = (Gal_VarMapping *) caller_data;
  if (Gal_Framep(pred)) {
    _Gal_SubstMappingsIntoFrame(Gal_FrameValue(pred), map);
  }
  return 1;
}

void _Gal_SubstMappingsIntoFrame(Gal_Frame f, Gal_VarMapping *map)
{
  __gal_frame_and_map fnm;

  fnm.fr = f;
  fnm.map = map;
  
  Gal_DoProperties(f, __gal_subst_key_mapping, (void *) &fnm);
  Gal_DoPreds(f, __gal_subst_in_preds, (void *) map);
}

Gal_Frame Gal_ReadVarFrameFromString(const char *buf, Gal_VarMapping *map)
{
  Gal_InputStream gs = Gal_MakeStringInputStream(buf);
  Gal_Frame f = NULL;

  if (gs) {
    f = __Gal_ReadFrame(gs, GAL_ALLOW_VARS);
    free(gs);
  }

  if (!f) {
    return f;
  }

  /* Now, recurse through the frame and look for keys with values
     which are tokens which start with $, and try to find
     a replacement. */

  _Gal_SubstMappingsIntoFrame(f, map);
  return f;
}

Gal_Object Gal_ReadVarObjectFromString(const char *buf, Gal_VarMapping *map)
{
  Gal_InputStream gs = Gal_MakeStringInputStream(buf);
  Gal_Object o = NULL;

  if (gs) {
    o = __Gal_ReadObject(gs, NULL, GAL_ALLOW_VARS, NULL);
    free(gs);
  }

  if (!o) {
    return o;
  }

  /* Now, recurse through the frame and look for keys with values
     which are tokens which start with $, and try to find
     a replacement. */

  _Gal_SubstMappingsIntoObject(o, map);
  return o;
}

Gal_Frame Gal_VAReadVarFrameFromString(const char *buf, int num_pairs, ...)
{
  va_list args;
  Gal_VarMapping *map;
  Gal_Frame f;

  va_start(args, num_pairs);
  map = __gal_create_var_mapping(num_pairs, args);
  va_end(args);
  f = Gal_ReadVarFrameFromString(buf, map);
  free(map);
  return f;
}

Gal_Object Gal_VAReadVarObjectFromString(const char *buf, int num_pairs, ...)
{
  va_list args;
  Gal_VarMapping *map;
  Gal_Object o;

  va_start(args, num_pairs);
  map = __gal_create_var_mapping(num_pairs, args);
  va_end(args);
  o = Gal_ReadVarObjectFromString(buf, map);
  free(map);
  return o;
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
