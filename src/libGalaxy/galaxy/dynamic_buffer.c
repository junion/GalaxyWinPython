/*
  This file (c) Copyright 2000 The MITRE Corporation.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/sysdep.h"
#include <stdarg.h>
#include <stdio.h>

#include "galaxy/gthread.h"

#include "gal_internal.h"

/* SAM 8/22/00: It finally occurred to me that I could use
   the string buffer structure as an expandable array for
   GAL_BINARY data (and other stuff as well), and that it would
   be useful to have a parallel structure for arrays of
   pointers. Again, the allocation code is essentially stolen
   from the Gal_Object allocation code. */

/* So I've consolidated the string buffer and pointer buffer code. */

/* Mutexes. */

GalUtil_LocalMutex byte_buffer_mutex;
GalUtil_LocalMutex pointer_buffer_mutex;

#define CHUNK 100

static _Gal_LocalMemory *__Gal_BufferMemory = (_Gal_LocalMemory *) NULL;
static _Gal_LocalMemory *__Gal_PointerMemory = (_Gal_LocalMemory *) NULL;

void _Gal_init_stream_util(void)
{
  GalUtil_InitLocalMutex(&byte_buffer_mutex);
  GalUtil_InitLocalMutex(&pointer_buffer_mutex);
  __Gal_BufferMemory = _Gal_LMCreate(sizeof(Gal_StringBuffer), CHUNK);
  __Gal_PointerMemory = _Gal_LMCreate(sizeof(Gal_PointerBuffer), CHUNK);
}

/*
 *   BYTE BUFFERS
 */

/* Because the objects are calloc'ed, everything will be 0 in the beginning. */

static Gal_StringBuffer *alloc_byte_buffer()
{
  Gal_StringBuffer *s;

  GalUtil_LockLocalMutex(&byte_buffer_mutex);
  if (!__Gal_BufferMemory) {
    GalUtil_UnlockLocalMutex(&byte_buffer_mutex);
    GalUtil_Fatal("String buffer memory not initialized; call Gal_InitializeStatics()");
  }

  s = (Gal_StringBuffer *) _Gal_LMAllocate(__Gal_BufferMemory, (int *) NULL);
  GalUtil_UnlockLocalMutex(&byte_buffer_mutex);
  
  if (s == NULL) {
    GalUtil_WarnWithLocation(__FUNCTION__, "String buffer allocation error\n");
    return(NULL);
  }
  /* Only dynamic managed memory is allocated from the store. */
  s->flags = GAL_MANAGE_MEMORY | GAL_DYNAMIC | GAL_FROM_STORE;
  return(s);
}

static void dealloc_byte_buffer(Gal_StringBuffer *b)
{
  if (b) {
    _Gal_LMDeallocate(__Gal_BufferMemory, (void *) b);
  }
}

static void __clear_string_buffer(void *elt)
{
  Gal_StringBuffer *buf = (Gal_StringBuffer *) elt;

  if (buf->buf) {
    free(buf->buf);
    buf->buf = (char *) NULL;
  }
}

int _Gal_FreeAllByteBuffers()
{
  int success = 0;
  
  GalUtil_LockLocalMutex(&byte_buffer_mutex);
  if (__Gal_BufferMemory) {
    int active_elements = _Gal_LMFree(__Gal_BufferMemory, __clear_string_buffer, 1);

    if (active_elements > 0) {
      GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't free string buffer repository; %d left", active_elements);
    } else {
      __Gal_BufferMemory = (_Gal_LocalMemory *) NULL;
      success = 1;
    }
  }
  GalUtil_UnlockLocalMutex(&byte_buffer_mutex);
  return success;
}

/* General function for making a byte buffer. If it's a
   fixed buffer, I'm going to allocate a fresh one, which
   will be freed. If manage_memory is 1, then the byte
   buffer will take care of managing the memory; if it's
   dynamic, it will allocate and reallocate its own
   internal memory, and if it's static, it will copy
   the buffer. If buf is non-NULL, then it will either
   be the contents of the byte buffer (if manage_memory is 0),
   or it will be copied into the byte buffer (if manage_memory
   is 1). Dynamic memory_managed buffers will be
   allocated from the store; others will be malloc'ed
   and freed. */

/* SAM 11/3/00: Got this wrong the first time I did it.
   manage_memory didn't mean the right thing: it was
   copying the memory, when it really meant 
   that the string buffer should treat the memory as if
   it were its own. Here's the right thing to do:

   dynamic & manage_memory: if no buffer, allocate from the
   store. If a buffer is passed in, use it. The user will
   need to retrieve the data before the string buffer
   is destroyed.

   dynamic & !manage_memory: don't allocate from the store.
   Use as a shell for expansion. The caller will need to
   retrieve the memory before the string buffer is destroyed.

   !dynamic & manage_memory: treat it as yours. Don't
   allocate from the store. free when done.

   !dynamic & !manage_memory: don't allocate from the
   store, don't free. */

/* One final bit of sorcery. Since I'm going to use these
   for shorts, ints and floats, too, we probably ought to
   have a "chunking factor", so when you write something to
   the byte buffer, it automatically multiplies. Cool. */

#define BUF_INC (10 * 1024)
#define BUF_TRIGGER 1024

/* Internally, this will always think of things
   in terms of bytes. */

Gal_StringBuffer *Gal_MakeDataBuffer(char *buf,
				     int bufpos,
				     int bufsize,
				     int manage_memory,
				     int dynamic,
				     int increment,
				     int padding,
				     int chunk_size)
{
  Gal_StringBuffer *temp;

  /* If there's no buf, go to the store if you can. */
  if (dynamic && manage_memory && !buf) {
    temp = alloc_byte_buffer();
  } else {
    temp = (Gal_StringBuffer *) malloc(sizeof(Gal_StringBuffer));
    temp->bufsize = 0;
    if (dynamic && manage_memory)
      temp->flags = GAL_DYNAMIC | GAL_MANAGE_MEMORY;
    else if (manage_memory)
      temp->flags = GAL_MANAGE_MEMORY;
    else if (dynamic)
      temp->flags = GAL_DYNAMIC;
    temp->buf = (char *) NULL;
  }
  temp->chunk_size = chunk_size;
  temp->bufpos = 0;
  temp->increment = increment;
  if (padding < 0) padding = 0;
  temp->padding = padding;

  if (buf) {
    /* If there's a buffer, no matter what it's
       intended to be used for, set things up. */
    temp->buf = buf;
    temp->bufsize = bufsize;
    temp->bufpos = bufpos * chunk_size;
  }

  return temp;
}

Gal_StringBuffer *Gal_MakeByteBuffer(char *buf,
				     int bufpos,
				     int bufsize,
				     int manage_memory,
				     int dynamic,
				     int increment,
				     int padding)
{
  return Gal_MakeDataBuffer(buf, bufpos, bufsize,
			    manage_memory, dynamic,
			    increment, padding, 1);
}

/* When we deallocate a byte buffer, we either return it
   to the store, with its allocated buffer intact, or
   we free it, and its buffer if we were managing the memory.
   We might end up putting in the store some elements which
   didn't originate in the store. */

void Gal_FreeByteBuffer(Gal_StringBuffer *buf)
{
  if (buf->flags & GAL_FROM_STORE) {
    dealloc_byte_buffer(buf);
  } else {
    if (buf->flags & GAL_MANAGE_MEMORY) {
      if (buf->buf)
	free(buf->buf);
    }
    free(buf);
  }
}

/* increment is the number of chunks. */

int _Gal_ExpandDataBuffer(Gal_StringBuffer *b, int increment)
{
  int new_size = b->bufpos + (increment * b->chunk_size);
  int buf_avail = b->bufsize - new_size;
  int bufsize = b->bufsize;
  int expand = 0;
    
  while (buf_avail < b->padding) {
    bufsize += b->increment;
    buf_avail = bufsize - new_size;
    expand = 1;
  }

  if (!expand) {
    /* there's enough room */
    return 1;
  } else if (!(b->flags & GAL_DYNAMIC)) {
    /* Can't expand, fail */
    return 0;
  } else if (b->buf) {
    /* Need to realloc */
    b->buf = (char *) realloc(b->buf, bufsize * sizeof(char));
    if (b->buf) {
      b->bufsize = bufsize;
      return 1;
    } else {
      b->bufsize = 0;
      return 0;
    }
  } else {
    /* Need to malloc */
    b->buf = (char *) calloc(bufsize, sizeof(char));
    if (b->buf) {
      b->bufsize = bufsize;
      return 1;
    } else {
      b->bufsize = 0;
      return 0;
    }
  }
}

int _Gal_ExpandByteBuffer(Gal_StringBuffer *buf, int increment)
{
  /* Increment is the number of bytes. */
  return _Gal_ExpandDataBuffer(buf, increment);
}

int Gal_ByteBufferDynamic(Gal_StringBuffer *b)
{
  return (b && (b->flags & GAL_DYNAMIC));
}

char *Gal_DataBufferData(Gal_StringBuffer *buf)
{
  return buf->buf;
}

char *Gal_ByteBufferBytes(Gal_StringBuffer *buf)
{
  return buf->buf;
}

/* This is the ACTUAL NUMBER OF ELEMENTS, not the
   size of the internal buffer. */

int Gal_DataBufferSize(Gal_StringBuffer *buf)
{
  return (buf->bufpos / buf->chunk_size);
}

int Gal_DataBufferByteCount(Gal_StringBuffer *buf)
{
  return buf->bufpos;
}

int Gal_ByteBufferSize(Gal_StringBuffer *buf)
{
  return Gal_DataBufferSize(buf);
}

/* Appending to the buffer. */

/* size is in chunks. */

int Gal_DataBufferAppend(Gal_StringBuffer *b, const char *s, int size)
{
  if (_Gal_ExpandByteBuffer(b, size)) {
    memcpy(b->buf + b->bufpos, s, (size * b->chunk_size));
    b->bufpos += (size * b->chunk_size);
    return 1;
  } else {
    return 0;
  }
}

int Gal_ByteBufferAppend(Gal_StringBuffer *b, const char *s, int size)
{
  return Gal_DataBufferAppend(b, s, size);
}

/*
 *   STRING BUFFERS
 */   

/* If buf is not NULL and buflen is not 0, then we mark the buffer as fixed
   and don't permit expansion. Otherwise, we dynamically expand when
   we need more space. We'll steal code from pr_util.c to do this. */

Gal_StringBuffer *Gal_MakeStringBuffer(char *buf, int bufsize)
{
  if (!buf) {
    return Gal_MakeByteBuffer((char *) NULL, 0, 0,
			      1, 1, BUF_INC, BUF_TRIGGER);
  } else {
    return Gal_MakeByteBuffer(buf, bufsize, bufsize,
			      0, 0, BUF_INC, BUF_TRIGGER);
  }
}

void Gal_FreeStringBuffer(Gal_StringBuffer *buf)
{
  Gal_FreeByteBuffer(buf);
}

char *Gal_StringBufferString(Gal_StringBuffer *buf)
{
  return buf->buf;
}

/* I'll probably want to inline this function in pr_util.c */

int Gal_StringBufferWrite(Gal_StringBuffer *b, int increment,
			  const char *s, ...)
{
  va_list args;
  
  /* A bad, bad approximation when there are args, but hell. */
  if (increment == -1) {
    increment = strlen(s);
  }
  /* Always make sure you have room for the null byte
     in case this is being used as a string buffer. */
  if (_Gal_ExpandByteBuffer(b, increment + 1)) {
    /* Add the string. */
    va_start(args, s);
    vsprintf(b->buf + b->bufpos, s, args);
    va_end(args);
    b->bufpos += strlen(b->buf + b->bufpos);
    return 1;
  } else {
    return 0;
  }
}

/* SAM 11/07/00: It's not safe to write a string you don't
   know about using Gal_StringBufferWrite, since it might
   have % in it. */

int Gal_StringBufferWriteString(Gal_StringBuffer *b, const char *s)
{
  int increment = strlen(s);

  if (_Gal_ExpandByteBuffer(b, increment + 1)) {
    strcat(b->buf + b->bufpos, s);
    b->bufpos += strlen(b->buf + b->bufpos);
    return 1;
  } else {
    return 0;
  }
}

/*
 *   POINTER BUFFERS
 */

static int __Gal_ExpandPointerBuffer(Gal_PointerBuffer *b, int increment);

/* Because the objects are calloc'ed, everything will be 0 in the beginning. */

static Gal_PointerBuffer *alloc_pointer_buffer()
{
  Gal_PointerBuffer *s;

  GalUtil_LockLocalMutex(&pointer_buffer_mutex);
  if (!__Gal_PointerMemory) {
    GalUtil_UnlockLocalMutex(&pointer_buffer_mutex);
    GalUtil_Fatal("Pointer buffer memory not initialized; call Gal_InitializeStatics()");
  }

  s = (Gal_PointerBuffer *) _Gal_LMAllocate(__Gal_PointerMemory, (int *) NULL);
  GalUtil_UnlockLocalMutex(&pointer_buffer_mutex);
  
  if (s == NULL) {
    GalUtil_WarnWithLocation(__FUNCTION__, "Pointer buffer allocation error\n");
    return(NULL);
  }
  s->flags = GAL_MANAGE_MEMORY | GAL_DYNAMIC | GAL_FROM_STORE;
  return(s);
}

static void dealloc_pointer_buffer(Gal_PointerBuffer *b)
{
  if (b) {
    _Gal_LMDeallocate(__Gal_PointerMemory, (void *) b);
  }
}

static void __clear_pointer_buffer(void *elt)
{
  Gal_PointerBuffer *buf = (Gal_PointerBuffer *) elt;

  if (buf->buf) {
    free(buf->buf);
    buf->buf = (void **) NULL;
  }
}

int _Gal_FreeAllPointerBuffers()
{
  int success = 0;
  
  GalUtil_LockLocalMutex(&pointer_buffer_mutex);
  if (__Gal_PointerMemory) {
    int active_elements = _Gal_LMFree(__Gal_PointerMemory, __clear_pointer_buffer, 1);

    if (active_elements > 0) {
      GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't free pointer buffer repository; %d left", active_elements);
    } else {
      __Gal_PointerMemory = (_Gal_LocalMemory *) NULL;
      success = 1;
    }
  }
  GalUtil_UnlockLocalMutex(&pointer_buffer_mutex);
  return success;
}


/* General function for making a pointer buffer. If it's a
   fixed buffer, I'm going to allocate a fresh one, which
   will be freed. If manage_memory is 1, then the byte
   buffer will take care of managing the memory; if it's
   dynamic, it will allocate and reallocate its own
   internal memory, and if it's static, it will copy
   the buffer. If buf is non-NULL, then it will either
   be the contents of the byte buffer (if manage_memory is 0),
   or it will be copied into the byte buffer (if manage_memory
   is 1). Dynamic memory_managed buffers will be
   allocated from the store; others will be malloc'ed
   and freed. */

/* SAM 11/3/00: Same problem as for byte buffers.
   manage_memory should mean: it's your memory. Don't
   copy it. Here are the right settings:

   dynamic & manage_memory: if no array, allocate from the
   store. If an array is passed in, use it. The user will
   need to retrieve the data before the pointer buffer
   is destroyed.

   dynamic & !manage_memory: don't allocate from the store.
   Use as a shell for expansion. The caller will need to
   retrieve the memory before the pointer buffer is destroyed.

   !dynamic & manage_memory: treat it as yours. Don't
   allocate from the store. free when done.

   !dynamic & !manage_memory: don't allocate from the
   store, don't free. */

Gal_PointerBuffer *Gal_MakePointerBuffer(void **buf,
					 int pointer_type,
					 int bufpos,
					 int bufsize,
					 int manage_memory,
					 int dynamic,
					 void (*free_fn)(void *),
					 int increment,
					 int padding)
{
  Gal_PointerBuffer *temp;
    
  if (dynamic && manage_memory & !buf) {
    temp = alloc_pointer_buffer();
  } else {
    temp = (Gal_PointerBuffer *) malloc(sizeof(Gal_PointerBuffer));
    temp->bufsize = 0;
    if (dynamic && manage_memory)
      temp->flags = GAL_DYNAMIC | GAL_MANAGE_MEMORY;
    else if (manage_memory)
      temp->flags = GAL_MANAGE_MEMORY;
    else if (dynamic)
      temp->flags = GAL_DYNAMIC;
    temp->buf = (void **) NULL;
  }

  temp->free_fn = free_fn;
  temp->bufpos = 0;
  temp->increment = increment;
  if (padding < 0) padding = 0;
  temp->padding = padding;
  temp->pointer_type = pointer_type;
  
  if (buf) {
    /* If there's a buffer, no matter what it's
       intended to be used for, set things up. */
    temp->buf = buf;
    temp->bufsize = bufsize;
    temp->bufpos = bufpos;
  }
  
  return temp;
}

/* When we deallocate a byte buffer, we either return it
   to the store, with its allocated buffer intact, or
   we free it, and its buffer if we were managing the memory. */

void Gal_FreePointerBuffer(Gal_PointerBuffer *buf)
{
  /* If we've been given a free function, call it
     on all the members before we free the buffer. */
  if (buf->free_fn) {
    int i;
    for (i = 0; i < buf->bufpos; i++) {
      (*buf->free_fn)(buf->buf[i]);
    }
  }
  
  if (buf->flags & GAL_FROM_STORE) {
    dealloc_pointer_buffer(buf);
  } else {
    if (buf->flags & GAL_MANAGE_MEMORY) {
      if (buf->buf)
	free(buf->buf);
    }
    free(buf);
  }
}

static int __Gal_ExpandPointerBuffer(Gal_PointerBuffer *b, int increment)
{
  int new_size = b->bufpos + increment;
  int buf_avail = b->bufsize - new_size;
  int bufsize = b->bufsize;
  int expand = 0;
    
  while (buf_avail < b->padding) {
    bufsize += b->increment;
    buf_avail = bufsize - new_size;
    expand = 1;
  }

  if (!expand) {
    /* there's enough room */
    return 1;
  } else if (!(b->flags & GAL_DYNAMIC)) {
    /* Can't expand, fail */
    return 0;
  } else if (b->buf) {
    /* Need to realloc */
    b->buf = (void **) realloc(b->buf, bufsize * sizeof(void *));
    if (b->buf) {
      b->bufsize = bufsize;
      return 1;
    } else {
      b->bufsize = 0;
      return 0;
    }
  } else {
    /* Need to malloc */
    b->buf = (void **) calloc(bufsize, sizeof(void *));
    if (b->buf) {
      b->bufsize = bufsize;
      return 1;
    } else {
      b->bufsize = 0;
      return 0;
    }
  }
}

void **Gal_PointerBufferPointers(Gal_PointerBuffer *buf)
{
  return buf->buf;
}

/* This is the ACTUAL NUMBER OF ELEMENTS, not the
   size of the internal buffer. */

int Gal_PointerBufferSize(Gal_PointerBuffer *buf)
{
  return buf->bufpos;
}

/* Appending to the buffer. BEWARE! If the buffer has a free
   function, it will be applied to whatever you put in here. */

int Gal_PointerBufferAddMultiple(Gal_PointerBuffer *b, void **s, int size)
{
  if (__Gal_ExpandPointerBuffer(b, size)) {
    int i;
    for (i = 0; i < size; i++) {
      b->buf[i + b->bufpos] = s[i];
    }
    b->bufpos += size;
    return 1;
  } else {
    return 0;
  }
}

int Gal_PointerBufferAdd(Gal_PointerBuffer *b, void *s)
{
  return Gal_PointerBufferAddMultiple(b, &s, 1);
}

/* Removes the elements and squashes the rest. Doesn't
   unallocate memory. */

void Gal_PointerBufferRemove(Gal_PointerBuffer *b, void *s)
{
  int i, j;
  for (i = 0; i < b->bufpos; i++) {
    if (b->buf[i] == s) {
      if (b->free_fn)
	(*b->free_fn)(s);
      /* Squash. */
      for (j = i + 1; j < b->bufpos; j++) {
	b->buf[j - 1] = b->buf[j];
      }
      b->buf[b->bufpos - 1] = (void *) NULL;
      b->bufpos--;
      break;
    }
  }
}

/* Gal_PointerBufferCopy respects the properties it was originally
   called with. So it doesn't manage memory if not asked to,
   and it's not dynamic, etc. */

Gal_PointerBuffer *Gal_PointerBufferCopy(Gal_PointerBuffer *b,
					 void *(*copy_fn)(void *),
					 void (*free_fn)(void *))
{
  Gal_PointerBuffer *new_b;
  int i;
  void **buf;
  
  /* The idea is that if you copy the elements of a pointer buffer,
     you should make sure you free them. Similarly, if you don't
     copy them, you shouldn't free them, no matter what, because
     the original is holding the instructions on whether to free
     them or not. Finally, if you're not managing memory, you should
     neither copy nor free, no matter what is passed in. */

  if (!(b->flags & GAL_MANAGE_MEMORY)) {
    copy_fn = NULL;
    free_fn = NULL;
  } else if (copy_fn && !free_fn) {
    free_fn = b->free_fn;
    if (!free_fn) {
      GalUtil_Warn("Can't pass a copy function to copy a pointer buffer with a free function");
      return (Gal_PointerBuffer *) NULL;
    }
  } else if (!copy_fn) {
    free_fn = NULL;
  }

  if (b->flags & GAL_MANAGE_MEMORY) {
    /* Copy the buffer, dammit. */
    if (b->bufpos > 0) {
      buf = (void **) calloc(b->bufpos, sizeof(void *));
      for (i = 0; i < b->bufpos; i++) {
	buf[i] = b->buf[i];
      }
    } else {
      buf = (void **) NULL;
    }
  } else {
    buf = b->buf;
  }

  new_b = Gal_MakePointerBuffer(buf,
				b->pointer_type,
				b->bufpos,
				b->bufpos,
				b->flags & GAL_MANAGE_MEMORY ? 1 : 0,
				b->flags & GAL_DYNAMIC ? 1 : 0,
				free_fn,
				b->increment,
				b->padding);

  if (copy_fn) {
    /* If we're given a copy function, we already know
       we'll have a free_fn and we're managing memory. */
    for (i = 0; i < b->bufpos; i++) {
      new_b->buf[i] = (*copy_fn)(new_b->buf[i]);
    }
  }
  return new_b;
}

int Gal_PointerBufferDynamic(Gal_PointerBuffer *b)
{
  return (b && (b->flags & GAL_DYNAMIC));
}

void *Gal_PointerBufferNthElement(Gal_PointerBuffer *b, int i)
{
  if (i < b->bufpos)
    return b->buf[i];
  else
    return (void *) NULL;
}

int Gal_PointerBufferSetNthElement(Gal_PointerBuffer *b, int i, void *elt)
{
  if (i < b->bufpos) {
    if (b->free_fn)
      (*b->free_fn)(b->buf[i]);
    b->buf[i] = elt;
    return 1;
  } else {
    return 0;
  }
}
