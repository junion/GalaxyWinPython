/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _GALAXY_H
#define _GALAXY_H

/* Current GC version is 4.0 */

#define GC_VERSION 0x40000

/* This will be used to differentiate between
   different versions of the transport. I hope. */

#define GAL_TRANSPORT_PROTOCOL_VERSION 1

#ifdef _MSC_VER
#ifdef _DEBUG
#pragma comment(lib,"libGalaxy_debug.lib")
#else
#pragma comment(lib,"libGalaxy.lib")
#endif
#endif

#include <stdio.h>
#include <stdarg.h>
#include <sys/types.h>
#ifndef WIN32
#include <sys/time.h>			/* for timed_tasks.c stuff */
#else
#include <winsock2.h>			/* for fd_set; BEWARE: only works for sockets */
#endif

#include "galaxy/util.h"
#include "galaxy/gthread.h"
/* This file contains common declarations that
   all the headers need. */
#include "galaxy/common_decls.h"

#ifndef MAX_FNAME_LENGTH
#define MAX_FNAME_LENGTH 1024
#endif

#ifndef WIN32
#define GAL_DIRSEP '/'
#else
#define GAL_DIRSEP '\\'
#define GAL_ALT_DIRSEP '/'
#endif
#define GAL_EOL_CHAR '\n'
#define GAL_LINE_LENGTH 1024

/* Gal boolean defs */
typedef enum
{
  GAL_TRUE = 1,
  GAL_FALSE = 0
} Gal_Boolean;

/* Gal_Frame types */
typedef enum
{
  GAL_NULLFRAME = 0,
  GAL_TOPIC,
  GAL_CLAUSE,
  GAL_PRED
} Gal_FrameType;

typedef struct _gal_tag_map
{
  char *tag_name;
  int tag;
} Gal_TagMap;

typedef struct _gal_tag_array
{
  char **tags;
  int offset;
  int size;
} *Gal_TagArray;

typedef struct _gal_input_stream *Gal_InputStream;

typedef int (*Gal_NextCharFn)(Gal_InputStream st);
typedef void (*Gal_RewindFn)(Gal_InputStream st, int i);

typedef struct _gal_inputfnpkg {
  Gal_NextCharFn next_char_fn;
  Gal_RewindFn rewind_fn;
} Gal_InputFnPkg;

/* SAM 9/16/99: This was private, but I need it to be public in order
   to exploit it elsewhere. */

typedef struct _gal_input_stream
{
  int type;
  void *stream;
  long position;
  Gal_InputFnPkg *fn_pkg;
} GAL_INPUT_STREAM;

typedef struct _gal_output_stream
{
  int type;
  void *stream;  
} GAL_OUTPUT_STREAM, *Gal_OutputStream;

typedef struct _gal_var_mapping {
  char *var;
  Gal_Object value;
} Gal_VarMapping;

#define GAL_MANAGE_MEMORY 1
#define GAL_DYNAMIC 2
#define GAL_FROM_STORE 4

typedef struct _gal_string_buffer
{
  char *buf;
  int bufpos;
  int bufsize;
  int flags;
  int increment;
  int padding;
  int chunk_size;
} Gal_StringBuffer;

typedef struct _gal_pointer_buffer
{
  void **buf;
  int pointer_type;
  int bufpos;
  int bufsize;  
  void (*free_fn)(void *);
  int flags;
  int increment;
  int padding;
} Gal_PointerBuffer;

typedef int (*Gal_IntFnPtr)();

typedef struct _gal_dialogue_function_map
{
  char *name;
  Gal_IntFnPtr fn;
} GAL_DIALOGUE_FUNCTION_MAP;

typedef Gal_Frame (*Gal_FrameDataFnPtr)(Gal_Frame frame, void *data);

/* SAM 10/2/99: This is a little more convoluted than one might think
   it needs to be. Instead of having a single array with signature
   and functions, I'm going to keep a parallel array of signatures
   and function mappings for two reasons: (1) I prefer not to
   alter the MIT structure GAL_SERVER_FUNCTION_MAP in case there
   are dependencies I'm not aware of, and (2) I need to pass
   signatures back and forth from Hub to server, and I don't need
   the function mappings. I'll take this opportunity to cache
   the encoding of the signature for sending to each server which
   attaches (be sure not to free it when you free the frame!).

   SAM 8/31/01: I finally convinced myself that I could extend the
   GAL_SERVER_FUNCTION_MAP function safely, and cleaned up a bunch
   of stuff in the context of adding the ability to override the
   invocation process. I'm still keeping the name in the
   signatures, because they still need to be free-standing. */

typedef struct __gal_dispatch_fn_signature_key_entry {
  char *key;
  Gal_ObjectType value_type;
  int obligatory;
} Gal_DispatchFnSignatureKeyEntry;

typedef struct __gal_dispatch_fn_signature {
  char *name;
  Gal_DispatchFnSignatureKeyEntry* in_key_array;  
  int allow_other_in_keys;
  int reply_provided;
  Gal_DispatchFnSignatureKeyEntry* out_key_array;  
  int allow_other_out_keys;
} Gal_DispatchFnSignature;  

typedef struct _gal_server_function_map
{
  char *name;
  Gal_FrameDataFnPtr fn_with_data;
  Gal_DispatchFnSignature sig;
  void *client_data;
} GAL_SERVER_FUNCTION_MAP;

struct __gal_dispatch_fn_invocation;

typedef int (*Gal_DispatchFunctionSelector)(struct __gal_dispatch_fn_invocation *i);

typedef Gal_Frame (*Gal_DispatchFunctionInvoker)(struct __gal_dispatch_fn_invocation *i);

struct __gal_dispatch_fn_pkg;

typedef Gal_DispatchFnSignature *(*Gal_DispatchFnSignatureLister)(struct __gal_dispatch_fn_pkg *pkg);

typedef struct __gal_dispatch_fn_pkg {
  GAL_SERVER_FUNCTION_MAP *fn_map;
  int num_entries;
  int allocated;
  Gal_Object encoded_signature;
  /* SAM 8/31/01: Add a selector function
     which we can change. */
  Gal_DispatchFunctionSelector selector;
  /* And a signature lister, since we're
     completely bypassing the function map. */
  Gal_DispatchFnSignatureLister lister;
  /* SAM 8/31/01: Really ought to have an
     invoker, too, which is changed in parallel
     to the selector. That way we don't have
     to look up functions more than once. */
  Gal_DispatchFunctionInvoker invoker;
  void *client_data;
} Gal_DispatchFnPkg;

/* The fn_map is in the call_client_data in the default case.
   The whole selection process needs to stand independently of
   the SERVER_FUNCTION_MAP stuff because there may not be
   any entries in the SERVER_FUNCTION_MAP. */

typedef struct __gal_dispatch_fn_invocation {
  Gal_DispatchFnPkg *pkg;
  Gal_DispatchFnSignature *sig;
  char *bare_op_name;
  Gal_Frame frame;
  GalSS_Environment *env;
  void *call_client_data;
} Gal_DispatchFnInvocation;

/* For the enhanced time task loop. */

/* These are the run reason constants. */

#define GAL_SOCKET_READABLE 1
#define GAL_SOCKET_WRITABLE 2
#define GAL_SOCKET_ERR_READABLE 4
#define GAL_FILE_READABLE 8
#define GAL_FILE_WRITABLE 16
#define GAL_FILE_ERR_READABLE 32
#define GAL_TIMER_EXPIRED 64
#define GAL_CONDITION_SATISFIED 128
#define GAL_THREAD_READABLE 256

typedef int (*Gal_TaskConditionFn)(void *caller_data);

typedef struct __TaskPkg {
  /* This is the task which will be run. */
  void (*task)(struct __TaskPkg *);
  /* This is the caller data. */
  void *caller_data;
  /* This is whether the task has read blocking set. */
  int read_blocking;
  /* This is how long you should wait in between
     task invocations. */
  long num_millisecs;
  /* This is whether the task is executing or not. */
  int executing;
  /* This is whether the task has been fired for the
     last time or not. */
  int done;
  /* This is the cleanup function for the task. */
  void (*cleanup_fn)(void *);
  /* This is the thread ID for the task, for the threaded case. */
  GalUtil_ThreadID thread;
  /* This is how long until the next firing. */
  struct timeval tv;
  /* These are the read and write elements, etc. */
  GAL_SOCKET *read_socket;
  GAL_SOCKET *write_socket;
  GAL_SOCKET *err_socket;
  FILE *read_file;
  FILE *write_file;
  FILE *err_file;
  Gal_TaskConditionFn condition;
  /* These are the reasons the task is running. See above. */
  int run_reasons;
  /* This is the environment object. We can use that here,
     now that we've sorted out the common declarations. */
  GalSS_Environment *host_env;
} Gal_TaskPkg;

/* Function protos for file dispatch_function.c */

/* These are some of the info we can use in defining our signatures. */

enum {GAL_KEY_ALWAYS, GAL_KEY_SOMETIMES, GAL_OTHER_KEYS_MAYBE,
      GAL_OTHER_KEYS_NEVER, GAL_REPLY_PROVIDED, GAL_REPLY_NONE, GAL_REPLY_UNKNOWN};

Gal_DispatchFnSignatureKeyEntry *Gal_CopyDispatchFnKeyArray(Gal_DispatchFnSignatureKeyEntry *array);
Gal_DispatchFnSignatureKeyEntry *_Gal_CreateEmptyDispatchFnKeyArray(int i);
void _Gal_PopulateDispatchFnKeyArrayCell(Gal_DispatchFnSignatureKeyEntry *array, int index, const char *key, Gal_ObjectType t, int obligatory);
Gal_DispatchFnSignatureKeyEntry *Gal_CreateDispatchFnKeyArray(int ignore, ...);
void Gal_FreeDispatchFnKeyArray(Gal_DispatchFnSignatureKeyEntry *entry);
void Gal_FreeDispatchFnSignature(Gal_DispatchFnSignature *sigs);
Gal_DispatchFnPkg *Gal_AddDispatchFunctionEntry(Gal_DispatchFnPkg *table,
						const char *name,
						Gal_FrameDataFnPtr fn_with_data,
						Gal_DispatchFnSignatureKeyEntry *in_key_array,
						int allow_other_in_keys, 
						int reply_provided,
						Gal_DispatchFnSignatureKeyEntry *out_key_array,
						int allow_other_out_keys);
Gal_DispatchFnSignature *
Gal_CreateDispatchFnSignature(const char *name,
			      Gal_DispatchFnSignatureKeyEntry *in_key_array,
			      int allow_other_in_keys, 
			      int reply_provided,
			      Gal_DispatchFnSignatureKeyEntry *out_key_array,
			      int allow_other_out_keys);
int
Gal_FindDispatchFunctionEntry(Gal_DispatchFnInvocation *i);
void Gal_DispatchFnPkgSetClientData(Gal_DispatchFnPkg *pkg,
				    const char *op_name,
				    void *data);
Gal_DispatchFnPkg *
_Gal_DispatchFnPkgSetAccess(Gal_DispatchFnPkg *pkg,
			    Gal_DispatchFunctionSelector s,
			    Gal_DispatchFnSignatureLister l,
			    Gal_DispatchFunctionInvoker i,
			    void *invocation_client_data);
Gal_Frame _Gal_InvokeDispatchFn(Gal_DispatchFnInvocation *i);
Gal_DispatchFnSignature *
_Gal_ListDispatchFnSignatures(Gal_DispatchFnPkg *pkg);
void *Gal_DispatchFnPkgGetClientData(Gal_DispatchFnPkg *pkg,
				     const char *op_name);
void Gal_ValidateDispatchFnInput(const char *op_name, Gal_Frame frame,
				 Gal_DispatchFnSignature *sig, char **exclusions);
void Gal_ValidateDispatchFnOutput(const char *op_name, Gal_Frame frame,
				  Gal_DispatchFnSignature *sig, char **exclusions);
Gal_Object Gal_EncodeDispatchFnPkgSigs(Gal_DispatchFnPkg *pkg);
Gal_Object Gal_EncodeDispatchFnSignatures(Gal_DispatchFnPkg *pkg);
Gal_DispatchFnSignature *Gal_DecodeDispatchFnSignatures(Gal_Object maybe_list);
Gal_DispatchFnSignature *Gal_FindNamedSignature(Gal_DispatchFnSignature *sigs,
						const char *name);
/* Function protos for file grovel.c */
Gal_Object Gal_FindKey(Gal_Frame fr, const char *key_name);
Gal_Object Gal_MatchKeyValue(Gal_Frame fr, const char *key_name,
			     Gal_Object match);
Gal_Frame  Gal_FindPred(Gal_Frame fr, const char *pred_name);
Gal_Frame  Gal_FindTopic(Gal_Frame fr, const char *topic_name);
void       Gal_DeletePreds(Gal_Frame fr, const char *pred_name);
Gal_Frame  Gal_FindPredParent(Gal_Frame frame, const char *name, Gal_Frame parent, int findpar, int nth);

/* Function protos for file init.c */
void Gal_InitializeStatics(void);

/* Function protos for file nfio.c */

void Gal_SetTagObjectHash(Gal_HashTable ht);
Gal_HashTable Gal_GetTagObjectHash(void);
Gal_Object Gal_GetTagObject(const char *str);
void Gal_SetTagObject(const char *key, Gal_Object value);

Gal_Frame  Gal_ReadFrameFromString(const char *buf);
Gal_Object Gal_ReadObjectFromString(const char *buf);
Gal_Frame  Gal_ReadFrameFromFile(FILE *fp);
Gal_Frame  Gal_ReadNextFrameFromFile(FILE *fp, int bol);
Gal_Object Gal_ReadObjectFromFile(FILE *fp);

Gal_VarMapping *Gal_CreateVarMapping(int num_pairs, ...);
Gal_Frame Gal_VAReadVarFrameFromString(const char *buf, int num_pairs, ...);
Gal_Frame Gal_ReadVarFrameFromString(const char *buf, Gal_VarMapping *map);
Gal_Object Gal_VAReadVarObjectFromString(const char *buf, int num_pairs, ...);
Gal_Object Gal_ReadVarObjectFromString(const char *buf, Gal_VarMapping *map);

/* Function protos for file nframe.c */
/* frame allocation and copying, frame type, frame comparison */
Gal_Frame Gal_MakeFrame(const char *name, Gal_FrameType type);
Gal_Frame Gal_MakeClauseFrame(const char *name);
Gal_Frame Gal_MakeTopicFrame(const char *name);
Gal_Frame Gal_MakePredFrame(const char *name);
void      Gal_FreeFrame(Gal_Frame fr);
Gal_Frame Gal_CopyFrame(Gal_Frame fr);
void Gal_UpdateFrameProperties(Gal_Frame target_frame,
			       Gal_Frame source_properties,
			       char ** delete_properties);
int       Gal_FrameEqual(Gal_Frame sf, Gal_Frame sem);
int       Gal_MatchFrame(Gal_Frame sf, Gal_Frame sem);
/* name and type operations */
Gal_Frame     Gal_SetFrameName(Gal_Frame fr, const char *name);
Gal_Frame     Gal_SetFrameType(Gal_Frame fr, Gal_FrameType type);
Gal_FrameType Gal_GetFrameType(Gal_Frame fr);
char         *Gal_FrameName(Gal_Frame fr);
int           Gal_FrameNameEq(Gal_Frame fr, const char *name);
int           Gal_FrameNamesEq(Gal_Frame fr1, Gal_Frame fr2);
int           Gal_FrameIsType(Gal_Frame fr, Gal_FrameType type);
int           Gal_ClauseFramep(Gal_Frame fr);
int           Gal_TopicFramep(Gal_Frame fr);
int           Gal_PredFramep(Gal_Frame fr);
/* predicate operations */
Gal_Frame  Gal_AddPred(Gal_Frame fr, Gal_Frame pred);
Gal_Frame  Gal_GetPred(Gal_Frame fr, int i);
Gal_Frame  Gal_GetPredByName(Gal_Frame fr, const char *name);
Gal_Object Gal_RemPred(Gal_Frame fr, int i);
Gal_Object Gal_RemPredByName(Gal_Frame fr, const char *name);
void       Gal_DelPred(Gal_Frame fr, int i);
void       Gal_DelPredByName(Gal_Frame fr, const char *name);
int        Gal_NumPreds(Gal_Frame fr);
void       Gal_ClearPreds(Gal_Frame fr);
/* property list operations */
Gal_Object  Gal_SetProp(Gal_Frame fr, const char *key, Gal_Object val);
Gal_Object  Gal_GetObject(Gal_Frame fr, const char *key);
Gal_Frame   Gal_GetFrame(Gal_Frame fr, const char *key);
Gal_Frame   Gal_GetTopicFrame(Gal_Frame fr, const char *key);
char       *Gal_GetString(Gal_Frame fr, const char *key);
int         Gal_GetInt(Gal_Frame fr, const char *key);
float       Gal_GetFloat(Gal_Frame fr, const char *key);
Gal_Object *Gal_GetList(Gal_Frame fr, const char *key, int *length);
void       *Gal_GetBinary(Gal_Frame fr, const char *key, int *size);
void *Gal_GetInt16(Gal_Frame fr, const char *key, int *size);
void *Gal_GetInt32(Gal_Frame fr, const char *key, int *size);
void *Gal_GetInt64(Gal_Frame fr, const char *key, int *size);
void *Gal_GetFloat32(Gal_Frame fr, const char *key, int *size);
void *Gal_GetFloat64(Gal_Frame fr, const char *key, int *size);
GalSS_BrokerProxy *Gal_GetProxy(Gal_Frame fr, const char *key);
Gal_Object  Gal_RemProp(Gal_Frame fr, const char *key);
int         Gal_DelProp(Gal_Frame fr, const char *key);
int         Gal_NumProperties(Gal_Frame fr);
int         Gal_NumNonNullProperties(Gal_Frame fr);
char **Gal_GetProperties(Gal_Frame fr, int *nkeys);

/* Function protos for file pr_util.c */

enum {GAL_PP_PRINT, GAL_PR_PRINT, GAL_PP_TRUNC_PRINT};

char *Gal_PrintFrameToString(Gal_Frame fr, char *irpbuf, int *bufsizeptr,
			     int how);
void Gal_PrintFrameToFile(Gal_Frame fr, FILE *fp, int how);
void GalUtil_PrintObject(int gal_verbose_level, Gal_Object to, int how);

void  Gal_PrFrame(Gal_Frame fr);
void  Gal_PPFrame(Gal_Frame fr);
void  GalUtil_PPFrame(int gal_verbose_level, Gal_Frame fr);
void  GalUtil_CPPFrame(int gal_verbose_level, int fore, int back, Gal_Frame fr);
char *Gal_PrFrameToString(Gal_Frame fr, char *irpbuf, int *bufsizeptr);
char *Gal_PPFrameToString(Gal_Frame fr, char *irpbuf, int *bufsizeptr);
void  Gal_PrFrameToFile(Gal_Frame fr, FILE *fp);
void  Gal_PPFrameToFile(Gal_Frame fr, FILE *fp);
void  Gal_OutlineFrame(Gal_Frame fr, int gal_verbose_level);
void  Gal_PrObject(Gal_Object to);
void  Gal_PPObject(Gal_Object to);
void  GalUtil_PPObject(int gal_verbose_level, Gal_Object to);
void  GalUtil_PrObject(int gal_verbose_level, Gal_Object to);
void  GalUtil_CPPObject(int gal_verbose_level, int fore, int back, Gal_Object to);
char *Gal_ObjectToString(Gal_Object to);
char *Gal_ObjectString(Gal_Object to, Gal_StringBuffer **buf);
void Gal_ObjectStringToBuffer(Gal_Object to, Gal_StringBuffer *buf);
void  Gal_PrObjectToFile(Gal_Object to, FILE *fp);
void  Gal_PPObjectToFile(Gal_Object to, FILE *fp);
void  Gal_OutlineObject(Gal_Object to, int gal_verbose_level);

/* Function protos for file signal.c */

void Gal_InitializeSignals();
void Gal_AddSignalHandler(int sig, void (*handler)(int));
int Gal_SignalsInitialized();

/* Function protos for file stream_util.c */
Gal_InputStream Gal_MakeStringInputStream(const char *sp);
char      *Gal_StringInputStreamString(Gal_InputStream gs);
Gal_InputStream Gal_MakeFileInputStream(FILE *fp);
/* I should be able to make the line a const, but it calls a
   system function which may not have the right signature. */
char      *Gal_FileNextLine(Gal_InputStream gs, char *line, int max);
char      *Gal_FileCurrentLine(Gal_InputStream gs, char *line, int max);
int       Gal_InputStreamNextChar(Gal_InputStream gs);
void      Gal_InputStreamRewind(Gal_InputStream gs, int i);

Gal_OutputStream Gal_MakeStringOutputStream(char *buf, int bufsize);
void Gal_FreeStringOutputStream(Gal_OutputStream gs);
char *Gal_StringOutputStreamString(Gal_OutputStream gs);
int Gal_StringStreamWriteString(Gal_OutputStream gs, int increment, const char *s, ...);

/* Function protos for file dynamic_buffer.c */
Gal_StringBuffer *Gal_MakeDataBuffer(char *buf,
				     int bufpos,
				     int bufsize,
				     int manage_memory,
				     int dynamic,
				     int increment,
				     int padding,
				     int chunk_size);
char *Gal_DataBufferData(Gal_StringBuffer *buf);
int Gal_DataBufferSize(Gal_StringBuffer *buf);
int Gal_DataBufferByteCount(Gal_StringBuffer *buf);
int Gal_DataBufferAppend(Gal_StringBuffer *b, const char *s, int size);

Gal_StringBuffer *Gal_MakeByteBuffer(char *buf,
				     int bufpos,
				     int bufsize,
				     int manage_memory,
				     int dynamic,
				     int increment,
				     int padding);
void Gal_FreeByteBuffer(Gal_StringBuffer *buf);
char *Gal_ByteBufferBytes(Gal_StringBuffer *buf);
int Gal_ByteBufferSize(Gal_StringBuffer *buf);
int Gal_ByteBufferDynamic(Gal_StringBuffer *b);
int Gal_ByteBufferAppend(Gal_StringBuffer *b, const char *s, int size);

Gal_StringBuffer *Gal_MakeStringBuffer(char *buf, int bufsize);
void Gal_FreeStringBuffer(Gal_StringBuffer *buf);
char *Gal_StringBufferString(Gal_StringBuffer *buf);
int Gal_StringBufferWrite(Gal_StringBuffer *b, int increment,
			  const char *s, ...);
int Gal_StringBufferWriteString(Gal_StringBuffer *b, const char *s);

/* These types are relevant to pointer buffers or to
   pointer queues. */

enum {GAL_SOCKQUEUE_PTYPE, GAL_SOCK_OBJECT_PTYPE,
      GAL_CONTINUATION_PTYPE, GAL_CONNECTION_PTYPE,
      GAL_CALLBACK_PTYPE, GAL_BROKER_PTYPE,
      GAL_CHAR_STAR_PTYPE, GAL_OBJECT_PTYPE,
      GAL_SERVICE_PROVIDER_PTYPE, GAL_SERVICE_TYPE_PTYPE,
      GAL_LISTENER_PROXY_PTYPE, GAL_LOCK_INFO_PTYPE,
      GAL_HUB_CONTINUATION_PTYPE, GAL_SERVER_MESSAGE_PTYPE,
      GAL_PROVIDER_SPEC_PTYPE};

Gal_PointerBuffer *Gal_MakePointerBuffer(void **buf,
					 int pointer_type,
					 int bufpos,
					 int bufsize,
					 int manage_memory,
					 int dynamic,
					 void (*free_fn)(void *),
					 int increment,
					 int padding);
void Gal_FreePointerBuffer(Gal_PointerBuffer *buf);
void **Gal_PointerBufferPointers(Gal_PointerBuffer *buf);
int Gal_PointerBufferSize(Gal_PointerBuffer *buf);
int Gal_PointerBufferAddMultiple(Gal_PointerBuffer *b, void **s, int size);
int Gal_PointerBufferAdd(Gal_PointerBuffer *b, void *s);
Gal_PointerBuffer *Gal_PointerBufferCopy(Gal_PointerBuffer *b,
					 void *(*copy_fn)(void *),
					 void (*free_fn)(void *));
void *Gal_PointerBufferNthElement(Gal_PointerBuffer *b, int i);
int Gal_PointerBufferSetNthElement(Gal_PointerBuffer *b, int i, void *elt);
int Gal_PointerBufferDynamic(Gal_PointerBuffer *b);
void Gal_PointerBufferRemove(Gal_PointerBuffer *b, void *s);

/* Function protos for file string_util.c */
char *Gal_ReadToken(Gal_InputStream gs, char *tok, int toksize,
		    const char *stop_chars, int do_rewind);
char *Gal_NextToken(const char *sent, char *tok, int toksize);
int   Gal_StringEq(const char *str1, const char *str2);
int   Gal_StringCaseEq(const char *str1, const char *str2);
void  Gal_StringRightTrim(char *str, const char *trim);
void  Gal_StringLeftTrim(char *str, const char *trim);
void  Gal_StringTrim(char *str, const char *trim);
int   Gal_DigitStringp(const char *word);
int   Gal_FloatStringp(const char *word);

/* Function protos for file sym.c */
/* hash table utilities */
Gal_HashTable Gal_MakeHash(int size);
Gal_HashTable Gal_CopyHash(Gal_HashTable old_ht);
Gal_Object    Gal_GetHash(const char *str, Gal_HashTable hp);
int           Gal_HashHasKey(const char *key, Gal_HashTable ht);
Gal_Object    Gal_SetHash(const char *str, Gal_Object val, Gal_HashTable hp);
void Gal_MapHash(Gal_HashTable hp, void (*fn)(char *str, Gal_Object val, Gal_HashTable hp));
void          Gal_ClearHash(Gal_HashTable hp);
void          Gal_FreeHash(Gal_HashTable hp);
char         *Gal_HashListNext(Gal_HashTable hp, Gal_Object *value);
void          Gal_HashListRewind(Gal_HashTable hp);

/* string to int mapping utilities */
Gal_HashTable Gal_InitializeTagTable(Gal_TagMap *tag_map, int size);
void          Gal_AddToTagTable(Gal_TagMap *tag_map, Gal_HashTable ht);
Gal_TagArray  Gal_InitializeTagArray(Gal_TagMap *tag_map);
void          Gal_AddToTagArray(Gal_TagMap *tag_map, Gal_TagArray tag_array);
char         *Gal_GetTagArrayTag(int tag, Gal_TagArray tag_array);
void          Gal_FreeTagArray(Gal_TagArray tag_array);


/* Function protos for file timed_tasks.c */
int  Gal_AddTimedTask(void *task, void *caller_data, long num_millisecs);
int  Gal_AddTimedTaskWithFileIO(void *task, void *caller_data, long num_millisecs, FILE *read_file, FILE *write_file);
int Gal_AddTimedTaskWithSocketIO(void *task, void *caller_data, long num_millisecs, GAL_SOCKET *read_socket, GAL_SOCKET *write_socket);
int  Gal_RemoveTimedTask(void *task, void *caller_data);
int Gal_StartTask(Gal_TaskPkg *pkg, int num_millisecs);
int  Gal_TimedTasksLoopHandler(struct timeval *tv);
void Gal_TimedTasksLoop(void);
void Gal_TimedTasksLoopExit(void);
void Gal_TimedTaskLoopThreadWaiter(void);
void Gal_EndTasks(int immediate);
void Gal_MaybeEndTask(int immediate, int deferred);
/* New for thread support. */
void Gal_EnableTimedTaskThreads();
int Gal_TimedTaskThreadsEnabled();

Gal_TaskPkg *Gal_AddTask(void (*task)(Gal_TaskPkg *), void *caller_data,
			 long num_millisecs, int read_blocking_available,
			 void (*cleanup_fn)(void *));
Gal_TaskPkg *Gal_AddTaskWithSocketIO(void (*task)(Gal_TaskPkg *), void *caller_data,
			 long num_millisecs, int read_blocking_available,
			 GAL_SOCKET *read_socket, GAL_SOCKET *write_socket,
			 void (*cleanup_fn)(void *));
Gal_TaskPkg *Gal_AddTaskWithFileIO(void (*task)(Gal_TaskPkg *), void *caller_data,
			 long num_millisecs, int read_blocking_available,
			 FILE *read_file, FILE *write_file,
			 void (*cleanup_fn)(void *));
Gal_TaskPkg *Gal_AddTaskExtended(void (*task)(Gal_TaskPkg *),
				 void *caller_data,
				 long num_millisecs,
				 int read_blocking_available,
				 GAL_SOCKET *read_socket,
				 GAL_SOCKET *write_socket,
				 GAL_SOCKET *err_socket,
				 FILE *read_file, FILE *write_file,
				 FILE* err_file,
				 Gal_TaskConditionFn condition,
				 void (*cleanup_fn)(void *));
void Gal_ReAddTask(Gal_TaskPkg *p, void *caller_data,
		   long num_millisecs, int read_blocking_available,
		   void (*cleanup_fn)(void *));
void Gal_ReAddTaskWithSocketIO(Gal_TaskPkg *p, void *caller_data,
		   long num_millisecs, int read_blocking_available,
		   GAL_SOCKET *read_socket, GAL_SOCKET *write_socket,
		   void (*cleanup_fn)(void *));
void Gal_ReAddTaskWithFileIO(Gal_TaskPkg *p, void *caller_data,
		   long num_millisecs, int read_blocking_available,
		   FILE *read_file, FILE *write_file,
		   void (*cleanup_fn)(void *));
void Gal_ReAddTaskExtended(Gal_TaskPkg *p, void *caller_data,
			   long num_millisecs, int read_blocking_available,
			   GAL_SOCKET *read_socket, GAL_SOCKET *write_socket,
			   GAL_SOCKET *err_socket,
			   FILE *read_file, FILE *write_file, FILE *err_file,
			   Gal_TaskConditionFn condition,
			   void (*cleanup_fn)(void *));

void Gal_RemoveTask(Gal_TaskPkg *task_id);
int Gal_TaskPkgBlocking(Gal_TaskPkg *pkg);
void *Gal_TaskPkgData(Gal_TaskPkg *pkg);
int Gal_TaskPkgRunReasons(Gal_TaskPkg *pkg);

/* New for idle tasks. */
typedef void (*Gal_IdleFunction)(void *client_data);
int Gal_AddIdleFunction(Gal_IdleFunction func, void *client_data);
void Gal_RemoveIdleFunction(Gal_IdleFunction func);
void Gal_RunIdleFunctions();

/* Function protos for file tobj.c */
/* allocation and copying */
void       Gal_FreeObject(Gal_Object to);
void       Gal_FreeWrapper(Gal_Object to);
Gal_Object Gal_CopyObject(Gal_Object to);

/* object type */
Gal_ObjectType Gal_GetObjectType(Gal_Object to);
Gal_ObjectType Gal_GetDetailedType(Gal_Object to);
char *Gal_GetObjectTypeString(Gal_Object to);

/* type checking predicates */
int Gal_Binaryp(Gal_Object to);
int Gal_Clausep(Gal_Object to);
int Gal_Topicp(Gal_Object to);
int Gal_Predp(Gal_Object to);
int Gal_Framep(Gal_Object to);
int Gal_Floatp(Gal_Object to);
int Gal_Intp(Gal_Object to);
int Gal_Keywordp(Gal_Object to);
int Gal_Listp(Gal_Object to);
int Gal_Stringp(Gal_Object to);
int Gal_Tagp(Gal_Object to);
int Gal_Tokenp(Gal_Object to);
int Gal_Pointerp(Gal_Object to);
int Gal_Symbolp(Gal_Object to);
int Gal_Int16p(Gal_Object to);
int Gal_Int32p(Gal_Object to);
int Gal_Int64p(Gal_Object to);
int Gal_Float32p(Gal_Object to);
int Gal_Float64p(Gal_Object to);

/* object comparison */
int Gal_ObjectEqual(Gal_Object to, Gal_Object sem);
int Gal_ObjectCaseEqual(Gal_Object to, Gal_Object sem);

/* value to object conversion */
/* formerly make_tfloat, make_tframe, etc. */
Gal_Object Gal_FloatObject(float value);
Gal_Object Gal_FrameObject(Gal_Frame value);
Gal_Object Gal_IntObject(int value);
Gal_Object Gal_SymbolObject(const char *value);
Gal_Object Gal_KeywordObject(const char *value);
Gal_Object Gal_TagObject(const char *value);
Gal_Object Gal_ListObject(Gal_Object *values, int n);
Gal_Object Gal_ListObjectFromElements(int n, ...);
Gal_Object Gal_BinaryObject(void *data, int size);
int Gal_ArrayObjectAdd(Gal_Object obj, void *data, int size);
int Gal_ArrayObjectExpandable(Gal_Object obj);
Gal_Object Gal_PointerObject(void *val);
Gal_Object Gal_StringObject(const char *val);
Gal_Object Gal_TokenObject(const char *val);

/* Memory managed versions. */

/* These can't be const, because you may free it. */

Gal_Object Gal_CreateFrameObject(Gal_Frame value, int manage_memory);
Gal_Object Gal_CreateStringObject(char *cp, int manage_memory);
Gal_Object Gal_CreateTokenObject(char *cp, int manage_memory);
Gal_Object Gal_CreateBinaryObject(void *data, int size, int manage_memory);
Gal_Object Gal_CreateFloatObject(float *value, int manage_memory);
Gal_Object Gal_CreateListObject(Gal_Object *values, int n,
				void (*free_fn)(void *),
				int manage_memory);
void _gal_free_object(void *obj);
void *_gal_copy_object(void *obj);
void Gal_DoProperties(Gal_Frame fr,
		      int (*prop_fn)(const char *, Gal_Object, void *),
		      void *caller_data);
void Gal_DoPreds(Gal_Frame fr, int (*pred_fn)(Gal_Object, void *),
		 void *caller_data);

Gal_Object Gal_CreateInt16Object(void *data, int num_int_16,
				 int manage_memory);
Gal_Object Gal_CreateInt32Object(void *data, int num_int_32,
				 int manage_memory);
Gal_Object Gal_CreateInt64Object(void *data, int num_int_64,
				 int manage_memory);
Gal_Object Gal_CreateFloat32Object(void *data, int num_float_32,
				   int manage_memory);
Gal_Object Gal_CreateFloat64Object(void *data, int num_float_64,
				   int manage_memory);
/* object to value conversion */

Gal_Frame Gal_FrameValue(Gal_Object to);
Gal_Frame Gal_ClauseValue(Gal_Object to);
Gal_Frame Gal_TopicValue(Gal_Object to);
Gal_Frame Gal_PredValue(Gal_Object to);
char     *Gal_KeywordValue(Gal_Object to);
char     *Gal_StringValue(Gal_Object to);
int       Gal_IntValue(Gal_Object to);
float     Gal_FloatValue(Gal_Object to);
void     *Gal_PointerValue(Gal_Object to);

/* list object operations */
Gal_Object *Gal_ListValue(Gal_Object obj, int *length);
int         Gal_ListLength(Gal_Object to);
Gal_Object  Gal_GetListObject(Gal_Object to, int n);
int Gal_SetListObject(Gal_Object obj, int n, Gal_Object elt);
int Gal_ListObjectExpandable(Gal_Object obj);
void       *Gal_GetListValue(Gal_Object to, int n, Gal_ObjectType type);
int Gal_ListObjectAdd(Gal_Object obj, Gal_Object elt);

/* binary object operations */
void *Gal_BinaryValue(Gal_Object obj, int *size);
int   Gal_BinarySize(Gal_Object to);
void *Gal_Int16Value(Gal_Object obj, int *size);
int Gal_Int16Size(Gal_Object to);
void *Gal_Int32Value(Gal_Object obj, int *size);
int Gal_Int32Size(Gal_Object to);
void *Gal_Int64Value(Gal_Object obj, int *size);
int Gal_Int64Size(Gal_Object to);
void *Gal_Float32Value(Gal_Object obj, int *size);
int Gal_Float32Size(Gal_Object to);
void *Gal_Float64Value(Gal_Object obj, int *size);
int Gal_Float64Size(Gal_Object to);

int Gal_ObjectByteCount(Gal_Object obj);

/* object type printing */
char *Gal_ObjectTypeString(Gal_ObjectType object_type);

/* proxy object support */

GalIO_BrokerStruct *Gal_ProxyObjectBroker(Gal_Object obj);
Gal_ObjectType Gal_ProxyObjectType(Gal_Object obj);
Gal_Object Gal_ProxyObjectObject(Gal_Object obj);
int Gal_Proxyp(Gal_Object obj);
Gal_Object Gal_CreateProxyObject(GalSS_BrokerProxy *p,
				 int manage_memory);
Gal_Object Gal_ProxyObject(GalSS_BrokerProxy *p);
GalSS_BrokerProxy *Gal_ProxyValue(Gal_Object o);

#ifdef _GAL_DEBUG_MEMORY
#include "debug_memory.h"
#endif

#include "name_barrier.h"

#endif  /* _GALAXY_H */
