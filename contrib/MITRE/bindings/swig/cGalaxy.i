/* SAM 6/2/00: I embark on my attempt to use SWIG to generate the
   Python bindings, and eventually Tcl, Perl and Allegro. */

%module cGalaxy

/* SAM 9/6/01: Not sure I'm ready to upgrade to SWIG 1.3, so
   I've added symbols on the command lines in ../swig/swig.make.
   These next two defines transfer the SWIG symbols to the C code,
   so that the right things happen after SWIG is run. */

#ifdef SWIG11
%{
#define SWIG11
%}
#endif

#ifdef SWIG13
%{
#define SWIG13
%}
#endif

%{

#include "galaxy/galaxy_all.h"
#include "binding_support.h"

/* Working without a net. We will need to get the
   binary size and use it to create an object. Actually,
   I can't for the life of me figure out how to grab the
   elements on the out end and manipulate them. So I'll
   just return the object itself and dismantle it
   in the out. */

extern void *_Gal_ArrayValue(Gal_Object obj, int *size);

Gal_Object GBGal_ArrayValue(Gal_Object obj)
{
  return obj;
}
%} 

enum {GC_VERSION};

/* We begin with the the Galaxy core library: frames and objects. */

/* Gal_Frame types */
typedef enum
{
  GAL_NULLFRAME = 0,
  GAL_TOPIC,
  GAL_CLAUSE,
  GAL_PRED
} Gal_FrameType;

/* Gal_Object types. Hard wire the values for backwards compatibility */
typedef enum
{
  GAL_FREE = 0,
  GAL_FRAME,
  GAL_STRING,
  GAL_INT,
  GAL_FLOAT,
  GAL_SYMBOL,
  GAL_LIST,
  GAL_PTR,
  GAL_TOPIC_FRAME,
  GAL_CLAUSE_FRAME,
  GAL_PRED_FRAME,
  GAL_BINARY,
  GAL_INT_16,
  GAL_INT_32,
  GAL_INT_64,
  GAL_FLOAT_32,
  GAL_FLOAT_64,
  GAL_KEYWORD,
  GAL_TAG,
  GAL_TOKEN,
  GAL_PROXY
} Gal_ObjectType;

enum {GAL_PP_PRINT, GAL_PR_PRINT};

/* I need to convince SWIG that these are actually pointers. */

typedef struct _Gal_Frame *Gal_Frame;
typedef struct _Gal_Object *Gal_Object;

/* I will call this at load time, just to make sure. */

void Gal_InitializeStatics();

/* Function protos for file nframe.c */
/* frame allocation and copying, frame type, frame comparison */
Gal_Frame Gal_MakeFrame(char *name, Gal_FrameType type);
void      Gal_FreeFrame(Gal_Frame fr);
/* name and type operations */
Gal_FrameType Gal_GetFrameType(Gal_Frame fr);
char         *Gal_FrameName(Gal_Frame fr);
/* predicate operations */
Gal_Frame  Gal_AddPred(Gal_Frame fr, Gal_Frame pred);
Gal_Frame  Gal_GetPred(Gal_Frame fr, int i);
int        Gal_NumPreds(Gal_Frame fr);
/* property list operations */
Gal_Object  Gal_SetProp(Gal_Frame fr, char *key, Gal_Object val);
Gal_Object  Gal_GetObject(Gal_Frame fr, char *key);

/* Function protos for file tobj.c */

/* allocation and copying */
void       Gal_FreeObject(Gal_Object to);
void       Gal_FreeWrapper(Gal_Object to);

/* object type */
Gal_ObjectType Gal_GetObjectType(Gal_Object to);

/* value to object conversion */
/* formerly make_tfloat, make_tframe, etc. */
Gal_Object Gal_FloatObject(float value);
Gal_Object Gal_FrameObject(Gal_Frame value);
Gal_Object Gal_IntObject(int value);
Gal_Object Gal_SymbolObject(char *value);
Gal_Object Gal_StringObject(char *val);
int Gal_ListObjectAdd(Gal_Object obj, Gal_Object elt);
int Gal_ListLength(Gal_Object to);
Gal_Object Gal_GetListObject(Gal_Object obj, int n);
char *Gal_ObjectTypeString(Gal_ObjectType object_type);

Gal_Frame Gal_FrameValue(Gal_Object to);
char     *Gal_StringValue(Gal_Object to);
int       Gal_IntValue(Gal_Object to);
float     Gal_FloatValue(Gal_Object to);
char     *Gal_KeywordValue(Gal_Object to);

/* Function protos for file nfio.c */
Gal_Frame  Gal_ReadFrameFromString(char *buf);
Gal_Object Gal_ReadObjectFromString(char *buf);

/* Function protos for file pr_util.c */

char *Gal_ObjectToString(Gal_Object to);

/* Function protos for file dispatch_function.c */

enum {GAL_KEY_ALWAYS, GAL_KEY_SOMETIMES, GAL_OTHER_KEYS_MAYBE,
      GAL_OTHER_KEYS_NEVER, GAL_REPLY_PROVIDED, GAL_REPLY_NONE,
      GAL_REPLY_UNKNOWN};

Gal_DispatchFnSignatureKeyEntry *_Gal_CreateEmptyDispatchFnKeyArray(int i);
void _Gal_PopulateDispatchFnKeyArrayCell(Gal_DispatchFnSignatureKeyEntry *array, int index, char *key, Gal_ObjectType t, int obligatory);

/* Function protos for local headers. */

char *Gal_FrameToString(Gal_Frame fr, int how_to);

%typemap(python,out) Gal_PointerBuffer * {
  int i;
  int size = Gal_PointerBufferSize($source);
  char **names = (char **) Gal_PointerBufferPointers($source);
  
  $target = PyList_New(size);
  for (i = 0; i < size; i++) {
    PyList_SetItem($target, i, PyString_FromString(names[i]));
  }
  Gal_FreePointerBuffer($source);
}

Gal_PointerBuffer *GBGal_GetProperties(Gal_Frame fr);

%typemap(python,out) Gal_PointerBuffer *;

/* We've enabled dynamic list objects in the core library,
   so we don't need to do anything special here. */

Gal_Object GBGal_EmptyListObject();

/* In order to handle binary data, we need to convert the
   void * from a string. PyString_AsString doesn't copy,
   so I'd better. */

%typemap(python, in) void * {
  /* We need to copy all the memory, and strdup won't cut it. */
  int len = PyString_Size($source);
  char *data = (char *) malloc(len);
  
  memcpy(data, PyString_AsString($source), len);
  $target = (void *) data;
}

Gal_Object Gal_CreateBinaryObject(void *data, int size, int manage_memory);
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

%typemap(python, in) void *;

/* I can't figure out how to reconstruct the string from
   an outarg length, so GBGal_ArrayValue does everything
   in its out mapping. */

%typemap(python, out) Gal_Object {
  void *data;
  int len = Gal_ObjectByteCount($source);

  data = _Gal_ArrayValue($source, NULL);
  $target = PyString_FromStringAndSize(data, len);
}

Gal_Object GBGal_ArrayValue(Gal_Object obj);

%typemap(python, out) Gal_Object;

/* SAM 6/6/00: Step 2: the IO library. */

/* Snatch the types for the server status. */

enum {GAL_CONNECTION_LISTENER, GAL_BROKER_LISTENER,
      GAL_HUB_CLIENT, GAL_SERVER_TYPE_MASK,
      GAL_HUB_CLIENT_CONNECT_FAILURE_MASK,
      GAL_HUB_CLIENT_CONNECT_FAILURE_RETRY,
      GAL_HUB_CLIENT_CONNECT_FAILURE_NOOP,
      GAL_HUB_CLIENT_CONNECT_FAILURE_SHUTDOWN,
      GAL_HUB_CLIENT_DISCONNECT_MASK,
      GAL_HUB_CLIENT_DISCONNECT_RETRY,
      GAL_HUB_CLIENT_DISCONNECT_SHUTDOWN,
      GAL_HUB_CLIENT_DISCONNECT_NOOP};

/* ip_util.c */
/* return the IP address of the current host as a string */

char *GalIO_IPAddress();

/* io_msg_types.h */

enum {GAL_OBJECT_MSG_TYPE, GAL_MESSAGE_MSG_TYPE, GAL_REPLY_MSG_TYPE,
      GAL_DESTROY_MSG_TYPE,
      GAL_BROKER_START_MSG_TYPE, GAL_BROKER_END_MSG_TYPE,
      GAL_ERROR_MSG_TYPE, GAL_DISCONNECT_MSG_TYPE,
      GAL_POSTPONE_MSG_TYPE};

enum {GAL_APPLICATION_ERROR, GAL_NO_OPNAME_ERROR,
      GAL_TRANSMISSION_ERROR, GAL_RECEPTION_ERROR,
      GAL_SERVER_DOWN_ERROR,
      GAL_NO_FRAME_ERROR, GAL_CONN_REJECTION_ERROR};

/* hub_server.c */

%include typemaps.i

typedef int GalIO_MsgType;

Gal_Frame GBGal_ResultArrayFrame(GBGal_ResultArray *r);
int GBGal_ResultArrayStatus(GBGal_ResultArray *r);
GalIO_CommStruct *GBGal_ResultArrayCommStruct(GBGal_ResultArray *r);
GalIO_MsgType GBGal_ResultArrayMsgType(GBGal_ResultArray *r);
void GBGal_FreeResultArray(GBGal_ResultArray *r);

GBGal_ResultArray *GBGalIO_CommReadFrame(GalIO_CommStruct *gcomm, int do_block);
GBGal_ResultArray *GBGalIO_CommReadMessage(GalIO_CommStruct *gcomm, int do_block);
GBGal_ResultArray *GBGalIO_DispatchViaHub(GalIO_CommStruct *gcomm, Gal_Frame frame);
GBGal_ResultArray *GBGalSS_EnvDispatchFrame(GalSS_Environment *env,
					    Gal_Frame frame);
GBGal_ResultArray *GBGalIO_ClientConnect(char *name,
					 char *host, unsigned short port,
					 int silent,
					 Gal_Frame welcome_frame);
GBGal_ResultArray *GBGalIO_ServerCallbackHandler(GalIO_ServerStruct *gcomm, int read_blocking);

int GalIO_CommWriteFrame(GalIO_CommStruct *gcomm, Gal_Frame frame, int do_block);
int GalIO_CommWriteMessage(GalIO_CommStruct *gcomm, Gal_Frame frame, GalIO_MsgType msg_type, int do_block);
int GalIO_CommReadReady(GalIO_CommStruct *gcomm);
int GalIO_CommWriteReady(GalIO_CommStruct *gcomm);
int GalIO_ServerIsClient(GalIO_ServerStruct *gcomm);
int GalIO_ServerIsListener(GalIO_ServerStruct *gcomm);
void GalIO_ServerCheckHubContacts(GalIO_ServerStruct *gcomm);
void GalIO_DestroyServerStruct(GalIO_ServerStruct *gcomm);

/* This will be a disaster when we get to Windows, but we'll
   worry about it then. */

typedef int GAL_SOCKET;

GAL_SOCKET GalIO_GetCommSocket(GalIO_CommStruct *gcomm);
GAL_SOCKET GalIO_GetServerListenSocket(GalIO_ServerStruct *gcomm);
unsigned short GalIO_GetServerListenPort(GalIO_ServerStruct *gcomm);
void GalIO_EnableDispatchFnValidation(GalIO_ServerStruct *gcomm);
int GalIO_CommValidating(GalIO_CommStruct *gcomm);
void GalIO_SetServerName(GalIO_ServerStruct *gcomm, char *name);
void GalIO_SetServerMaxConnections(GalIO_ServerStruct *gcomm, int max);
unsigned short GalIO_GetServerDefaultPort(GalIO_ServerStruct *scomm);

int GalIO_ConnectionCallbackHandler(GalIO_CommStruct *gcomm,
				    int read_blocking);


/* Types for setting up the callbacks. */

enum {
  /* These take a server and a void *. */
  GAL_SERVER_LISTENER_STARTUP_EVENT,
  GAL_SERVER_LISTENER_SHUTDOWN_EVENT,
  GAL_SERVER_CLIENT_POLL_STARTUP_EVENT,
  GAL_SERVER_DESTRUCTION_EVENT,
  /* This takes a server and a connection and a void *. */
  GAL_SERVER_CONNECTION_CREATION_EVENT,
  /* These take a connection and a broker and a void *. */
  GAL_CONNECTION_BROKER_OUT_STARTUP_EVENT,
  GAL_CONNECTION_BROKER_IN_STARTUP_EVENT,
  GAL_CONNECTION_BROKER_OUT_CREATION_EVENT,
  GAL_CONNECTION_BROKER_IN_CREATION_EVENT,
  /* This takes a frame and an environment. */
  GAL_CONNECTION_DISPATCH_FN_EVENT,
  /* This takes a connection and a void *. */
  GAL_CONNECTION_SHUTDOWN_EVENT,
  GAL_CONNECTION_DESTRUCTION_EVENT,
  /* These take a broker and a void *. */
  GAL_BROKER_DATA_DONE_EVENT,
  GAL_BROKER_ABORT_EVENT,
  GAL_BROKER_DESTRUCTION_EVENT,
  GAL_BROKER_CONNECTION_EVENT
};

%{

/* This is for all callbacks. I'm copying this from Tkinter. */

/* static int errorInCmd;
static PyObject *excInCmd;
static PyObject *valInCmd;
static PyObject *trbInCmd; */

static void __PyAcknowledgeError()
{
  /* errorInCmd = 1;
     PyErr_Fetch(&excInCmd, &valInCmd, &trbInCmd); */
  GalUtil_Print(-1, "Error in Python callback:\n");
  PyErr_Print();
}

static void __PyConnectionCallback(GalIO_CommStruct *gcomm, void *caller_data)
{
  PyObject *callback_func = (PyObject *) caller_data;
  PyObject *result;
  PyObject *arglist = Py_BuildValue("()");
  
  result = PyEval_CallObject(callback_func, arglist);

  Py_DECREF(arglist);
  
  if (result == Py_None) {
    /* If the result is None, don't do anything. */
    return;
  } else if (result) {
    Py_DECREF(result);
    return;
  } else {
    /* Something went seriously wrong. We should
       signal the error when possible, but right now I'm just
       going to print it. */
    __PyAcknowledgeError();
    return;
  }
}

static void __galio_connection_py_decref(GalIO_CommStruct *gcomm,
					 void *caller_data)
{
  Py_DECREF((PyObject *) caller_data);
}

static void __PyConnectionDecrefCallback(GalIO_CommStruct *gcomm,
					 void *caller_data)
{
  __PyConnectionCallback(gcomm, caller_data);
  __galio_connection_py_decref(gcomm, caller_data);
}

/* Make sure you do the right thing when the event
   is a destruction event. Don't rely on the order in
   which the callbacks are declared. */

void
GalIO_PyAddConnectionCallback(GalIO_CommStruct *gcomm,
			      int callback_event,
			      PyObject *connect_callback)
{
  if (callback_event != GAL_CONNECTION_DESTRUCTION_EVENT) {
    GalIO_AddConnectionCallback(gcomm, callback_event,
				__PyConnectionCallback,
				(void *) connect_callback);
    GalIO_AddConnectionCallback(gcomm, GAL_CONNECTION_DESTRUCTION_EVENT,
				__galio_connection_py_decref,
				(void *) connect_callback);
    Py_INCREF(connect_callback);
  } else {
    GalIO_AddConnectionCallback(gcomm, callback_event,
				__PyConnectionDecrefCallback,
				(void *) connect_callback);
    Py_INCREF(connect_callback);
  }
}

static void __PyConnectCallback(GalIO_ServerStruct *scomm,
				GalIO_CommStruct *gcomm,
				void *callback_data)
{
  PyObject *connect_func = (PyObject *) callback_data;
  PyObject *result;
  PyObject *arglist;
  PyObject *swig_ptr_obj;
#ifdef SWIG11
  char swig_ptr[128];

  SWIG_MakePtr(swig_ptr, (void *) gcomm, "_GalIO_CommStruct_p");
  swig_ptr_obj = PyString_FromString(swig_ptr);
#endif
#ifdef SWIG13
  swig_ptr_obj = SWIG_NewPointerObj((void *) gcomm,
				    SWIGTYPE_p_GalIO_CommStruct);
#endif
  arglist = Py_BuildValue("(O)", swig_ptr_obj);

  result = PyEval_CallObject(connect_func, arglist);
  Py_DECREF(arglist);

  if (result == Py_None) {
    /* If the result is None, don't do anything. */
    return;
  } else if (result) {
    Py_DECREF(result);
    return;
  } else {    
    /* Something went seriously wrong. We should
       signal the error when possible, but right now I'm just
       going to print it. */
    __PyAcknowledgeError();
    return;
  }
}

static void __galio_server_py_decref(GalIO_ServerStruct *scomm,
				     void *caller_data)
{
  Py_DECREF((PyObject *) caller_data);
}

void GalIO_PyAddServerConnectCallback(GalIO_ServerStruct *scomm,
				      PyObject *connect_callback)
{
  GalIO_AddServerConnectCallback(scomm, __PyConnectCallback,
				 (void *) connect_callback);
  GalIO_AddServerCallback(scomm, GAL_SERVER_DESTRUCTION_EVENT,
			  __galio_server_py_decref,
			  (void *) connect_callback);
  Py_INCREF(connect_callback);
}

static void __PyServerCallback(GalIO_ServerStruct *scomm, void *caller_data)
{
  PyObject *callback_func = (PyObject *) caller_data;
  PyObject *result;
  PyObject *arglist = Py_BuildValue("()");
  
  result = PyEval_CallObject(callback_func, arglist);

  Py_DECREF(arglist);
  
  if (result == Py_None) {
    /* If the result is None, don't do anything. */
    return;
  } else if (result) {
    Py_DECREF(result);
    return;
  } else {
    /* Something went seriously wrong. We should
       signal the error when possible, but right now I'm just
       going to print it. */
    __PyAcknowledgeError();
    return;
  }
}

static void __PyServerDecrefCallback(GalIO_ServerStruct *scomm,
				     void *caller_data)
{
  __PyServerCallback(scomm, caller_data);
  __galio_server_py_decref(scomm, caller_data);
}

/* Make sure you do the right thing for when the
   event is a server destruction event. Don't rely on the
   order in which callbacks are added. */

void GalIO_PyAddServerCallback(GalIO_ServerStruct *scomm,
			       int callback_event,
			       PyObject *server_callback)
{
  if (callback_event != GAL_SERVER_DESTRUCTION_EVENT) {
    GalIO_AddServerCallback(scomm, callback_event,
			    __PyServerCallback,
			    (void *) server_callback);
    GalIO_AddServerCallback(scomm, GAL_SERVER_DESTRUCTION_EVENT,
			    __galio_server_py_decref,
			    (void *) server_callback);
    Py_INCREF(server_callback);
  } else {
    GalIO_AddServerCallback(scomm, callback_event,
			    __PyServerDecrefCallback,
			    (void *) server_callback);
    Py_INCREF(server_callback);
  }
}

%}

%typemap (python, in) PyObject *server_callback, PyObject *connect_callback {
  if (!PyCallable_Check($source)) {
    PyErr_SetString(PyExc_TypeError, "not a callable object");
    return NULL;
  }
  $target = $source;
}

void GalIO_PyAddConnectionCallback(GalIO_CommStruct *gcomm,
				   int callback_event,
				   PyObject *connect_callback);
void GalIO_PyAddServerConnectCallback(GalIO_ServerStruct *scomm,
				      PyObject *connect_callback);
void GalIO_PyAddServerCallback(GalIO_ServerStruct *scomm,
			       int callback_event,
			       PyObject *server_callback);
void GalIO_DestroyCommStruct(GalIO_CommStruct *gcomm);
void GalIO_SetCommDone(GalIO_CommStruct *gcomm);

%typemap (python, in) PyObject *server_callback, PyObject *connect_callback;

/* broker_data.c */

void GalIO_BrokerDataOutDone(GalIO_BrokerStruct *b);
int GalIO_BrokerDataOutCallbackHandler(GalIO_BrokerStruct *b);
int GalIO_BrokerReadReady(GalIO_BrokerStruct *b);
int GalIO_BrokerWriteReady(GalIO_BrokerStruct *b);
void GalIO_DestroyBrokerStruct(GalIO_BrokerStruct *b);

/* These can all have null bytes in the string, so I think I
   need to use a typemap. */

%typemap (python, in) void *data {
  $target = (void *) PyString_AsString($source);
}

int GalIO_BrokerWriteBinary(GalIO_BrokerStruct *b, void *data, int n_bytes);
int GalIO_BrokerWriteInt16(GalIO_BrokerStruct *b, void *data, int n_ints);
int GalIO_BrokerWriteInt32(GalIO_BrokerStruct *b, void *data, int n_ints);
int GalIO_BrokerWriteInt64(GalIO_BrokerStruct *b, void *data, int n_ints);
int GalIO_BrokerWriteFloat32(GalIO_BrokerStruct *b, void *data, int n_floats);
int GalIO_BrokerWriteFloat64(GalIO_BrokerStruct *b, void *data, int n_floats);

%typemap(python, in) void *data;

int GalIO_BrokerWriteFrame(GalIO_BrokerStruct *b, Gal_Frame frame);
int GalIO_BrokerWriteString(GalIO_BrokerStruct *b, char *str);
int GalIO_BrokerWriteObject(GalIO_BrokerStruct *b, Gal_Object o);

/* And now, we need to handle the callbacks for the brokering stuff.
   The broker caller data will be the Python callable, which will get
   the type and the object.
*/

%{

static void __PyBrokerCallback(GalIO_BrokerStruct *b, void *caller_data)
{
  PyObject *callback_func = (PyObject *) caller_data;
  PyObject *result;
  PyObject *arglist = Py_BuildValue("()");
  
  result = PyEval_CallObject(callback_func, arglist);

  Py_DECREF(arglist);
  
  if (result == Py_None) {
    /* If the result is None, don't do anything. */
    return;
  } else if (result) {
    Py_DECREF(result);
    return;
  } else {
    /* Something went seriously wrong. We should
       signal the error when possible, but right now I'm just
       going to print it. */
    __PyAcknowledgeError();
    return;
  }
}

static void __galio_broker_py_decref(GalIO_BrokerStruct *b,
				     void *caller_data)
{
  Py_DECREF((PyObject *) caller_data);
}

static void __PyBrokerDecrefCallback(GalIO_BrokerStruct *b, void *caller_data)
{
  __PyBrokerCallback(b, caller_data);
  __galio_broker_py_decref(b, caller_data);
}  

void
GalIO_PyAddBrokerCallback(GalIO_BrokerStruct *b,
			  int callback_event,
			  PyObject *broker_callback)
{
  /* I need to do something special if the event is
     a broker destruction event. The callbacks are
     guaranteed to be evaluated in the order in which
     they appear, but I really don't want to rely on that. */
  if (callback_event != GAL_BROKER_DESTRUCTION_EVENT) {
    GalIO_AddBrokerCallback(b, callback_event,
			    __PyBrokerCallback,
			    (void *) broker_callback);
    GalIO_AddBrokerCallback(b, GAL_BROKER_DESTRUCTION_EVENT,
			    __galio_broker_py_decref,
			    (void *) broker_callback);
    Py_INCREF(broker_callback);
  } else {
    /* When it's a destruction event, I want to do the decref
       at the same time. So I call a special callback. This
       will even work if I pass the same callback to multiple
       functions, since it's incref'ed each time. */
    GalIO_AddBrokerCallback(b, callback_event,
			    __PyBrokerDecrefCallback,
			    (void *) broker_callback);
    Py_INCREF(broker_callback);
  }
}

/* Since broker callbacks are responsible for freeing the
   data that's passed in, we need to take responsibility
   for that here. You don't want to free the frame, though,
   since it's a pointer into C, not a native object, and it isn't
   done being used until the callback copies it. So the frame
   gets freed AFTER the call. Remember, the frames are
   entirely internal to Python, and I don't have to worry
   about having copies stored, since the frames don't keep
   around the C frames. */

/* 11/10/00: I've decided that it's driving me nuts to have
   the types be all over the place. I'm going to build a
   Gal_Object and pass that in consistently. */

extern char *sym_name(Sym sp);

static void __PyBrokerReaderCallback(GalIO_BrokerStruct *broker_struct,
				     void *data, Gal_ObjectType data_type,
				     int n_samples)
{
  PyObject *arglist, *result;
  /* These need to be allocated. */
  Gal_Object o = (Gal_Object) NULL;
  PyObject *func = (PyObject *) GalIO_GetBrokerData(broker_struct);

  /* I want to get the pointers from f and env and pass them in.
     I "just happen to know" the names of the types; SWIG doesn't
     seem yet to "publish" them at all. */
  switch (data_type) {    
  case GAL_INT_16:
    o = Gal_CreateInt16Object(data, n_samples, 1);
    break;
  case GAL_INT_32:    
    o = Gal_CreateInt32Object(data, n_samples, 1);
    break;
  case GAL_FLOAT_32:    
    o = Gal_CreateFloat32Object(data, n_samples, 1);
    break;    
  case GAL_FLOAT_64:
    o = Gal_CreateFloat64Object(data, n_samples, 1);
    break;
  case GAL_BINARY:    
    o = Gal_CreateBinaryObject(data, n_samples, 1);
    break;
  case GAL_STRING:
    o = Gal_CreateStringObject((char *) data, 1);
    break;
  case GAL_SYMBOL:
    o = Gal_SymbolObject(sym_name((Gal_Symbol) data));
    break;
  case GAL_INT:
    o = Gal_IntObject((int) data);
    break;
  case GAL_FLOAT:
    o = Gal_FloatObject(*((float *) data));
    free(data);
    break;
  case GAL_LIST:
    /* In this special case, I need to build a list object,
       because that's pretty much the only way Python can
       deal with this (uncode it, for instance). */
    o = Gal_ListObject((Gal_Object *) data, n_samples);
    break;
  case GAL_FRAME:
    o = Gal_FrameObject((Gal_Frame) data);
    break;
  case GAL_PROXY:
    o = Gal_ProxyObject((GalSS_BrokerProxy *) data);
    break;
  default:
    break;
  }
  if (o) {
    PyObject *o_pointer_obj;
#ifdef SWIG11
    char o_pointer[128];

    SWIG_MakePtr(o_pointer, (void *) o, "_Gal_Object");
    o_pointer_obj = PyString_FromString(o_pointer);
#endif
#ifdef SWIG13
    o_pointer_obj = SWIG_NewPointerObj((void *) o,
				       SWIGTYPE_Gal_Object);
#endif
    arglist = Py_BuildValue("(iO)", data_type, o_pointer_obj);
    result = PyEval_CallObject(func, arglist);
    Gal_FreeObject(o);
    Py_DECREF(arglist);
  } else {
    PyErr_SetString(PyExc_TypeError, "not a broker readable object");
    result = NULL;
  }
  
  if (result == Py_None) {
    /* If the result is None, don't do anything. */
    return;
  } else if (result) {
    Py_DECREF(result);
    return;
  } else {    
    /* Something went seriously wrong. We should
       signal the error when possible, but right now I'm just
       going to print it. */
    __PyAcknowledgeError();
    return;
  }
}

static void __PyBrokerFinalizer(void *caller_data)
{
  Py_DECREF((PyObject *) caller_data);
}

GalIO_BrokerStruct *GalIO_PyCommBrokerDataInInit(GalIO_CommStruct *gcomm,
						 char *host, unsigned short port,
						 Gal_Frame frame,
						 PyObject *callback_fnptr)
{

  Py_INCREF(callback_fnptr);
  return GalIO_CommBrokerDataInInit(gcomm, host, port, frame,
				    __PyBrokerReaderCallback,
				    -1, (void *) callback_fnptr,
				    __PyBrokerFinalizer);
}

GalIO_BrokerStruct *GalIO_PyBrokerDataOutInit(GalIO_CommStruct *gcomm,
					      int timeout_seconds)
{
  return GalIO_BrokerDataOutInit(gcomm, -1, timeout_seconds);
}

/* For the environment objects. */

GalIO_BrokerStruct *
GalSS_PyEnvBrokerDataInInit(GalSS_Environment *env,
			    char *host, unsigned short port,
			    Gal_Frame frame,
			    PyObject *callback_fnptr)
{
  Py_INCREF(callback_fnptr);
  return GalSS_EnvBrokerDataInInit(env, host, port, frame,
				   __PyBrokerReaderCallback,
				   -1, (void *) callback_fnptr,
				   __PyBrokerFinalizer);
}

GalIO_BrokerStruct *
GalSS_PyEnvBrokerProxyInInit(GalSS_Environment *env,
			     GalSS_BrokerProxy *p,
			     PyObject *callback_fnptr)
{
  Py_INCREF(callback_fnptr);
  return GalSS_EnvBrokerProxyInInit(env, p,
				    __PyBrokerReaderCallback,
				    -1, (void *) callback_fnptr,
				    __PyBrokerFinalizer);
}

/* And now, for the unproxification callbacks. */

typedef struct __PyProxyCallbacks {
  PyObject *callback_fnptr;
  PyObject *data_done_fnptr;
  PyObject *abort_fnptr;
} __PyProxyCallbacks;

static void __free_py_proxy_callbacks(void *data)
{
  __PyProxyCallbacks *ppc = (__PyProxyCallbacks *) data;
  Py_DECREF(ppc->callback_fnptr);
  Py_DECREF(ppc->data_done_fnptr);
  Py_DECREF(ppc->abort_fnptr);
  free(ppc);
}

static void
__PyProxyDataHandlerCallback(GalSS_Environment *env,
			     Gal_ObjectType proxy_type,
			     Gal_Object elt,
			     void *caller_data)
{
  __PyProxyCallbacks *ppc = (__PyProxyCallbacks *) caller_data;
  PyObject *o_pointer_obj;
  PyObject *result;
  PyObject *arglist;
#ifdef SWIG11
  char o_pointer[128];  

  SWIG_MakePtr(o_pointer, (void *) elt, "_Gal_Object");
  o_pointer_obj = PyString_FromString(o_pointer);
#endif
#ifdef SWIG13
  o_pointer_obj = SWIG_NewPointerObj((void *) elt,
				     SWIGTYPE_Gal_Object);
#endif

  arglist = Py_BuildValue("(O)", o_pointer_obj);
  
  result = PyEval_CallObject(ppc->callback_fnptr, arglist);

  Gal_FreeObject(elt);
  Py_DECREF(arglist);
  
  if (result == Py_None) {
    /* If the result is None, don't do anything. */
    return;
  } else if (result) {
    Py_DECREF(result);
    return;
  } else {
    /* Something went seriously wrong. We should
       signal the error when possible, but right now I'm just
       going to print it. */
    __PyAcknowledgeError();
    return;
  }
}

static void 
__PyProxyDataDoneCallback(GalSS_Environment *env,
			  Gal_ObjectType proxy_type,
			  void *caller_data)
{
  __PyProxyCallbacks *ppc = (__PyProxyCallbacks *) caller_data;
  PyObject *result;
  PyObject *arglist = Py_BuildValue("()");
  
  result = PyEval_CallObject(ppc->data_done_fnptr, arglist);

  Py_DECREF(arglist);
  
  if (result == Py_None) {
    /* If the result is None, don't do anything. */
    return;
  } else if (result) {
    Py_DECREF(result);
    return;
  } else {
    /* Something went seriously wrong. We should
       signal the error when possible, but right now I'm just
       going to print it. */
    __PyAcknowledgeError();
    return;
  }
}

static void 
__PyProxyAbortCallback(GalSS_Environment *env,
		       Gal_ObjectType proxy_type,
		       void *caller_data)
{
  __PyProxyCallbacks *ppc = (__PyProxyCallbacks *) caller_data;

  PyObject *result;
  PyObject *arglist = Py_BuildValue("()");
  
  result = PyEval_CallObject(ppc->abort_fnptr, arglist);

  Py_DECREF(arglist);
  
  if (result == Py_None) {
    /* If the result is None, don't do anything. */
    return;
  } else if (result) {
    Py_DECREF(result);
    return;
  } else {
    /* Something went seriously wrong. We should
       signal the error when possible, but right now I'm just
       going to print it. */
    __PyAcknowledgeError();
    return;
  }
}

void GalSS_PyUnproxify(GalSS_Environment *env,
		       GalSS_BrokerProxy *p,
		       PyObject *callback_fnptr,
		       PyObject *data_done_fnptr,
		       PyObject *abort_fnptr,
		       int immediate)
{
  __PyProxyCallbacks *ppc = (__PyProxyCallbacks *) calloc(1, sizeof(__PyProxyCallbacks));
  Py_INCREF(callback_fnptr);
  Py_INCREF(data_done_fnptr);
  Py_INCREF(abort_fnptr);
  ppc->callback_fnptr = callback_fnptr;
  ppc->data_done_fnptr = data_done_fnptr;
  ppc->abort_fnptr = abort_fnptr;
  GalSS_Unproxify(env, p, __PyProxyDataHandlerCallback,
		  __PyProxyDataDoneCallback,
		  __PyProxyAbortCallback, immediate, -1,
		  (void *) ppc, __free_py_proxy_callbacks);
}

%}

%typemap (python, in) PyObject *callback_fnptr, PyObject *broker_callback, PyObject *data_done_fnptr, PyObject *abort_fnptr {
  if (!PyCallable_Check($source)) {
    PyErr_SetString(PyExc_TypeError, "not a callable object");
    return NULL;
  }
  $target = $source;
}  
GalIO_BrokerStruct *GalIO_PyCommBrokerDataInInit(GalIO_CommStruct *gcomm,
						 char *host, unsigned short port,
						 Gal_Frame frame,
						 PyObject *callback_fnptr);
GalIO_BrokerStruct *GalIO_PyBrokerDataOutInit(GalIO_CommStruct *gcomm,
					      int timeout_seconds);
GalIO_BrokerStruct *
GalSS_PyEnvBrokerDataInInit(GalSS_Environment *env,
			    char *host, unsigned short port,
			    Gal_Frame frame,
			    PyObject *callback_fnptr);
void
GalIO_PyAddBrokerCallback(GalIO_BrokerStruct *b,
			  int callback_event,
			  PyObject *broker_callback);
GalSS_Environment *GalSS_BrokerGetEnvironment(GalIO_BrokerStruct *b);
GalIO_BrokerStruct *
GalSS_PyEnvBrokerProxyInInit(GalSS_Environment *env,
			     GalSS_BrokerProxy *p,
			     PyObject *callback_fnptr);
void GalSS_PyUnproxify(GalSS_Environment *env,
		       GalSS_BrokerProxy *p,
		       PyObject *callback_fnptr,
		       PyObject *data_done_fnptr,
		       PyObject *abort_fnptr,
		       int immediate);

%typemap (python, in) PyObject *callback_fnptr, PyObject *broker_callback, PyObject *data_done_fnptr, PyObject *abort_fnptr;

int GalIO_BrokerDataInCallbackHandler(GalIO_BrokerStruct *b,
				      int read_blocking);
void GalIO_SetBrokerActive(GalIO_BrokerStruct *b);
void GalIO_BrokerDataDone(GalIO_BrokerStruct *b);
void GalIO_ForceBrokerExpiration(GalIO_BrokerStruct *b);

GAL_SOCKET GalIO_GetBrokerSocket(GalIO_BrokerStruct *b);
GAL_SOCKET GalIO_GetBrokerListenSocket(GalIO_BrokerStruct *b);
unsigned short GalIO_GetBrokerListenPort(GalIO_BrokerStruct *b);
char *GalIO_GetBrokerCallID(GalIO_BrokerStruct *b);
/* generic-server.c */

GalIO_CommStruct *GalSS_EnvComm(GalSS_Environment *env);
int GalSS_EnvError(GalSS_Environment *env, char *description);
int GalSS_EnvErrorOfType(GalSS_Environment *env, int type, char *description);
int GalSS_EnvDestroyToken(GalSS_Environment *env);
int GalSS_EnvReply(GalSS_Environment *env, Gal_Frame f);

GalSS_Environment *GalSS_EnvCreate(GalIO_CommStruct *gcomm);
void GalSS_EnvUpdateSessionID(GalSS_Environment *env, char *session_id);
int GalSS_EnvReturnRequired(GalSS_Environment *env);

%typemap (python, out) char * {
  if ($source == (char *) NULL) {
    $target = Py_None;
    Py_INCREF(Py_None);
  } else {
    $target = PyString_FromString($source);
  }
}

char *GalSS_EnvGetSessionID(GalSS_Environment *env);

%typemap (python, out) char *;

/* generic-server-main.c */

enum {GAL_LOOP_TT, GAL_LOOP_THREADS, GAL_LOOP_EXTERNAL};

/* Function protos for frame_util.c */

int GalSS_EnvWriteFrame(GalSS_Environment *env,
			Gal_Frame frame, int do_block);
void GalSS_InitializeServerDefaults(GalIO_ServerStruct *gcomm, char *name, unsigned short port);

%typemap (python, in) char *provider {
  if ($source == Py_None) {
    $target = (char *) NULL;
  } else if (!PyString_Check($source)) {
    PyErr_SetString(PyExc_TypeError, "not a string");
    return NULL;
  } else {
    $target = PyString_AsString($source);
  }
}

GBGal_ResultArray *GBGalSS_EnvDispatchFrameToProvider(GalSS_Environment *env,
						      Gal_Frame frame,
        					      char *provider);
int GalSS_EnvWriteFrameToProvider(GalSS_Environment *env,
			          Gal_Frame frame, char *provider, 
                                  int do_block);

%typemap (python, in) char *provider;

%typemap (python, out) char * {
  if ($source == (char *) NULL) {
    $target = Py_None;
    Py_INCREF(Py_None);
  } else {
    $target = PyString_FromString($source);
  }
}

char *GalSS_EnvGetOriginatingProvider(GalSS_Environment *env);

%typemap (python, out) char *;

void GalSS_EnvLock(GalSS_Environment *env);
void GalSS_EnvUnlock(GalSS_Environment *env);
void GalSS_EnvInheritTokenTimestamp(GalSS_Environment *env);
double GalSS_EnvGetTokenTimestamp(GalSS_Environment *env);

/* The allocated memory really needs to be freed here. */

%typemap(python,in) char **INPUT(char **names) {
  int size, i;

  if ($source == Py_None) {
    names = (char **) NULL;
  } else if (!PyList_Check($source)) {
    PyErr_SetString(PyExc_TypeError, "not a list");
    return NULL;
  } else {
    size = PyList_Size($source);
    names = (char **) calloc(size + 1, sizeof(char *));
    for (i = 0; i < size; i++) {
      PyObject *elt = PyList_GetItem($source, i);
      if (!PyString_Check(elt)) {
	PyErr_SetString(PyExc_TypeError, "not a string");
	return NULL;
      }
      names[i] = PyString_AsString(elt);
    }
  }
  $target = names;
}

%typemap(python,freearg) char **INPUT {
  if (names)
    free(names);
}

Gal_Frame GalSS_EnvGetSessionProperties(GalSS_Environment *env, char **INPUT);
void GalSS_EnvModifySessionProperties(GalSS_Environment *env,
				      Gal_Frame properties_to_set,
				      char **INPUT);
Gal_Frame GalSS_EnvGetServerProperties(GalSS_Environment *env, char **INPUT);
void GalSS_EnvModifyServerProperties(GalSS_Environment *env,
				     Gal_Frame properties_to_set,
				     char **INPUT);
void GalIO_ServerModifyProperties(GalIO_ServerStruct *server,
				  Gal_Frame new_properties,
				  char **INPUT);

enum {GAL_SERVER_READS_ONLY_FROM_SESSION,
      GAL_SESSION_WRITES_ONLY_TO_SERVER,
      GAL_SERVER_WRITES_ONLY_TO_SESSION,
      GAL_PERMANENT_LOCK};

%typemap(python,in) char **INPUT;
%typemap(python,freearg) char **INPUT;

void GalSS_EnvSetSession(GalSS_Environment *env, char *session_name, int lock_info);

/* Function protos for continuation.c */

%{

static void __py_free_continuation_fn(void *arg)
{
  Py_DECREF((PyObject *) arg);
}

#ifdef SWIG11
static int __PySWIG_GetPtr(PyObject *py_obj, void **dest, char *type)
{
  if (!PyString_Check(py_obj)) {
    /* Not a string. */
    *dest = (void *) NULL;
    return 0;
  }
  if (SWIG_GetPtr(PyString_AsString(py_obj), dest, type)) {
    *dest = (void *) NULL;
    return 0;
  } else {
    return 1;
  }
}
#endif

Gal_Frame __PyDoContinuation(Gal_Frame f, GalIO_MsgType msg_type,
			     GalSS_Environment *env, void *continuation_state)
{
  PyObject *func = (PyObject *) continuation_state;
  PyObject *arglist, *result;
  /* Assemble the arglist. Nost of this can come right from
     __PyDispatchFn. */
  Gal_Frame f_result = (Gal_Frame) NULL;
  PyObject *f_pointer_obj, *env_pointer_obj;
  /* This will need to be freed. */
  
  /* I want to get the pointers from f and env and pass them in.
     I "just happen to know" the names of the types; SWIG doesn't
     seem yet to "publish" them at all. */
#ifdef SWIG11
  char f_pointer[128];
  char env_pointer[128];

  SWIG_MakePtr(f_pointer, (void *) f, "_Gal_Frame");
  f_pointer_obj = PyString_FromString(f_pointer);
  SWIG_MakePtr(env_pointer, (void *) env, "_GalSS_Environment_p");
  env_pointer_obj = PyString_FromString(env_pointer);
#endif
#ifdef SWIG13
  f_pointer_obj = SWIG_NewPointerObj((void *) f,
				     SWIGTYPE_Gal_Frame);
  env_pointer_obj = SWIG_NewPointerObj((void *) env,
				       SWIGTYPE_p_GalSS_Environment);
#endif
  arglist = Py_BuildValue("(OOi)", f_pointer_obj, env_pointer_obj, msg_type);
  result = PyEval_CallObject(func, arglist);
  Py_DECREF(arglist);
  if (result == Py_None) {
    /* If the result is None, return NULL. */
    return (Gal_Frame) NULL;
  } else if (result) {
    /* If there's a result, return it. */
#ifdef SWIG11
    if (!__PySWIG_GetPtr(result, (void **) &f_result, "_Gal_Frame")) {
      PyErr_SetString(PyExc_TypeError, "not a frame");
      __PyAcknowledgeError();
    }
#endif
#ifdef SWIG13
    if (SWIG_ConvertPtr(result, (void **) &f_result, SWIGTYPE_Gal_Frame, 1) == -1) {      
       __PyAcknowledgeError();
    }
#endif
    Py_DECREF(result);
    return f_result;    
  } else {
    /* Something went seriously wrong. We should
       signal the error when possible, but right now I'm just
       going to print it. */
    __PyAcknowledgeError();
    return (Gal_Frame) NULL;
  }
}

int GalSS_PyEnvDispatchFrameWithContinuation(GalSS_Environment *env,
					     Gal_Frame frame,
					     PyObject *continuation_fn)
{
  int res = GalSS_EnvDispatchFrameWithContinuation(env, frame,
						   __PyDoContinuation,
						   (void *) continuation_fn,
						   __py_free_continuation_fn);
  /* If sending the continuation failed, there's no need to
     incref the continuation function. */
  if (res != -1) {
    Py_INCREF(continuation_fn);
  }
  return res;
}

int GalSS_PyEnvDispatchFrameToProviderWithContinuation(GalSS_Environment *env,
						       Gal_Frame frame,
						       char *provider, 
					               PyObject *continuation_fn)
{
  int res = GalSS_EnvDispatchFrameToProviderWithContinuation(env, frame,
                                                             provider,
						   __PyDoContinuation,
						   (void *) continuation_fn,
						   __py_free_continuation_fn);
  /* If sending the continuation failed, there's no need to
     incref the continuation function. */
  if (res != -1) {
    Py_INCREF(continuation_fn);
  }
  return res;
}

%}

int GalSS_PyEnvDispatchFrameWithContinuation(GalSS_Environment *env,
					     Gal_Frame frame,
					     PyObject *continuation_fn);

%typemap (python, in) char *provider {
  if ($source == Py_None) {
    $target = (char *) NULL;
  } else if (!PyString_Check($source)) {
    PyErr_SetString(PyExc_TypeError, "not a string");
    return NULL;
  } else {
    $target = PyString_AsString($source);
  }
}

int GalSS_PyEnvDispatchFrameToProviderWithContinuation(GalSS_Environment *env,
						       Gal_Frame frame,
						       char *provider, 
					               PyObject *continuation_fn);

%typemap (python, in) char *provider;

%{

/* For setting dispatch functions, I need to create a dispatch
   function which has the Communicator signature. I also need to
   tuck away the Python dispatch function using my newly defined
   client data. There will always be a dispatch function.

   In the adding insult to injury department, I need to figure
   out how to retrieve the name that the function will be stored
   under. I'll have to get it out of the frame itself, dammit.
   This would be so much easier if I could just change the
   function signatures...
*/

static
Gal_Frame __PyDispatchFn(Gal_DispatchFnInvocation *i)
{
  GalSS_Environment *env = i->env;
  Gal_Frame f = i->frame;
  PyObject *arglist, *result;
  Gal_Frame f_result = (Gal_Frame) NULL;
  PyObject *func = (PyObject *) i->call_client_data;
  PyObject *f_pointer_obj, *env_pointer_obj;
#ifdef SWIG11
  /* These need to be allocated. */
  char f_pointer[128], env_pointer[128];

  /* I want to get the pointers from f and env and pass them in.
     I "just happen to know" the names of the types; SWIG doesn't
     seem yet to "publish" them at all. */
  SWIG_MakePtr(f_pointer, (void *) f, "_Gal_Frame");
  f_pointer_obj = PyString_FromString(f_pointer);
  SWIG_MakePtr(env_pointer, (void *) env, "_GalSS_Environment_p");
  env_pointer_obj = PyString_FromString(env_pointer);
#endif
#ifdef SWIG13
  f_pointer_obj = SWIG_NewPointerObj((void *) f,
				     SWIGTYPE_Gal_Frame);
  env_pointer_obj = SWIG_NewPointerObj((void *) env,
				       SWIGTYPE_p_GalSS_Environment);
#endif
  arglist = Py_BuildValue("(OO)", f_pointer_obj, env_pointer_obj);
  result = PyEval_CallObject(func, arglist);
  Py_DECREF(func);
  i->call_client_data = (void *) NULL;
  Py_DECREF(arglist);
  if (result == Py_None) {
    /* If the result is None, return NULL. */
    return (Gal_Frame) NULL;
  } else if (result) {
    /* If there's a result, return it. */
#ifdef SWIG11
    if (!__PySWIG_GetPtr(result, (void **) &f_result, "_Gal_Frame")) {
      PyErr_SetString(PyExc_TypeError, "not a frame");
      __PyAcknowledgeError();
    }
#endif
#ifdef SWIG13
    if (SWIG_ConvertPtr(result, (void **) &f_result,
			SWIGTYPE_Gal_Frame, 1) == -1) {
      __PyAcknowledgeError();
    }
#endif
    Py_DECREF(result);
    return f_result;    
  } else {
    /* Something went seriously wrong. We should
       signal the error when possible, but right now I'm just
       going to print it. */
    __PyAcknowledgeError();
    return (Gal_Frame) NULL;
  }
}

#if 0
void GalSS_AddPyDispatchFunction(GalIO_ServerStruct *i, char *name,
				 PyObject *fn,
				 Gal_DispatchFnSignatureKeyEntry *in_key_array,
				 int allow_other_in_keys, 
				 int reply_provided,
				 Gal_DispatchFnSignatureKeyEntry *out_key_array,
				 int allow_other_out_keys)
{
  GalSS_AddDispatchFunction(i, name, __PyDispatchFn,
			    in_key_array, allow_other_in_keys,
			    reply_provided, out_key_array,
			    allow_other_out_keys);
  GalIO_SetServerClientData(i, name, (void *) fn);
  Py_INCREF(fn);
}
#endif

/* This function takes the Python function stored in the
   invocation client data and calls it with the name of
   the function. It gets back a result which is the
   signature and the Python function to call, which
   goes in the data. There is no fn_map. */

static int
__PyDispatchFunctionSelector(Gal_DispatchFnInvocation *inv)
{
  PyObject **two_fns = (PyObject **) inv->pkg->client_data;
  PyObject *selecting_fn = two_fns[1];
  PyObject *arglist, *result;
  PyObject *fn_to_call, *py_sig_ptr;
  Gal_DispatchFnSignature *sig = (Gal_DispatchFnSignature *) NULL;
  
  arglist = Py_BuildValue("(s)", inv->bare_op_name);
  result = PyEval_CallObject(selecting_fn, arglist);

  Py_DECREF(arglist);
  PyArg_ParseTuple(result, "OO", &fn_to_call, &py_sig_ptr);

  if (fn_to_call == Py_None) {
    return 0;
  }
  
  if (!PyCallable_Check(fn_to_call)) {
    PyErr_SetString(PyExc_TypeError, "not a callable object");
    __PyAcknowledgeError();
    return 0;
  }

  if (py_sig_ptr != Py_None) {
#ifdef SWIG11
    if (!__PySWIG_GetPtr(py_sig_ptr, (void **) &sig,
			 "_Gal_DispatchFnSignature_p")) {
      PyErr_SetString(PyExc_TypeError,
		      "signature was not a C signature pointer");
      __PyAcknowledgeError();
    }
#endif
#ifdef SWIG13
    if (SWIG_ConvertPtr(py_sig_ptr, (void **) &sig,
			SWIGTYPE_p_Gal_DispatchFnSignature, 1) == -1) {
      __PyAcknowledgeError();
    }
#endif
  }
  inv->call_client_data = (void *) fn_to_call;
  /* Make sure you increment the fn_to_call before you
     decrement the result. The reason is (I think) that the 
     result is a tuple, and PyTuple_GetItem only borrows
     references, and so if you decrement the result and
     it's freed, then fn_to_call won't exist. Sigh. */
  Py_INCREF(fn_to_call);
  inv->sig = sig;
  Py_DECREF(result);
  return 1;
}

static
Gal_DispatchFnSignature *__PyDispatchFnLister(Gal_DispatchFnPkg *pkg)
{
  /* The lister calls a Python function which returns a
     Python list of signatures. */
  PyObject **two_fns = (PyObject **) pkg->client_data;
  PyObject *listing_fn = two_fns[0];
  PyObject *arglist, *result;
  Gal_DispatchFnSignature *sigs = (Gal_DispatchFnSignature *) NULL;
  
  arglist = Py_BuildValue("()");
  result = PyEval_CallObject(listing_fn, arglist);
  Py_DECREF(arglist);
  
  if (result != Py_None) {
    if (!PyList_Check(result)) {
      PyErr_SetString(PyExc_TypeError, "not a list");
      __PyAcknowledgeError();
    } else {
      int i, size = PyList_Size(result);
      sigs = (Gal_DispatchFnSignature *) calloc(size + 1, sizeof(Gal_DispatchFnSignature));
      for (i = 0; i < size; i++) {
	PyObject *elt = PyList_GetItem(result, i);
	Gal_DispatchFnSignature *sig;
#ifdef SWIG11
	if (!__PySWIG_GetPtr(elt, (void **) &sig,
			     "_Gal_DispatchFnSignature_p")) {
	  PyErr_SetString(PyExc_TypeError,
			  "list element was not a C signature pointer");
	  __PyAcknowledgeError();
	  free(sigs);
	  sigs = (Gal_DispatchFnSignature *) NULL;
	  break;
	}
#endif
#ifdef SWIG13
	if (SWIG_ConvertPtr(elt, (void **) &sig,
			    SWIGTYPE_p_Gal_DispatchFnSignature, 1) == -1) {
	  __PyAcknowledgeError();
	  free(sigs);
	  sigs = (Gal_DispatchFnSignature *) NULL;
	  break;
	}
#endif
	sigs[i] = *sig;
      }
    }
  }
  Py_DECREF(result);
  return sigs;
}

/* In order to make this work, I need to change the dispatch
   function selector and then call it later. Arrgh. */

void _GalIO_PySetServerDispatchFnAccess(GalIO_ServerStruct *server,
					PyObject *listing_fn,
					PyObject *selecting_fn)
{
  PyObject **two_fns = (PyObject **) calloc(2, sizeof(PyObject *));
  two_fns[0] = listing_fn;
  two_fns[1] = selecting_fn;
  _GalIO_SetServerDispatchFnAccess(server,
				   __PyDispatchFunctionSelector,
				   __PyDispatchFnLister,
				   __PyDispatchFn,
				   (void *) two_fns);
  Py_INCREF(listing_fn);
  Py_INCREF(selecting_fn);
}

%}

/* Check that the input is a Python callable. */

%typemap (python, in) PyObject *fn, PyObject *listing_fn, PyObject *selecting_fn {
  if (!PyCallable_Check($source)) {
    PyErr_SetString(PyExc_TypeError, "not a callable object");
    return NULL;
  }
  $target = $source;
}  

/*
void GalSS_AddPyDispatchFunction(GalIO_ServerStruct *i, char *name,
				 PyObject *fn,
				 Gal_DispatchFnSignatureKeyEntry *in_key_array,
				 int allow_other_in_keys, 
				 int reply_provided,
				 Gal_DispatchFnSignatureKeyEntry *out_key_array,
				 int allow_other_out_keys);
*/

void _GalIO_PySetServerDispatchFnAccess(GalIO_ServerStruct *server,
					PyObject *listing_fn,
					PyObject *selecting_fn);
Gal_DispatchFnSignature *
Gal_CreateDispatchFnSignature(char *name,
			      Gal_DispatchFnSignatureKeyEntry *in_key_array,
			      int allow_other_in_keys, 
			      int reply_provided,
			      Gal_DispatchFnSignatureKeyEntry *out_key_array,
			      int allow_other_out_keys);

%typemap (python, in) PyObject *fn, PyObject *listing_fn, PyObject *selecting_fn;

/* local definitions */

/* I need to typemap the string, because it can be NULL. */

%typemap (python, in) char *client_pair_string, char *session_id, char *server_locations_file, char *server_name, char *slf_name {
  if ($source == Py_None) {
    $target = (char *) NULL;
  } else if (!PyString_Check($source)) {
    PyErr_SetString(PyExc_TypeError, "not a string");
    return NULL;
  } else {
    $target = PyString_AsString($source);
  }
}

GalSS_ServerArgs *_GBGalSS_EncapsulateArguments(char *server_name,
						unsigned short server_port,
						int max_conns,
						int do_assert,
						int validate,
						int verbosity,
						int server_listen_status,
						char *client_pair_string,
						char *session_id,
						char *server_locations_file,
                                                char *slf_name);

%typemap (python, in) char *client_pair_string, char *session_id, char *server_locations_file, char *server_name, char *slf_name;

GalIO_ServerStruct *
GBGalSS_SetupServer(GalSS_ServerArgs *arg_pkg);
void GalSS_FreeArgPkg(GalSS_ServerArgs *arg_pkg);
GalIO_ServerStruct *GalIO_ServerStart(GalIO_ServerStruct *scomm);
void GalIO_AddServiceType(GalIO_ServerStruct *server, char *stype);

/* And now, broker proxies. */

Gal_Object Gal_CreateProxyObject(GalSS_BrokerProxy *p,
				 int manage_memory);
Gal_Object Gal_ProxyObject(GalSS_BrokerProxy *p);
GalSS_BrokerProxy *Gal_ProxyValue(Gal_Object o);
void GalSS_FreeBrokerProxy(GalSS_BrokerProxy *p);
GalSS_BrokerProxy *GalSS_CopyBrokerProxy(GalSS_BrokerProxy *bp);
GalSS_BrokerProxy *
GalSS_ProxifyObjectType(GalSS_Environment *env,
			Gal_ObjectType t,
			int poll_ms, int timeout_seconds);
int GalSS_BrokerProxyOutCallbackHandler(GalSS_BrokerProxy *bp);
int GalSS_BrokerProxyWriteReady(GalSS_BrokerProxy *bp);
int GalSS_BrokerProxyInCallbackHandler(GalSS_BrokerProxy *bp);
int GalSS_BrokerProxyReadReady(GalSS_BrokerProxy *bp);
int GalSS_ProxyWrite(GalSS_BrokerProxy *p, Gal_Object obj,
		     int manage_memory);
void GalSS_ProxyDone(GalSS_BrokerProxy *p);
Gal_Object GalSS_UnproxifyObject(GalSS_Environment *env,
				 GalSS_BrokerProxy *p);
Gal_ObjectType GalSS_BrokerProxyObjectType(GalSS_BrokerProxy *bp);
void GalSS_ForceProxyExpiration(GalSS_BrokerProxy *bp);
int GalSS_ProxySelfTerminates(GalSS_BrokerProxy *p);
/* Use this SPARINGLY. */
GalIO_BrokerStruct *GalSS_BrokerProxyBroker(GalSS_BrokerProxy *bp);
GAL_SOCKET GalSS_GetBrokerProxySocket(GalSS_BrokerProxy *bp);

/* See above for GalSS_PyEnvBrokerProxyInInit, GalSS_Unproxify. */

/* Control the verbosity, dammit. */
int GalUtil_SetVerbose(int verbose_level);

/* Constants. The SWIG 1.3 way is infinitely better. The
   constant generation in SWIG 1.1 didn't work too well. */

#ifdef SWIG13
const char *const GAL_ERROR_NUMBER_FRAME_KEY;
const char *const GAL_ERROR_DESCRIPTION_FRAME_KEY;
const char *const GAL_SESSION_ID_FRAME_KEY;
const char *const GAL_SERVER_TOKEN_INDEX_FRAME_KEY;
const char *const GAL_HUB_OPAQUE_DATA_FRAME_KEY;
const char *const GAL_ROUND_TRIP_FRAME_KEY;
#endif
#ifdef SWIG11
#define GAL_ERROR_NUMBER_FRAME_KEY ":errno"
#define GAL_ERROR_DESCRIPTION_FRAME_KEY ":err_description"
#define GAL_SESSION_ID_FRAME_KEY ":session_id"
#define GAL_SERVER_TOKEN_INDEX_FRAME_KEY ":server_tidx"
#define GAL_HUB_OPAQUE_DATA_FRAME_KEY ":hub_opaque_data"
#define GAL_ROUND_TRIP_FRAME_KEY ":reply_requested"
#endif
