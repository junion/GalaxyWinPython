/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* In this file, we set up the ACL C callbacks. */

#include "lisp.h"
#include "galaxy/sysdep.h"
#include "galaxy/galaxy_all.h"

/* The first callback is the server connection callback. */

/* I think I'm going to introduce some general callback
   support in cGalaxy.cl. The idea will be that whether
   or not they're callables, I'll get the lisp_value
   of the function all the arguments and pass
   it in as an array of ints. Then, I'll add the address
   of the rest of the arguments to a call array and
   pass these elements back into Lisp, assuming I can
   do that... */

extern void *lisp_call_address(int index);
extern long lisp_value(int index);

/* Testing with a frame. By coercing to an unsigned long,
   I can pass foreign pointers back in, which should
   help me a lot. */

typedef struct __ACLCallbackStruct {
  int funcall_index;
  int lisp_fn_index;
  int lisp_array_index;
} ACLCallbackStruct;

ACLCallbackStruct *ACLGal_CreateACLCallback(int funcall_index,
					    int fn_index,
					    int lisp_array_index)
{
  ACLCallbackStruct *s = (ACLCallbackStruct *) malloc(sizeof(ACLCallbackStruct));
  s->funcall_index = funcall_index;
  s->lisp_fn_index = fn_index;
  s->lisp_array_index = lisp_array_index;
  return s;
}

void ACLGal_FreeACLCallback(ACLCallbackStruct *s)
{
  free(s);
}

void __free_acl_callback(void *s)
{
  ACLGal_FreeACLCallback((ACLCallbackStruct *) s);
}

/* This actually returns a foreign pointer as a Lisp object. Ugh. */

int ACLGal_FuncallLisp(ACLCallbackStruct *s, int *local_args,
			int num_local_args)
{
  int (*func)() = lisp_call_address(s->funcall_index);
  return FixnumToInt((*func)(lisp_value(s->lisp_fn_index),
			     lisp_value(s->lisp_array_index),
			     local_args, num_local_args));
}

int ACLGal_NthNativeArg(int i, int *local_args)
{
  return local_args[i];
}

static void __server_free_acl_callback(GalIO_ServerStruct *scomm,
				       void *caller_data)
{
  ACLGal_FreeACLCallback((ACLCallbackStruct *) caller_data);
}

static void __ACLServerConnectCallback(GalIO_ServerStruct *scomm,
				       GalIO_CommStruct *gcomm,
				       void *callback_data)
{
  ACLCallbackStruct *s = (ACLCallbackStruct *) callback_data;
  int *local_args = (int *) &gcomm;
  ACLGal_FuncallLisp(s, local_args, 1);
}

void GalIO_ACLAddServerConnectCallback(GalIO_ServerStruct *scomm,
				       ACLCallbackStruct *s)
{
  /* I'm going to use the server data slot to store the disconnect func. */
  GalIO_AddServerConnectCallback(scomm, __ACLServerConnectCallback,
				 (void *) s);
  GalIO_AddServerCallback(scomm,
			  GAL_SERVER_DESTRUCTION_EVENT,
			  __server_free_acl_callback,
			  (void *) s);
}

Gal_Frame __ACLDispatchFn(Gal_Frame f, void *server_data)
{
  GalSS_Environment *env = (GalSS_Environment *) server_data;
  Gal_Frame f_result;
  /* This will need to be freed. */
  char *op_name = Gal_SplitOperationName(Gal_FrameName(f), (char **) NULL);  
  ACLCallbackStruct *s = (ACLCallbackStruct *) GalSS_EnvGetClientData(env, op_name);
  /* This is for the argument pointers. */
  int arg_pointers[2];
  
  free(op_name);
  arg_pointers[0] = (int) f;
  arg_pointers[1] = (int) env;
  /* Well, this is the only way I can see to get the
     result back. Sigh. */
  return (Gal_Frame) ACLGal_FuncallLisp(s, arg_pointers, 2);  
}

void GalSS_ACLAddDispatchFunction(GalIO_ServerStruct *i, char *name,
				  ACLCallbackStruct *s,
				  Gal_DispatchFnSignatureKeyEntry *in_key_array,
				  int allow_other_in_keys, 
				  int reply_provided,
				  Gal_DispatchFnSignatureKeyEntry *out_key_array,
				  int allow_other_out_keys)
{
  GalSS_AddDispatchFunction(i, _gal_strdup(name), __ACLDispatchFn,
			    in_key_array, allow_other_in_keys,
			    reply_provided, out_key_array,
			    allow_other_out_keys);
  GalIO_SetServerClientData(i, name, (void *) s);
}

/* Server callbacks. */

static void __ACLServerCallback(GalIO_ServerStruct *scomm, void *caller_data)
{
  ACLCallbackStruct *s = (ACLCallbackStruct *) caller_data;
  ACLGal_FuncallLisp(s, (int *) NULL, 0);
}

static void __ACLServerFreeCallback(GalIO_ServerStruct *scomm,
				    void *caller_data)
{
  __ACLServerCallback(scomm, caller_data);
  __server_free_acl_callback(scomm, caller_data);
}

void GalIO_ACLAddServerCallback(GalIO_ServerStruct *scomm,
				int event,
				ACLCallbackStruct *s)
{
  if (event != GAL_SERVER_DESTRUCTION_EVENT) {
    GalIO_AddServerCallback(scomm, event,
			    __ACLServerCallback,
			    (void *) s);
    GalIO_AddServerCallback(scomm, GAL_SERVER_DESTRUCTION_EVENT,
			    __server_free_acl_callback,
			    (void *) s);
  } else {
    GalIO_AddServerCallback(scomm, event,
			    __ACLServerFreeCallback,
			    (void *) s);
  }
}

/* Connection callbacks. */

static void __ACLConnectionCallback(GalIO_CommStruct *gcomm, void *caller_data)
{
  ACLCallbackStruct *s = (ACLCallbackStruct *) caller_data;
  ACLGal_FuncallLisp(s, (int *) NULL, 0);
}

static void __connection_free_acl_callback(GalIO_CommStruct *gcomm,
					   void *caller_data)
{
  ACLGal_FreeACLCallback((ACLCallbackStruct *) caller_data);
}

static void __ACLConnectionFreeCallback(GalIO_CommStruct *gcomm,
					void *caller_data)
{
  __ACLConnectionCallback(gcomm, caller_data);
  __connection_free_acl_callback(gcomm, caller_data);
}

void GalIO_ACLAddConnectionCallback(GalIO_CommStruct *gcomm,
				    int callback_event,
				    ACLCallbackStruct *s)
{
  if (callback_event != GAL_CONNECTION_DESTRUCTION_EVENT) {
    GalIO_AddConnectionCallback(gcomm, callback_event,
				__ACLConnectionCallback,
				(void *) s);
    GalIO_AddConnectionCallback(gcomm, GAL_CONNECTION_DESTRUCTION_EVENT,
				__connection_free_acl_callback,
				(void *) s);
  } else {
    GalIO_AddConnectionCallback(gcomm, callback_event,
				__ACLConnectionFreeCallback,
				(void *) s);
  }
}

/* Broker callbacks. */

GalIO_BrokerStruct *GalIO_ACLBrokerDataOutInit(GalIO_CommStruct *gcomm,
					       int timeout_seconds)
{
  return GalIO_BrokerDataOutInit(gcomm, -1, timeout_seconds);
}

/* All the data needs to be freed, ultimately. Since all the data
   is copied, essentially, we can free it all. But after the call,
   since Lisp needs to copy the data first. */

/* 11/10/00: Actually, I'm going to turn everything into a
   Gal_Object, because that's the easiest thing to manipulate in
   Lisp. */

extern char *sym_name(Gal_Symbol sp);

static void __ACLBrokerReaderCallback(GalIO_BrokerStruct *broker_struct,
				      void *data, Gal_ObjectType data_type,
				      int n_samples)
{
  ACLCallbackStruct *cb = (ACLCallbackStruct *) GalIO_GetBrokerData(broker_struct);
  /* This is for the argument pointers. */
  int arg_pointers[2];
  Gal_Object o = (Gal_Object) NULL;
  
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

  arg_pointers[0] = (int) data_type;
  arg_pointers[1] = (int) o;
  ACLGal_FuncallLisp(cb, arg_pointers, 2);
  if (o) Gal_FreeObject(o);
}

static void __ACLBrokerCallback(GalIO_BrokerStruct *broker_struct,
				void *caller_data)
{
  ACLCallbackStruct *cb = (ACLCallbackStruct *) caller_data;
  ACLGal_FuncallLisp(cb, (int *) NULL, 0);
}  

void __broker_free_acl_callback(GalIO_BrokerStruct *b, void *s)
{
  ACLGal_FreeACLCallback((ACLCallbackStruct *) s);
}

static void __ACLBrokerFreeCallback(GalIO_BrokerStruct *broker_struct,
				    void *caller_data)
{
  __ACLBrokerCallback(broker_struct, caller_data);
  __broker_free_acl_callback(broker_struct, caller_data);
}    

/* I need to make sure I do the right thing when the
   event is a destruction event. In particular, I don't
   want to rely on the order in which the callbacks
   are registered. Fortunately, the bindings set up
   a new callback struct every time a callback is added,
   so multiple references to the same Lisp callback
   function are distinct. So freeing is safe. */

void GalIO_ACLAddBrokerCallback(GalIO_BrokerStruct *b,
				int callback_event,
				ACLCallbackStruct *cb)
{
  if (callback_event != GAL_BROKER_DESTRUCTION_EVENT) {
    GalIO_AddBrokerCallback(b, callback_event,
			    __ACLBrokerCallback,
			    (void *) cb);
    GalIO_AddBrokerCallback(b, GAL_BROKER_DESTRUCTION_EVENT,
			    __broker_free_acl_callback,
			    (void *) cb);
  } else {
    GalIO_AddBrokerCallback(b, callback_event,
			    __ACLBrokerFreeCallback,
			    (void *) cb);
  }
}

GalIO_BrokerStruct *GalIO_ACLCommBrokerDataInInit(GalIO_CommStruct *gcomm,
						  char *host,
						  unsigned short port,
						  Gal_Frame frame,
						  ACLCallbackStruct *callback)
{
  return GalIO_CommBrokerDataInInit(gcomm, host, port, frame,
				    __ACLBrokerReaderCallback,
				    -1, (void *) callback,
				    __free_acl_callback);
}

GalIO_BrokerStruct *GalSS_ACLEnvBrokerDataInInit(GalSS_Environment *env,
						 char *host,
						 unsigned short port,
						 Gal_Frame frame,
						 ACLCallbackStruct *callback)
{
  return GalSS_EnvBrokerDataInInit(env, host, port, frame,
				   __ACLBrokerReaderCallback,
				    -1, (void *) callback,
				    __free_acl_callback);
}

/* Broker proxies. */

GalIO_BrokerStruct *GalSS_ACLEnvBrokerProxyInInit(GalSS_Environment *env,
						  GalSS_BrokerProxy *p,
						  ACLCallbackStruct *callback)
{
  return GalSS_EnvBrokerProxyInInit(env, p,
				    __ACLBrokerReaderCallback,
				    -1, (void *) callback,
				    __free_acl_callback);
}

/* The unproxification stuff is just plain ugleeee.... */

typedef struct __ACLProxyCallbacks {
  ACLCallbackStruct *op_callback;
  ACLCallbackStruct *data_done_callback;
  ACLCallbackStruct *abort_callback;
} __ACLProxyCallbacks;

static void __free_acl_proxy_callbacks(void *data)
{
  __ACLProxyCallbacks *apc = (__ACLProxyCallbacks *) data;
  ACLGal_FreeACLCallback(apc->op_callback);
  ACLGal_FreeACLCallback(apc->data_done_callback);
  ACLGal_FreeACLCallback(apc->abort_callback);
  free(apc);
}

static void
__ACLProxyDataHandlerCallback(GalSS_Environment *env,
			      Gal_ObjectType proxy_type,
			      Gal_Object elt,
			      void *caller_data)
{
  __ACLProxyCallbacks *apc = (__ACLProxyCallbacks *) caller_data;
  
  ACLGal_FuncallLisp(apc->op_callback, (int *) &elt, 1);
  Gal_FreeObject(elt);
}

static void 
__ACLProxyDataDoneCallback(GalSS_Environment *env,
			   Gal_ObjectType proxy_type,
			   void *caller_data)
{
  __ACLProxyCallbacks *apc = (__ACLProxyCallbacks *) caller_data;
  ACLGal_FuncallLisp(apc->data_done_callback, (int *) NULL, 0);
}

static void 
__ACLProxyAbortCallback(GalSS_Environment *env,
			Gal_ObjectType proxy_type,
			void *caller_data)
{
  __ACLProxyCallbacks *apc = (__ACLProxyCallbacks *) caller_data;
  ACLGal_FuncallLisp(apc->abort_callback, (int *) NULL, 0);
}

void GalSS_ACLUnproxify(GalSS_Environment *env,
			GalSS_BrokerProxy *p,
			ACLCallbackStruct *op_callback,
			ACLCallbackStruct *data_done_callback,
			ACLCallbackStruct *abort_callback,
			int immediate)
{
  __ACLProxyCallbacks *apc = (__ACLProxyCallbacks *) calloc(1, sizeof(__ACLProxyCallbacks));

  apc->op_callback = op_callback;
  apc->data_done_callback = data_done_callback;
  apc->abort_callback = abort_callback;

  GalSS_Unproxify(env, p, __ACLProxyDataHandlerCallback,
		  __ACLProxyDataDoneCallback,
		  __ACLProxyAbortCallback, immediate, -1,
		  (void *) apc, __free_acl_proxy_callbacks);
}

/* Continuations. */

Gal_Frame __ACLDoContinuation(Gal_Frame f, GalIO_MsgType msg_type,
			      GalSS_Environment *env, void *continuation_state)
{
  Gal_Frame f_result;
  ACLCallbackStruct *s = (ACLCallbackStruct *) continuation_state;
  /* This is for the argument pointers. */
  int arg_pointers[3];
  
  arg_pointers[0] = (int) f;
  arg_pointers[1] = (int) env;
  arg_pointers[2] = (int) msg_type;
  /* Well, this is the only way I can see to get the
     result back. Sigh. */
  return (Gal_Frame) ACLGal_FuncallLisp(s, arg_pointers, 3);
}

int GalSS_ACLEnvDispatchFrameWithContinuation(GalSS_Environment *env,
					      Gal_Frame frame,
					      ACLCallbackStruct *continuation)
{
  return GalSS_EnvDispatchFrameWithContinuation(env, frame,
						__ACLDoContinuation,
						(void *) continuation,
						__free_acl_callback);
}

int GalSS_ACLEnvDispatchFrameToProviderWithContinuation(GalSS_Environment *env,
							Gal_Frame frame,
							char *provider,
							ACLCallbackStruct *continuation)
{
  return GalSS_EnvDispatchFrameToProviderWithContinuation(env, frame, provider,
							  __ACLDoContinuation,
							  (void *) continuation,
							  __free_acl_callback);
}


void *Gal_ACLCopyArray(char *data, int size)
{
  char *s = (char *) malloc(size);

  memcpy((void *) s, data, size);
  return (void *) s;
}

/* I need to get the i'th float from the array.
   This had better be four-byte floats. */

float Gal_ACLReadSingleFloat(int i, void *data)
{
  float f[1];
  char *s = data;

  memcpy((void *) f, s + (i * 4), 4);
  return f[0];
}

double Gal_ACLReadDoubleFloat(int i, void *data)
{
  double f[1];
  char *s = data;

  memcpy((void *) f, s + (i * 8), 8);
  return f[0];
}

/* And now, for creation. */

Gal_Object Gal_ACLCreateFloat32Object(float *data, int num_float_32)
{
  char *vdata = (char *) malloc(num_float_32 * 4);
  memcpy((void *) vdata, (void *) data, num_float_32 * 4);
  return Gal_CreateFloat32Object(vdata, num_float_32, 1);
}

Gal_Object Gal_ACLCreateFloat64Object(double *data, int num_float_64)
{
  char *vdata = (char *) malloc(num_float_64 * 8);
  memcpy((void *) vdata, (void *) data, num_float_64 * 8);
  return Gal_CreateFloat64Object(vdata, num_float_64, 1);
}

/* Ditto for broker writing. Arrrgh. */

int GalIO_ACLBrokerWriteFloat32(GalIO_BrokerStruct *b, float *data,
				int num_floats)
{
  return GalIO_BrokerWriteFloat32(b, (void *) data, num_floats);
}

int GalIO_ACLBrokerWriteFloat64(GalIO_BrokerStruct *b, double *data,
				int num_floats)
{
  return GalIO_BrokerWriteFloat64(b, (void *) data, num_floats);
}
