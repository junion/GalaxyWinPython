/*
  Portions of this file (c) Copyright 1998 - 2000 M.I.T.
  Portions of this file (c) Copyright 2000 - 2001 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _H_GENERIC_SERVER
#define _H_GENERIC_SERVER

/* loads "galaxy/galaxy.h", which will load the appropriate
   version of the common types, which always loads
   "generic-server-types.h". */
#include "galaxy/galaxy_io.h"

typedef GalIO_ServerStruct Gal_Server;

/* **************************************************************** */

/*  init_server is an external function which the server should
 *  provide for server-specific initialization at start time.
 *  It should exit on error.
 */
void *_GalSS_init_server(GalIO_ServerStruct *server, int argc, char **argv);

/*  for backwards compatibility */

#define init_server _GalSS_init_server

/*  print_usage is an external function which the server should
 *  provide for printing a usage message after the generic server
 *  usage has been printed.
 */
void _GalSS_print_usage(int argc, char **argv);

/*  
 *  Exported functions
 */

/* generic-server-main.c */

enum {GAL_LOOP_TT, GAL_LOOP_THREADS, GAL_LOOP_EXTERNAL};

void GalSS_RunServer(GalIO_ServerStruct *server);
GalIO_ServerStruct *GalSS_InitializeServer(unsigned short server_port,
					   int max_conns, int use_color,
					   int do_assert, int use_ttloop,
					   int validate,
					   int new_argc, char **new_argv);
GalIO_ServerStruct *GalSS_InitializeServerToplevel(unsigned short server_port,
						   int max_conns,
						   int use_color,
						   int do_assert,
						   int loop_type,
						   int validate,
						   int verbosity,
						   int server_listen_status,
						   const char *client_pair_string,
						   const char *session_id,
						   int new_argc,
						   char **new_argv);
GalIO_ServerStruct *GalSS_CmdlineInitializeServer(int argc, char **argv);
GalIO_ServerStruct *
GalSS_InitializeServerFromServerArgs(GalSS_ServerArgs *arg_pkg,
				     int new_argc, char **new_argv);
GalIO_ServerStruct *
GalSS_SetupServer(GalSS_ServerArgs *arg_pkg,
		  int new_argc, char **new_argv);
int GalSS_StartAndRunServer(GalIO_ServerStruct *server);
GalIO_ServerStruct *GalSS_CmdlineSetupServer(int argc, char **argv);
GalSS_ServerArgs *GalSS_ExtractServerArgs(int argc, char **argv,
					  int *new_argc_ptr,
					  char ***new_argv_ptr);
GalSS_ServerArgs *GalSS_ExtractCmdlineServerArgs(GalSS_ServerArgs *arg_pkg,
						 int argc, char **argv,
						 int *new_argc_ptr,
						 char ***new_argv_ptr);
void GalSS_FreeArgv(int argc, char **argv);
void GalSS_FreeArgPkg(GalSS_ServerArgs *arg_pkg);
/* Fixing the arguments. */
GalSS_ServerArgs *GalSS_DefaultServerArgs();
unsigned short GalSS_SAFixPort(GalSS_ServerArgs *arg_pkg, unsigned short port);
char *GalSS_SAFixServerLocations(GalSS_ServerArgs *arg_pkg, const char *server_locations_file);
char *GalSS_SAFixSLFName(GalSS_ServerArgs *arg_pkg, const char *slf_name);
int GalSS_SAFixMaxConns(GalSS_ServerArgs *arg_pkg, int max_conns);
int GalSS_SAFixVerbosity(GalSS_ServerArgs *arg_pkg, int verbosity);
int GalSS_SAFixColor(GalSS_ServerArgs *arg_pkg, int color);
int GalSS_SAFixAssert(GalSS_ServerArgs *arg_pkg, int assert);
int GalSS_SAFixValidate(GalSS_ServerArgs *arg_pkg, int validate);
int GalSS_SAFixLoopType(GalSS_ServerArgs *arg_pkg, int loop_type);
char * GalSS_SAFixContactHubInfo(GalSS_ServerArgs *arg_pkg,
				 const char *client_pair_status,
				 const char *session_id,
				 char **old_session_id_ptr);
int GalSS_SAFixServerListenStatus(GalSS_ServerArgs *arg_pkg,
				  int server_listen_status);
GalIO_ServerStruct *
GalSS_CmdlineSetupServerForHubContact(int argc, char **argv,
				      char **client_string_ptr,
				      char **session_id_ptr,
				      int allow_listener,
				      int client_poll_flags,
				      int loop_type);
GalIO_CommStruct *GalSS_SetupConnection(const char *host,
					unsigned short port,
					char *retrieved_contact_info,
					int shutdown_after,
					GalIO_ServerStruct *scomm);
GalSS_Environment *GalSS_SetupEnvironment(const char *host,
					  unsigned short port,
					  const char *session_id,
					  char *retrieved_contact,
					  char *retrieved_session,
					  int shutdown_after,
					  GalIO_ServerStruct *scomm);
/* generic-server.c */
int GalSS_FrameHandler(GalIO_CommStruct *gcomm, Gal_Frame frame);
void *GalSS_EnvGetCommData(GalSS_Environment *env);
void GalSS_EnvSetCommData(GalSS_Environment *env, void *data, void (*free_fn)(void *));
GalIO_CommStruct *GalSS_EnvComm(GalSS_Environment *env);
void *GalSS_EnvGetClientData(GalSS_Environment *env, const char *name);

/* Function protos for frame_util.c */

int GalSS_EnvWriteFrame(GalSS_Environment *env, Gal_Frame frame, int do_block);
int GalSS_EnvWriteFrameToProvider(GalSS_Environment *env, Gal_Frame frame,
				  const char *provider_id, int do_block);
Gal_Frame GalSS_EnvDispatchFrame(GalSS_Environment *env, Gal_Frame frame,
				 GalIO_MsgType *t);
Gal_Frame
GalSS_EnvDispatchFrameToProvider(GalSS_Environment *env, Gal_Frame frame,
				 const char *provider_id, GalIO_MsgType *t);

int GalSS_EnvError(GalSS_Environment *env, const char *description);
int GalSS_EnvErrorOfType(GalSS_Environment *env, int type,
			 const char *description);
int GalSS_EnvDestroyToken(GalSS_Environment *env);
GalSS_Environment *GalSS_EnvCreate(GalIO_CommStruct *gcomm);
GalSS_Environment *GalSS_EnvCopy(GalSS_Environment *old_env);
GalSS_Environment *GalSS_EnvCopyNoLock(GalSS_Environment *old_env);
void GalSS_EnvUpdateSessionID(GalSS_Environment *env, const char *session_id);
char *GalSS_EnvGetSessionID(GalSS_Environment *env);
int GalSS_EnvReturnRequired(GalSS_Environment *env);
void GalSS_EnvLock(GalSS_Environment *env);
void GalSS_EnvUnlock(GalSS_Environment *env);
int GalSS_EnvReply(GalSS_Environment *env, Gal_Frame f);
int GalSS_EnvPostponeReply(GalSS_Environment *env);
Gal_Frame GalSS_EnvGetSessionProperties(GalSS_Environment *env, char **keys);
void GalSS_EnvSetSessionProperties(GalSS_Environment *env, Gal_Frame properties);
void GalSS_EnvDeleteSessionProperties(GalSS_Environment *env, char **keys);
void GalSS_EnvModifySessionProperties(GalSS_Environment *env,
				      Gal_Frame properties_to_set,
				      char **properties_to_delete);
void GalSS_EnvSetSession(GalSS_Environment *env, const char *session_name, int lock_info);
Gal_Frame GalSS_EnvGetServerProperties(GalSS_Environment *env, char **keys);
void GalSS_EnvSetServerProperties(GalSS_Environment *env, Gal_Frame properties);
void GalSS_EnvDeleteServerProperties(GalSS_Environment *env, char **keys);
void GalSS_EnvModifyServerProperties(GalSS_Environment *env,
				     Gal_Frame properties_to_set,
				     char **properties_to_delete);
double GalSS_EnvGetTokenTimestamp(GalSS_Environment *env);
void GalSS_EnvInheritTokenTimestamp(GalSS_Environment *env);
char *GalSS_EnvGetOriginatingProvider(GalSS_Environment *env);

/* Lock info data */

#define GAL_SERVER_READS_ONLY_FROM_SESSION 1
#define GAL_SESSION_WRITES_ONLY_TO_SERVER 2
#define GAL_SERVER_WRITES_ONLY_TO_SESSION 4
#define GAL_PERMANENT_LOCK 8

int GalSS_SessionDecodeLocks(Gal_Frame fr, int *mask_ptr, int *value_ptr);

/* The name here isn't copied, so no const. */
void GalSS_InitializeServerDefaults(GalIO_ServerStruct *gcomm, char *name, unsigned short port);
void GalSS_AddDispatchFunction(GalIO_ServerStruct *i, const char *name,
			       Gal_FrameDataFnPtr fn,
			       Gal_DispatchFnSignatureKeyEntry *in_key_array,
			       int allow_other_in_keys, 
			       int reply_provided,
			       Gal_DispatchFnSignatureKeyEntry *out_key_array,
			       int allow_other_out_keys);
Gal_Frame GalSS_CallServerFunction(Gal_Frame frame, GalSS_Environment *env, int optional);


/* continuation.c */
typedef Gal_Frame (*GalSS_ContinuationFn)(Gal_Frame, GalIO_MsgType,
					  GalSS_Environment *, void *);

int GalSS_EnvDispatchFrameWithContinuation(GalSS_Environment *env,
					   Gal_Frame frame,
					   GalSS_ContinuationFn fn,
					   void *continuation_state,
					   void (*continuation_state_free_fn)(void *));
int GalSS_EnvDispatchFrameToProviderWithContinuation(GalSS_Environment *env,
						     Gal_Frame frame,
						     const char *provider,
						     GalSS_ContinuationFn fn,
						     void *continuation_state,
						     void (*continuation_state_free_fn)(void *));

GalIO_BrokerStruct *
GalSS_EnvBrokerDataInInit(GalSS_Environment *env,
			  const char *host, unsigned short port,
			  Gal_Frame frame, GalIO_BrokerDataHandler fnptr,
			  int poll_ms, void *refptr, void (*free_fn)(void *));
void GalSS_EnvStartBroker(GalSS_Environment *env,
			  GalIO_BrokerStruct *b, int poll_ms);
void GalSS_BrokerSetEnvironment(GalIO_BrokerStruct *b, GalSS_Environment *env);
GalSS_Environment *GalSS_BrokerGetEnvironment(GalIO_BrokerStruct *b);

Gal_TaskPkg *GalSS_EnvAddTask(GalSS_Environment *env,
			      void (*task)(Gal_TaskPkg *),
			      void *caller_data,
			      long num_millisecs, int read_blocking_available,
			      GAL_SOCKET *read_socket,
			      GAL_SOCKET *write_socket,
			      GAL_SOCKET *err_socket,
			      FILE *read_file, FILE *write_file,
			      FILE* err_file,
			      Gal_TaskConditionFn condition,
			      void (*cleanup_fn)(void *));
void GalSS_TaskSetEnvironment(Gal_TaskPkg *p, GalSS_Environment *env);
GalSS_Environment *GalSS_TaskGetEnvironment(Gal_TaskPkg *p);

void GalSS_EnvMaintainInLocation(GalIO_CommStruct *gcomm,
				 const char *initial_session_id,
				 GalSS_Environment **env_loc);

/* server_locations.c */

GalSS_ServerLocationEntry *
GalSS_FindServerLocationEntry(const char *filename,
			      const char *server_name);
GalSS_ServerLocationEntry *GalSS_DigestServerLocationFile(const char *filename);
void GalSS_FreeProviderSpec(GalSS_ProviderSpec *spec);
char *GalSS_FormatProviderSpec(GalSS_ProviderSpec *spec,
			       Gal_StringBuffer **bufptr);

/* galaxy_elr.c */

GalSS_ELR *GalSS_ELRCreate(GalIO_ServerStruct *scomm,
			   GalSS_ELTimerSetFn timer_set_fn,
			   GalSS_ELUnsetFn timer_unset_fn,
			   GalSS_ELFDSetFn fd_set_fn,
			   GalSS_ELUnsetFn fd_unset_fn,
			   GalSS_ELBehaviorFn behavior_fn,
			   int timer_is_persistent);
GalSS_ELR *GalSS_ELRCopy(GalSS_ELR *source);
void GalSS_ELRShutdown(GalSS_ELR *elr);
void GalSS_ELRDestroy(GalSS_ELR *elr);
void GalSS_ELRSetLoopData(GalSS_ELR *elr, void *loop_data,
			  void (*loop_data_free_fn)(void *));
void *GalSS_ELRGetLoopData(GalSS_ELR *elr);
void GalSS_ELRUpdatePollIntervals(GalSS_ELR *elr,
				  int server_client_poll_ms,
				  int conn_ms, int broker_ms);
void GalSS_ELRDoCallback(GalSS_ELR *elr, int timer_or_fd);
GalSS_ELR *GalSS_ELRSetupServer(GalSS_ServerArgs *external_arg_pkg,
				int argc, char **argv,
				GalSS_ELTimerSetFn timer_set_fn,
				GalSS_ELUnsetFn timer_unset_fn,
				GalSS_ELFDSetFn fd_set_fn,
				GalSS_ELUnsetFn fd_unset_fn,
				GalSS_ELBehaviorFn behavior_fn,
				void *loop_data,
				void (*loop_data_free_fn)(void *),
				int timer_is_persistent);
GalIO_ServerStruct *GalSS_ELRSComm(GalSS_ELR *elr);
GalIO_CommStruct *GalSS_ELRGComm(GalSS_ELR *elr);
GalIO_BrokerStruct *GalSS_ELRBroker(GalSS_ELR *elr);
void GalSS_ELRSetConnectionCallback(GalSS_ELR *elr, GalSS_ELCallbackFn fn);
void GalSS_ELRSetBrokerOutCallback(GalSS_ELR *elr, GalSS_ELCallbackFn fn);
void GalSS_ELRSetBrokerInCallback(GalSS_ELR *elr, GalSS_ELCallbackFn fn);
void GalSS_ELRSetServerListenerCallback(GalSS_ELR *elr, GalSS_ELCallbackFn fn);
void GalSS_ELRSetServerClientCallback(GalSS_ELR *elr, GalSS_ELCallbackFn fn);

/* broker_proxy.c */

typedef void 
(*GalSS_ProxyDataHandler)(GalSS_Environment *env,
			  Gal_ObjectType proxy_type,
		          Gal_Object elt,
			  void *caller_data);
typedef void 
(*GalSS_ProxyDataEventHandler)(GalSS_Environment *env,
			       Gal_ObjectType proxy_type,
			       void *caller_data);
void GalSS_FreeBrokerProxy(GalSS_BrokerProxy *p);
GalSS_BrokerProxy *GalSS_CopyBrokerProxy(GalSS_BrokerProxy *bp);
GalSS_BrokerProxy *GalSS_CreateBrokerProxy(const char *host, int port,
					   const char *call_id,
					   Gal_ObjectType object_type,
					   Gal_Object obj_data);
GalIO_BrokerStruct *GalSS_BrokerProxyBroker(GalSS_BrokerProxy *bp);
Gal_ObjectType GalSS_BrokerProxyObjectType(GalSS_BrokerProxy *bp);
Gal_Object GalSS_BrokerProxyObject(GalSS_BrokerProxy *bp);
GalSS_BrokerProxy *
GalSS_ProxifyObject(GalSS_Environment *env, Gal_Object obj,
		    int poll_ms, int timeout_seconds);
Gal_Object
GalSS_ObjProxifyObject(GalSS_Environment *env, Gal_Object obj,
		       int poll_ms, int timeout_seconds);
GalSS_BrokerProxy *
GalSS_ProxifyObjectType(GalSS_Environment *env,
			Gal_ObjectType t,
			int poll_ms, int timeout_seconds);
Gal_Object
GalSS_ObjProxifyObjectType(GalSS_Environment *env,
			   Gal_ObjectType t,
			   int poll_ms, int timeout_seconds);
int GalSS_ProxyListAdd(GalSS_BrokerProxy *p, Gal_Object elt);
int GalSS_ObjProxyListAdd(Gal_Object obj, Gal_Object elt);
int GalSS_ProxyArrayAdd(GalSS_BrokerProxy *p, void *data, int size);
int GalSS_ObjProxyArrayAdd(Gal_Object obj, void *data, int size);
int GalSS_ProxyWrite(GalSS_BrokerProxy *p, Gal_Object obj,
		     int manage_memory);
int GalSS_ObjProxyWrite(Gal_Object proxy_obj, Gal_Object obj,
			int manage_memory);
void GalSS_ProxyDone(GalSS_BrokerProxy *p);
void GalSS_ObjProxyDone(Gal_Object obj);
int GalSS_ProxySelfTerminates(GalSS_BrokerProxy *p);
int GalSS_ObjProxySelfTerminates(Gal_Object obj);
int GalSS_BrokerProxyOutCallbackHandler(GalSS_BrokerProxy *bp);
int GalSS_BrokerProxyWriteReady(GalSS_BrokerProxy *bp);
void GalSS_ForceProxyExpiration(GalSS_BrokerProxy *bp);
Gal_Object GalSS_UnproxifyObject(GalSS_Environment *env, GalSS_BrokerProxy *p);
Gal_Object GalSS_ObjUnproxifyObject(GalSS_Environment *env, Gal_Object obj);
void GalSS_Unproxify(GalSS_Environment *env,
		     GalSS_BrokerProxy *p,
		     GalSS_ProxyDataHandler fn,
		     GalSS_ProxyDataEventHandler done_fn,
		     GalSS_ProxyDataEventHandler abort_fn,
		     int immediate,
		     int poll_ms,
		     void *caller_data,
		     void (*caller_data_free_fn)(void *));
void GalSS_ObjUnproxify(GalSS_Environment *env,
			Gal_Object obj,
			GalSS_ProxyDataHandler fn,
			GalSS_ProxyDataEventHandler done_fn,
			GalSS_ProxyDataEventHandler abort_fn,
			int immediate,
			int poll_ms,
			void *caller_data,
			void (*caller_data_free_fn)(void *));
GalIO_BrokerStruct *
GalSS_EnvBrokerProxyInInit(GalSS_Environment *env,
			   GalSS_BrokerProxy *p,
			   GalIO_BrokerDataHandler fnptr,
			   int poll_ms,
			   void *refptr, void (*free_fn)(void *));
GalIO_BrokerStruct *
GalSS_EnvBrokerProxyObjInInit(GalSS_Environment *env,
			      Gal_Object proxy,
			      GalIO_BrokerDataHandler fnptr,
			      int poll_ms,
			      void *refptr, void (*free_fn)(void *));
int GalSS_BrokerProxyInCallbackHandler(GalSS_BrokerProxy *bp);
GAL_SOCKET GalSS_GetBrokerProxySocket(GalSS_BrokerProxy *bp);
#endif
