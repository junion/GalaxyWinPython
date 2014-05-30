/*
  This file (c) Copyright 1998 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef __H_GALAXY_IO_
#define __H_GALAXY_IO_

#include "galaxy/galaxy.h"
#include "galaxy/distinguished_keys.h"

/*
 *  Function prototypes
 */

/* ip_util.c */
/* return the IP address of the current host as a string */
char *GalIO_IPAddress(void);

/* broker_data.c */
/* server to server data brokering */


GalIO_Callback *GalIO_AddBrokerCallback(GalIO_BrokerStruct *b,
					int callback_event,
					GalIO_BrokerCallbackFn fn,
					void *callback_data);
void GalIO_RemoveBrokerCallback(GalIO_BrokerStruct *b,
				GalIO_Callback *cb);
void GalIO_FrameSetBrokerCallID(Gal_Frame f, const char *call_id);
void GalIO_DestroyBrokerStruct(GalIO_BrokerStruct *b);
GAL_SOCKET GalIO_GetBrokerSocket(GalIO_BrokerStruct *b);
GAL_SOCKET GalIO_GetBrokerListenSocket(GalIO_BrokerStruct *b);
Gal_Frame GalIO_GetBrokerFrame(GalIO_BrokerStruct *b);
unsigned short GalIO_GetBrokerListenPort(GalIO_BrokerStruct *b);
char *GalIO_GetBrokerCallID(GalIO_BrokerStruct *b);
void GalIO_BrokerPopulateFrame(GalIO_BrokerStruct *b,
			       Gal_Frame f,
			       const char *host_key,
			       const char *port_key);
void *GalIO_GetBrokerData(GalIO_BrokerStruct *b);
void *GalIO_GetBrokerCallerData(GalIO_BrokerStruct *b);
void GalIO_SetBrokerData(GalIO_BrokerStruct *b, void *caller_data,
			 void (*free_fn)(void *));
void GalIO_SetBrokerActive(GalIO_BrokerStruct *b);
void GalIO_BrokerSetFinalizer(GalIO_BrokerStruct *b,
			      GalIO_BrokerDataFinalizer finalizer);
int GalIO_BrokerWriteFrame(GalIO_BrokerStruct *b, Gal_Frame frame);
/* I could probably make this a const, but it would mean
   I'd need to work may way down into a lot of const void
   casts in the xdr encoding calls. Not gonna do that. */
int GalIO_BrokerWriteString(GalIO_BrokerStruct *b, char *str);
int GalIO_BrokerWriteInt(GalIO_BrokerStruct *b, int i);
int GalIO_BrokerWriteFloat(GalIO_BrokerStruct *b, float f);
int GalIO_BrokerWriteList(GalIO_BrokerStruct *b, Gal_Object *elts, int n_elts);
int GalIO_BrokerWriteBinary(GalIO_BrokerStruct *b, void *data, int n_bytes);
int GalIO_BrokerWriteInt16(GalIO_BrokerStruct *b, void *data, int n_ints);
int GalIO_BrokerWriteInt32(GalIO_BrokerStruct *b, void *data, int n_ints);
int GalIO_BrokerWriteInt64(GalIO_BrokerStruct *b, void *data, int n_ints);
int GalIO_BrokerWriteFloat32(GalIO_BrokerStruct *b, void *data, int n_floats);
int GalIO_BrokerWriteFloat64(GalIO_BrokerStruct *b, void *data, int n_floats);
int GalIO_BrokerWriteObject(GalIO_BrokerStruct *b, Gal_Object o);
void GalIO_BrokerDataDone(GalIO_BrokerStruct *b);
int GalIO_BrokerIsDone(GalIO_BrokerStruct *b);
GalIO_BrokerStruct *GalIO_BrokerDataOutInit(GalIO_CommStruct *gcomm, int poll_ms, int timeout_seconds);
void GalIO_CommStartBroker(GalIO_CommStruct *gcomm,
			   GalIO_BrokerStruct *b, int poll_ms);
int GalIO_BrokerDataOutHandler(GalIO_BrokerStruct *b);
void GalIO_BrokerDataOutDone(GalIO_BrokerStruct *b);
GalIO_BrokerStruct *GalIO_BrokerDataInInit(const char *host, unsigned short port, Gal_Frame frame,
					   GalIO_BrokerDataHandler fnptr, void *refptr, int poll_ms);
GalIO_BrokerStruct *
GalIO_CommBrokerDataInInit(GalIO_CommStruct *host_gcomm,
			   const char *host, unsigned short port, Gal_Frame frame,
			   GalIO_BrokerDataHandler fnptr,
			   int poll_ms,
			   void *caller_data, void (*caller_data_free_fn)(void *));
int GalIO_BrokerDataInHandler(GalIO_BrokerStruct *b);
int GalIO_BrokerDataInCallbackHandler(GalIO_BrokerStruct *b,
				      int read_blocking);
int GalIO_BrokerDataOutCallbackHandler(GalIO_BrokerStruct *b);
int GalIO_BrokerReadReady(GalIO_BrokerStruct *b);
int GalIO_BrokerWriteReady(GalIO_BrokerStruct *b);
GalIO_BrokerStruct *GalIO_BrokerStructQueueAppend(GalIO_BrokerStruct *b, GalIO_BrokerStruct *bqueue); 
GalIO_BrokerStruct *GalIO_BrokerStructQueuePop(GalIO_BrokerStruct *bqueue); 
GalIO_BrokerStruct *GalIO_BrokerStructDequeue(GalIO_BrokerStruct *b, GalIO_BrokerStruct *bqueue);
void GalIO_ForceBrokerExpiration(GalIO_BrokerStruct *b);
/* hub_server.c */

/* Settings for listen status. */

/* First three bit positions are what type of server. */
#define GAL_CONNECTION_LISTENER 1
#define GAL_BROKER_LISTENER 2
#define GAL_HUB_CLIENT 4
#define GAL_SERVER_TYPE_MASK 7

/* Backward compatibility, to the greatest degree possible... */

#define GAL_LISTENING_SERVER GAL_CONNECTION_LISTENER

/* SAM 9/7/01: Did some really stupid things in setting
   up these flags that make them impossible to understand.
   Also left out one. Looks like it's time to do them right. */

/* Defaults will be poll to connect, poll after disconnect */
/* Fourth and fifth bit positions are to poll to connect (0),
   shutdown on failure (fourth bit), do nothing on failure (fifth bit).
   Before 9/7/01: Fourth bit position is 0 to poll to connect, 1 to give up. */

#define GAL_HUB_CLIENT_CONNECT_FAILURE_MASK 24
#define GAL_HUB_CLIENT_CONNECT_FAILURE_RETRY 0
#define GAL_HUB_CLIENT_CONNECT_FAILURE_SHUTDOWN 8
#define GAL_HUB_CLIENT_CONNECT_FAILURE_NOOP 16

/* Sixth and seventh bit positions are to poll after disconnect (0),
   shutdown (sixth bit), don't do anything (seventh bit)
   Before 9/7/01:
   Fifth and sixth bit positions are to poll after disconnect (0),
   shutdown (fifth bit), don't do anything (sixth bit) */
#define GAL_HUB_CLIENT_DISCONNECT_MASK 96
#define GAL_HUB_CLIENT_DISCONNECT_RETRY 0
#define GAL_HUB_CLIENT_DISCONNECT_SHUTDOWN 32
#define GAL_HUB_CLIENT_DISCONNECT_NOOP 64

/* values for callback lists. */

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

void GalIO_DestroyCommStruct(GalIO_CommStruct *gcomm);
void GalIO_DestroyServerStruct(GalIO_ServerStruct *scomm);
GAL_SOCKET GalIO_GetServerListenSocket(GalIO_ServerStruct *scomm);
void GalIO_CloseServerListenSocket(GalIO_ServerStruct *scomm);
unsigned short GalIO_GetServerListenPort(GalIO_ServerStruct *scomm);
GAL_SOCKET GalIO_GetCommSocket(GalIO_CommStruct *gcomm);
void GalIO_CloseCommSocket(GalIO_CommStruct *gcomm);
char *GalIO_GetCommHost(GalIO_CommStruct *gcomm);
void *GalIO_GetCommServerData(GalIO_CommStruct *gcomm);
void *GalIO_GetCommData(GalIO_CommStruct *gcomm);
void GalIO_SetCommData(GalIO_CommStruct *gcomm, void *data, void (*free_fn)(void *));
void *GalIO_GetServerData(GalIO_ServerStruct *scomm);
void GalIO_SetServerData(GalIO_ServerStruct *scomm, void *data, void (*free_fn)(void *));
void GalIO_SetCommDone(GalIO_CommStruct *gcomm);
void GalIO_SetServerDone(GalIO_ServerStruct *scomm);
char *GalIO_GetServerName(GalIO_ServerStruct *scomm);
char *GalIO_GetCommServerName(GalIO_CommStruct *gcomm);
/* This function doesn't copy the name. */
void GalIO_SetServerName(GalIO_ServerStruct *scomm, char *name);
int GalIO_GetServerMaxConnections(GalIO_ServerStruct *scomm);
GalIO_CommStruct *GalIO_GetUniqueConnection(GalIO_ServerStruct *scomm);
Gal_Frame GalIO_DispatchViaHub(GalIO_CommStruct *gcomm, Gal_Frame frame, GalIO_MsgType *msg_type_ptr);
void GalIO_SetServerMaxConnections(GalIO_ServerStruct *scomm, int max);
int GalIO_GetServerNumConnections(GalIO_ServerStruct *scomm);
GalIO_ServerStruct *GalIO_CommServer(GalIO_CommStruct *gcomm);
unsigned short GalIO_GetServerDefaultPort(GalIO_ServerStruct *scomm);
void GalIO_SetServerDefaultPort(GalIO_ServerStruct *scomm, unsigned short port);
int GalIO_ServerUsesTimedTasks(GalIO_ServerStruct *server);
void GalIO_AddServerDispatchFunctionEntry(GalIO_ServerStruct *server,
					  const char *name,
					  Gal_FrameDataFnPtr fn_with_data,
					  Gal_DispatchFnSignatureKeyEntry *in_key_array,
					  int allow_other_in_keys, 
					  int reply_provided,
					  Gal_DispatchFnSignatureKeyEntry *out_key_array,
					  int allow_other_out_keys);
void GalIO_AddCommDispatchFunctionEntry(GalIO_CommStruct *gcomm,
					const char *name, 
					Gal_FrameDataFnPtr fn_with_data,
					Gal_DispatchFnSignatureKeyEntry *in_key_array,
					int allow_other_in_keys, 
					int reply_provided,
					Gal_DispatchFnSignatureKeyEntry *out_key_array,
					int allow_other_out_keys);
void GalIO_SetCommClientData(GalIO_CommStruct *gcomm,
			     const char *name, void *client_data);
void _GalIO_SetCommDispatchFnAccess(GalIO_CommStruct *gcomm,
				    Gal_DispatchFunctionSelector s,
				    Gal_DispatchFnSignatureLister l,
				    Gal_DispatchFunctionInvoker i,
				    void *invocation_client_data);
void *GalIO_GetCommClientData(GalIO_CommStruct *gcomm, const char *name);
Gal_DispatchFnPkg *GalIO_GetCommDispatchFnPkg(GalIO_CommStruct *gcomm);
void GalIO_SetCommDispatchFnPkg(GalIO_CommStruct *gcomm, Gal_DispatchFnPkg *pkg);
void GalIO_SetServerClientData(GalIO_ServerStruct *server,
			       const char *name, void *client_data);
void _GalIO_SetServerDispatchFnAccess(GalIO_ServerStruct *server,
				      Gal_DispatchFunctionSelector s,
				      Gal_DispatchFnSignatureLister l,
				      Gal_DispatchFunctionInvoker i,
				      void *invocation_client_data);
void *GalIO_GetServerClientData(GalIO_ServerStruct *server, const char *name);
Gal_DispatchFnPkg *GalIO_GetServerDispatchFnPkg(GalIO_ServerStruct *scomm);
void GalIO_SetServerDispatchFnPkg(GalIO_ServerStruct *scomm, Gal_DispatchFnPkg *pkg);
void GalIO_EnableDispatchFnValidation(GalIO_ServerStruct *scomm);
int GalIO_CommValidating(GalIO_CommStruct *gcomm);
int GalIO_ResetCommSockets(GalIO_ServerStruct *scomm);
int GalIO_CommWriteFrame(GalIO_CommStruct *gcomm, Gal_Frame frame, int do_block);
int GalIO_QueueProcessOut(GalIO_CommStruct *gcomm,  int do_block);
int GalIO_CommWriteMessage(GalIO_CommStruct *gcomm, Gal_Frame frame, GalIO_MsgType msg_type, int do_block);
void GalIO_CommFlushOutQueue(GalIO_CommStruct *gcomm);
int GalIO_CommReadReady(GalIO_CommStruct *gcomm);
int GalIO_CommWriteReady(GalIO_CommStruct *gcomm);
int GalIO_CommReadFrame(GalIO_CommStruct *gcomm, Gal_Frame *frame, int do_block);
int GalIO_CommReadMessage(GalIO_CommStruct *gcomm, Nframe *frame, GalIO_MsgType *msg_type_ptr, int do_block);
Gal_Frame GalIO_CommDispatchFrame(GalIO_CommStruct *gcomm, Gal_Frame dispatch, GalIO_MsgType *msg_type_ptr, const char *reply_key);
GalIO_ServerStruct *GalIO_ListenerCreate(unsigned short port, int require_port, GalIO_FrameHandler foo_ptr, void *server_data, int poll_ms, int max_connections);
int GalIO_ServerIsClient(GalIO_ServerStruct *scomm);
int GalIO_ServerIsListener(GalIO_ServerStruct *scomm);
int GalIO_ServerListensForConnections(GalIO_ServerStruct *s);
int GalIO_ServerListensForBrokers(GalIO_ServerStruct *s);
int GalIO_StartListener(GalIO_ServerStruct *scomm, int additional_flags);
int GalIO_StartClient(GalIO_ServerStruct *scomm);
void GalIO_SetDisconnectCallback(GalIO_CommStruct *gcomm, void (*disconnect_callback)(GalIO_CommStruct *server_data));
GalIO_Callback *
GalIO_AddServerCallback(GalIO_ServerStruct *scomm,
			int callback_event,
			GalIO_ServerCallbackFn fn,
			void *callback_data);
GalIO_Callback *
GalIO_AddServerConnectCallback(GalIO_ServerStruct *scomm,
			       GalIO_ServerConnectCallbackFn connect_callback,
			       void *callback_data);
void GalIO_RemoveServerCallback(GalIO_ServerStruct *scomm,
				GalIO_Callback *cb);
GalIO_Callback *
GalIO_AddConnectionCallback(GalIO_CommStruct *gcomm,
			    int callback_event,
			    GalIO_ConnectionCallbackFn connect_callback,
			    void *callback_data);
GalIO_Callback *
GalIO_AddConnectionBrokerCallback(GalIO_CommStruct *gcomm,
				  int callback_event,
				  GalIO_ConnectionBrokerCallbackFn connect_callback,
				  void *callback_data);
GalIO_Callback *
GalIO_AddConnectionDispatchFnCallback(GalIO_CommStruct *gcomm,
				      GalIO_ConnectionDispatchFnCallbackFn dispatch_callback,
				      void *callback_data);
void GalIO_RemoveConnectionCallback(GalIO_CommStruct *gcomm,
				    GalIO_Callback *cb);
void GalIO_SetServerDisconnectCallback(GalIO_ServerStruct *scomm, void (*disconnect_callback)(GalIO_CommStruct *server_data));
GalIO_CommStruct *GalIO_ClientInit(const char *host, unsigned short port,
				   GalIO_FrameHandler fnptr, int poll_ms);
int GalIO_OutHandler(GalIO_CommStruct *gcomm);
int GalIO_InHandler(GalIO_CommStruct *gcomm);
int GalIO_VerificationHandler(GalIO_CommStruct *gcomm);
int GalIO_ConnectionPoll(GalIO_CommStruct *gcomm);
int GalIO_ServerPoll(GalIO_ServerStruct *scomm);
int GalIO_ServerHandler(GalIO_ServerStruct *scomm, GalIO_CommStruct **new_conn_ptr);
int GalIO_ServerCallbackHandler(GalIO_ServerStruct *scomm, int read_blocking,
				GalIO_CommStruct **new_conn_ptr);
int GalIO_ConnectionCallbackHandler(GalIO_CommStruct *gcomm,
				    int read_blocking);
GalIO_ServerStruct *GalIO_ServerInit(unsigned short port, int require_port, GalIO_FrameHandler fnptr, void *server_data, int poll_ms, int max_connections);
GalIO_ServerStruct *GalIO_ServerCreate(unsigned short port, int require_port, GalIO_FrameHandler fnptr, void *server_data, int poll_ms, int max_connections);
GalIO_CommStruct *GalIO_ContactHub(const char *host, unsigned short port,
				   GalIO_ServerStruct *scomm,
				   const char *session_id,
				   int client_poll_flags);
void GalIO_ServerCheckHubContacts(GalIO_ServerStruct *scomm);
void GalIO_OperateOnConnections(GalIO_ServerStruct *scomm,
				void *arg,
				void (*op)(GalIO_CommStruct *, void *));
GalIO_ServerStruct *GalIO_ServerStart(GalIO_ServerStruct *scomm);
void GalIO_SetServerListenStatus(GalIO_ServerStruct *scomm,
				 int server_listen_status,
				 const char *client_pair_string,
				 const char *session_id);
GalIO_ServerLocation *GalIO_DigestServerLocations(const char *client_pair_string);
void GalIO_FreeServerLocations(GalIO_ServerLocation *loc);
GalIO_ServerLocation *GalIO_AddServerLocation(GalIO_ServerStruct *scomm,
					      const char *host,
					      unsigned short port,
					      int client_poll_flags);
int GalIO_NumServerLocations(GalIO_ServerLocation *locs);
char *GalIO_NthHostAndPort(GalIO_ServerLocation *locs, int i,
			   unsigned short *port);
GalIO_ServerLocation *GalIO_GetServerLocations(GalIO_ServerStruct *scomm);
int GalIO_ServerListenStatus(GalIO_ServerStruct *scomm);
char *GalIO_ServerSessionID(GalIO_ServerStruct *scomm);
GalIO_CommStruct *GalIO_ClientConnect(const char *name,
				      const char *host, unsigned short port,
				      int silent,
				      Gal_Frame welcome_frame,
				      Gal_Frame *reply_frame);
Gal_Frame GalIO_CreateErrorFrame(int errnum, const char *error);
int GalIO_GetError(Gal_Frame f, char **err_desc);
GalIO_CommStruct *GalIO_AcceptUniqueConnection(GalIO_ServerStruct *scomm);
/* added by spc 10/18/99 */
char *GalIO_MsgTypeToName(GalIO_MsgType mt);

Gal_Frame GalIO_ServerProperties(GalIO_ServerStruct *server);
void GalIO_AddServiceType(GalIO_ServerStruct *server, const char *stype);
void GalIO_ServerModifyProperties(GalIO_ServerStruct *server,
				  Gal_Frame new_properties,
				  char **delete_properties);

/* sockqueue.c */

/* we don't handle overflows gracefully so make the queues big enough! */
GalIO_SockQueueStruct *GalIO_CreateSockQueue(GAL_SOCKET sockfd, int in_size, int out_size);

/* destroy it - warning, unsent data will be lost! */
void GalIO_DestroySockQueue(GalIO_SockQueueStruct *q);

/* set the comment field */
void GalIO_SetSockQueueComment(GalIO_SockQueueStruct *q, char *comment);

/* get the socket error */
int GalIO_GetSockQueueError(GalIO_SockQueueStruct *q);

/* returns 1 if samples in queue, 0 if all samples sent, -1 if error */
int GalIO_SockQueueProcessSend(GalIO_SockQueueStruct *q);

/* returns 1 if samples in queue, 0 if queue is empty, -1 if error */
int GalIO_SockQueueProcessReceive(GalIO_SockQueueStruct *q);

/* returns 0 if success, -1 if insufficient space in queue */
int GalIO_SockQueueWrite(GalIO_SockQueueStruct *q, void *data, long len);

/* returns 0 if no data, 1 if data, -1 if error */
/* this isn't very efficient if characters are read out of the queue one at a time. */
int GalIO_SockQueueRead(GalIO_SockQueueStruct *q, char *data, int max_data_len, int *data_len);

/* returns 0 if complete atom cannot be read, 1 if atom complete, -1 if error */
int GalIO_SockQueueReadAtom(GalIO_SockQueueStruct *q, char *data, int data_len);

/* broker_proxy.c */
int GalSS_BrokerProxyReadReady(GalSS_BrokerProxy *bp);

#endif /* #ifndef __H_GALAXY_IO_ */
