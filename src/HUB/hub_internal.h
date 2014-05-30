/*
  Portions of this file (c) Copyright 1998 - 2000 M.I.T.
  Portions of this file (c) Copyright 1999 - 2000 The MITRE Corporation.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "hub.h"
#include "galaxy/generic-server.h"

extern char *GalHUB_AsynchronousBuiltins[];

/* hub_util.c */
double generate_timestamp();
void destroyTokenIfDone(HUB *h, TOKEN *t, SESSION *session,
			Gal_Frame return_msg,
			GalIO_MsgType return_type);
void printTokens(HUB *h);
TOKEN *getToken(HUB *h, int tidx);
int list_member(char *name, char **list);

HUB *GalHUB_GetHubFromLocalServerData(GalIO_CommStruct *gcomm);
void GalHUB_EnqueueHubContinuation(SESSION *s,
				   int tidx,
				   Gal_Object script_info,
				   int scriptless,
				   char *invoked_stype,
				   Gal_Frame *reply_matches,
				   Gal_Frame *error_matches,
				   char *stype_name,
				   int provider_id);
int GalHUB_HubContinuationMatched(HUB *h, SERVER_MESSAGE *msg);
int send_to_local_server(HUB *h, Gal_Frame fr, GalIO_MsgType t);
int local_server_ready(HUB *h);
int read_from_local_server(HUB *h, Gal_Frame *frame, GalIO_MsgType *t);
void GalHUB_InitializeLocalServer(HUB *h);
void GalHUB_DestroyLocalServer(HUB *h);
int _gal_hub_break(HUB *h, TOKEN *t, SERVICE_PROVIDER *sp, Gal_Frame msg,
		   SESSION *session);
GalSS_ProviderSpec *GalHUB_SplitServerName(char *server_string);
GalSS_ProviderSpec *GalHUB_SplitServerAndLocation(char *server_string,
						  int permit_location);
void free_key_pairs(KeyPair **old_pairs);
void free_entity_pairs(EntityPair **ps);
EntityPair **copy_entity_pairs(EntityPair **pairs);

/* logfile.c */
void GalHUB_LogfileUpdateMsgOnSend(SERVER_MESSAGE *msg, GalIO_MsgType mt,
				   TOKEN *t);
void GalHUB_LogfileUpdateMsgOnRead(SERVICE_PROVIDER *server,
				   SERVER_MESSAGE *msg, GalIO_MsgType mt,
				   TOKEN *t);

void GalHUB_LogfileLogGetSessionLock(SESSION *session,SERVICE_PROVIDER *server, int tidx);
void GalHUB_LogfileLogReleaseSessionLock(SESSION *session,SERVICE_PROVIDER *server, int tidx);
void GalHUB_LogfileLogServeAnySession(SESSION *session,SERVICE_PROVIDER *server, int tidx);
void GalHUB_LogfileLogServeThisSessionOnly(SESSION *session,SERVICE_PROVIDER *server, int tidx);
LOGKEYS *GalHUB_LogfileGetLogkeysFromHub(HUB *h, SERVER_MESSAGE *msg,
					 int from_script_ok);
LOGKEYS *GalHUB_LogfileGetLogkeysFromHubViaFrameName(HUB *h, char *name,
						     int from_script_ok);

void free_logrecord(LOGRECORD *log);
void free_logfile(LOGFILE *log);
void open_logfile (SESSION *session);
void close_log_file(SESSION *s);
int logfile_is_open(SESSION *s);
void make_timestamps (char *ds, char *ts);

/* hub_process.c */
void process_sessions(HUB *h);
TObj interpret_value(char *value);
void test_server();
void GalHUB_IncrementTokenReference(TOKEN *t,int where);
void GalHUB_DecrementTokenReference(TOKEN *t,int where);
void print_session_locks(int level, int debug);
void force_hub_exit();
int GalHUB_PerhapsAddServerToFD(SERVICE_PROVIDER *server,
				fd_set *writefd_ptr,
				fd_set *readfd_ptr);
SERVICE_PROVIDER *GalHUB_CheckServerFD(HUB *h, SERVICE_PROVIDER *server,
				       fd_set *writefd_ptr,
				       fd_set *readfd_ptr);
void GalHUB_AttemptServerReconnection(HUB *h, SERVICE_PROVIDER *s,
				      int special);
void GalHUB_ReturnMMToOwner(HUB *h, TOKEN *t, Gal_Frame frame,
			    SESSION *session, GalIO_MsgType msg_type);
/* hub_init.c */
HUB *init_hub(HUB *h, char *init_vars,
	      char *location_overrides,
	      char *locations_file);
HUB *new_hub(char *filename, int validate, int debug,
	     int suppress_pacifier, char *gui_location);
void quit_hub(HUB *hub);
void free_hub(HUB *hub);
int initialize_connection_to_server(SERVICE_PROVIDER *s, HUB *h, int silent, int special, int reconnect);
int initialize_connections(HUB *h);
void override_locations(HUB *h, char *location_overrides);
void override_locations_with_file(HUB *h, char *locations_file);
void GalHUB_AcceptClientConnection(LISTENER_PROXY *p, HUB *h);
void _GalHUB_CopyInitKeys(Gal_Frame fr, KeyPair **init_kps, int override);
int _GalHUB_InitializeInternalHubServer(SERVICE_TYPE *stype, HUB *h);

/* session.c */
void rotate_abort_utts(int *abort_utt_list, int max_utts, int min_utts);
int GalHUB_SessionMustWriteToProvider(SESSION *session, SERVICE_PROVIDER *s);
void free_session(SESSION *session);
void _GalHUB_SessionUnlockAndEnd(HUB *h, SESSION *session, int force_unlock);
int _GalHUB_SessionUnlockAndFlushLocks(SESSION *s, int force_unlock);

/* server_queue.c */
SERVER_MESSAGE *GalHUB_RemoveQueue(HUB *h,SERVICE_PROVIDER *s);
void GalHUB_InsertQueue(HUB *h, TOKEN *t, SESSION *session,
			SERVER_MESSAGE *mes);

/* hub_special.c */

SPECIAL_SERVER *GalHUB_CreateSpecialServer(char *server_location,
					   char *stype_name);
void GalHUB_FreeSpecialServer(SPECIAL_SERVER *s);
void GalHUB_InitializeSpecialServers(HUB *h);
void GalHUB_InitializeSpecialServer(HUB *h, SPECIAL_SERVER *special_s);
int GalHUB_PerhapsAddSpecialServer(SPECIAL_SERVER *special_s,
				   fd_set *writefd, fd_set *readfd);
int GalHUB_PerhapsAddSpecialServers(HUB *h, fd_set *writefd, fd_set *readfd);
void GalHUB_ReconnectToSpecialServers(HUB *h);
void GalHUB_ReconnectToSpecialServer(HUB *h, SPECIAL_SERVER *special_s);
void GalHUB_CheckSpecialServerFD(HUB *h, SPECIAL_SERVER *special_s,
				 fd_set *writefd_ptr, fd_set *readfd_ptr);
void GalHUB_CheckSpecialServerFDs(HUB *h, fd_set *writefd_ptr,
				  fd_set *readfd_ptr);
int GalHUB_IsSpecialServer(HUB *h, SERVICE_PROVIDER *provider);

/* special server update functions */

void _GalHUB_GUIAnnounceServiceType(HUB *h, SERVICE_TYPE *stype);
void _GalHUB_GUIAnnounceServiceProvider(HUB *h, SERVICE_PROVIDER *provider);
void _GalHUB_GUIAnnounceServiceProviderConnection(HUB *h,
						  SERVICE_PROVIDER *provider,
						  int connected,
						  int reconnect);
void _GalHUB_GUIAnnounceMessage(HUB *h, Gal_Frame msg, GalIO_MsgType msg_type,
				SERVICE_PROVIDER *provider, char *destination);
void _GalHUB_GUIAnnounceAvailableProviders(HUB *h, SERVICE_PROVIDER **providers, int num_providers);

/* hub_report_status.c */ 

#define GAL_HUB_SESSION_ALARM_STATUS 0
#define GAL_HUB_SESSION_ALARM_ENABLED 1 
#define GAL_HUB_SESSION_ALARM_DISABLED 2
#define GAL_HUB_SESSION_ALARM_RESET 3
#define GAL_HUB_SESSION_ALARM_EXPIRED 4

void _GalHUB_ReportSessionAlarmStatus(HUB *h, int level, int action, char* key,
				      char* session_id, int expiration_secs,
				      int remaining_secs);
void _GalHUB_CReportSessionAlarmStatus(HUB *h, int level, int fore, int back, 
				       int action, char* key, char* session_id, 
				       int expiration_secs, int remaining_secs);

#define GAL_HUB_SESSION_LOCK_STATUS 0
#define GAL_HUB_SESSION_LOCK_SET 1
#define GAL_HUB_SESSION_LOCK_RELEASED 2

void _GalHUB_ReportSessionLockStatus(HUB *h, int level, int action,
				     SESSION *sessions, int lock_type, 
				     char *session_id, int debug, 
				     char *fmt, ...);
void _GalHUB_CReportSessionLockStatus(HUB *h, int level, int fore, int back, 
				      int action, SESSION *sessions,
				      int lock_type, char *session_id, 
				      int debug, char *fmt, ...);

#define GAL_HUB_TOKEN_STATUS 0
#define GAL_HUB_TOKEN_CREATED 1
#define GAL_HUB_TOKEN_DESTROYED 2

void _GalHUB_ReportTokenStatus(HUB *h, int level, int action, TOKEN *t, int debug);
void _GalHUB_CReportTokenStatus(HUB *h, int level, int fore, int back, int action, TOKEN *t, int debug);

#define GAL_HUB_SESSION_SETTINGS 0
#define GAL_HUB_SESSION_CREATED 1
#define GAL_HUB_SESSION_DESTROYED 2
#define GAL_HUB_SESSION_HISTORY 3

void _GalHUB_ReportSessionStatus(HUB *h, int level, int action, SESSION *session, int debug);
void _GalHUB_CReportSessionStatus(HUB *h, int level, int fore, int back, int action, SESSION *session, int debug);

#define GAL_HUB_LISTENER_INITIALIZED 0
#define GAL_HUB_LISTENER_INITIALIZATION_ERROR 1

void _GalHUB_ReportListenerStatus(HUB *h, int level, int action, int port, int reused);
void _GalHUB_CReportListenerStatus(HUB *h, int level, int fore, int back, int action, int port, int reused);

#define GAL_HUB_MESSAGE_QUEUE_CONTENTS 0

void _GalHUB_ReportMessageQueueStatus(HUB *h, int level, int action);
