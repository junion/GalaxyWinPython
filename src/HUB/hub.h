/*
  Portions of this file (c) Copyright 1998 - 2000 M.I.T.
  Portions of this file (c) Copyright 1999 - 2000 The MITRE Corporation.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef _GalHUB_H
#define _GalHUB_H

#ifndef WIN32
#include <time.h>
#endif

#include "galaxy/galaxy_io.h"
#include "galaxy/program.h"

#define DEFAULT_SESSION_ID "Default"

/* status of the server connection */
#define LOGDISCONNECTED -4  /* disconnected, already logged */
#define DISCONNECTED -3
#define LOCAL        -2
#define FREE         -1
/* >= 0 is token index that has been sent to it and for which we await a reply */

#define MAX_N 10
#define MAX_UTTS 1
#define MAX_USERS 2
#define MAX_SERVERS 1024
#define MAX_LOG_MESSAGES 1024

#define MAX_SERVER_MESSAGES 100

#define BUILTIN_SERVER "Builtin"

/* Session Defines */

/* This key records the user for a session. */
#define GAL_HUB_SESSION_USER_HUB_FRAME_KEY ":user"

/* This key is used by the scripting module to mark the
   server for the frame as unavailable. */
#define GAL_HUB_SERVER_UNAVAILABLE_HUB_FRAME_KEY ":hub_server_unavailable"

/* This key is where the Hub stores its utterance ID for the session. */
#define GAL_HUB_SESSION_UTTERANCE_ID_HUB_FRAME_KEY ":utterance_id"

/* This key marks a token state as not being abortable. */
#define GAL_NO_ABORT_HUB_FRAME_KEY ":no_abort"

/* This key tracks how many session errors there have been. */
#define GAL_SESSION_ERROR_COUNT_HUB_FRAME_KEY ":session_error_count"

/* This is the key which contains opaque data for the scripting module. */
#define GAL_OPAQUE_SCRIPT_MODULE_HUB_FRAME_KEY ":hub_program_info"

/* This is the key which contains the information about scriptfulness. */
#define GAL_SCRIPT_STATUS_HUB_FRAME_KEY ":scriptless"

/* This is the key which tells which stype was invoked. */
#define GAL_INVOKED_STYPE_HUB_FRAME_KEY ":invoked_stype"

/*  This is the key which stores the log prefix in the session. */
#define GAL_HUB_LOG_PREFIX_HUB_FRAME_KEY ":hub_log_prefix"

/* This is the key which stores the log directory in the session. */
#define GAL_HUB_LOGDIR_HUB_FRAME_KEY ":hub_logdir"

/* This is the key which stores the log file in the session. */
#define GAL_HUB_LOGFILE_HUB_FRAME_KEY ":hub_logfile"

/* This is the directory the Hub thinks it's in. */
#define GAL_HUB_PWD_HUB_FRAME_KEY ":hub_pwd"

/* Database and Session defines */
/* maximum number of dialogue exchanges in a single session */
#define Max_Abort_Utts 1000 /* this is used for the size of session->aborted_utts */
#define Max_History_Utts 50 
#define Min_Utts 10
#define GalHUB_MaxDomains 10

typedef struct LOGFILE 
{	
  FILE *logfile;
  int session_under_day;
  int begin_written;		/* wrote BEGIN_UTT */
} LOGFILE; 

typedef struct LOGRECORD
{ 
  Gal_HashTable msg_ht;

  /* special timestamps */
  int log_serve_any_session;
  int log_serve_this_session_only;
  int log_get_session_lock;
  int log_release_session_lock;
  int log_alarm_activity;
  int log_system_errors;
  char *user_version;
} LOGRECORD;

/* SAM 4/29/02: The LOGKEYS structure will now hold a bit
   more information than it used to. It will continue to keep
   track of the in and out keys for programs and messages. However,
   it will also keep track of the DESCRIPTIONS of providers
   in the case of the TIMESTAMP: line. Arrays of this structure
   will now be the value of everything in the msg_ht, and we
   have to be careful to make sure we have an empty spec when
   we mean there are no conditions. 
   This will be distinguished from the NULL at the end of the array. */

typedef struct LOGKEYS
{
  GalSS_ProviderSpec *spec;
  int literal_match;
  /* SAM 4/30/02: edges_only means only when the Hub is reading a
     new message or writing a reply. */
  int edges_only;
  /* SAM 4/30/02: if this is declared by virtue of the scripting
     language (e.g., PROGRAM: instead of MESSAGE:), then it's only
     relevant sometimes (e.g., not when there's a target provider ID
     and the scripting is going to be bypassed. */
  int from_script;
  EntityPair **in;
  EntityPair **out;
} LOGKEYS;

/* This is a holder for hub control information */
typedef void *CONTROL_INFO; 

/* This is for what kind of server it is. */

enum {GAL_HUB_STYPE_CLIENT, /* A connection to a remote server. */
      GAL_HUB_STYPE_SERVER /* A server which remote clients contact. */
};

/* How many servers the listener proxy makes room
   for at a time. */

#define LISTENER_PROXY_SERVER_INCREMENT 10

/* How many listener proxies the Hub makes room
   for at a time. */

#define LISTENER_PROXY_HUB_INCREMENT 10

typedef struct LISTENER_PROXY {
  int port;
  GalIO_ServerStruct *scomm;
  Gal_PointerBuffer *stypes;
} LISTENER_PROXY;

/* SAM 11/15/00: I split the SERVER structure into a 
   SERVICE_TYPE and a SERVICE_PROVIDER. */

struct SERVICE_PROVIDER;
struct SESSION;
struct SESSION_LOCK_INFO;
struct SPECIAL_SERVER;

typedef struct SERVICE_TYPE {
  char *name;    
  /* If listener_port is not -1, we will set up a
     LISTENER_PROXY, which will in turn have a GalIO_ServerStruct 
     which will listen for connections. This LISTENER_PROXY may be
     used with multiple servers. The LISTENER_PROXY will use
     the handshake return to determine which of the servers it points
     back to should be used. If the connection which is
     established turns out to be general-purpose, that should be the
     only general-purpose connection which is accepted. If a connection
     is locked to a session, we'll make it a dispatch server. */
  LISTENER_PROXY *listener_proxy;
  int listener_port;
  /* If it's a listener. */
  int require_port;
  int sockid;
  char		**operations;
  KeyPair	**init_kps;
  Gal_PointerBuffer *providers;
  KeyPair **properties;
  Gal_TestClause *conditions;
  /* These are the default keys for any server. */
  EntityPair **in;
  /* This is if it's a special server. */
  struct SPECIAL_SERVER *special;
} SERVICE_TYPE;

typedef struct SP_WORKING_ON {
  /* this server is busy with this session */
  struct SESSION *session;  
  /* token index */
  int tidx;
  /* chosen stype */
  char *stype_name;
  /* is the message being handled scriptless? */
  int scriptless;
} SP_WORKING_ON;

typedef struct SERVICE_PROVIDER {
  Gal_PointerBuffer *stypes;
  char *host;
  int  port;
  GAL_SOCKET sock;
  int sockid;
  GalIO_CommStruct *gcomm;
  /* Provided by the scripting for reference purposes. */
  char *id_name;
  /* LOCAL, DISCONNECTED, FREE, >= 0 means working on tidx */
  int        	status;
  /* waiting for a reply from a module-to-module (M-M) */
  int	        awaiting_mm_reply;
  int        	id;
  /* This information is duplicated in the session. */
  struct SESSION_LOCK_INFO *only_session_to_write_to;
  struct SESSION_LOCK_INFO *only_session_to_read_from;

  SP_WORKING_ON working_on;
  
  KeyPair	**init_kps;
  Gal_DispatchFnSignature *signatures;
  int listen_status;
  Gal_Frame properties;
  Gal_TestClause *conditions;
  /* Internal name */
  char iname[512];
  /* Pretty name */
  char pname[512];
  /* These are the default keys for any server. */
  EntityPair **in;
} SERVICE_PROVIDER;

struct SERVER_MESSAGE;

/* These are all the things which ought to be done right
   before the write. */

enum {GAL_INCOMING_MSG, GAL_OUTGOING_MSG};

#define MAX_NAMESPACES 6

enum {GAL_SESSION_NAMESPACE,
      GAL_UTTERANCE_DB_NAMESPACE,
      GAL_MESSAGE_NAMESPACE,
      GAL_SERVER_NAMESPACE,
      GAL_TOKEN_NAMESPACE,
      GAL_GLOBAL_NAMESPACE};

/* There are a few possible provider reasons. */

enum {GAL_SESSION_MUST_WRITE_TO_PROVIDER,
      GAL_PROVIDER_MUST_READ_FROM_SESSION,
      GAL_SINGLE_PROVIDER_FOR_SERVICE,
      GAL_PROVIDER_REQUESTED_BY_ID,
      GAL_RANDOM_SELECTION};

typedef struct SESSION_LOCK_INFO {
  int value;
  SERVICE_PROVIDER *provider;
  struct SESSION *session;  
  int via_listener;
} SESSION_LOCK_INFO;

typedef struct SESSION
{
  /* a string whose format is determined by the hub,
     use for identification within one hub */
  char *session_id;
  /* the utterance within the current session */  
  int utterance_id;
  /* a pointer into the discourse history */
  int discourse_id;

  struct LOGFILE *log;

  /* time this timer was last reset */
  time_t *alarm_last_reset;
  /* # secs to expiration for this alarm */
  int *alarm_secs_to_expiration;
  /* true means disabled */
  int *alarm_disabled;
  /* a list of the utterance_id's of the aborted utterances */       	
  int *aborted_utts;		

  Gal_Frame session_vars;

  int num_system_errors;
  Gal_Frame *history;
  /* prior histories after domain switching !! */
  Gal_Frame **domain_histories;

  /* I was going to put all the lock information on the
     session rather than the server, but the server
     exclusive read/write locks really need to be on
     the servers. Arrgh. So this information is duplicated
     in the service providers. */
  Gal_PointerBuffer *lock_info;
  struct SESSION *next;

  /* to avoid reentrancy in down list */
  struct SERVICE_PROVIDER *server_down_list[MAX_SERVERS];	
  int n_server_down_list;
  /* SAM 11/30/00: It turns out that if you're going
     to reuse a session, the only sense we can make of
     it is to say that the first time new_session is called,
     it's superfluous, but each subsequent time, it
     resets the session. So we'll encode that logic
     explicitly. */
  int new_session_called;
  /* Sometimes, sessions don't actually get deleted if
     you end them, because other servers which are
     locked to them are still active. If this flag is set,
     we can free the session. */
  int session_ended;
  Gal_PointerBuffer *continuation_buffer;
} SESSION;

#define PROVIDER_FLAG_CELL_INCREMENT 20

typedef struct PROVIDER_FLAG_CELL {
  int provider_id;
  SERVICE_TYPE *stype;
} PROVIDER_FLAG_CELL;

typedef struct PROVIDER_FLAG_ARRAY {
  int alloc_size;
  int buffer_size;
  PROVIDER_FLAG_CELL *cells;
} PROVIDER_FLAG_ARRAY;

typedef struct SERVER_MESSAGE
{
  Gal_Frame message;
  int direction;
  GalIO_MsgType msg_type;
  /* This is for the TRUE CANONICALIZED NAME
     which is computed from the service_name
     and the provider_id_name. These should be
     separate because I'll want to compare messages
     already in the queue with incoming servers,
     and with the info in the logging hash table. */
  GalSS_ProviderSpec true_provider_spec;
  char *bare_operation;
  /* Each message has an array of provider IDs and
     info flags. */
  PROVIDER_FLAG_ARRAY provider_array;
  /* These are for the SELECTED PAIR. */
  SERVICE_TYPE *stype;
  SERVICE_PROVIDER *provider;
  int provider_reason;
  int lock_mask;
  int lock_value;
  int no_return;
  KeyPair *kp_array;
  int force_timestamp;
  int scriptless;
  Gal_Object opaque_script_info;
  int tidx;
  SESSION *session;
  Gal_Frame namespace_array[MAX_NAMESPACES];
} SERVER_MESSAGE;

typedef struct TOKEN
{
  int          tidx;            /* user index */
  int          server_tidx;     /* token index for return (from server) */
  int          uttidx;          /* a pointer into the discourse (:discourse_id) */
  int	       mm;		/* processing a module-to-module (M-M) request */
  char 	       *session_id;			
  char         *name;
  CONTROL_INFO *ctrl_info;      /* pointer to scripting control information */
  void (*ctrl_info_free_fn)(CONTROL_INFO *); /* free fn for control info */

  Nframe       state;
  SERVICE_PROVIDER  *owner;           /* owner of token --> return/dispose */
  struct TOKEN *prev;
  struct TOKEN *next;
  int           ref;            /* number of references - used for garbage collection */
  int 		destroy; 	/* mark for destruction */
  double        timestamp;      /* timestamp in seconds, with millisecond precision */
  char *target_provider_id;
} TOKEN;

/* Special servers are used for Hub-specific functions, like
   visualization, gui, etc. If they are initialized as
   listeners, they can accept only one connection. These
   providers and listeners are not folded in to the normal
   list of listeners, etc., since access to them is restricted
   to the Hub. Their signatures are also fixed. They CAN send
   messages to the Hub, so they're polled as normal servers,
   but otherwise, they're unusual. */

typedef struct SPECIAL_SERVER {
  SERVICE_TYPE *stype;
  SERVICE_PROVIDER *provider;
} SPECIAL_SERVER;

typedef struct HUB 
{ 
  char      *hub_control_file_name;
  Nframe 	init_state;
  KeyPair ** init_keys;

  int         num_tokens;
  TOKEN      *token;

  /* Alarms */
  int num_alarms;

  /* These are the service PROVIDERS. */
  Gal_PointerBuffer *servers;
  /* a list of the subset of servers that we want to connect to */
  Gal_PointerBuffer *active_servers;

  /* These are the service TYPES. The service type
     holds the server queue. */
  Gal_PointerBuffer *stypes;
  /* The HUB has a series of listener proxies which
     point back to a set of servers. */
  Gal_PointerBuffer *listener_proxies;
  
  char      *session_id_key;
  char      *user_id_key;

  int        maxfd;
  int        localReady;

  int        debug;
  int        suppress_pacifier;
  int        validate;

  char 	     *default_domain_key;
  Gal_Frame globals;

  int 	logging_id;

  short script;

  /* log stuff */
  char *data_topdir_key;
  struct LOGRECORD *log_record;

  /* Special servers. At the moment, we
     only have one: the gui. */
  SPECIAL_SERVER *gui;

  /* Builtin server. */
  GalIO_CommStruct *local_server;

  /* This is to make the Hub run more slowly. Only
     applies to non-Builtin. This can be used for demo
     purposes. In milliseconds. */
  int hub_pause;
  GalIO_PointerQueue *message_queue;
  int msg_queue_length;  
} HUB;

extern HUB *Hub;

/* These are the namespaces. */

/* Just to be on the safe side. This
   should be the number of enums. */

extern Gal_NamespaceEntry HubNamespaceTable[];

/* HUB export functions to CONTROL units */
/* hub_util.c */
TOKEN *GalHUB_GetTokenFromIndex(int tidx);
TOKEN *GalHUB_NewToken(Gal_Frame state, SERVICE_PROVIDER *s, int uttidx,
		       char *session_id, Gal_Frame admin_info);
void GalHUB_OutlineFrame(Gal_Frame fr);

/* General Seters */
void GalHUB_SetHubToken(HUB *hub, TOKEN *t);
void GalHUB_SetHubInitialKeys(HUB *hub, KeyPair **init_keys);
void GalHUB_SetHubInitState(HUB *hub, Gal_Frame init_state);
void GalHUB_SetHubLogTopDataDir(HUB *hub, char *dir, char *dir_key);
void GalHUB_SetHubUserID(HUB *hub, char *user_id, char *user_id_key);
void GalHUB_SetHubSessionId(HUB *hub, char *session_id, char *session_id_key);
void GalHUB_SetHubDefaultDomain(HUB *hub, char *domain, char *domain_key);
void GalHUB_SetHubActiveServers(HUB *hub, Gal_PointerBuffer *active_servers);
void GalHUB_SetHubServices(HUB *hub, Gal_PointerBuffer *providers,
			   Gal_PointerBuffer *service_types);
void GalHUB_SetHubGlobal(HUB *hub, char *key, Gal_Object value);

/* General Getters */
int GalHUB_GetNumServers(HUB *hub);
Gal_Object GalHUB_GetHubUserID(HUB *hub);
Gal_Object GalHUB_GetHubDefaultDomain(HUB *hub);
Gal_Object GalHUB_GetHubSessionId(HUB *hub);
Gal_Object GalHUB_GetHubLogTopDataDir(HUB *hub);
int GalHUB_GetNewServerID();
Gal_Object GalHUB_GetHubGlobal(HUB *hub, char *key);
void GalHUB_DeleteHubGlobal(HUB *hub, char *key);
SERVICE_TYPE *GalHUB_FindServiceType(HUB *h, char *name);

/* Search functions */
int GalHUB_FindServiceTypeForOperation(HUB *h, SERVER_MESSAGE *msg, 
				       Gal_Frame *namespace_array);
int GalHUB_PerhapsAddProvidersToMsg(SERVER_MESSAGE *msg,
				    SERVICE_TYPE *stype,
				    Gal_Frame *namespace_array,
				    SERVICE_PROVIDER **providers,
				    int num_providers);
SESSION *GalHUB_FindSessionForFrame(Gal_Frame frame, int create);
SERVICE_PROVIDER *
GalHUB_ProviderSpecToProvider(HUB *h, GalSS_ProviderSpec *spec);

/* Server related */
/* Public constructors */
void GalHUB_EnqueueServerMessage(HUB *h, TOKEN *t, SERVER_MESSAGE *msg);
void GalHUB_AddEligibleProvider(SERVER_MESSAGE *msg, SERVICE_TYPE *stype,
				SERVICE_PROVIDER *provider);
SERVER_MESSAGE *GalHUB_NewServerMessage(Gal_Frame f,
					int direction,
					GalIO_MsgType msg_type,
					char *service_name,
					char *provider_id_name,
					SERVICE_PROVIDER *provider,
					int lock_mask,
					int lock_value,
					int no_return,
					int scriptless,
					Gal_Object opaque_script_info,
					int force_timestamp,
					TOKEN *t,
					SESSION *s);
void GalHUB_PopulateServerMessageNamespaceArray(SESSION *session,
						SERVER_MESSAGE *msg, TOKEN *t);
void GalHUB_FreeServerMessage(SERVER_MESSAGE *msg);
SERVICE_PROVIDER *GalHUB_NewServiceProvider(char *host, int port,
					    SERVICE_TYPE *stype);
char *GalHUB_ServiceProviderID(SERVICE_PROVIDER *s);
void GalHUB_ModifyServiceProvider(SERVICE_PROVIDER *s, char *hostname, int port);
void GalHUB_RecordServiceProvider(HUB *hub, SERVICE_PROVIDER *s);
void GalHUB_AddServiceProviderStype(SERVICE_PROVIDER *s, SERVICE_TYPE *stype);
SERVICE_TYPE *GalHUB_NewServiceType(char *name, int listener_port);
void GalHUB_RemoveServiceType(SERVICE_TYPE *stype, HUB *h,
			      int remove_from_providers, int remove_from_hub);
void GalHUB_RemoveServiceProvider(HUB *h, SERVICE_PROVIDER *s,
				  int remove_from_stype, int remove_from_hub);
/* Load-time specific functions */
void GalHUB_LRemoveServiceProvider(HUB *h, SERVICE_PROVIDER *s,
				   int remove_from_stype, int remove_from_hub);
/* HUB Session API */
SESSION *GalHUB_SessionNew(Gal_Frame frame);
Gal_Frame *GalHUB_SessionFetchDB(char *session_id);
int GalHUB_SessionRestoreSwitchedHistory(SESSION *session, char *switch_domain);
int GalHUB_SessionResetHistory(SESSION *session, char *current_domain, char *switch_domain);
void GalHUB_SessionEnd(HUB *h, SESSION *session);
int GalHUB_SessionFlushLocks(SESSION *s);
void GalHUB_SessionFlushLogfile(SESSION *s);
void GalHUB_SessionFlushQueues(HUB *h, SESSION *s);
void GalHUB_SessionSetVar(SESSION *s,char *var,TObj value);
/* this does NOT return a copy... */
TObj GalHUB_SessionGetVar(SESSION *s,char *var);
void GalHUB_SessionDeleteVar(SESSION *s,char *var);
void GalHUB_SessionFreeVars(SESSION *s);
SESSION *GalHUB_SessionLookupByID(char *session_id, int create);
SESSION *GalHUB_SessionLookupBySocketDescriptor(int sockid); 
SESSION *GalHUB_SessionLookupByFrame(Gal_Frame frame, int create);
SESSION *GalHUB_SessionLookupByFrameInHUB(Gal_Frame frame, int create);
int GalHUB_SessionGetAbortUtt(int utterance_id, char *session_id);
void GalHUB_SessionSetAbortUtt(int utterance_id, char *session_id);
char *GalHUB_SessionCreateID();
void GalHUB_SessionAlarmDisable(SESSION *s, char *key);
void GalHUB_SessionAlarmEnable(SESSION *s, char *key);
void GalHUB_SessionAlarmResetTo(SESSION *s, char *key, int secs);
void GalHUB_SessionHandleLocks(SESSION *session, SERVICE_PROVIDER *server, int tidx, int lock_mask, int lock_value, int log_locks, int force_unlock, int via_listener);
void GalHUB_SessionDeduceLocks(SESSION *session, SERVICE_PROVIDER *s,
			       int *server_writes_to_session,
			       int *server_reads_from_session,
			       int *session_writes_to_server);

/* HUB Alarm API */
char *GalHUB_AlarmGetDisableKey();
char *GalHUB_AlarmGetEnableKey();
char *GalHUB_AlarmIndexToName(int i);
int GalHUB_AlarmNameToIndex(char *name);
void GalHUB_AlarmAdd(char *alarm);
int GalHUB_AlarmGetNum(void);

/* HUB Database API */
void GalHUB_DBErase(Gal_Frame *db);
void GalHUB_DBFree(Gal_Frame *db);
void GalHUB_DBEraseHistories(SESSION *session, int free_db);
Gal_Frame *GalHUB_DBInit();
void GalHUB_DBStoreInHistory(char *session_id, char *key, Gal_Object value);
Gal_Object GalHUB_DBRetrieveFromHistory(char *session_id, char *key, int offset);
void GalHUB_DBClearHistory(Nframe frame);
void GalHUB_DBPrint(Nframe *db) ;
void GalHUB_DBEraseAllHistory(Gal_Frame frame);

/* HUB Logfile API */
void GalHUB_LogfileLogMessage(SESSION *session, SERVICE_PROVIDER *s,Gal_Frame fr, char *category, char *mt);
void GalHUB_LogfileLogError(SESSION *session, SERVICE_PROVIDER *s,Gal_Frame fr, char *mt);
void GalHUB_LogfileLogEvent(SESSION *session, SERVICE_PROVIDER *s, char *category, char *event, int tidx);
void GalHUB_LogfileLogAlarmEvent(SESSION *session, char *alarm_name, char *condition);
void GalHUB_LogfileLogLockEvent(SESSION *session, SERVICE_PROVIDER *s, char *event, int tidx);
void GalHUB_LogfileTimestampKey (SESSION *s, char *key, char *event, int tidx);
void GalHUB_LogfileRecordUtteranceForSession(SESSION *s); /* used by builtin */
void GalHUB_LogfileLogServerStatus(SESSION *sessions, SERVICE_PROVIDER *server, char *status_key);
int GalHUB_TimestampIsForced(SERVER_MESSAGE *msg, LOGRECORD *rec,
			     int on_edge, int from_script_ok);
void 
GalHUB_LogfileUpdateServerMessageFromEntityPairs(EntityPair **ep_list, SERVER_MESSAGE *msg,
						 Gal_Frame *namespace_array);
void GalHUB_LogErrorMessageKeys(SERVER_MESSAGE *msg);
void GalHUB_InstantiateLogRecord(HUB *h);
void GalHUB_FreeLogRecord(HUB *h);
void GalHUB_AddMessageToTimestampTable(HUB *h, char *msg_name,
				       EntityPair **in, EntityPair **out,
				       GalSS_ProviderSpec *provider_conds,
				       int literal_match,
				       int edges_only,
				       int from_script);
int GalHUB_ProviderSpecsMatch(GalSS_ProviderSpec *reference_spec,
			      GalSS_ProviderSpec *spec_candidate,
			      int literal_match);
int GalHUB_SpecFoundInActiveServers(GalSS_ProviderSpec *spec,
				    Gal_PointerBuffer *active_servers);
int GalHUB_StypeFoundInActiveServers(char *stype_name,
				     Gal_PointerBuffer *active_servers);
int GalHUB_ProviderFoundInActiveServers(SERVICE_PROVIDER *p,
					SERVICE_TYPE *stype,
					Gal_PointerBuffer *active_servers);
/* HUB CONTROL FUNCTIONS */
int HC_NextOperations(HUB *h, TOKEN *t, int debug);
int HC_UpdateToken(TOKEN *t, SERVER_MESSAGE *msg);
int HC_ReadControlFile(HUB *hub, char *filename, char *startup_vars);
void HC_FreePrograms();
void HC_PrepareOperation(SESSION *session, TOKEN *t, SERVER_MESSAGE *msg);
#endif  /* _GalHUB_H */
