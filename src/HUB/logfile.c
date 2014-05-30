/*
  Portions of this file (c) Copyright 1998 - 2000 M.I.T.
  Portions of this file (c) Copyright 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include <time.h>
#ifndef WIN32
#include <sys/time.h>
#endif
#include <sys/timeb.h>
#include <fcntl.h>
#include "galaxy/galaxy_all.h"
#include "hub_internal.h"
#ifdef WIN32
#include <direct.h>
#include <errno.h>
#endif

static char *months[12] = {"JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"};

#define LOG_EVENT_MAX 100

static void record_begin_utt(SESSION *s);
static void
UpdateRecordFromKeyPair(SERVER_MESSAGE *msg,
			char *event, GalIO_MsgType msg_type);
static void GalHUB_LogfileUpdateRecord(char *key, Gal_Object value, SESSION *session);
static void update_timestamps(SERVER_MESSAGE *msg, char *event,
			      char *msg_type, SERVICE_PROVIDER *server,
			      int force, int on_edge, int from_script_ok);
static void dc_print_timestamp (SESSION *s, char *event, char *msg_type, int tidx, char *op, SERVICE_PROVIDER *server);
static void
dc_print_event_timestamp(SESSION *session, char *event, char *sub_event, char *condition);
/* Define the version number for the logfile */
static char *LOGFILE_FORMAT_VERSION = "1.0";

static int get_lock(char *file)
{
#ifdef WIN32
  /* no locking for WIN32 */
  return -1;
#else
  struct flock lock;
  int lock_fd = creat(file, 0666);
  if (lock_fd < 0) {
    GalUtil_WarnWithLocation(__FUNCTION__, "Lock file %s not created for log directory, exiting", file);
    exit(-1);
  }

  lock.l_type = F_WRLCK;
  lock.l_start = 0;
  lock.l_whence = SEEK_SET;
  lock.l_len = 0;
  fcntl(lock_fd, F_SETLKW, &lock);
  return lock_fd;
#endif
}

static void release_lock(int lock_fd)
{
#ifndef WIN32
  struct flock lock;

  lock.l_type = F_UNLCK;
  lock.l_start = 0;
  lock.l_whence = SEEK_SET;
  lock.l_len = 0;

  fcntl(lock_fd, F_SETLK, &lock);
  close(lock_fd);
#endif
}

extern
int _Gal_CreateFullPath(char *path, int path_size, int num_components, ...);
  
void open_logfile (SESSION *session)
{
  char dir[MAX_FNAME_LENGTH], lock[MAX_FNAME_LENGTH], file[MAX_FNAME_LENGTH];
  char date[GAL_LINE_LENGTH];
  time_t now = time(0);
  struct tm *tp;
  int i,lockfd;
#if defined(WIN32) && defined(__STDC__)
  struct _stat statbuf;
#else
  struct stat statbuf;
#endif /* WIN32 && __STDC__ */
  char *user_id = Gal_StringValue(GalHUB_GetHubUserID(Hub));
  char *data_topdir = Gal_StringValue(GalHUB_GetHubLogTopDataDir(Hub));
  /* toplogdir/usr/YYYYMMDD/session_under_day gets put in here */
  char log_dir[MAX_FNAME_LENGTH];
  /* usr-YYYYMMDD-session_under_day gets put in here */
  char log_prefix[MAX_FNAME_LENGTH];
  char *root_dir = (char *) malloc(MAX_FNAME_LENGTH);
  int root_dir_size = MAX_FNAME_LENGTH;
  
  if (data_topdir) {
    char name[MAX_FNAME_LENGTH];

    /* Make sure the buffer is big enough. */
#ifdef WIN32
    while (!_getcwd(root_dir, root_dir_size)) {
#else
    while (!getcwd(root_dir, root_dir_size)) {
#endif /* WIN32 */
      if (errno == ERANGE) {
	root_dir = (char *) realloc(root_dir, root_dir_size + MAX_FNAME_LENGTH);
	root_dir_size += MAX_FNAME_LENGTH;
	errno = 0;
      } else {
	free(root_dir);
	root_dir = NULL;
	break;
      }
    }

    tp = localtime(&now);
    strftime(date, GAL_LINE_LENGTH, "%Y%m%d", tp);
    _Gal_CreateFullPath(dir, MAX_FNAME_LENGTH, 3,
			data_topdir, user_id, date);
	
    GalUtil_Mkdirp(dir,0775);
    _Gal_CreateFullPath(lock, MAX_FNAME_LENGTH, 2, dir, ".lock");
    lockfd = get_lock(lock);
    for(i=0;;i++) {
      char int_array[8];
      
      sprintf(int_array,"%03d", i);
      _Gal_CreateFullPath(file, MAX_FNAME_LENGTH, 2, dir, int_array);
      if(_gal_stat(file,&statbuf))
	break;
    }
    GalUtil_Mkdirp(file,0775);
    release_lock(lockfd);

    if(!session->log) {
      session->log = (LOGFILE *) calloc(1, sizeof(LOGFILE));
    }
    session->log->session_under_day = i;
    /*     session->log->working_on_utt_id = -1; */

    sprintf(log_prefix,"%s-%s-%03d",user_id,date,i);
    GalHUB_SessionSetVar(session, GAL_HUB_LOG_PREFIX_HUB_FRAME_KEY,
			 Gal_StringObject(log_prefix));

    /* This reference to GAL_DIRSEP is OK. */
    sprintf(log_dir, "%s%c", file, GAL_DIRSEP);
    GalHUB_SessionSetVar(session, GAL_HUB_LOGDIR_HUB_FRAME_KEY,
			 Gal_StringObject(log_dir));

    if (root_dir) {
      GalHUB_SessionSetVar(session, GAL_HUB_PWD_HUB_FRAME_KEY,
			   Gal_StringObject(root_dir));
    }
    session->log->begin_written = 0;

    /* This is OK, too. */
    sprintf(name, "%s%s-hublog.txt", log_dir, log_prefix);

    GalHUB_SessionSetVar(session, GAL_HUB_LOGFILE_HUB_FRAME_KEY,
			 Gal_StringObject(name));

    session->log->logfile = fopen(name,"w");
    if (session->log->logfile) {
      GalUtil_Print(-1, "Logging to the file %s\n", name);
      /* Print logfile version */
      GalUtil_fprintf(session->log->logfile,"LOGFILE_FORMAT_VERSION: %s\n\n",LOGFILE_FORMAT_VERSION);
      if (Hub->log_record->user_version) {
	GalUtil_fprintf(session->log->logfile,"LOG_VERSION: %s\n\n",
			Hub->log_record->user_version);
      }
      GalUtil_fprintf(session->log->logfile,"SESSION_ID: %s\n\n\n",session->session_id);
    } else {
      /* Oops, can't open the file. */
      GalUtil_WarnWithLocation(__FUNCTION__, "Couldn't open log file %s, exiting", name);
      exit(-1);
    }
  } else {
    session->log = NULL;
  }
  if (root_dir) free(root_dir);
}

/* ******************* TIMESTAMPS  ******************************* */

void
make_timestamps (char *ds, char *ts)
{
  struct timeval tvs;
  struct tm *tms;
  int cs;

  _gal_gettimeofday(&tvs);
  tms = localtime(&tvs.tv_sec);
  cs = (int) tvs.tv_usec / 10000;

  sprintf(ds, "%02d-%s-%d", tms->tm_mday, months[tms->tm_mon], 1900 + tms->tm_year);
  sprintf(ts, "%02d:%02d:%02d.%02d", tms->tm_hour, tms->tm_min,
tms->tm_sec, cs);
}

/* ******************** FRAME LOGS *************************** */

/* handles string, int, Nframe and TObj * */
static void
write_variable_to_log(FILE *fp, TObj value, char *variable_name, int logidx)
{ 
  if (!fp) return;
  if (!value) return;
  if (Gal_Listp(value)) {
    GalUtil_fprintf (fp, "[Begin %s (%03d)]\n", variable_name, logidx);
    Gal_PrObjectToFile(value, fp);
    GalUtil_fprintf (fp, "[End %s (%03d)]\n", variable_name, logidx);	    
  } else  if (Gal_Framep(value)) {
    GalUtil_fprintf (fp, "[Begin %s (%03d)]\n", variable_name, logidx);
    Gal_PPFrameToFile(Gal_FrameValue(value), fp);
    GalUtil_fprintf (fp, "[End %s (%03d)]\n", variable_name, logidx);	    
  } else {
    /* Use the string formatters. */
    GalUtil_fprintf(fp, "%s (%03d): ", variable_name, logidx);
    Gal_PrObjectToFile(value, fp);
  }
}

/************************* Logging routines *********************/

static void print_timestamp_to_file(SESSION *session, char *timestamp)
{
  if(session && session->log && session->log->logfile) {
    GalUtil_fprintf(session->log->logfile,"%s", timestamp);
  }
}

/* SAM 4/29/02: This is called in the scriptless case. We need to
   look up the element in question in two different ways. If the
   name of the frame is foo.bar, we need to look up the literal
   foo.bar and also the conditions imposed on "bar". We should
   probably do this after we call GalHUB_FindServiceTypeForOperation,
   since at that point all the relevant info about the message
   has been computed. Actually, we're computing the information
   when we create the message now, so we can call it a little
   earlier. */

int GalHUB_TimestampIsForced(SERVER_MESSAGE *msg, LOGRECORD *rec,
			     int on_edge,
			     int from_script_ok)
{
  LOGKEYS **keys;
  int i;
  GalSS_ProviderSpec *msg_spec = &(msg->true_provider_spec);
  
  if ((!rec) || (!rec->msg_ht))
    return 0;

  if (Gal_HashHasKey(":hub_all_operations", rec->msg_ht))
    return 1;

  /* This is for the scriptless case. There may be a case where the
     literal name of the frame doesn't give us any information. For
     example, if the name of the frame is "foo.bar", and I've specified
     a target provider, if PROGRAM: foo.bar was declared, you shouldn't
     do the timestamp, because the program isn't called. Since this
     is only called in scriptless mode, PROGRAM: matches shouldn't
     count anyway. If I only check the bare operation entry here,
     and look for the descriptions, that's probably the best thing
     to do, since those will always be present for messages. */

  /* Check the bare operation and try to match the
     conditions in entry with the conditions in the message.
     If there's a match, return 1. */

  /* The literal match setting now controls whether or not
     subsumption is permitted. This rules out the case where PROGRAM: bar
     can match {c foo.bar }. */
  
  keys = (LOGKEYS **) Gal_PointerValue(Gal_GetHash(msg->bare_operation,
						   rec->msg_ht));

  if (keys) {
    for (i = 0; keys[i]; i++) {
      /* Do the match. */
      GalSS_ProviderSpec *spec = keys[i]->spec;
      if (GalHUB_ProviderSpecsMatch(spec, msg_spec,
				    keys[i]->literal_match) &&
	  ((!keys[i]->edges_only) || on_edge) &&
	  (from_script_ok || (!keys[i]->from_script)))
	return 1;
    }
  }
  return 0;
}

static void update_timestamps(SERVER_MESSAGE *msg, char *event,
			      char *msg_type, SERVICE_PROVIDER *server,
			      int force, int on_edge,
			      int from_script_ok)
{
  Gal_Frame frame = msg->message;
  SESSION *session = GalHUB_SessionLookupByFrameInHUB(frame, 0);
  int tidx = msg->tidx;
  
  if (!session || !session->log || !Hub->log_record ||
      !Hub->log_record->msg_ht) {
    return;
  }

  record_begin_utt(session);

  if (force || GalHUB_TimestampIsForced(msg, Hub->log_record, on_edge,
					from_script_ok))
    dc_print_timestamp(session, event, msg_type, tidx,
		       msg->bare_operation, server);
}

static void
dc_print_timestamp (SESSION *s, char *event, char *msg_type, int tidx,
		    char *op, SERVICE_PROVIDER *server)
{
  char ds[16], ts[16], buf[1000];
  static char *local_event;
  static unsigned int event_size = LOG_EVENT_MAX;

  /* Allocate memory */
  if(!local_event)
    local_event = (char *)calloc(LOG_EVENT_MAX,sizeof(char));

  /* If it is not enough double it */
  if (event_size < ((msg_type ? strlen(msg_type) : 0) + strlen(event))) {
    event_size *= 2;
    local_event = (char *)realloc(local_event, event_size);
  }
  
  local_event[0] = '\0';
  strcat(local_event, event);
  if (msg_type) {
    strcat(local_event,":");
    strcat(local_event,msg_type);
  }

  make_timestamps (ds, ts);
  if (server) {
    sprintf (buf, "[Timestamp (%03d): %s %s(%s:%d) %d %s at %s on %s]\n", s->utterance_id, local_event, server->iname, server->host ? server->host : "<Builtin>", server->port, tidx,op, ts, ds);
  } else {
    sprintf (buf, "[Timestamp (%03d): %s %d %s at %s on %s]\n", s->utterance_id, local_event, tidx,op, ts, ds);
  }
  print_timestamp_to_file(s, buf);
}

/* SAM 7/31/00: Generalizing ALARM timestamps to support MIT
   logging of server up/server down. */

static void
dc_print_event_timestamp(SESSION *session, char *event, char *sub_event, char *condition)
{
  char ds[16], ts[16], buf[1000];
  make_timestamps (ds, ts);

  if (!sub_event)
    sub_event = "unspecified";
  if (!condition)
    condition = "unspecified";

  sprintf (buf, "[Timestamp (%03d): %s %s %s at %s on %s]\n", session->utterance_id, event, sub_event, condition, ts, ds);

  print_timestamp_to_file(session, buf);
}

/* void log_alarm_expired(SESSION *session, char *alarm_name) */
/* { */
/*   char ds[16], ts[16], buf[1000]; */
/*   make_timestamps (ds, ts); */

/*   if (!session || !Hub->log_record || !Hub->log_record->log_system_errors) */
/*     return; */

/*   sprintf (buf, "[Timestamp (%03d): SYSTEM_ERROR %s alarm_expired at %s on %s]\n", session->utterance_id, */
/* 	   alarm_name, ts, ds); */

/*   print_timestamp_to_file(session, buf); */
/* } */

static void record_begin_utt(SESSION *s)
{ int uttid;
  LOGFILE *log = s->log;
  uttid = s->utterance_id;
  /* this causes inconsistencies in the logfile */
  /* if (uttid < 0) uttid = 0; */
  if(!log->begin_written && log->logfile) 
  { GalUtil_fprintf(log->logfile, "\n\n:BEGIN_UTT (%03d)\n", uttid);
  }
  log->begin_written = 1;
}


/****************************************************************/
/* stuff to handle logging based on rules  */

/* this is done once whenever the utterance index changes. */
/* it's only done if there are logging variables assigned at the top of the file */
void GalHUB_LogfileRecordUtteranceForSession(SESSION *s)
{ int logidx;
  LOGFILE *log;

  if(!s || !s->log || !s->log->logfile )
    return;

  log = s->log;
  logidx = s->utterance_id;
/*   log->working_on_utt_id = logidx + 1; 	*//* next one */
  
  /* if (logidx < 0) return; don't update any negative -id utts . why?*/ 
  record_begin_utt(s);
  log->begin_written = 0;

  GalUtil_fprintf(log->logfile, "\n");
 
  GalUtil_fprintf(log->logfile, "\n:END_UTT (%03d)\n", logidx);
}

/* Begin Logging functions */

static int __GrowKpArray(SERVER_MESSAGE *msg, int increment)
{
  int cur_size = 0;
  int new_size;
  int i;
  
  if (msg->kp_array) {
    for (cur_size = 0; msg->kp_array[cur_size].key; cur_size++);
    new_size = cur_size + increment;
    msg->kp_array = (KeyPair *) realloc(msg->kp_array, (new_size + 1) * sizeof(KeyPair));
    /* Null it out. */
    for (i = cur_size; i <= new_size; i++) {
      msg->kp_array[i].key = (char *) NULL;
      msg->kp_array[i].value = (Gal_Object) NULL;
      msg->kp_array[i].tag = 0;
      msg->kp_array[i].newly_created = 0;
    }
  } else {
    msg->kp_array = (KeyPair *) calloc(increment + 1, sizeof(KeyPair));
  }
  return cur_size;
}

void GalHUB_LogErrorMessageKeys(SERVER_MESSAGE *msg)
{
  Gal_Object err_num_obj = Gal_GetObject(msg->message, GAL_ERROR_NUMBER_FRAME_KEY);
  Gal_Object err_dsc_obj = Gal_GetObject(msg->message, GAL_ERROR_DESCRIPTION_FRAME_KEY);
  int num_keys = 0;
  int cur_size;
  
  if (err_num_obj) num_keys++;
  if (err_dsc_obj) num_keys++;
  
  /* I don't care how many keys I found. */
  msg->force_timestamp = 1;

  if (num_keys > 0) {
    cur_size = __GrowKpArray(msg, num_keys);
    if (err_num_obj) {
      msg->kp_array[cur_size].key = GAL_ERROR_NUMBER_FRAME_KEY;
      msg->kp_array[cur_size].value = err_num_obj;
      cur_size++;
    }
    if (err_dsc_obj) {
      msg->kp_array[cur_size].key = GAL_ERROR_DESCRIPTION_FRAME_KEY;
      msg->kp_array[cur_size].value = err_dsc_obj;
    }
  }
}
    
/* I know, this way requires us to allocate memory, while the
   old way didn't. The alternative is to store enough information
   in the server message to be able to reconstruct this. We can worry
   about that later.

   Because we want to accumulate elements for the log,
   perhaps from different namespaces, I'm going to put this in
   the core logfile for now. */

void 
GalHUB_LogfileUpdateServerMessageFromEntityPairs(EntityPair **ep_list, SERVER_MESSAGE *msg,
						 Gal_Frame *namespace_array)
{
  /* We want to build a corresponding KeyPair *array. */
  int i, num_pairs, cur_size;
  Gal_Object value;
  char *key;
  Gal_NamespaceProgramEntity *ne;
  int newly_created;
  
  if (!ep_list)
    return;
  
  /* Count them. */
  for (num_pairs = 0; ep_list[num_pairs]; num_pairs++);
  cur_size = __GrowKpArray(msg, num_pairs);

  /* Now, load the key pair array. */
  i = 0;
  while (ep_list[i]) {
    if (ep_list[i]->target->entity_type != GAL_NAMESPACE_ENTITY)
      continue;
    ne = (Gal_NamespaceProgramEntity *) ep_list[i]->target->entity_data;
    if (!ne->is_default)
      continue;
    key = ne->key;      
    value = Gal_GetProgramEntity(ep_list[i]->source, namespace_array,
				 &newly_created);
    if (value) {
      msg->kp_array[cur_size].key = key;
      msg->kp_array[cur_size].value = value;
      msg->kp_array[cur_size].newly_created = newly_created;
      cur_size++;
    }
    i++;
  }
}

static void
UpdateRecordFromKeyPair(SERVER_MESSAGE *msg,
			char *event, GalIO_MsgType msg_type)
{
  int i = 0;
  KeyPair *kp;
  SESSION *session;
  Gal_Frame frame = msg->message;
  KeyPair *log_kps = msg->kp_array;

  if (!log_kps)
    return;
  
  session = GalHUB_SessionLookupByFrame(frame, 0);
  if (!session)
    return;

  while (log_kps[i].key) {
    char *key;
    Gal_Object value;

    kp = &(log_kps[i++]);
    key = kp->key;
    value = kp->value;

    if (value) {
      GalHUB_LogfileUpdateRecord(key, value, session);
    }
  }
}

/* this is for items loggged in the rules. */
static void
GalHUB_LogfileUpdateRecord(char *key, Gal_Object value, SESSION *session)
{ 
  if (!session || !session->log)
    return;

  write_variable_to_log(session->log->logfile, value, key, session->utterance_id); 
}

void GalHUB_LogfileLogError(SESSION *session, SERVICE_PROVIDER *s,
			    Gal_Frame fr, char * mt)
{
  if (!session || !Hub->log_record || !Hub->log_record->log_system_errors)
    return;

  GalHUB_LogfileLogMessage(session,s,fr,"SYSTEM_ERROR",mt);
}

void 
GalHUB_LogfileLogMessage(SESSION *session, SERVICE_PROVIDER *server,
			 Gal_Frame frame, char *event, char *msg_type)
{
  char **keys, *msg_name;
  int nkeys=0,i;

  /* Don't segfault */
  if (!session)
    return;
  
  msg_name = Gal_SplitOperationName(Gal_FrameName(frame), (char **)NULL);

  dc_print_timestamp(session,event,msg_type,
		     Gal_GetInt(frame, GAL_TOKEN_INDEX_FRAME_KEY),
		     msg_name,server);  
  free(msg_name);

  keys = Gal_GetProperties(frame, &nkeys);

  if (!keys)
    return;

  for(i=0;i < nkeys;i++) 
    GalHUB_LogfileUpdateRecord(keys[i], Gal_GetObject(frame, keys[i]),
			       session);
  free(keys);
  
}

/* SAM 4/30/02: This function relies on a description and a literal
   name. We look up the literal name using a literal match. If we
   don't find it, we look up the base name using the match
   properties of the entry. */

/* So here's the next problem. We need to make sure that a general
   declaration (say, TIMESTAMP: foo) doesn't overrule a specific
   generalization (MESSAGE: foo with log in and out). This function
   is only called in the edge case, so perhaps we should restrict
   ourselves to those declarations, since the non-edge declarations
   never contain log keys. */

static LOGKEYS *
__GalHUB_LogfileGetLogkeysFromNameAndDescription(HUB *h, char *literal_name,
						 char *bare_name,
						 GalSS_ProviderSpec *msg_spec,
						 int from_script_ok)
{
  if (h && h->log_record && h->log_record->msg_ht) {
    /* First, try the literal match with a NULL provider spec. */

    LOGRECORD *rec = h->log_record;
    LOGKEYS **keys = (LOGKEYS **) Gal_PointerValue(Gal_GetHash(literal_name,
							       rec->msg_ht));
    int i;
    
    if (keys) {
      for (i = 0; keys[i]; i++) {
	/* Do the match. */
	GalSS_ProviderSpec *spec = keys[i]->spec;
	if (keys[i]->edges_only &&
	    (from_script_ok || (!keys[i]->from_script)) &&
	    GalHUB_ProviderSpecsMatch(spec, (GalSS_ProviderSpec *) NULL, 1))
	  return keys[i];
      }
    }

    /* If we don't find it there, try the description match. */

    keys = (LOGKEYS **) Gal_PointerValue(Gal_GetHash(bare_name,
						     rec->msg_ht));

    if (keys) {
      for (i = 0; keys[i]; i++) {
	/* Do the match. */
	GalSS_ProviderSpec *spec = keys[i]->spec;
	if (keys[i]->edges_only &&
	    (from_script_ok || (!keys[i]->from_script)) &&
	    GalHUB_ProviderSpecsMatch(spec, msg_spec, keys[i]->literal_match))
	  return keys[i];
      }
    }
  }
  
  return (LOGKEYS *) NULL;
}

LOGKEYS *GalHUB_LogfileGetLogkeysFromHubViaFrameName(HUB *h, char *name,
						     int from_script_ok)
{
  /* First, we split the frame name. */
  char *stype_name = (char *) NULL;
  char *key = Gal_SplitOperationName(name, &stype_name);
  GalSS_ProviderSpec *spec = GalHUB_SplitServerName(stype_name);
  LOGKEYS *result = __GalHUB_LogfileGetLogkeysFromNameAndDescription(h, name, key, spec, from_script_ok);

  /* Free everything. */
  if (stype_name) free(stype_name);
  free(key);
  if (spec) GalSS_FreeProviderSpec(spec);

  return result;
}

LOGKEYS *GalHUB_LogfileGetLogkeysFromHub(HUB *h, SERVER_MESSAGE *msg,
					 int from_script_ok)
{
  return __GalHUB_LogfileGetLogkeysFromNameAndDescription(h, Gal_FrameName(msg->message), msg->bare_operation, &(msg->true_provider_spec), from_script_ok);
}

void
GalHUB_LogfileUpdateMsgOnSend(SERVER_MESSAGE *msg, GalIO_MsgType mt,
			      TOKEN *t) 
{
  Gal_Frame frame = msg->message;
  char *frame_name;
  int force_ts = 0;
  
  frame_name = Gal_FrameName(frame);
  
  switch(mt) {
  case GAL_MESSAGE_MSG_TYPE:
    if (msg->kp_array)
      force_ts = 1;
    /* Sending a new message is not on an edge, which also
       means that it's not OK to use PROGRAM: info. */
    update_timestamps(msg, "send", GalIO_MsgTypeToName(mt),
		      msg->provider, force_ts, 0, 0);
    UpdateRecordFromKeyPair(msg, "send", mt);
    break;
  case GAL_REPLY_MSG_TYPE:
  case GAL_ERROR_MSG_TYPE:
    /* If there's an error, the frame name might not be the
       right thing to look at (system_error will be the name,
       probably). We want to guarantee that if there are log_out
       keys, then the timestamp record will be forced. So
       in the case where an error is being written, I'll set
       things up in hub_process.c so that it will use this
       other mechanism. */
    if (msg->kp_array)
      force_ts = 1;
    /* Sending a reply message is on an edge. If we
       don't have a target provider ID, it's OK to use
       the PROGRAM: info. */
    update_timestamps(msg, "send", GalIO_MsgTypeToName(mt),
		      msg->provider, force_ts, 1,
		      (t->target_provider_id == (char *) NULL));
    if (msg->kp_array) {
      UpdateRecordFromKeyPair(msg, "send", mt);
    }
    break;
  default:
    return;
  }
}

void
GalHUB_LogfileUpdateMsgOnRead(SERVICE_PROVIDER *server,
			      SERVER_MESSAGE *msg, GalIO_MsgType mt,
			      TOKEN *t)
{
  int force_ts = msg->force_timestamp || (msg->kp_array != NULL);

  switch(mt) {
  case GAL_MESSAGE_MSG_TYPE:
  case GAL_REPLY_MSG_TYPE:
  case GAL_ERROR_MSG_TYPE:
    /* The only edge is reading a new message. */
    update_timestamps(msg, "read", GalIO_MsgTypeToName(mt),
		      server, force_ts, (mt == GAL_MESSAGE_MSG_TYPE),
		      ((mt == GAL_MESSAGE_MSG_TYPE) &&
		       (t->target_provider_id == (char *) NULL)));
    UpdateRecordFromKeyPair(msg, "read", mt);
    break;
  default:
    return;
  }
}


void
GalHUB_LogfileLogEvent(SESSION *session, SERVICE_PROVIDER *s,
		       char *category, char *event, int tidx)
{
  if (!session)
    return;
  dc_print_timestamp(session,category,NULL,tidx,event,s);
}

void
GalHUB_LogfileLogErrorEvent(SESSION *session, SERVICE_PROVIDER *s,
			    char *event, int tidx)
{
  if (!session || !Hub->log_record || !Hub->log_record->log_system_errors)
    return;
  GalHUB_LogfileLogEvent(session,s,"SYSTEM_ERROR",event,tidx);
}

void
GalHUB_LogfileLogAlarmEvent(SESSION *session, char *alarm_name, char *condition)
{
  if (!session || !Hub->log_record || !Hub->log_record->log_alarm_activity)
    return;

  dc_print_event_timestamp(session, "ALARM", alarm_name, condition);
}

void GalHUB_LogfileLogLockEvent(SESSION *session, SERVICE_PROVIDER *s,
				char *event, int tidx)
{
  GalHUB_LogfileLogEvent(session,s,"LOCK",event, tidx);
}

void
GalHUB_LogfileTimestampKey (SESSION *s, char *key, char *event, int tidx)
{
  char ds[16], ts[16], buf[1000];

  make_timestamps (ds, ts);

  sprintf (buf, "[KeyTimestamp (%03d): %d %s at %s on %s]\n", s->utterance_id, tidx, key, ts, ds);
  print_timestamp_to_file(s, buf);
}

void GalHUB_LogfileLogServeAnySession(SESSION *session,
				      SERVICE_PROVIDER *server, int tidx)
{
  if (!session || !Hub->log_record || !Hub->log_record->log_serve_any_session)
    return;
  GalHUB_LogfileLogLockEvent(session, server, GAL_HUB_SERVE_THIS_SESSION_ONLY_FRAME_KEY, tidx);
}


void  GalHUB_LogfileLogServeThisSessionOnly(SESSION *session,
					    SERVICE_PROVIDER *server, int tidx)
{
  if (!session || !Hub->log_record || !Hub->log_record->log_serve_this_session_only)
    return;
  
  GalHUB_LogfileLogLockEvent(session, server, GAL_HUB_SERVE_THIS_SESSION_ONLY_FRAME_KEY, tidx);
}

void GalHUB_LogfileLogGetSessionLock(SESSION *session,
				     SERVICE_PROVIDER *server, int tidx)
{
  if (!session || !Hub->log_record || !Hub->log_record->log_get_session_lock)
    return;
  GalHUB_LogfileLogLockEvent(session,server,GAL_HUB_GET_SESSION_LOCK_FRAME_KEY, tidx);
}

void GalHUB_LogfileLogReleaseSessionLock(SESSION *session,
					 SERVICE_PROVIDER *server, int tidx)
{
  if (!session || !Hub->log_record || !Hub->log_record->log_release_session_lock)
    return;
  GalHUB_LogfileLogLockEvent(session,server,GAL_HUB_RELEASE_SESSION_LOCK_FRAME_KEY, tidx);
}

/* this is called automatically when a server crashes */
void
GalHUB_LogfileLogServerStatus(SESSION *sessions, SERVICE_PROVIDER *server, char *status_key)
{
  SESSION *session;
  char server_name_buf[256];
  sprintf(server_name_buf, "%s:%d", server->host, server->port);
  for (session = sessions;(session);session = session->next) {
    if (session->log && session->log->logfile) {
      dc_print_event_timestamp(session, "SERVER_STATUS", server_name_buf,
			       status_key);
    }
  }
}

void close_log_file(SESSION *s)
{
  if (!s || !s->log)
    return;

  if (s->log->logfile) {
    GalHUB_SessionFlushLogfile(s);
    fclose(s->log->logfile);
  }
  s->log->logfile = 0;
}

int logfile_is_open(SESSION *s)
{
  return (s && s->log && s->log->logfile);
}

void free_logrecord(LOGRECORD *logrecord)
{
  if (logrecord) {
    Gal_FreeHash(logrecord->msg_ht);
    free(logrecord);
  }
}

void free_logfile(LOGFILE *log)
{
  if (log) free(log);
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
