/*
  Portions of this file (c) Copyright 1998 - 2000 M.I.T.
  Portions of this file (c) Copyright 2000 The MITRE Corporation 
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include "galaxy/galaxy_all.h"
#include "hub_internal.h"

/* For the local server */

#define SERVER_FUNCTIONS_INCLUDE "local-server.h"
#define GAL_SERVER_OPS_ONLY
#include "galaxy/server_functions.h"

/*
 *  Token Utilities
 */

static int tok_id = 1;

double generate_timestamp()
{ 
  struct timeval tv;
  if(_gal_gettimeofday(&tv) == -1) {
    GalUtil_Error("Error while generating timestamp");
    return -1.0;
  } else {
    return ((double)(tv.tv_sec) + (double)((tv.tv_usec/1000L)*0.001));
  }
}

TOKEN *newToken(HUB *h, Gal_Frame state, SERVICE_PROVIDER *s,
		int uttidx, char *session_id, 
		Gal_Frame admin_info) 
{
  TOKEN   *t, *t1;
  Gal_Object server_tidx_object;
  char *prog_name = Gal_FrameName(state);
  char *provider_id;

  if (!h)
    return(NULL);
  if (!prog_name || prog_name[0] == '\0')
    return(NULL);
  if (!_gal_strcasecmp(prog_name,"destroy"))
    return(NULL);

  t = (TOKEN*) calloc(1,sizeof(TOKEN));
  t->tidx = tok_id++;

  /* Set the token timestamp. */
  t->timestamp = generate_timestamp();

  t->name = _gal_strdup(prog_name);
  t->ref = 0;
  t->uttidx = uttidx;
  
  t->state = Gal_CopyFrame(state);
  
  server_tidx_object = Gal_GetObject(admin_info,
				     GAL_SERVER_TOKEN_INDEX_FRAME_KEY);
  if (server_tidx_object)
    t->server_tidx = Gal_IntValue(server_tidx_object);
  else
    t->server_tidx = -1;

  /* If there's a target provider, capture it. */
  provider_id = Gal_GetString(admin_info,
			      GAL_PROVIDER_ID_FRAME_KEY);
  if (provider_id)
    t->target_provider_id = _gal_strdup(provider_id);
  
  if (session_id)
  {
    t->session_id = _gal_strdup(session_id);
    Gal_SetProp(t->state, GAL_SESSION_ID_FRAME_KEY,
		Gal_StringObject(t->session_id));
  } else {
    t->session_id = _gal_strdup(DEFAULT_SESSION_ID);
  }
  t->owner = s;
  t->destroy = 0;

  /* Ick. I hope nobody really relies on this,
     because it can be changed. For now, I'm going to
     use it, just because I don't want to worry about
     how to protect it. */
  Gal_SetProp(t->state, GAL_TOKEN_INDEX_FRAME_KEY,
	      Gal_IntObject(t->tidx));

  /* insert at tail of linked list */
  if (!h->token) {
    h->token = t;
  } else {
    t1 = h->token;
    while (t1->next) t1 = t1->next;
    t1->next = t;
    t->prev = t1;
    t->next = NULL;
  }

  h->num_tokens++;

  _GalHUB_CReportTokenStatus(Hub, GAL_PINFO1_LEVEL,
			     5, 0, GAL_HUB_TOKEN_CREATED, t, 0);

  return(t);
}

TOKEN *GalHUB_NewToken(Gal_Frame state, SERVICE_PROVIDER *s, int uttidx,
		       char *session_id, Gal_Frame admin_info)
{
  return newToken(Hub, state, s, uttidx, session_id, admin_info);
}

TOKEN *getToken(HUB *h, int tidx)
{
  TOKEN *t;

  if (h == NULL)
    return NULL;

  t = h->token;
  while(t && (t->tidx != tidx)) {
    t = t->next;
  }

  return(t);

  if (!t) {
    GalUtil_Warn("No token with id %d found",tidx);
    return(NULL);
  } else
    return(t);
}

TOKEN *GalHUB_GetTokenFromIndex(int tidx) {
  return getToken(Hub, tidx);
}

void destroyTokenIfDone(HUB *h, TOKEN *t, SESSION *session,
			Gal_Frame return_msg,
			GalIO_MsgType return_type)
{
  if (!t)
    return;
  if (t->ref > 0) {
    return;
  }

  /* SAM 5/4/01: If the caller wants a return, make sure the
     caller gets one. */

  if (t->mm && t->owner && return_msg) {
    
    char *host = t->owner->host;
	
    if (!host) host = "<unknown>";
      
    GalUtil_CPInfo1WithLocation(__FUNCTION__, 5,0,"Done with token %d --> returning to owner %s@%s:%d\n", t->tidx, t->owner->iname, host, t->owner->port);
    GalHUB_ReturnMMToOwner(h, t, return_msg, session, return_type);
  }

  _GalHUB_ReportTokenStatus(Hub, GAL_PINFO1_LEVEL,
			    GAL_HUB_TOKEN_DESTROYED, t, 0);

  if (h->validate && t->mm) {
    /* If the token is being destroyed but an mm dialogue
       is active, then we print a warning. */
    GalUtil_Warn("Token %d is being destroyed, but the server expects a reply; execution could hang\n", t->tidx);
  }
  if (t->prev)
    t->prev->next = t->next;
  if (t->next) {
    if (h->token == t) {
      h->token = t->next;
      if(h->token)
	h->token->prev = NULL;
    }
    else
      t->next->prev = t->prev;
  }
  if (t->state)
    Gal_FreeFrame(t->state);
  if (h->token == t)
    h->token = NULL;
  free(t->session_id);
  free(t->name);
  if (t->target_provider_id)
    free(t->target_provider_id);
  if (t->ctrl_info && t->ctrl_info_free_fn)
    (*t->ctrl_info_free_fn)(t->ctrl_info);
  free(t);
  h->num_tokens--;
  printTokens(h);
  return;
}

void
printTokens(HUB *h) 
{
  extern int debug_token_gc;

  TOKEN *t;
  int i = 0;
  t = h->token;

  while(t) {
    char *plural = (t->ref == 1) ? "" : "s";

    if (i == 0) {
      /* First time. */
      if(debug_token_gc) 
	GalUtil_Print(-1, "Tokens: ");
      else
	GalUtil_CPInfo1(7,0,"Tokens: ");
    }    
    if(debug_token_gc)
      GalUtil_Print(-1, " %d (%d reference%s)",t->tidx,t->ref, plural);      
    else
      GalUtil_CPInfo1(7,0," %d (%d reference%s)",t->tidx,t->ref, plural);
    t = t->next;
    i++;
    if (i>h->num_tokens) {
      GalUtil_CPInfo1(7,0,"\n");       
      GalUtil_Warn("Token list broken");
      return;
    }
  }
  if(debug_token_gc) 
    GalUtil_Print(-1, "\n");
  else
    GalUtil_CPInfo1(7,0,"\n");

  if (i!=h->num_tokens) {
    GalUtil_Warn("Token list count mismatch: %d expected vs. %d counted",h->num_tokens,i);
    return;
  }
  
  return;
}

/*
 *  Local Server Utilities
 */

typedef struct __GalHub_LocalQueueElement {
  Gal_Frame fr;
  GalIO_MsgType msg_type;
  struct __GalHub_LocalQueueElement *next;
} __GalHub_LocalQueueElement;

typedef struct __GalHub_LocalData {
  HUB *h;
  __GalHub_LocalQueueElement *outbound;
  __GalHub_LocalQueueElement inbound;
} __GalHub_LocalData;

/* we queue on the output bec. a local server is never busy,
   but its output may not yet have been consumed by the .pgm */

/* SAM 10/5/99: Now that dispatch functions ALWAYS return their
   answer, and only create a new token by sending an explicit
   message, MIT's dispatch_to_main must change, because it
   treats its reply as a new message. However, that means that
   something more complex has to happen with GalSS_CallServerFunction,
   because I don't want to be writing into the local server's
   queue (I'm pretty sure it doesn't have one). We need to
   set the writer and dispatcher. */

static int insert_frame_in_local_queue(__GalHub_LocalData *d,
				       Gal_Frame frame, GalIO_MsgType t)
{
  __GalHub_LocalQueueElement *s, *last = NULL;
  
  if (frame) {
    s = d->outbound;
    while (s) {
      last = s;
      s = s->next;
    }
    s = calloc(1,sizeof(__GalHub_LocalQueueElement));
    s->fr = Gal_CopyFrame(frame);
    s->msg_type = t;
    if (last)
      last->next = s;
    else
      d->outbound = s;
    return(1);
  }
  return(-1);
}

static int __GalHub_LocalWriter(GalIO_CommStruct *gcomm,
				Gal_Frame frame, GalIO_MsgType t, int do_block)
{
  __GalHub_LocalData *d = (__GalHub_LocalData *) GalIO_GetCommData(gcomm);
  
  if (!d)
    return -1;
  /* Sort of a double check. */
  if (d->h->local_server != gcomm)
    return -1;
  return insert_frame_in_local_queue(d, frame, t);
}

static Gal_Frame __GalHub_LocalDispatcher(GalIO_CommStruct *gcomm,
					  Gal_Frame frame,
					  GalIO_MsgType *msg_type_ptr)
{
  GalUtil_Warn("It is not possible to wait for a reply inside a builtin function");
  if (msg_type_ptr) {
    *msg_type_ptr = GAL_ERROR_MSG_TYPE;
  }
  return (Gal_Frame) NULL;
}

static void __GalHub_LocalFrameFreer(Gal_Frame incoming,
				     Gal_Frame outgoing)
{
  if (outgoing && outgoing != incoming)
    Gal_FreeFrame(outgoing);
}

extern void _GalIO_CommSetFrameWriter(GalIO_CommStruct *gcomm, GalIO_FrameWriter fn);
extern void _GalIO_CommSetFrameDispatcher(GalIO_CommStruct *gcomm, GalIO_FrameDispatcher fn);
extern void _GalIO_CommSetFrameReader(GalIO_CommStruct *gcomm, GalIO_FrameReader fn);
void _GalIO_CommSetFrameFreer(GalIO_CommStruct *gcomm, void (*fn)(Gal_Frame, Gal_Frame));
int _GalIO_CommInvokeReadHandler(GalIO_CommStruct *gcomm);
extern void _GalSS_CommEnableContinuations(GalIO_CommStruct *gcomm);

/* I hate that this is global, but I'm not about to
   fix it at the moment. There's never more than one
   thing in it at a time. */

static int __GalHUB_LocalReader(GalIO_CommStruct *gcomm,
				Gal_Frame *frame_ptr,
				GalIO_MsgType *msg_type_ptr,
				GalIO_MsgQueueTestFn test_fn,
				void *client_data)
{
  __GalHub_LocalData *d = (__GalHub_LocalData *) GalIO_GetCommData(gcomm);
  if (!d)
    return 0;
  /* Sort of a double check. */
  if (d->h->local_server != gcomm)
    return 0;
  if ((d->inbound).fr && (*test_fn)((d->inbound).msg_type,
				     (void *) (d->inbound).fr,
				     1, GAL_FRAME, client_data)) {
    *frame_ptr = (d->inbound).fr;
    if (msg_type_ptr)
      *msg_type_ptr = (d->inbound).msg_type;
    (d->inbound).fr = (Gal_Frame) NULL;
    return 1;
  } else {
    return 0;
  }
}

int send_to_local_server(HUB *h, Gal_Frame fr, GalIO_MsgType msg_type)
{
  __GalHub_LocalData *d;
  
  if (!h->local_server)
    return -1;
  d = (__GalHub_LocalData *) GalIO_GetCommData(h->local_server);
  if (!d)
    return -1;
  (d->inbound).fr = fr;
  (d->inbound).msg_type = msg_type;

  return _GalIO_CommInvokeReadHandler(h->local_server);
}

static void __gal_hub_destroy_local_data(void *data)
{
  __GalHub_LocalData *d = (__GalHub_LocalData *) data;
  __GalHub_LocalQueueElement *de, *next_de;

  de = d->outbound;
  while (de) {
    next_de = de->next;
    if (de->fr)
      Gal_FreeFrame(de->fr);
    free(de);
    de = next_de;
  }
  if (d->inbound.fr) {
    Gal_FreeFrame(d->inbound.fr);
  } 
  free(d);
}

void GalHUB_InitializeLocalServer(HUB *h)
{
  GalIO_CommStruct *gcomm;
  __GalHub_LocalData *d;

  if (!h->local_server) {  
    gcomm = GalIO_ClientInit("<local>", -1, GalSS_FrameHandler, -1);
    GalIO_SetCommDispatchFnPkg(gcomm, _GalSS_InitializeSignatures((GalIO_ServerStruct *) NULL, GalIO_GetCommDispatchFnPkg(gcomm)));
    _GalIO_CommSetFrameWriter(gcomm, __GalHub_LocalWriter);
    _GalIO_CommSetFrameDispatcher(gcomm, __GalHub_LocalDispatcher);
    _GalIO_CommSetFrameFreer(gcomm, __GalHub_LocalFrameFreer);
    _GalSS_CommEnableContinuations(gcomm);
    _GalIO_CommSetFrameReader(gcomm, __GalHUB_LocalReader);
    h->local_server = gcomm;
    d = (__GalHub_LocalData *) calloc(1, sizeof(__GalHub_LocalData));
    d->h = h;
    GalIO_SetCommData(h->local_server, (void *) d, __gal_hub_destroy_local_data);
  }
}

extern void _Gal_FreeDispatchFnPkg(Gal_DispatchFnPkg *pkg);

void GalHUB_DestroyLocalServer(HUB *h)
{
  if (h->local_server) {
    /* This connection isn't associated with a server, so we have
       to do this here. */
    _Gal_FreeDispatchFnPkg(GalIO_GetCommDispatchFnPkg(h->local_server));
    GalIO_SetCommDone(h->local_server);
    GalIO_DestroyCommStruct(h->local_server);
    h->local_server = (GalIO_CommStruct *) NULL;
  }
}

HUB *GalHUB_GetHubFromLocalServerData(GalIO_CommStruct *gcomm)
{
  __GalHub_LocalData *d = (__GalHub_LocalData *) GalIO_GetCommData(gcomm);
  return d->h;
}

/* true if we have stuff waiting */
int local_server_ready(HUB *h)
{
  __GalHub_LocalData *d;
  
  if (!h->local_server)
    return 0;
  d = (__GalHub_LocalData *) GalIO_GetCommData(h->local_server);
  if (!d)
    return 0;
  if (d->outbound)
    return 1;
  else
    return 0;
}

int read_from_local_server(HUB *h, Gal_Frame *frame,
			   GalIO_MsgType *msg_type_ptr)
{
  __GalHub_LocalData *d;
  __GalHub_LocalQueueElement *s;
  
  if (!h->local_server)
    return 0;
  d = (__GalHub_LocalData *) GalIO_GetCommData(h->local_server);
  if (!d)
    return 0;
  if (d->outbound) {
    s = d->outbound;
    d->outbound = d->outbound->next;
    *frame = s->fr;
    *msg_type_ptr = s->msg_type;
    free(s);
    return(1);
  } else {
    return (0);
  }
}

/*
 *  Print Utilities
 */

static void hub_outline_nframe(Nframe fr)
{
  if(GAL_VERBOSE < GAL_PINFO1_LEVEL) {
    GalUtil_Print(-1, "Frame: %s\n", Gal_FrameName(fr));
  } else if(GAL_VERBOSE == GAL_PINFO1_LEVEL) {
    Gal_OutlineFrame(fr, GAL_PINFO1_LEVEL);
  } else {
    Gal_PPFrame(fr);
  }
}

void GalHUB_OutlineFrame(Gal_Frame fr) {
  hub_outline_nframe(fr);
}

/* Sends a "hub_break" message to the GUI server. The message contains a 
   message type (a string) and a frame and/or a text message. */
static void __send_hub_break_message(HUB *h, Gal_Frame f, char *type, char *msg)
{
  GalSS_Environment *env = GalSS_EnvCreate(h->gui->provider->gcomm);
  Gal_Frame new_f = Gal_MakeFrame("hub_debug_info", GAL_CLAUSE);
  Gal_SetProp(new_f, ":type", Gal_StringObject(type));
  if(f)
    Gal_SetProp(new_f, ":frame", Gal_FrameObject(f));
  if(msg)
    Gal_SetProp(new_f, ":msg", Gal_StringObject(msg));
  GalSS_EnvLock(env);
  GalSS_EnvWriteFrame(env, new_f, 1);
  GalSS_EnvUnlock(env);
  /* Remove the properties before freeing the frame, so we don't have
     to copy the frame. */
  Gal_RemProp(new_f, ":frame");
  Gal_FreeFrame(new_f);
}

/* The "title" argument is only used when printing out to the Hub's stdout. */
static void _gal_hub_print_namespace(HUB *h, char *title, Gal_Frame f, char *type, char *msg)
{ 
  if (h && h->gui && h->gui->provider &&
	  (h->gui->provider->status > DISCONNECTED)) {
    __send_hub_break_message(h, f, type, msg);
  } else {
    GalUtil_Print(-1, "%s\n", title);
    if(f)
      Gal_PPFrame(f);
  }
}

/* Sends notification of Hub break to GUI server and waits for
   the user debug command. */
static void __dispatch_hub_break_prompt(HUB *h, char *cmd)
{
  GalSS_Environment *env = GalSS_EnvCreate(h->gui->provider->gcomm);
  Gal_Frame new_f = Gal_MakeFrame("hub_debug_prompt", GAL_CLAUSE);
  Gal_Frame reply_f;
  GalIO_MsgType t;
  char *reply_string;

  GalSS_EnvLock(env);

  reply_f = GalSS_EnvDispatchFrame(env, new_f, &t);
  reply_string = Gal_GetString(reply_f, ":debug_cmd");
  if(reply_string)
    strcpy(cmd, reply_string);
  else
    GalUtil_Warn("Got null debug command");

  Gal_FreeFrame(new_f);
  Gal_FreeFrame(reply_f);
  GalSS_EnvUnlock(env);
}

/* Prompts the user for a Hub "debug" command (via the Hub's stdout or the
   viz server, if it is connected). Result is returned in "cmd" argument 
   (pre-allocated char buffer of size "buf_size"). */
static void __getHubBreakCmd(HUB *h, char *prompt, char *cmd, int buf_size)
{
  if (h && h->gui && h->gui->provider &&
      (h->gui->provider->status > DISCONNECTED)) {
    __dispatch_hub_break_prompt(h, cmd);
  } else {
    GalUtil_Print(-1, prompt);
    fgets(cmd, buf_size, stdin);
  }
}

/* This function is used for the break loop in hub_process.c as
   well as in Builtin.break. Returns whether to continue breaking or not. */
int _gal_hub_break(HUB *h, TOKEN *t, SERVICE_PROVIDER *sp, Gal_Frame msg,
		   SESSION *session)
{
  int buf_size = 256;
  char c[256];
  char *help_msg;

  memset(c,0,buf_size);
  __getHubBreakCmd(h, "(h for help, c or <return> to continue) --> ", c, buf_size);

  while(strlen(c) > 0) {
    if (c[0] == 'c' || c[0] == '\n')
      return 1;
    switch (c[0]) {
    case 'C':
      return 0;
    case 'd':
      _GalHUB_ReportSessionStatus(h, -1, GAL_HUB_SESSION_HISTORY, session, 1);
      break;
    case 'e':
      force_hub_exit();
      return 0;
    case 'g':
      _gal_hub_print_namespace(h, "Global settings:", h->globals, "global_settings", NULL);
      break;
    case 'h':
      help_msg = "C: disable debug and continue\nc: continue\nd: session DB\ne: exit Hub\ng: globals\nh: help\nl: locks\nm: message\nr: server\ns: session\nt: token\n";
      if (h && h->gui && h->gui->provider &&
	  (h->gui->provider->status > DISCONNECTED)) {
	/* do nothing */
      } else {
	GalUtil_Print(-1, help_msg);
      }
      break;
    case 'l':
      print_session_locks(-1, 1);
      break;
    case 'm':
      _gal_hub_print_namespace(h, "Current message:", msg, "current_message", NULL);
      break;
    case 'r':
      if (sp)
	_gal_hub_print_namespace(h, "Server settings:", sp->properties, "server_settings", NULL);
      else
	_gal_hub_print_namespace(h, "No server found.", NULL, "server_settings", NULL);
      break;
    case 's':
      _GalHUB_ReportSessionStatus(h, -1, GAL_HUB_SESSION_SETTINGS, session, 1);
      break;
    case 't':
      _GalHUB_ReportTokenStatus(h, -1, GAL_HUB_TOKEN_STATUS, t, 1);
      break;
    }
    memset(c,0,buf_size);
    __getHubBreakCmd(h, "--> ", c, buf_size);
  }
  return 1;
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
