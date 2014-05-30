/*
  This file (c) Copyright 2001 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include "galaxy/galaxy_all.h"
#include "galaxy/sysdep.h"
#include "hub.h"
#include "hub_internal.h"

/* SAM 1/22/01: This function will create a special server,
   which will be handled specially throughout the hub. These
   servers are special Hub-specific services, like a gui,
   or a visualization tool, or a logger. Right now, the first
   pass is just for a gui. */

SPECIAL_SERVER *GalHUB_CreateSpecialServer(char *server_location,
					   char *stype_name)
{
  int port = -1;
  char *host = Gal_SplitLocation(server_location, &port);
  SPECIAL_SERVER *special_s = (SPECIAL_SERVER *) NULL;
 
  if (Gal_StringEq(host, "<listener>")) {
    /* Hub listener. */
    SERVICE_TYPE *stype = GalHUB_NewServiceType(stype_name, port);

    special_s = (SPECIAL_SERVER *) calloc(1, sizeof(SPECIAL_SERVER));
    special_s->stype = stype;
    stype->special = special_s;
  } else if (host) {
    /* Server listener. */
    SERVICE_TYPE *stype = GalHUB_NewServiceType("__gui__", -1);
    SERVICE_PROVIDER *s = GalHUB_NewServiceProvider(host, port, stype);

    special_s = (SPECIAL_SERVER *) calloc(1, sizeof(SPECIAL_SERVER));
    special_s->stype = stype;
    special_s->provider = s;
    stype->special = special_s;
  } else {
    GalUtil_Warn("Unknown listener loc in special server location `%s'; skipping", server_location);
  }
  if (host) free(host);
  return special_s;
}

void GalHUB_FreeSpecialServer(SPECIAL_SERVER *s)
{
  if (s->stype) {
    GalHUB_RemoveServiceType(s->stype, (HUB *) NULL, 0, 0);
  }
  if (s->provider) {
    GalHUB_RemoveServiceProvider((HUB *) NULL, s->provider, 0, 0);
  }
  free(s);
}

/* As we see in GalHUB_CreateSpecialServer(), a server listener
   will already have a provider set up, while a Hub listener
   will not. */

void GalHUB_InitializeSpecialServers(HUB *h)
{
  if (h->gui)
    GalHUB_InitializeSpecialServer(h, h->gui);
}

void GalHUB_InitializeSpecialServer(HUB *h, SPECIAL_SERVER *special_s)
{
  /* If it's supposed to be a Hub listener, set up a listener.
     Otherwise, connect to the remote server. */
  if (!special_s)
    return;
  if ((special_s->stype->listener_port > -1) && !special_s->provider) {
    /* It's a listener. We pass the Hub in in order to 
       update the max FDs (important).*/
    _GalHUB_InitializeInternalHubServer(special_s->stype, h);
  } else {
    /* It's a client. We pass the Hub in in order to get
       the initialization keys (unnecessary but harmless) and
       update the max FDs (important). */
    initialize_connection_to_server(special_s->provider, h, 0, 1, 0);
  }
}

/* Return previous_ready. Checks the special servers and
   tries to start them all. */

int GalHUB_PerhapsAddSpecialServer(SPECIAL_SERVER *special_s,
				   fd_set *writefd, fd_set *readfd)
{
  if (special_s->provider)
    return GalHUB_PerhapsAddServerToFD(special_s->provider, writefd, readfd);
  return 0;
}  

int GalHUB_PerhapsAddSpecialServers(HUB *h, fd_set *writefd, fd_set *readfd)
{
  int previous_ready = 0;
  
  if (h->gui) {
    if (GalHUB_PerhapsAddSpecialServer(h->gui, writefd, readfd))
      previous_ready = 1;
  }
  return previous_ready;
}

/* Check FDs for special servers. */

void GalHUB_CheckSpecialServerFD(HUB *h, SPECIAL_SERVER *special_s,
				 fd_set *writefd_ptr, fd_set *readfd_ptr)
{
  if (special_s->provider)
    special_s->provider = GalHUB_CheckServerFD(h, special_s->provider,
					       writefd_ptr, readfd_ptr);
}

void GalHUB_CheckSpecialServerFDs(HUB *h, fd_set *writefd_ptr,
				  fd_set *readfd_ptr)
{
  if (h->gui) {
    GalHUB_CheckSpecialServerFD(h, h->gui, writefd_ptr, readfd_ptr);
  }
}

/* Checking for special servers. */

int GalHUB_IsSpecialServer(HUB *h, SERVICE_PROVIDER *provider)
{
  if (h->gui && (h->gui->provider == provider))
    return 1;
  return 0;
}

/* Attempt to restart special servers. */

void GalHUB_ReconnectToSpecialServers(HUB *h)
{
  if (h->gui)
    GalHUB_ReconnectToSpecialServer(h, h->gui);
}

void GalHUB_ReconnectToSpecialServer(HUB *h, SPECIAL_SERVER *special_s)
{
  if (special_s->provider) {
    GalHUB_AttemptServerReconnection(h, special_s->provider, 1);
  }
}

/* Special GUI interaction. */

void _GalHUB_GUIAnnounceServiceType(HUB *h, SERVICE_TYPE *stype)
{ 
  if (h->gui && h->gui->provider &&
      (h->gui->provider->status > DISCONNECTED)) {
    /* Send across the wire. */
    Gal_Frame f = Gal_MakeFrame("new_service_type", GAL_CLAUSE);
    
    Gal_SetProp(f, ":name", Gal_StringObject(stype->name));
    Gal_SetProp(f, ":port", Gal_IntObject(stype->listener_port));
    GalIO_CommWriteMessage(h->gui->provider->gcomm, f,
			   GAL_MESSAGE_MSG_TYPE, 1);
    Gal_FreeFrame(f);
  } else {
    GalUtil_CPInfo1(2,0,"\n--------------------------------------------------\n");
    GalUtil_PInfo1("service type: %s : %d\n",
		   stype->name, stype->listener_port);
  }
}

void _GalHUB_GUIAnnounceServiceProvider(HUB *h, SERVICE_PROVIDER *provider)
{
  if (h->gui && h->gui->provider &&
      (h->gui->provider->status > DISCONNECTED)) {
    /* Do nothing. */
  } else {
    GalUtil_CPInfo1(2,0,"\n--------------------------------------------------\n");
    GalUtil_PInfo1("provider: %s\n", provider->pname);
  }
}

static Gal_Frame __build_provider_status(SERVICE_PROVIDER *p,
					 int connected, int first_time)
{
  Gal_Frame props;
  
  if (first_time) {
    props = Gal_CopyFrame(p->properties);

    /* If no :screen_name, build one. */
    if (!Gal_GetString(props, ":screen_name")) {
      char *buf;

      if (p->id_name) {
	buf = (char *) malloc(strlen(p->iname) + strlen(p->id_name) + 3);
	sprintf(buf, "[%s]%s", p->id_name, p->iname);
      } else {
	/* More than enough room to print any integer we may get. */
	buf = (char *) malloc(strlen(p->iname) + 32);
	sprintf(buf, "[%d]%s", p->id, p->iname);
      }
      
      Gal_SetProp(props, ":screen_name", Gal_StringObject(buf));
      free(buf);
    }

    /* :init means this is the first time this provider's
       status is being reported. */
    Gal_SetProp(props, ":init", Gal_IntObject(1));
    /* We also want to announce that this server is available
       (not grayed out) by default. */
    Gal_SetProp(props, ":available", Gal_IntObject(1));
    /* This means it's persistent. */
    if (p->listen_status == GAL_HUB_STYPE_CLIENT) {
      Gal_SetProp(props, ":hub_is_client", Gal_IntObject(1));
    }
  } else {
    props = Gal_MakeFrame("provider", GAL_CLAUSE);
  }
  
  Gal_SetProp(props, ":provider_id", Gal_StringObject(GalHUB_ServiceProviderID(p)));
  Gal_SetProp(props, ":connected", Gal_IntObject(connected));
  return props;
}

void _GalHUB_GUIAnnounceAvailableProviders(HUB *h, SERVICE_PROVIDER **providers, int num_providers)
{
  if (h->gui && h->gui->provider &&
      (h->gui->provider->status > DISCONNECTED)) {
    /* Construct a list of all the servers, and send the properties
       across. */
    Gal_Object provider_list = Gal_CreateListObject((Gal_Object *) NULL, 0,
						    _gal_free_object, 1);
    
    Gal_Frame msg = Gal_MakeFrame("provider_status", GAL_CLAUSE);
    int i;
    
    for (i = 0; i < num_providers; i++) {
      Gal_Frame props;
      
      /* Don't send local servers like Builtin. */
      if (providers[i]->sockid == LOCAL)
	continue;

      /* Don't send servers which are GUI invisible. */
      if (Gal_GetInt(providers[i]->properties, ":gui_invisible"))
	continue;

      props = __build_provider_status(providers[i], 0, 1);
      Gal_ListObjectAdd(provider_list, Gal_FrameObject(props));
    }

    /* Now, build the message and send it. */
    Gal_SetProp(msg, ":provider_list", provider_list);
    GalIO_CommWriteMessage(h->gui->provider->gcomm, msg,
			   GAL_MESSAGE_MSG_TYPE, 1);
    Gal_FreeFrame(msg);
  }						    
}

void _GalHUB_GUIAnnounceServiceProviderConnection(HUB *h, SERVICE_PROVIDER *provider, int connected, int reconnect)
{
  if (h->gui && h->gui->provider &&
      (h->gui->provider->status > DISCONNECTED) &&
      (!Gal_GetInt(provider->properties, ":gui_invisible"))) {
    /* Send across the wire. */
    
    Gal_Frame f = Gal_MakeFrame("provider_status", GAL_CLAUSE);
    Gal_Frame props = __build_provider_status(provider, connected,
					      (reconnect == 0));
    Gal_SetProp(f, ":provider_list",
		Gal_ListObjectFromElements(1, Gal_FrameObject(props)));
    GalIO_CommWriteMessage(h->gui->provider->gcomm, f,
			   GAL_MESSAGE_MSG_TYPE, 1);
    Gal_FreeFrame(f);
  }
}

void _GalHUB_GUIAnnounceMessage(HUB *h, Gal_Frame msg, GalIO_MsgType msg_type,
				SERVICE_PROVIDER *provider,
				char *destination)
{
  /* Don't do anything if it's a builtin. */
  if (provider->sockid == LOCAL)
    return;

  /* Don't do anything if the provider in question is a
     special server. */

  if (GalHUB_IsSpecialServer(h, provider))
    return;
    
  if (h->gui && h->gui->provider &&
      (h->gui->provider->status > DISCONNECTED)) {

    /* Send across the wire. */
    Gal_Frame f = Gal_MakeFrame("message_event", GAL_CLAUSE);

    Gal_SetProp(f, ":provider_id", Gal_StringObject(GalHUB_ServiceProviderID(provider)));
    Gal_SetProp(f, ":msg", Gal_FrameObject(msg));
    Gal_SetProp(f, ":msg_type", Gal_IntObject(msg_type));
    Gal_SetProp(f, ":destination", Gal_StringObject(destination));

    GalIO_CommWriteMessage(h->gui->provider->gcomm, f,
			   GAL_MESSAGE_MSG_TYPE, 1);
    Gal_RemProp(f, ":msg");
    Gal_FreeFrame(f);
  }

  /* Pause if the hub pause is set. */
  if (h->hub_pause) {
    GalUtil_PInfo2("Pausing!\n");
    GalUtil_MilliSleep(h->hub_pause);
  }
}



