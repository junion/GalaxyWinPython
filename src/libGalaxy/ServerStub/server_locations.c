/*
  This file (c) Copyright 1998 - 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdio.h>
#include "galaxy/sysdep.h"
#include "generic-server-internal.h"
#include "galaxy/program.h"

/* I'm getting completely fed up with this problem of needing
   to modify .pgm files when I move servers around. There are
   more sophisticated ways of fixing this, but I'm going to do
   something simple in anticipation of doing something better later.
   We're going to have a very simple server location file, which
   consists of lines of the following sort:

   foo feta.mitre.org:2600 hub

   The first element is the server name, the second is the listener
   location, and the third is whether the hub or server is
   the listener. Both Hub and server can use this file to figure
   out how to set itself up.

   All the entries where the Hub is the listener must have the
   same host.

   Comments are #. Empty lines are ignored.

   I should be doing this stuff in XML, but I'm not ready to
   dive into XML in C yet... */

/* This will work in two modes: searching for a particular server
   and loading in the entire database. */

#define MAX_LINE_LENGTH 1024

GalSS_ServerLocationEntry *
_GalSS_DigestServerLocationFile(const char *filename,
				const char *server_name)
{
  char line[MAX_LINE_LENGTH];
  char server_buf[MAX_LINE_LENGTH];
  char location_buf[MAX_LINE_LENGTH];
  char where_buf[MAX_LINE_LENGTH];
  int who_listens;

  GalSS_ServerLocationEntry *head = (GalSS_ServerLocationEntry *) NULL;
  GalSS_ServerLocationEntry *tail = (GalSS_ServerLocationEntry *) NULL;
  GalSS_ServerLocationEntry *current = (GalSS_ServerLocationEntry *) NULL;
  
  /* If there's no server name, we accumulate the results. */

  /* SAM 10/22/01: Opening in binary mode is harmless on Unix,
     but crucial on Windows, since ftell() is used by the file reader
     and doesn't work correctly on Windows in text mode when the
     file doesn't contain "proper" line terminations (i.e., only
     \n instead of \r\n, or whatever it is. */
  
  FILE *fp = fopen(filename, "rb");
  int scanf_res;

  if (!fp) {
    GalUtil_Warn("Couldn't open server location file %s", filename);
    return (GalSS_ServerLocationEntry *) NULL;
  }
  /* Read until we can read no more... */
  for ( ; fgets(line, MAX_LINE_LENGTH, fp); ) {
    /* Find the non-whitespace beginning. */
    char *true_line = line + strspn(line, " \t\r\n");
    /* Ignore empty lines. */
    if (strlen(true_line) == 0) {
      continue;
    }
    /* Ignore comment lines. */
    if (true_line[0] == '#') {
      continue;
    }
    scanf_res = sscanf(true_line, "%s %s %s",
		       server_buf, location_buf, where_buf);

    if (scanf_res < 3) {
      GalUtil_Warn("Ignoring ill-formed server location line `%s'\n",
		   true_line);
      continue;
    }

    if (!_gal_strcasecmp(where_buf, "hub")) {
      who_listens = GAL_SL_HUB_LISTENS;
    } else if (!_gal_strcasecmp(where_buf, "server")) {
      who_listens = GAL_SL_SERVER_LISTENS;
    } else {
      GalUtil_Warn("Ignoring line containing ill-formed listener ID `%s'",
		   where_buf);
      continue;
    }
    
    /* At this point, we have everything we need. Build the
       object if we need it. */

    if ((!server_name) || (!strcmp(server_buf, server_name))) {
      /* If there's no server name, or if the server name matches,
	 build the structure. Note that the matching happens
	 before the split. */      
      current = (GalSS_ServerLocationEntry *) malloc(sizeof(GalSS_ServerLocationEntry));

      if (!_GalSS_PopulateProviderSpec(server_buf, location_buf,
				       &(current->provider_spec))) {
	/* Something in the parsing has failed, probably the location. */
	GalUtil_Warn("Ignoring line containing ill-formed server location `%s'",
		     location_buf);
	free(current);
      }

      current->hub_or_server = who_listens;
      current->next = (GalSS_ServerLocationEntry *) NULL;

      if (server_name) {
	/* We're done. Return it. */
	return current;
      } else {
	/* We're collecting. */
	if (!head) {
	  head = tail = current;
	} else {
	  tail->next = current;
	  tail = tail->next;
	}
      }	  
    }
  }
  /* head will be NULL when there's a server but no match. */
  /* Double check to make sure that all the Hub entries are
     compatible. */
  if (head) {
    char *first_hub_host = (char *) NULL;
    current = head;
    while (current) {
      if (current->hub_or_server == GAL_SL_HUB_LISTENS) {
	if (!first_hub_host) {
	  first_hub_host = current->provider_spec.host;
	} else if (strcmp(first_hub_host, current->provider_spec.host)) {
	  /* Oops. */
	  GalUtil_Warn("Ill-formed server location file: not all Hub listeners on the same host");
	  _GalSS_FreeServerLocationEntry(head);
	  return (GalSS_ServerLocationEntry *) NULL;
	}
      }
      current = current->next;
    }
  }    
  return head;
}

void _GalSS_FreeServerLocationEntry(GalSS_ServerLocationEntry *entry)
{
  GalSS_ServerLocationEntry *next;

  while (entry) {
    next = entry->next;
    _GalSS_ClearProviderSpec(&(entry->provider_spec));
    free(entry);
    entry = next;
  }
}

GalSS_ServerLocationEntry *
GalSS_FindServerLocationEntry(const char *filename,
			      const char *server_name)
{
  return _GalSS_DigestServerLocationFile(filename, server_name);
}

GalSS_ServerLocationEntry *GalSS_DigestServerLocationFile(const char *filename)
{
  return _GalSS_DigestServerLocationFile(filename, (char *) NULL);
}

/* These utility functions are used in a number of places, not
   just in the server location files. The location chunk, if present,
   needs to have host and port defined. */

void _GalSS_InitializeProviderSpec(GalSS_ProviderSpec *spec)
{
  spec->stype_name = (char *) NULL;
  spec->host = (char *) NULL;
  spec->port = -1;
  spec->id_name = (char *) NULL;
  spec->id = -1;
}

GalSS_ProviderSpec *
_GalSS_PopulateProviderSpec(char *server_chunk,
			    char *location_chunk,
			    GalSS_ProviderSpec *existing_spec)
{
  int port_num = -1;
  char *host_name = (char *) NULL;
  int provider_id = -1;
  char *provider_name = (char *) NULL;
  char *stype_name;
  
  /* First, we try to gather all the info. 
     Now, we try to set up the location object. First,
     we make sure the location is good. */
  if (location_chunk) {
    host_name = Gal_SplitLocation(location_chunk, &port_num);
    if ((!host_name) || (port_num == -1)) {
      if (host_name) free(host_name);
      return (GalSS_ProviderSpec *) NULL;
    }
  }
  stype_name = Gal_SplitServiceTypeName(server_chunk, &provider_id,
					&provider_name);
  if (!existing_spec) {
    existing_spec = (GalSS_ProviderSpec *) calloc(1, sizeof(GalSS_ProviderSpec));
  }
  _GalSS_InitializeProviderSpec(existing_spec);
  existing_spec->stype_name = stype_name;
  existing_spec->host = host_name;
  existing_spec->port = port_num;
  existing_spec->id_name = provider_name;
  existing_spec->id = provider_id;
  return existing_spec;
}

char *GalSS_FormatProviderSpec(GalSS_ProviderSpec *spec,
			       Gal_StringBuffer **bufptr)
{
  int print_at = 0;
  
  if (!*bufptr) {
    *bufptr = Gal_MakeStringBuffer((char *) NULL, 0);
  } else {
    /* Rewind! */
    (*bufptr)->bufpos = 0;
  }
  if (spec->id > -1) {
    print_at = 1;
    Gal_StringBufferWrite(*bufptr, 18, "[%d]", spec->id);
  } else if (spec->id_name) {
    print_at = 1;
    Gal_StringBufferWrite(*bufptr, strlen(spec->id_name) + 2,
			  "[%s]", spec->id_name);
  }
  if (spec->stype_name) {
    print_at = 1;
    Gal_StringBufferWrite(*bufptr, -1, spec->stype_name);
  }
  if (spec->host || (spec->port > -1)) {
    if (print_at)
      Gal_StringBufferWrite(*bufptr, -1, "@");
    if (spec->host && (spec->port > -1)) {
      Gal_StringBufferWrite(*bufptr, strlen(spec->host) + 18,
			    "%s:%d", spec->host, spec->port);
    } else if (spec->host) {
      Gal_StringBufferWrite(*bufptr, -1, spec->host);
    } else {
      Gal_StringBufferWrite(*bufptr, 18, "%d", spec->port);
    }      
  }
  return (*bufptr)->buf;
}

void _GalSS_ClearProviderSpec(GalSS_ProviderSpec *spec)
{
  if (spec->host) free(spec->host);
  if (spec->stype_name) free(spec->stype_name);
  if (spec->id_name) free(spec->id_name);
}

void GalSS_FreeProviderSpec(GalSS_ProviderSpec *spec)
{
  _GalSS_ClearProviderSpec(spec);
  free(spec);
}
