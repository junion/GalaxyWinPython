/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include <stdlib.h>
#include "galaxy/galaxy_all.h"
#include "MITRE_galaxy.h"

#ifdef WIN32
#include "galaxy/sysdep.h"
#endif

/* This is how you encode and decode brokering without dropping
   the pieces on the floor. */

/* 0 is 100 ms poll, > 10 is poll_ms poll, < 0 is no timed task. */

/* This is broker data going out. */

GalIO_BrokerStruct *MGal_AddOutgoingBrokering(GalIO_CommStruct *gcomm,
					      Gal_Frame fr, int poll_ms,
					      int timeout_ms)
{
  GalIO_BrokerStruct *b;
  b = GalIO_BrokerDataOutInit(gcomm, poll_ms,
			      timeout_ms);
  if (b && (GalIO_GetBrokerListenPort(b) > 0)) {
    GalIO_BrokerPopulateFrame(b, fr, ":broker_host", ":broker_port");
    return b;
  } else {
    return (GalIO_BrokerStruct *) NULL;
  }
}

/* This is broker data coming in. */

static MGal_BrokerDTEntry *DT_Table = (MGal_BrokerDTEntry *) NULL;

void MGal_AddBrokerDTHandler(Gal_ObjectType dt, void *val,
			     MGal_BrokerDTHandler h)
{
  MGal_BrokerDTEntry *new_entry = (MGal_BrokerDTEntry *) malloc(sizeof(MGal_BrokerDTEntry));

  new_entry->data_type = dt;
  new_entry->val = val;
  new_entry->handler = h;
  new_entry->next = DT_Table;

  DT_Table = new_entry;
}

static void MGal_BrokerHandler(GalIO_BrokerStruct *broker_struct,
			       void *data, Gal_ObjectType data_type,
			       int n_samples)
{
  MGal_BrokerDTEntry *e = DT_Table;
  int do_it = 0;
  
  while (e) {
    if (data_type == e->data_type) {
      if (!e->val) {
	do_it = 1;
      } else {
	if (data_type == GAL_STRING) {
	  if (!strcmp((char *) e->val, (char *) data)) {
	    do_it = 1;
	  } 
	}
      }
      if (do_it) {
	(*(e->handler))(broker_struct, data, n_samples);
	return;
      }
    }
    e = e->next;
  }
  GalUtil_WarnWithLocation(__FUNCTION__, "Unexpected data type %s\n", Gal_ObjectTypeString(data_type));
}

GalIO_BrokerStruct *MGal_AddIncomingBrokering(Gal_Frame fr,
					      int poll_ms,
					      void *caller_data,
					      int activate)
{
  char *host = Gal_GetString(fr, ":broker_host");
  unsigned short port = Gal_GetInt(fr, ":broker_port");
  GalIO_BrokerStruct *b;
 
  if (host && port) {
    b = GalIO_BrokerDataInInit(host, port, fr,
			       MGal_BrokerHandler, caller_data,
			       poll_ms);
    if (b && activate) GalIO_SetBrokerActive(b);
    return b;
  } else {
    return (GalIO_BrokerStruct *) NULL;
  }
}
    
  
