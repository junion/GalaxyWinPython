/*
  This file (c) Copyright 1998 - 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

/* MITRE added this file to the GalaxyCommunicator core
   because we need message types before we can load
   galaxy_io.h in io_internal.h, but we need them in public
   as well. */

/* These are the various message types. */

#ifndef _GALIO_MSG_TYPES
#define _GALIO_MSG_TYPES

typedef enum {GAL_OBJECT_MSG_TYPE, GAL_MESSAGE_MSG_TYPE, GAL_REPLY_MSG_TYPE,
	      GAL_DESTROY_MSG_TYPE, 
	      GAL_BROKER_START_MSG_TYPE, GAL_BROKER_END_MSG_TYPE,
	      GAL_ERROR_MSG_TYPE, GAL_DISCONNECT_MSG_TYPE,
              GAL_POSTPONE_MSG_TYPE} GalIO_MsgType;

typedef enum {GAL_APPLICATION_ERROR, GAL_NO_OPNAME_ERROR,
	      GAL_TRANSMISSION_ERROR, GAL_RECEPTION_ERROR,
	      GAL_SERVER_DOWN_ERROR,
	      GAL_NO_FRAME_ERROR, GAL_CONN_REJECTION_ERROR} GalIO_IOError;

#endif /* _GALIO_MSG_TYPES */
