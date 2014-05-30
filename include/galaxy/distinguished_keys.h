/*
  This file (c) Copyright 1998 - 2000 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#ifndef __GAL_DISTINGUISHED_KEYS_H__
#define __GAL_DISTINGUISHED_KEYS_H__

/* LANGUAGE BINDINGS MUST RESPECT THESE KEYS! */

/* These two keys form the contents of an error message. */
#define GAL_ERROR_NUMBER_FRAME_KEY ":errno"
#define GAL_ERROR_DESCRIPTION_FRAME_KEY ":err_description"

/* This is the key which indicates which session it is. */
#define GAL_SESSION_ID_FRAME_KEY ":session_id"

/* This is the unique index of the token. */
#define GAL_TOKEN_INDEX_FRAME_KEY ":tidx"

/* This is the unique index of the message sent to the Hub. */
#define GAL_SERVER_TOKEN_INDEX_FRAME_KEY ":server_tidx"

/* This is the key for Hub info which is "opaque" to the server.
   The material in this frame is guaranteed to be returned untouched. */
#define GAL_HUB_OPAQUE_DATA_FRAME_KEY ":hub_opaque_data"

/* This is the key which reports message signatures back to the Hub. */
#define GAL_SIGNATURES_FRAME_KEY ":signatures"

/* This is the key which reports server properties back to the Hub. */
#define GAL_SERVER_PROPERTIES_FRAME_KEY ":properties"

/* This is the key which reports additional service types back to
   the Hub. */
#define GAL_SERVICE_TYPE_FRAME_KEY ":extra_service_types"

/* This is the key which contains the unique key to match for
   broker connections. */
#define GAL_BROKER_CALL_ID_FRAME_KEY ":call_id"

/* This is the key which announces that a server wants an
   answer to its message, or that the Hub wants an answer
   to its message. */
#define GAL_ROUND_TRIP_FRAME_KEY ":reply_requested"

/* This is the key by which a server announces that it wishes
   to create a new session with the specified session id. */
#define GAL_HUB_NEW_SESSION_FRAME_KEY ":hub_new_session"

/* This key stores the utterance ID. */
#define GAL_UTTERANCE_ID_FRAME_KEY ":utterance_id"

/* This key allows the Hub to serve any session for this server. */
#define GAL_HUB_SERVE_ANY_SESSION_FRAME_KEY ":hub_serve_any_session"

/* This key requests that the Hub use this session for
   the specified server only. */
#define GAL_HUB_SERVE_THIS_SESSION_ONLY_FRAME_KEY ":hub_serve_this_session_only"


/* This key requests a session lock for a server. */
#define GAL_HUB_GET_SESSION_LOCK_FRAME_KEY ":hub_get_session_lock"

/* This key releases a session lock. */
#define GAL_HUB_RELEASE_SESSION_LOCK_FRAME_KEY ":hub_release_session_lock"

/* This key makes all the locks in both direction permanent */
#define GAL_HUB_SESSION_LOCK_PERMANENT_FRAME_KEY ":hub_permanent_lock"

/* This key stores the connection type in the handshake. */
#define GAL_CONNECTION_TYPE_FRAME_KEY ":conn_type"

/* This key stores the protocol version in the handshake. */
#define GAL_PROTOCOL_VERSION_FRAME_KEY ":protocol_version"

/* This key stores an indication to reset the session in the
   opaque information. */

#define GAL_RESET_SESSION_FRAME_KEY ":reset_session"

/* These flags encode an indication to set the session lock
   information in the opaque information. */

#define GAL_LOCK_MASK_FRAME_KEY ":lock_mask"

#define GAL_LOCK_VALUE_FRAME_KEY ":lock_value"

/* This key stores the current token timestamp. */
#define GAL_TOKEN_TIMESTAMP_FRAME_KEY ":timestamp"

/* This key stores the originating server. */

#define GAL_PROVIDER_ID_FRAME_KEY ":provider_id"

#endif
