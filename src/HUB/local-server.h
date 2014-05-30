/*
  This file (c) Copyright 1999 - 2000 M.I.T.
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy.h"

/* SAM 10/2/99: Now that we don't need to run the stubber, we can
   use the same definitions for the local server as everything else.
   This will simplify what I do with the server function maps. */

GAL_SERVER_OP_SIGNATURE(hub_turn_management, 
			GAL_SERVER_OP_KEYS(":parse_frame" _ GAL_FRAME _ GAL_KEY_ALWAYS),
			GAL_OTHER_KEYS_MAYBE,
			GAL_REPLY_PROVIDED,
			GAL_SERVER_OP_KEYS(":reply_frame" _ GAL_FRAME _ GAL_KEY_ALWAYS),
			GAL_OTHER_KEYS_MAYBE)
GAL_SERVER_OP(dispatch_to_main)
GAL_SERVER_OP(set_session_parameters)
GAL_SERVER_OP(dispatch_token)
GAL_SERVER_OP(call_program)
GAL_SERVER_OP(abort_main)
GAL_SERVER_OP_SIGNATURE(increment_utterance,
			GAL_SERVER_OP_KEYS(":session_id" _ GAL_STRING _ GAL_KEY_SOMETIMES _
					   ":domain" _ GAL_STRING _ GAL_KEY_SOMETIMES),
			GAL_OTHER_KEYS_NEVER,
			GAL_REPLY_PROVIDED,
			GAL_SERVER_OP_KEYS(":utterance_id" _ GAL_INT _ GAL_KEY_ALWAYS),
			GAL_OTHER_KEYS_NEVER)
GAL_SERVER_OP(new_session)
GAL_SERVER_OP(end_session)
GAL_SERVER_OP(destroy)
GAL_SERVER_OP(reset_history)
GAL_SERVER_OP_SIGNATURE(nop, NULL, GAL_OTHER_KEYS_NEVER,
			GAL_REPLY_PROVIDED, NULL,
			GAL_OTHER_KEYS_NEVER)
GAL_SERVER_OP(debug_token)
GAL_SERVER_OP_SIGNATURE(log_keys, NULL, GAL_OTHER_KEYS_MAYBE,
			GAL_REPLY_NONE,
			NULL, GAL_OTHER_KEYS_MAYBE)
GAL_SERVER_OP(modify_properties)
GAL_SERVER_OP(get_properties)
GAL_SERVER_OP(set_session)
GAL_SERVER_OP(hub_ping)
GAL_SERVER_OP(hub_available_servers)
GAL_SERVER_OP(hub_print)
GAL_SERVER_OP(hub_break)
GAL_SERVER_OP(hub_exit)
GAL_SERVER_OP(hub_raise_error)
GAL_SERVER_OP(hub_token_owner)
GAL_SERVER_OP(hub_pause)
GAL_SERVER_OP(hub_gui_notify)
GAL_SERVER_OP(hub_continue)
GAL_SERVER_OP(hub_set_verbosity)
GAL_SERVER_OP(hub_token_timestamp)
GAL_SERVER_OP(hub_gc_version)
GAL_SERVER_OP(hub_sleep)
