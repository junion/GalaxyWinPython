/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

GAL_SERVER_NAME(multiply)
GAL_SERVER_PORT(2900)
GAL_SERVER_OP(reinitialize)
GAL_SERVER_OP_SIGNATURE(multiply, 
			GAL_SERVER_OP_KEYS(":int" _ GAL_INT _ GAL_KEY_ALWAYS), 
			GAL_OTHER_KEYS_NEVER, 
			GAL_REPLY_PROVIDED, 
			GAL_SERVER_OP_KEYS(":int" _ GAL_INT _ GAL_KEY_ALWAYS), 
			GAL_OTHER_KEYS_NEVER)
