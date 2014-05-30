/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

GAL_SERVER_NAME(double)
GAL_SERVER_PORT(2800)
GAL_SERVER_OP_SIGNATURE(twice,
                        GAL_SERVER_OP_KEYS(":int" _ GAL_INT _ GAL_KEY_ALWAYS),
                        GAL_OTHER_KEYS_NEVER,
                        GAL_REPLY_NONE,
                        NULL,
                        GAL_OTHER_KEYS_NEVER)
GAL_SERVER_OP_SIGNATURE(complex_twice,
                        GAL_SERVER_OP_KEYS(":int" _ GAL_INT _ GAL_KEY_ALWAYS),
                        GAL_OTHER_KEYS_NEVER,
                        GAL_REPLY_NONE,
                        NULL,
                        GAL_OTHER_KEYS_NEVER)
GAL_SERVER_OP_SIGNATURE(continuation_complex_twice,
                        GAL_SERVER_OP_KEYS(":int" _ GAL_INT _ GAL_KEY_ALWAYS),
                        GAL_OTHER_KEYS_NEVER,
                        GAL_REPLY_NONE,
                        NULL,
                        GAL_OTHER_KEYS_NEVER)
GAL_SERVER_OP_SIGNATURE(reinitialize,
                        NULL,
                        GAL_OTHER_KEYS_NEVER,
                        GAL_REPLY_PROVIDED,
                        NULL,
                        GAL_OTHER_KEYS_NEVER)
GAL_SERVER_OP(echo_frame)
