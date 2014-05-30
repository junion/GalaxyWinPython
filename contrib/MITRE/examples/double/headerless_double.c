/*
  This file (c) Copyright 1998 - 2002 The MITRE Corporation
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "galaxy/galaxy_all.h"

extern Gal_Frame twice(Gal_Frame f, void *server_data);
extern Gal_Frame complex_twice(Gal_Frame f, void *server_data);
extern Gal_Frame reinitialize(Gal_Frame f, void *server_data);
extern Gal_Frame continuation_complex_twice(Gal_Frame f, void *server_data);
extern Gal_Frame echo_frame(Gal_Frame f, void *server_data);

void _GalSS_InitializeDefaults(GalIO_ServerStruct *s)
{
  GalSS_InitializeServerDefaults(s, "double", 2800);
  GalSS_AddDispatchFunction(s, "twice", twice, NULL,
                            GAL_OTHER_KEYS_MAYBE, GAL_REPLY_UNKNOWN,
                            NULL, GAL_OTHER_KEYS_MAYBE);
  GalSS_AddDispatchFunction(s, "complex_twice", complex_twice, NULL,
                            GAL_OTHER_KEYS_MAYBE, GAL_REPLY_UNKNOWN,
                            NULL, GAL_OTHER_KEYS_MAYBE);
  GalSS_AddDispatchFunction(s, "continuation_complex_twice",
			    continuation_complex_twice, NULL,
                            GAL_OTHER_KEYS_MAYBE, GAL_REPLY_UNKNOWN,
                            NULL, GAL_OTHER_KEYS_MAYBE);
  GalSS_AddDispatchFunction(s, "echo_frame",
			    echo_frame, NULL,
                            GAL_OTHER_KEYS_MAYBE, GAL_REPLY_UNKNOWN,
                            NULL, GAL_OTHER_KEYS_MAYBE);  
  GalSS_AddDispatchFunction(s, "reinitialize", reinitialize, NULL,
                            GAL_OTHER_KEYS_MAYBE,
                            GAL_REPLY_UNKNOWN,
                            NULL, GAL_OTHER_KEYS_MAYBE);
}
