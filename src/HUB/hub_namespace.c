/*
  This file (c) Copyright 2000 The MITRE Corporation 
  
  This file is part of the Galaxy Communicator system. It is licensed
  under the conditions described in the file LICENSE in the root 
  directory of the Galaxy Communicator system.
*/

#include "hub.h"

/* These are defined in the same order as they're enum-ed. */

Gal_NamespaceEntry HubNamespaceTable[] =
{
  {GAL_SESSION_NAMESPACE, "session", 1, 1},
  {GAL_UTTERANCE_DB_NAMESPACE, "utterance_db", 1, 1},
  {GAL_MESSAGE_NAMESPACE, "message", 1, 1},
  {GAL_SERVER_NAMESPACE, "server", 1, 1},
  {GAL_TOKEN_NAMESPACE, "token", 1, 1},
  {GAL_GLOBAL_NAMESPACE, "global", 1, 1},
  {0, NULL, 0, 0}
};
