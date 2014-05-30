;; This file (c) Copyright 1998 - 2002 The MITRE Corporation
;; 
;; This file is part of the Galaxy Communicator system. It is licensed
;; under the conditions described in the file LICENSE in the root 
;; directory of the Galaxy Communicator system.

(defpackage MGalaxy
  (:use "COMMON-LISP"))

(in-package "MGALAXY")

(defconstant *documentation* "
MGal_ActivateStdinPoll(obj):            [not implemented yet]
MGalIO_CreateStdinPoll(prompt, ...):    [not implemented yet]
MGalSS_CreateStdinPoll(prompt, ...):    [not implemented yet]
MGal_FreeStdinPoll(obj):                [not needed]
MGal_PollStdin(obj):                    [not implemented yet]
MGal_GetStdinPollData(...):             [not implemented yet]
MGal_SetStdinPollData(...):             [not implemented yet]
MGal_SetStdinPollPrompt(c, prompt):     [not implemented yet]

MGal_CreateFullFrame(name, type, ...):  [not needed]

MGal_AddOutgoingBrokering(port, f, ...): [not implemented yet]
MGal_AddBrokerDTHandler(dt, val, h):     [not implemented yet]
MGal_AddIncomingBrokering(fr, ms, data): [not implemented yet]

MGal_AddBinaryDataType(t, encoder, decoder):  [not implemented yet]
MGal_OpaqueObject(obj, dt):                   [not implemented yet]
MGal_GetOpaque(f, key, dt):                   [not implemented yet]
MGal_GetOpaqueWarn(f, key, dt):               [not implemented yet]
")
