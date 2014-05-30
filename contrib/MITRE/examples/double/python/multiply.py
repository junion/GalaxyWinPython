# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import os, sys, string, math, random

sys.path.insert(0, os.path.join(os.environ["GC_HOME"],
				"contrib", "MITRE", "templates"))

import GC_py_init

import Galaxy, GalaxyIO

Factor = 1

import exceptions

MultiplyError = "MultiplyError"

def Multiply(env, dict):
    try:
        res = dict[":int"] * Factor
        if (type(dict[":int"]) is type(0)) and \
           (type(res) is type(0L)):
            # Overflow.
            raise MultiplyError, "multiply would overflow MAXINT"
        else:
            return {":int": res}
    except exceptions.OverflowError:
        raise MultiplyError, "multiply would overflow MAXINT"    

def Welcome(env, dict):
    global Factor
    try:
	Factor = dict[":factor"]
    except: pass
    return None

def main():
    s = GalaxyIO.Server(sys.argv, "multiply", default_port = 2900)
    s.AddDispatchFunction("multiply", Multiply,
			  [[[":int", Galaxy.GAL_INT, Galaxy.GAL_KEY_ALWAYS]],
			   Galaxy.GAL_OTHER_KEYS_NEVER, 
			   Galaxy.GAL_REPLY_PROVIDED,
			   [[":int", Galaxy.GAL_INT, Galaxy.GAL_KEY_ALWAYS]],
			   Galaxy.GAL_OTHER_KEYS_NEVER])
    s.AddDispatchFunction("reinitialize", Welcome)
    s.RunServer()

main()
