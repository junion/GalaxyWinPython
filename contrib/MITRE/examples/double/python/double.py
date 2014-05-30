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

InitialIncrement = 1

import exceptions

DoubleError = "DoubleError"

def Welcome(env, dict):
    try:
	prog_name = dict[":program"]
    except: prog_name = "main"
    env.WriteFrame(Galaxy.Frame(prog_name, Galaxy.GAL_CLAUSE,
				 {":int": InitialIncrement}))
    return None

def Double(env, dict):
    try:
	prog_name = dict[":program"]
    except: prog_name = "main"
    try:
        res = dict[":int"] * 2
        if (type(dict[":int"]) is type(0)) and \
           (type(res) is type(0L)):
            # Overflow.
            raise DoubleError, "double would overflow MAXINT"
        else:
            env.WriteFrame(Galaxy.Frame(prog_name, Galaxy.GAL_CLAUSE,
                                        {":int": res}))
    except exceptions.OverflowError:
        raise DoubleError, "double would overflow MAXINT"
    return None

# The programming model for the return function is a little strange,
# because you can't raise an error.

def ComplexDouble(env, dict):
    try:
	resdict = env.DispatchFrame(Galaxy.Frame("multiply",
						  Galaxy.GAL_CLAUSE,
						  {":int": dict[":int"]}))
	env.calling_dict = dict
	__ContinueComplexDouble(env, resdict, GalaxyIO.GAL_REPLY_MSG_TYPE)
    except GalaxyIO.DispatchError:
	pass
    return None

def __ContinueComplexDouble(env, resdict, msg_type):
    if msg_type == GalaxyIO.GAL_REPLY_MSG_TYPE:			      
	try:
	    prog_name = env.calling_dict[":program"]
	except: prog_name = "main"
        try:
            res = resdict[":int"] * 2
            if (type(resdict[":int"]) is type(0)) and \
               (type(res) is type(0L)):
                # Overflow.
                raise DoubleError, "double would overflow MAXINT"
            else:
                env.WriteFrame(Galaxy.Frame(prog_name, Galaxy.GAL_CLAUSE,
                                            {":int": res}))
        except exceptions.OverflowError:
            raise DoubleError, "double would overflow MAXINT"
    
def ContinuationComplexDouble(env, dict):
    env.calling_dict = dict
    env.DispatchFrameWithContinuation(Galaxy.Frame("multiply",
						    Galaxy.GAL_CLAUSE,
						    {":int": dict[":int"]}),
				       __ContinueComplexDouble)
    return None

def EchoFrame(env, dict):
    return dict

# oas in C is -increment i.

OAS = [("-increment i", "initial increment")]

# Write a wrapper for the usage check.

class DoubleServer(GalaxyIO.Server):
    def CheckUsage(self, oas_list, args):
        global InitialIncrement
        data, out_args = GalaxyIO.Server.CheckUsage(self, OAS + oas_list, args)
        if data.has_key("-increment"):
            InitialIncrement = data["-increment"][0]
            del data["-increment"]
        return data, out_args

def main():
    s = DoubleServer(sys.argv, "double",
                     default_port = 2800)
    s.AddDispatchFunction("twice", Double,
			  [[[":int", Galaxy.GAL_INT, Galaxy.GAL_KEY_ALWAYS]],
			   Galaxy.GAL_OTHER_KEYS_NEVER,
			   Galaxy.GAL_REPLY_NONE, [],
			   Galaxy.GAL_OTHER_KEYS_NEVER])
    s.AddDispatchFunction("complex_twice", ComplexDouble,
			  [[[":int", Galaxy.GAL_INT, Galaxy.GAL_KEY_ALWAYS]],
			   Galaxy.GAL_OTHER_KEYS_NEVER,
			   Galaxy.GAL_REPLY_NONE, [],
			   Galaxy.GAL_OTHER_KEYS_NEVER])
    s.AddDispatchFunction("continuation_complex_twice", ContinuationComplexDouble,
			  [[[":int", Galaxy.GAL_INT, Galaxy.GAL_KEY_ALWAYS]],
			   Galaxy.GAL_OTHER_KEYS_NEVER,
			   Galaxy.GAL_REPLY_NONE, [],
			   Galaxy.GAL_OTHER_KEYS_NEVER])
    s.AddDispatchFunction("reinitialize", Welcome,
			  [[], Galaxy.GAL_OTHER_KEYS_NEVER,
			   Galaxy.GAL_REPLY_NONE, [],
			   Galaxy.GAL_OTHER_KEYS_NEVER])
    s.AddDispatchFunction("echo_frame", EchoFrame)
    s.RunServer()

main()
