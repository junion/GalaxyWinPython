# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import os, sys, string, pickle

sys.path.insert(0, os.path.join(os.environ["GC_HOME"],
				"contrib", "MITRE", "templates"))

import GC_py_init

import Galaxy, GalaxyIO, MGalaxy

class Test1Obj:
    def __init__(self, dict = None):
	self.first = 0
	self.last = 0
	self.msg = ""
	if dict:
	    self.init_from_dict(dict)
	    
    def init_from_dict(self, dict):
	self.first = dict[":test_first"]
	self.last = dict[":test_last"]
	self.msg = dict[":test_msg"]
	
    def Print(self):
	print ("First is %d, last is %d, msg is `%s'" % \
	      (self.first, self.last, self.msg))

    def ReturnVal(self):
	return {":test_first": self.first,
		":test_last": self.last,
		":test_msg": self.msg}

class Test2Obj:
    def __init__(self, dict = None):
	self.key = 0
	self.val = ""
	if dict:
	    self.init_from_dict(dict)
	    
    def init_from_dict(self, dict):
	self.key = dict[":test_first"]
	self.val = dict[":test_msg"]
	
    def Print(self):
	print ("Key is %d, val is `%s'" % \
	      (self.key, self.val))

    def ReturnVal(self):
	return {":test_first": self.key,
		":test_msg": self.val}

HowMany = 1

def Welcome(conn, dict):
    global HowMany
    if (HowMany % 2) == 0:
	cl = Test1Obj
    else:
	cl = Test2Obj
    HowMany = HowMany + 1
    conn.WriteFrame(Galaxy.Frame("main", Galaxy.GAL_CLAUSE,
				 {":binary_data":
				  conn.server.mapper.OpaqueObject(cl(dict = dict), cl)}))

def ReceiveBinary(conn, dict):
    o = conn.server.mapper.GetOpaque(dict, ":binary_data")
    o.Print()
    return o.ReturnVal()

def main():
    s = GalaxyIO.Server(sys.argv, "data_binary",
			default_port = 17900)
    # Grab a mapper, use the default encode/decode.
    s.mapper = MGalaxy.OpaqueMapper()
    s.mapper.AddBinaryDataType(Test1Obj, MGalaxy.OpaqueClass)
    s.mapper.AddBinaryDataType(Test2Obj, MGalaxy.OpaqueClass)
    s.AddDispatchFunction("receive_binary", ReceiveBinary)
    s.AddDispatchFunction("reinitialize", Welcome)
    s.RunServer()

main()
