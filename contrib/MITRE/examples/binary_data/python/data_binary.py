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

class TestObj:
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
    def encode(self):	
	return Galaxy.BinaryObject(Galaxy.GAL_BINARY, map(ord, pickle.dumps(self)))
    def Print(self):
	print ("First is %d, last is %d, msg is `%s'" % \
	      (self.first, self.last, self.msg))

def Welcome(conn, dict):
    conn.WriteFrame(Galaxy.Frame("main", Galaxy.GAL_CLAUSE,
				 {":binary_data": TestObj(dict = dict).encode()}))
    return None

def ReceiveBinary(conn, dict):
    o = pickle.loads(string.joinfields(map(chr, dict[":binary_data"]), ""))
    o.Print()    
    return {":test_first": o.first,
	    ":test_last": o.last,
	    ":test_msg": o.msg}

def main():
    s = GalaxyIO.Server(sys.argv, "data_binary",
			default_port = 17900)
    s.AddDispatchFunction("receive_binary", ReceiveBinary)
    s.AddDispatchFunction("reinitialize", Welcome)
    s.RunServer()

main()
