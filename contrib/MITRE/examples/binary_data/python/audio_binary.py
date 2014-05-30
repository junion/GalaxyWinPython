# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import os, sys, string

sys.path.insert(0, os.path.join(os.environ["GC_HOME"],
				"contrib", "MITRE", "templates"))

import GC_py_init

import Galaxy, GalaxyIO

def Welcome(conn, dict):
    file = dict[":audiofile"]
    if not file:
	sys.stderr.write("No filename provided\n")
	return dict
    fp = open(file, "r")
    a = Galaxy.BinaryObject(Galaxy.GAL_BINARY)
    # Get length. Position 0 relative to file end (2).
    fp.seek(0, 2)
    l = fp.tell()
    # Back to the beginning
    fp.seek(0, 0)
    a.fromfile(fp, l)
    fp.close()
    conn.WriteFrame(Galaxy.Frame("main", Galaxy.GAL_CLAUSE, {":audio_data": a}))
    return None

def ReceiveAudio(conn, dict):
    data = dict[":audio_data"]
    try:
	fp = open("/dev/audio", "w")
	data.tofile(fp)
	fp.close()
    except IOError:
	pass
    return None

def main():
    s = GalaxyIO.Server(sys.argv, "audio_binary",
			default_port = 17800)
    s.AddDispatchFunction("receive_audio", ReceiveAudio)
    s.AddDispatchFunction("reinitialize", Welcome)
    s.RunServer()

main()
