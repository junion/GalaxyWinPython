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

def _EnvNotify(env, msg):
    env.WriteFrame(Galaxy.Frame(name = "notify", contents = {":notification": msg})) 

def _PlayAudio(audio_data, ostream):
    try:
	fp = open("/dev/audio", "w")
	audio_data.tofile(fp)
	fp.close()
    except IOError:
	ostream.warn("Couldn't open /dev/audio")

class AudioInBroker(GalaxyIO.BrokerDataIn):
    def __init__(self, *args, **kw):
	self.audio_data = Galaxy.BinaryObject(Galaxy.GAL_BINARY)
	apply(GalaxyIO.BrokerDataIn.__init__, (self,) + args, kw)
    def DataDoneCallback(self):
	_PlayAudio(self.audio_data, self.ostream)
	self.Notify("Audio received.")
	GalaxyIO.BrokerDataIn.DataDoneCallback(self)
    def AbortCallback(self):
	self.Notify("Audio aborted.")
	GalaxyIO.BrokerDataIn.AbortCallback(self)

class CommAudioInBroker(AudioInBroker):
    def Notify(self, msg):
	self.conn.WriteFrame(Galaxy.Frame(name = "notify", contents = {":notification": msg}))
    def HandleBinary(self, obj):
	self.audio_data = self.audio_data + obj
	
class EnvAudioInBroker(AudioInBroker):
    def Notify(self, msg):
	_EnvNotify(self.env, msg)
    def EnvHandleBinary(self, env, obj):
	self.audio_data = self.audio_data + obj

# This class should support the callbacks HandleObject(),
# DataDoneCallback(), AbortCallback(). 

class AudioProxyInStream(GalaxyIO.BrokerProxyInStream):
    def HandleObject(self, obj):
	_PlayAudio(obj, self.ostream)
    def DataDoneCallback(self):
	_EnvNotify(self.env, "Audio received.")
    def AbortCallback(self):
	_EnvNotify(self.env, "Audio aborted.")

# The broker method is one of the strings "original_env",
# "original_comm", "proxy_obj", "proxy_stream", "proxy_original".

def ReceiveAudio(env, dict):
    if env.conn.broker_method in [None, "original_env"]:
	host = dict[":binary_host"]
	port = dict[":binary_port"]
	b = EnvAudioInBroker(env, host, port, dict)
    elif env.conn.broker_method == "original_comm":
	host = dict[":binary_host"]
	port = dict[":binary_port"]
	b = CommAudioInBroker(env.conn, host, port, dict)
	b.conn = env.conn
    elif env.conn.broker_method == "proxy_obj":
	bp = dict[":binary_proxy"]
	obj = bp.UnproxifyObject(env)
	if Galaxy.GetObjectType(obj) == Galaxy.GAL_BINARY:
	    _PlayAudio(obj, env.conn.ostream)
	    _EnvNotify(env, "Audio received.")
	else:
	    _EnvNotify(env, "Audio aborted.")
    elif env.conn.broker_method == "proxy_stream":
	bp = dict[":binary_proxy"]
	bp.Unproxify(env, AudioProxyInStream, immediate = 0)	
    elif env.conn.broker_method == "proxy_original":	
	bp = dict[":binary_proxy"]
	b = EnvAudioInBroker(env, proxy = bp)
    return None

def Welcome(env, dict):
    try:
	env.conn.broker_method = dict[":broker_method"]
    except KeyError:
	env.conn.broker_method = None

def main():
    s = GalaxyIO.Server(sys.argv, "testaudio_receive", default_port = 12346)
    s.AddDispatchFunction("receive_audio", ReceiveAudio)
    s.AddDispatchFunction("reinitialize", Welcome)
    s.RunServer()

main()
