# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import os, time, string, re, pickle

# These are mine.

import io_device, url_directory

from GC_log_formatting import FormattableTag, DefaultFormatter, \
     FormattableXMLDocument

# I've moved all the directory handling into GC_log_directory.py.
# This file now deals exclusively with the MIT log and getting it
# into XML. I've moved the classes from GC_log_classes in here
# as well.

# NOTE: I HAVE NOT YET HANDLED HUB OUTPUT.

LogfileError = "LogfileError"

class MIT_Logfile:

    def __init__(self, logfile, io = None, parser = None,
		 default_user_version = None):
        if parser is None:
            self.parser_class = LogfileParser
        else:
            self.parser_class = parser
	# This will now be a Path object.
        self.logfile = logfile
        if not self.logfile:
	    raise LogfileError, "must specify logfile"
	self.host_list = []
        if io:
            self.io_device = io
        else:
            self.io_device = io_device.DefaultIODevice()
        
	if default_user_version is not None:
	    self.user_version = default_user_version
	else:
	    self.user_version = None
	self.turns = []
        self.session_id = None
	self.start_time = None
	self.end_time = None
	
    def Xmlize(self):
	self.io_device.ReportStatus("Converting to XML: " + self.logfile.PathString())
	log_properties = {}	
	if self.user_version:
	    log_properties["logfile_version"] = self.user_version
	session_properties = {}
	if self.session_id is not None:
	    session_properties["id"] = "%s" % self.session_id
	## compatibility with pre-2.0 logs: add an empty id attribute
	else:
	    session_properties["id"] = "None"
	if self.start_time is not None:
	    session_properties["stime"] = "%f" % self.start_time
	if self.end_time is not None:
	    session_properties["etime"] = "%f" % self.end_time
	children = []
	for t in self.turns:
	    child = t.Xmlize()
	    if child is not None:
		children.append(child)
	log_child = FormattableTag("GC_SESSION", session_properties,
				   children)	
	top_tag = FormattableTag("GC_LOG", log_properties, [log_child])
	d = FormattableXMLDocument()
	d.AddChild(top_tag)
	self.io_device.ReportStatus("...converted.")
    	return d

    def LoadDatapoints(self):
	self.io_device.ReportStatus("Reading raw Hub log: " + self.logfile.PathString())
	lines = self.logfile.Readlines()
	# ConstructDatapoints no longer adds the log
	# object. This happens after the uncacheing/parsing.
	self.ConstructDatapoints(lines)
	self.ComputeTimes()	
	self.io_device.ReportStatus("...read.")
	
    def ComputeTimes(self):
	self.start_time = None
	self.end_time = None
	for t in self.turns:
	    t.start_time = None
	    t.end_time = None
	    for utt_dp in t.datapoints:
		if utt_dp.start_time:
		    if t.start_time == None:
			t.start_time = utt_dp.start_time
		    elif utt_dp.start_time < t.start_time:
			t.start_time = utt_dp.start_time
		if utt_dp.end_time:
		    if t.end_time == None:
			t.end_time = utt_dp.end_time
		    elif utt_dp.end_time > t.end_time:
			t.end_time = utt_dp.end_time
	    if t.start_time:
		if self.start_time == None:
		    self.start_time = t.start_time
		elif t.start_time < self.start_time:
		    self.start_time = t.start_time
	    if t.end_time:
		if self.end_time == None:
		    self.end_time = t.end_time
		elif t.end_time > self.end_time:
		    self.end_time = t.end_time

    def ConstructDatapoints(self, lines):
        p = self.parser_class()
        self.turns = p.Parse(self, lines)
    
    def RetrieveServer(self, name, host, port):
	for elt in self.host_list:
	    if name == elt.name and \
	       host == elt.host and \
	       port == elt.port:
		return elt
	else:
	    new_server = Server(name, host, port)
	    self.host_list.append(new_server)
	    return new_server

    def _PrepareForDeletion(self):
	# I need to get rid of all the cycles.
	for t in self.turns:
	    for dp in t.datapoints:
		dp._PrepareForDeletion()		    

# Classes built during the parse. Previously in GC_log_classes.py.

import xml_tree, Galaxy, cgi, types

class Server:
    def __init__(self, name, host, port):
	self.name = name
	self.host = host
	self.port = port
    def Format(self):
	return "%s(%s:%d)" % (self.name, self.host, self.port)

# Major reorganization for 2.0 and the logfile standard.
# (1) Reading the datapoints will be a property of the
# parser, not the logfile object. Done.
# (2) Formatting will be the job of the datapoint. Done.
# (3) The values can now be digested with
# the equivalent of Gal_ReadObject().
# (4) "Interesting" elements will be subclasses of Datapoint,
# and the travel log summary will handle that.
# (5) Three phases instead of two: initial digestion,
# annotation and visualization.
# (6) Early translation into XML, and all formatting moved
# to XML. Addition of XMLize method for everything.

# Datapoints can be associated with servers and operations, or not.
# The ones which are not are key timestamps.

# This is going to be a little convoluted, because I really need
# multiple inheritance. I need to know, for instance, how to format
# a header for a speech datapoint when I'm rendering HTML. So I
# have this formatter object which is essentially an "interface",
# which implements "other" behavior.

class DataPoint:
    def __init__(self):
	pass
    def __repr__(self):
	return self._desc_()
    def _desc_(self):
	return "<Datapoint at %s>" % hex(hash(self))
    # SAM 12/10/99: Moving John Aberdeen's code into the core, so
    # that we can start from XML whenever we do anything.
    def Xmlize(self):
	return None

class OperationDataPoint(DataPoint):
    def __init__(self, start_time, turn_number, tidx):
	DataPoint.__init__(self)
	self.start_time = start_time
	self.end_time = None
	self.server = None
	self.operation = ""
	self.in_dict = {}
	self.out_dict = {}
	self.closed = 0
	self.turn_number = turn_number
	self.tidx = tidx
	self.asynchronous = 0
	self.reply_status = None
    def AddData(self, strKey, strContent, dtype = "unknown"):
        if (self.end_time is not None) and (not self.asynchronous):
            self.out_dict[strKey] = DataDataPoint(self, strKey,
                                                  strContent, dtype)
        else:
            self.in_dict[strKey] = DataDataPoint(self, strKey,
                                                 strContent, dtype)
    def Close(self):
	self.closed = 1
	if self.end_time is None:
	    self.asynchronous = 1
	    self.end_time = self.start_time
    def _desc_(self):
	# Let's see if we can do this.
	if self.asynchronous:
	    return "<Operation %s:%s at %f>" % \
		   (self.server.name, self.operation, self.start_time)
	else:
	    return "<Operation %s:%s from %f to %f>" % \
		   (self.server.name, self.operation,
		    self.start_time, self.end_time)

    # These next methods xmlize an operation data point.

    def Xmlize(self):
	tag_name = "GC_OPERATION"
	properties = {}
	if self.operation != "":
	    properties["name"] = self.operation
	## compatibility with pre-2.0 logs: add empty name attribute
	else:
	    properties["name"] = "None"
	if self.server is not None:
	    properties["server"] = self.server.name
	    host = self.server.host
	    if host[0] == "<" and host[-1] == ">":
		host = host[1:-1]
	    properties["location"] = ("%s:%d" % (host, self.server.port))
	## compatibility with pre-2.0 logs: add empty server and
	## location attributes
	else:
	    properties["server"] = "None"
	    properties["location"] = "None"
	if self.turn_number is not None:
	    properties["turnid"] = ("%d" % self.turn_number)
	if self.start_time is not None:
	    properties["stime"] = "%f" % self.start_time
	if self.end_time is not None:
	    properties["etime"] = "%f" % self.end_time
	if self.tidx is not None:
	    properties["tidx"] = "%d" % self.tidx
	if self.asynchronous:
	    reply_string = "asynchronous"
	elif self.reply_status == "reply":
	    reply_string = "normal"
	elif self.reply_status == "error":
	    reply_string = "error"
	elif self.reply_status == "destroy":
	    reply_string = "destroy"
	else:
	    reply_string = ""
	if reply_string:
	    properties["reply_status"] = reply_string
	children = []
	if self.in_dict or self.out_dict:
	    if self.in_dict:
		children = self.XmlizeDict(self.in_dict)
		for child in children:
		    child.properties["direction"] = "in"
	    if self.out_dict:
		other_children = self.XmlizeDict(self.out_dict)
		for child in other_children:
		    child.properties["direction"] = "out"
		children = children + other_children
	return FormattableTag(tag_name, properties, children)

    def XmlizeDict(self, theDict):
	children = []
	for key, value in theDict.items():
	    if key == ":tidx":
		pass ## it is already in the attributes
	    else:
		new_child = self.XmlizeDataDataPoint(value)
		if new_child is not None: children.append(new_child)
	return children

    def XmlizeDataDataPoint(self, ddp):
	if ddp.dtype == "frame":
	    return self.XmlizeFrame(ddp.key, ddp.data)
	elif ddp.dtype == "list":
	    return self.XmlizeList(ddp.key, ddp.data)
	elif ddp.dtype == "string":
	    return self.XmlizeData(ddp.key, ddp.data,
			    ddp.dtype)
	elif ddp.dtype == "integer":
	    return self.XmlizeData(ddp.key, ddp.data, ddp.dtype)
	elif ddp.dtype == "float":
	    return self.XmlizeData(ddp.key, ddp.data, ddp.dtype)
	elif ddp.dtype == "unknown":
	    return self.XmlizeData(ddp.key, ddp.data, ddp.dtype)
	else:
	    return None

    def FrameDict2Data(self, dict):
	children = []
	for key, value in dict.items():
	    new_child = None
	    if type(value) is types.InstanceType and \
		 value.__class__ is Galaxy.Frame:
		new_child = self.XmlizeFrameObject(key, value)
	    elif type(value) is type([]):
		new_child = self.XmlizeListObject(key, value)
	    else:
		dtype = self.GetObjectType(Galaxy.GetObjectType(value))
		if dtype == "string":		    
		    new_child = self.XmlizeData(key, value, dtype)
		else:
		    new_child = self.XmlizeData(key, Galaxy.OPr(value), dtype)
	    if new_child is not None:
		children.append(new_child)
	return children

    def XmlizeFrame(self, key, value):
	frameDict = Galaxy.Frame(str=value)
	return self.XmlizeFrameObject(key, frameDict)

    def XmlizeFrameObject(self, key, frameDict):
	if frameDict is not None:
	    frameType = self.GetFrameType(frameDict.type)
	    children = self.FrameDict2Data(frameDict)
	    for predFrame in frameDict.preds:
		new_pred = self.XmlizeFrameObject(":pred", predFrame)
		children.append(new_pred)
	    frame_child = FormattableTag("GC_FRAME",
					 {"name": frameDict.name,
					  "frame_type": frameType},
					 children)
	    return FormattableTag("GC_DATA",
				  {"key": key,
				   "dtype": "frame"}, [frame_child])
	else:
	    return None

    def XmlizeList(self, key, value):
	theList, ignore = Galaxy._read_irp_value(value)
	return self.XmlizeListObject(key, theList)

    def XmlizeListObject(self, key, theList):
	if theList is not None:
	    itemNum = 0
	    children = []
	    for listItem in theList:
		new_child = None
		dtype = self.GetObjectType(Galaxy.GetObjectType(listItem))
		if dtype == "frame":
		    new_child = self.XmlizeFrameObject(key + '[%d]' % itemNum,
						       listItem)
		elif dtype == "string":
		    new_child = self.XmlizeData(key + '[%d]' % itemNum, 
						listItem, dtype)
		elif dtype == "integer":
		    new_child = self.XmlizeData(key + '[%d]' % itemNum,
						`listItem`, dtype)
		elif dtype == "float":
		    new_child = self.XmlizeData(key + '[%d]' % itemNum,
						`listItem`, dtype)
		elif dtype == "list":
		    new_child = self.XmlizeListObject(key + '[%d]' % itemNum,
						      listItem)
		elif dtype == "binary":
		    pass
		else:
		    new_child = self.XmlizeData(key + '[%d]' % itemNum, 
						listItem, dtype)
		if new_child is not None:
		    children.append(new_child)
		itemNum = itemNum + 1
	    list_child = FormattableTag("GC_LIST", {"name": key},
					children)
	    return FormattableTag("GC_DATA", {"key": key,
					      "dtype": "list"},
				  [list_child])
	else:
	    return None

    def XmlizeData(self, key, value, dtype):
	children = []
	if string.strip(value):
	    children.append(xml_tree.Data(string.strip(value)))
	elif value:
	    children.append(xml_tree.Data(""))
	return FormattableTag("GC_DATA", {"key": key,
					  "dtype": dtype},
			      children)

    def GetFrameType(self, type):
	if type == Galaxy.GAL_TOPIC:
	    return "topic"
	elif type == Galaxy.GAL_CLAUSE:
	    return "clause"
	elif type == Galaxy.GAL_PRED:
	    return "pred"
	else:
	    return type

    def GetObjectType(self, type):
	if type == Galaxy.GAL_FRAME:
	    return "frame"
	elif type == Galaxy.GAL_STRING:
	    return "string"
	elif type == Galaxy.GAL_INT:
	    return "integer"
	elif type == Galaxy.GAL_FLOAT:
	    return "float"
	elif type == Galaxy.GAL_LIST:
	    return "list"
	elif type == Galaxy.GAL_BINARY:
	    return "binary"
	else:
	    return type

    def _PrepareForDeletion(self):
	# Remove dp backpoints from any of the in_dict and
	# out_dict elements. Better yet, remove
	# the in_dict and out_dict completely.
	self.in_dict = {}
	self.out_dict = {}


# This class is a type of datapoint as well. It's the value
# of any dictionary key. We need it to be able to mark it as
# interesting and subclass it.

class DataDataPoint(DataPoint):
    def __init__(self, dp, key, data, dtype = "unknown"):
	DataPoint.__init__(self)
        self.dp = dp
	self.data = data
	self.key = key
        self.dtype = dtype
    def _desc_(self):
	return "<%s %s>" % (self.key, self.data)

# In version 0, keys are sometimes just floating on the ether.
# This should be treated as a timestamped event, basically.

class KeyDataPoint(OperationDataPoint):
    def _desc_(self):
	return "<Key datapoint %s at %f>" % \
	       (self.in_dict.keys()[0], self.start_time)

# This is for internal Hub events.

# These two are new for 1.0, so they always have tidxes.

class EventDataPoint(OperationDataPoint):

    def _desc_(self):
	return "<Hub event %s at %f>" % \
	       (self.event, self.start_time)

    def Xmlize(self):
	tag_name = "GC_EVENT"
	properties = {"etype": self.event}
	if self.server is not None:
	    properties["server"] = self.server.name
	    host = self.server.host
	    if host[0] == "<" and host[-1] == ">":
		host = host[1:-1]
	    properties["location"] = ("%s:%d" % (host, self.server.port))
	if self.turn_number is not None:
	    properties["turnid"] = ("%d" % self.turn_number)
	if self.start_time is not None:
	    properties["time"] = "%f" % self.start_time
	if self.operation is not None:
	    properties["name"] = self.operation
	if self.tidx is not None:
	    properties["tidx"] = "%d" % self.tidx
	children = []
	if self.in_dict or self.out_dict:
	    if self.in_dict:
		children = self.XmlizeDict(self.in_dict)
	    if self.out_dict:
		children = children + self.XmlizeDict(self.out_dict)
	return FormattableTag(tag_name, properties, children)

# This is for new messages in and replies out of the Hub.

class MessageDataPoint(OperationDataPoint):

    def _desc_(self):
	return "<Message %s %s:%s at %f>" % \
	       (self.direction, self.server.name,
		self.operation, self.start_time)

    def Xmlize(self):
	tag_name = "GC_MESSAGE"
	properties = {}
	if self.direction is not None:
	    if self.direction == "read":
		properties["direction"] = "server_to_hub"
	    else:
		properties["direction"] = "hub_to_server"
		if self.direction == "reply":
		    properties["reply_status"] = "normal"
		else:
		    properties["reply_status"] = self.direction
	if self.server is not None:
	    properties["server"] = self.server.name
	    host = self.server.host
	    if host[0] == "<" and host[-1] == ">":
		host = host[1:-1]
	    properties["location"] = ("%s:%d" % (host, self.server.port))
	if self.turn_number is not None:
	    properties["turnid"] = ("%d" % self.turn_number)
	if self.operation is not None:
	    properties["name"] = self.operation
	if self.start_time is not None:
	    properties["time"] = "%f" % self.start_time
	if self.tidx is not None:
	    properties["tidx"] = "%d" % self.tidx
	children = []
	if self.in_dict or self.out_dict:
	    if self.in_dict:
		children = self.XmlizeDict(self.in_dict)
	    if self.out_dict:
		children = children + self.XmlizeDict(self.out_dict)
	return FormattableTag(tag_name, properties, children)

class Utterance(DataPoint):
    def __init__(self, utt_id):
	DataPoint.__init__(self)
	self.utterance_id = utt_id
	self.datapoints = []
	self.start_time = None
	self.end_time = None
    def _desc_(self):
	return "<Utterance %d>" % self.utterance_id

    def Xmlize(self):
	tag = "GC_TURN"
	properties = {"turnid": "%s" % self.utterance_id}
	if self.start_time is not None:
	    properties["stime"] = "%f" % self.start_time
	if self.end_time is not None:
	    properties["etime"] = "%f" % self.end_time
	children = []
	for dp in self.datapoints:
	    child = dp.Xmlize()
	    if child is not None:
		children.append(child)
	return FormattableTag(tag, properties, children)

# The actual parsing.

class DPCache:
    # This class will serve to handle the matching, etc., in the
    # MIT logfile. I think I'm going to need it.

    def __init__(self):
	self.InitCache()

    def InitCache(self):
	self.active_datapoint = None
	self.open_datapoints = []
	# self.cur_datapoint is (server, operation)
        self.cur_datapoint = None
	self.turn_dict = {}

    def Flush(self):
        for elt in self.open_datapoints[:]:
	    self.CloseDatapoint(elt)
	keys = self.turn_dict.keys()
	keys.sort()
	return map(lambda x, d = self.turn_dict: d[x], keys)
    
    def SetActiveDatapoint(self, dp):
        self.active_datapoint = dp

    def UpdateCurrentID(self, log, name, host, port, operation):
	server = log.RetrieveServer(name, host, port)
	self.cur_datapoint = (server, operation)

    def FindFirstNonClosedDatapoint(self, log, name, host, port,
				    operation, tidx):
	server = log.RetrieveServer(name, host, port)
	return self.FindCurrentDatapointByServer(server, operation, tidx)

    def FindCurrentDatapointByServer(self, server, operation, tidx):
	# In version 0, there was no tidx, so we had to do this by
	# matching operations. In version 1, there is a tidx, which
	# is excellent, because we also have the problem of error
	# and destroy replies not having the same operation name.
	# However, we have to make sure that we don't reject matches
	# where no tidx is passed in here but there is one in the
	# log, because in version 0 we treated key tidx and :tidx
	# as special.

	# SAM 12/7/99: We have to be careful not to pair up
	# things which aren't special reply names, but we still ought
	# to keep the matching where possible.
	
	for elt in self.open_datapoints:
	    if elt.server == server and \
	       (((tidx is not None) and (elt.tidx == tidx) and
		 ((operation in ["system_error", "destroy"]) or \
		  (elt.operation == operation))) or \
		((tidx is None) and elt.operation == operation)):
		return elt
	else:
	    return None

    def FindCurrentDatapoint(self):
        # DON'T TAKE THIS FROM THE OPEN DATAPOINTS!
        # The open datapoints list closes a datapoint
        # when it encounters a second read, but the keys
        # have yet to be set.
	# return self.open_datapoints[-1]
        # But the last datapoint to be created isn't the one
        # I want either. I need a notion of "active"
        # datapoint.
        # return self.datapoints[-1]
        return self.active_datapoint
	
    def NewDatapoint(self, time, turn_number, tidx):
	new_dp = OperationDataPoint(time, turn_number, tidx)
	if not self.turn_dict.has_key(turn_number):
	    self.NewUtterance(turn_number)
	self.turn_dict[turn_number].datapoints.append(new_dp)
	self.open_datapoints.append(new_dp)
	return new_dp

    def NewUtterance(self, utterance_id):
	self.turn_dict[utterance_id] = Utterance(utterance_id)

    def AddCurrentServerInfo(self, dp):	
	dp.server, dp.operation = self.cur_datapoint

    def CloseDatapoint(self, dp):
	dp.Close()
	self.open_datapoints.remove(dp)

    def EstablishNewCurrentDatapoint(self, time,
				     turn_number, tidx):
	dp = self.NewDatapoint(time, turn_number, tidx)
	self.AddCurrentServerInfo(dp)
	self.SetActiveDatapoint(dp)
	return dp

class LogfileParser:

    def __init__(self):
	self.cache = DPCache()

    def Parse(self, logfile_obj, lines):
	
	self.fcontent = lines
	# The first nonblank line
	# needs to be the version. If it's not, assume
	# version 0.
	self.cache.InitCache()
	i = 0
	type = "NOTYPE"
	while i < len(self.fcontent):
	    if string.strip(self.fcontent[i]):
		spl = string.split(self.fcontent[i])
		if spl[0] == "LOGFILE_FORMAT_VERSION:":
		    self.interpreter = ParserDict[spl[1]](self)
		    i = i + 1
		else:
		    self.interpreter = Version0Parser(self)
		break
	    else: i = i + 1
	while i < len(self.fcontent):
	    line = self.fcontent[i]
	    type = self.interpreter.parseLine(line, type)
	    if hasattr(self.interpreter, type):
		i = getattr(self.interpreter, type)(logfile_obj, i)
	    else:
		if type == "NOTYPE":
		    raise LogfileError, ("can't parse line %d: %s" % (i, line))
	    i = i + 1
	return self.cache.Flush()
            
    def iterator(self, index, endType, type, logfile_obj):
        j = index + 1
        dataContent = ""
        while (self.interpreter.parseLine(self.fcontent[j], type) != endType):
            dataContent = dataContent + self.fcontent[j]
            j = j + 1
        return j, dataContent

    def AddDataToDatapoint(self, strKey, strContent, dtype = "unknown"):
        dp = self.cache.FindCurrentDatapoint()
	if dp is None:
	    return
        dp.AddData(strKey, strContent, dtype)

    def AddUtterance(self, utterance_id):
	self.cache.NewUtterance(utterance_id)

    def AddSessionID(self, logfile_obj, session_id):
	logfile_obj.session_id = session_id

    def AddLogVersion(self, logfile_obj, version):
	logfile_obj.user_version = version
	
    def AddNewOperationTimestamp(self, logfile_obj, turn_number,
				 server_name, server_host,
				 port_number, operation, tidx, time):
	
        self.cache.UpdateCurrentID(logfile_obj, server_name, server_host,
				   port_number, operation)
	self.cache.EstablishNewCurrentDatapoint(time,
						turn_number, tidx)

    def AddOperationReplyTimestamp(self, logfile_obj, turn_number,
				   server_name, server_host,
				   port_number, operation, tidx,
				   reply_status, time):

	self.cache.UpdateCurrentID(logfile_obj, server_name, server_host,
				   port_number, operation)
	# Find the first non-closed datapoint of this type. We want	
	# to discard this one because messages are dispatched before
	# the responses to the previous message is logged. So you
	# may have two sends in a row, and the following receive
	# is the response to the first one. This doesn't look
	# like it can happen in GalaxyCommunicator 2.0, but the old
	# logs still have this problem.

	# We search from
	# the beginning of the list because there are never
	# going to be more than two of the given type. If the
	# response has been found, close it and remove it from
	# the list; otherwise, leave it, and it will be
	# found again by the second call. In GC 2.0, we'll be
	# able to match on token indices, so this will be
	# a superfluous test.
	
	idp = self.cache.FindFirstNonClosedDatapoint(logfile_obj,
						     server_name,
						     server_host,
						     port_number,
						     operation, tidx)
	if idp is None:
	    # There was no open datapoint of this form. We had
	    # better create one. This addresses the issue of
	    # logs from 2.0 where only LOG_OUT: was specified, not
	    # TIMESTAMP:.
	    self.cache.EstablishNewCurrentDatapoint(time,
						    turn_number, tidx)
	elif idp.end_time is not None:
	    # Close it.
	    self.cache.CloseDatapoint(idp)
	# Then find the next non-closed one, or the same one
	# if it hadn't been closed.
	dp = self.cache.FindFirstNonClosedDatapoint(logfile_obj,
						    server_name,
						    server_host,
						    port_number,
						    operation, tidx)
	# There STILL may not be a datapoint.
	if dp is None:
	    dp = self.cache.EstablishNewCurrentDatapoint(time,
							 turn_number, tidx)
	dp.end_time = time
	dp.reply_status = reply_status
	self.cache.SetActiveDatapoint(dp)

    def AddMessageTimestamp(self, logfile_obj, turn_number,
			    server_name, server_host,
			    port_number, operation, tidx, direction, time):
	self.cache.UpdateCurrentID(logfile_obj, server_name, server_host,
				   port_number, operation)
	dp = self.cache.EstablishNewCurrentDatapoint(time, turn_number, tidx)
	dp.__class__ = MessageDataPoint
	dp.direction = direction
	self.cache.CloseDatapoint(dp)
	
    def AddKeyTimestamp(self, turn_number, time, key, tidx):
        dp = self.cache.NewDatapoint(time, turn_number, tidx)
	dp.__class__ = KeyDataPoint
        dp.AddData(key, "", "unknown")
        self.cache.CloseDatapoint(dp)

    def AddEventTimestamp(self, logfile_obj, time, turn_number,
                          server_name, server_host, port_number,
                          operation, action, tidx):
        self.cache.UpdateCurrentID(logfile_obj, server_name, server_host,
                                   port_number, operation)
	dp = self.cache.EstablishNewCurrentDatapoint(time, turn_number, tidx)
	dp.__class__ = EventDataPoint
	dp.event = action
	# Close the datapoint, but it will still be current, and
	# it will be able to get keys.
	self.cache.CloseDatapoint(dp)



CantParseError = "CantParseError"

class BaseParser:
    def interpFrame(self, logfile_obj, i):
	raise CantParseError, "frame"
    def interpValue(self, logfile_obj, i):
	raise CantParseError, "value"
    def interpBeginUtterance(self, logfile_obj, i):
	raise CantParseError, "begin utterance"
    def interpEndUtterance(self, logfile_obj, i):
	raise CantParseError, "end utterance"
    def interpSessionID(self, logfile_obj, i):
	raise CantParseError, "session ID"
    def interpTimestamp(self, logfile_obj, i):
	raise CantParseError, "timestamp"
    def interpKeyTimestamp(self, logfile_obj, i):
	raise CantParseError, "key timestamp"
    def interpIgnore(self, logfile, i):
	return i  

class Version0Parser(BaseParser):

    def __init__(self, parser):
	self.parser = parser
	self.frame_keys = ["tidx", "ridx"]

    ## These frames shoud probably be grouped together under 1 method...

    def interpFrame(self, logfile_obj, i):
        splitLine = string.split(self.parser.fcontent[i])
        turnNumber = self.getTurnNumber(splitLine[2])
        j, frameContent = self.parser.iterator(i, 'interpEndFrame',
					       "interpFrame", logfile_obj)
	frameKey = splitLine[1]
	if string.strip(frameContent)[0] == '(':
	    self.parser.AddDataToDatapoint(frameKey, frameContent, "list")
	elif string.strip(frameContent)[0] == '{':
	    self.parser.AddDataToDatapoint(frameKey, frameContent, "frame")
        return j

    def interpValue(self, logfile_obj, i):
        line = string.strip(self.parser.fcontent[i])
        splitLine = string.split(line)
        turnNumber = self.getTurnNumber(splitLine[1])
        strContent = string.strip(line[string.find(line,':',1)+1:])	
        strKey = splitLine[0]
	if strKey in ["tidx", ":tidx"]:
            # in some of the old logs, "tidx" is the string "tidx",
            # because of some stupidity.
            try:
                tidx = string.atoi(strContent)
            except:
                tidx = None
	    self.parser.cache.FindCurrentDatapoint().tidx = tidx
	else:
	    self.parser.AddDataToDatapoint(strKey, strContent)
        return i

    def interpBeginUtterance(self, logfile_obj, i):
        # This information can be extracted otherwise.
	line = string.strip(self.parser.fcontent[i])
	turn_number = self.getTurnNumber(string.split(line)[1])
	self.parser.AddUtterance(turn_number)
        return i

    def interpEndUtterance(self, logfile_obj, i):
        # This information will be ignored.
        return i

    def interpSessionID(self, logfile_obj, i):
        #line = self.fcontent[i]
        #print 'interpSessionID args:',string.split(line)[1]
        #print "line:",line
	self.parser.AddSessionID(logfile_obj,
				 string.split(self.parser.fcontent[i])[1])
        return i

    def interpTimestamp(self, logfile_obj, i):
        line = string.split(string.strip(self.parser.fcontent[i]))
        turnNumber = self.getTurnNumber(line[1])
        myTime = self.getTime(line[6:])
        m = re.match("(\w*)\(([\w<>]*):([-\d]*)\)", line[3])
        server_name, server_host, port_number = m.groups()
        port_number = string.atoi(port_number)
        # This is where lastID was updated.
	if line[2] == "send":
	    self.parser.AddNewOperationTimestamp(logfile_obj, turnNumber,
						 server_name, server_host,
						 port_number, line[4],
						 None, myTime)
	elif line[2] == "read":
	    self.parser.AddOperationReplyTimestamp(logfile_obj, turnNumber,
						   server_name, server_host,
						   port_number, line[4], None,
						   "reply", myTime)
        return i
    
    def interpKeyTimestamp(self, logfile_obj, i):
        #print 'interpKeyTimestamp args:'    
        line = string.split(string.strip(self.parser.fcontent[i]))
        turnNumber = self.getTurnNumber(line[1])
        myTime = self.getTime(line[4:])
	self.parser.AddKeyTimestamp(turnNumber, myTime, line[2], None)
        return i
    
    def interpIgnore(self, logfile_obj, i):
        return i
    ## Utility functions...

    def parseLine(self, line, type):
        line =  string.strip(line)
        if line:
            if string.find(line,"[Timestamp") == 0:
                return "interpTimestamp"
            elif string.find(line,"[KeyTimestamp") == 0:
                return "interpKeyTimestamp"
            elif string.find(line,"SESSION_ID") == 0:
                return "interpSessionID"
            elif string.find(line,":BEGIN_UTT") == 0:
                return "interpBeginUtterance"
            elif string.find(line,":END_UTT") == 0:
                return "interpEndUtterance"
            elif line[0] == ':':
                return "interpValue"
            elif string.find(line,'[Begin ') == 0:
                return "interpFrame"
            elif string.find(line,'[End ') == 0:
                return "interpEndFrame"
            else:
		# tidx and ridx were special in version 0
                for elt in self.frame_keys:
                    if string.find(line, elt) == 0:
                        return "interpValue"
                else:
                    #print "Unnsuported type:",line
                    #raise NotSupported
                    return type
        else:
            return "interpIgnore"

    def getTurnNumber(self, word):
        m = re.match("\((-?\d*)\)", word)
        return string.atoi(m.group(1))

    def getTime(self, words):
        months = {'JAN': 1, 'FEB':2, 'MAR':3, 'APR':4, 'MAY':5,
                  'JUN':6, 'JUL':7,
                  'AUG':8, 'SEP':9, 'OCT':10, 'NOV':11, 'DEC':12}
        date = string.split(words[2],'-')
        ltime = string.split(words[0],':')
        milisecsp = string.find(ltime[2],'.')
        # should probably use a mapping function here...
        newtime = time.mktime((string.atoi(date[2][0:len(date[2]) -1]), \
                              months[date[1]], \
                              string.atoi(date[0]), \
                              string.atoi(ltime[0]), \
                              string.atoi(ltime[1]), \
                              string.atoi(ltime[2][0:milisecsp]),
                              0,1,0)) + string.atof(ltime[2][milisecsp:])
        return newtime

# We're going to specialize the version 0 parser where we can.

class Version1Point0Parser(Version0Parser):

    # Keys and values are treated the same, tidx and ridx are
    # gone, begin/end utt are the same, session ID is the same,
    # key timestamps will never be floating, program
    # invocations and Hub activity can now be logged.
    
    def __init__(self, parser):
	Version0Parser.__init__(self, parser)
	self.frame_keys = []

    # New for 1.0.
    def interpLogVersion(self, logfile_obj, i):
	line = string.strip(self.parser.fcontent[i])
	version_string = string.splitfields(line, None, 1)[1]
	self.parser.AddLogVersion(logfile_obj, version_string)
	return i

    # No special handling for tidx values here (not needed).
    # However, the types of the data are now recoverable.
    # This can lead to problems when I'm trying to
    # find file names or postprocess recognition strings.
    # Why don't we annotate with data types?
    # Rules: if the first character is a {, it's a frame.
    # If the first character is a ", it's a string.
    # If string.atof works, it's a float.
    # If string.atoi works, it's an int.
    # I'll worry about the rest later.
    
    def interpValue(self, logfile_obj, i):
        line = string.strip(self.parser.fcontent[i])
        splitLine = string.split(line)
        turnNumber = self.getTurnNumber(splitLine[1])
        strContent = string.strip(line[string.find(line,':',1)+1:])
        strKey = splitLine[0]
	if strKey in [":tidx", "tidx"]:
	    return i
        if strContent[0] == '"':
            dtype = "string"
	    # Make sure that the last character is also a close
	    # quote, or something. In other words, make sure that
	    # multi-line strings are handled correctly.
	    # This is the same algorithm that Galaxy.py uses, but
	    # I don't have all the data ahead of time. So I'm
	    # just going to recreate the algorithm, using the
	    # string chunks.
	    chunks = []
	    last_index = 0
	    done = 0
	    strSegment = strContent[1:]
	    while 1:	    
		in_escape = 0
		j = 0
		for ch in strSegment:
		    if ch == '"' and (not in_escape):
			# We're done
			if last_index != j:
			    chunks.append(strSegment[last_index:j])
			done = 1
			break
		    elif ch == "\\" and not in_escape:
			# skip this character.
			chunks.append(strSegment[last_index:j])
			in_escape = 1
			last_index = j + 1
		        j = j + 1
		    else:
			j = j + 1
			in_escape = 0
		if not done:
		    chunks.append(strSegment[last_index:])
		    i = i + 1
		    try:
			strSegment = string.strip(self.parser.fcontent[i])
		    except IndexError:
			break
		else: break
            strContent = string.joinfields(chunks, "\n")
        else:
            try:
                string.atoi(strContent)
                dtype = "integer"
            except:
                try:
		    string.atof(strContent)
		    dtype = "float"
                except:
                    dtype = "unknown"
	self.parser.AddDataToDatapoint(strKey, strContent, dtype)
        return i
	
    def interpKeyTimestamp(self, logfile_obj, i):
        #print 'interpKeyTimestamp args:'
	# In version 1, it's just like any other key, but no value.
	# Ignore the timestamp. Also, the 3d element is the
	# tidx, not the key. So we want line[3] instead of line[2].
        line = string.split(string.strip(self.parser.fcontent[i]))
	self.parser.AddDataToDatapoint(line[3], "")
        return i
    
    ReplyTable = {"read":
		  {"GAL_MESSAGE_MSG_TYPE": ("read", MessageDataPoint),
		   "GAL_REPLY_MSG_TYPE": ("reply", OperationDataPoint),
		   "GAL_ERROR_MSG_TYPE": ("error", OperationDataPoint),
		   "GAL_DESTROY_MSG_TYPE": ("destroy", OperationDataPoint)},
		  "send":
		  {"GAL_MESSAGE_MSG_TYPE": ("send", OperationDataPoint),
		   "GAL_REPLY_MSG_TYPE": ("reply", MessageDataPoint),
		   "GAL_ERROR_MSG_TYPE": ("error", MessageDataPoint),
		   "GAL_DESTROY_MSG_TYPE": ("destroy", MessageDataPoint)}
		  }
    
    def interpTimestamp(self, logfile_obj, i):
	# In version 1, we have a few options. We can have a new
	# message from the server, which we should make the
	# active datapoint but then close it immediately.
	# We should have a response to the server, which we
	# should do the same thing with.
	# We can have a new message to a server or a response
	# from, which we should pair as we always did.
	# Finally, we can have Hub activity, which doesn't
	# have the server(host:port) info.
	# Also, we have the token index in all these.
        line = string.split(string.strip(self.parser.fcontent[i]))
        turnNumber = self.getTurnNumber(line[1])
	action = line[2]
	action_parts = string.splitfields(action, ":")
        # HUB internal action timestamps and normal messages
        # are logged with the same format. However, sometimes
        # there's no server, in which case the server line
        # is omitted. What we need to do here is see whether the
        # third component is a tidx or a server operation.
	if len(action_parts) == 1:
	    # It's a Hub internal action.
            msg_class = EventDataPoint
        else:
	    reply_status, msg_class = self.ReplyTable[action_parts[0]][action_parts[1]]
	# There are three possibilities for the third position.
	# (1) It's a token index. In this case, it's probably
	# a Hub event for a particular token, which doesn't
	# have associated host and port.
	# (2) It's a message or operation, in which case the
	# third position is the host:port description.
	# (3) It's neither of the above, in which case it's
	# almost certainly a Hub event with no token, such as
	# an alarm or server up/down notification. In this
	# case, the third position is going to be
	# a name of some sort. And there wouldn't be any
	# token index.
        try:
            tidx = string.atoi(line[3])
            server_name, server_host, port_number = "unknown", "unknown", -1
            remainder = line[4:]
        except:            
	    m = re.match("(\S*)\(([\S<>]*):([-\d]*)\)", line[3])
	    if m:
		server_name, server_host, port_number = m.groups()
		tidx = string.atoi(line[4])
		remainder = line[5:]
		port_number = string.atoi(port_number)
	    else:
		server_name = line[3]
		server_host, port_number = "unknown", -1
		remainder = line[4:]
		tidx = None
        operation = remainder[0]
        myTime = self.getTime(remainder[2:])
        if msg_class == EventDataPoint:
	    self.parser.AddEventTimestamp(logfile_obj, myTime,
                                          turnNumber, server_name,
                                          server_host, port_number,
                                          operation, action, tidx)
	elif msg_class == MessageDataPoint:
            self.parser.AddMessageTimestamp(logfile_obj,
                                            turnNumber, server_name,
                                            server_host,
                                            port_number, operation, tidx,
                                            reply_status, myTime)
        elif reply_status == "send":
            self.parser.AddNewOperationTimestamp(logfile_obj, turnNumber,
                                                 server_name, server_host,
                                                 port_number,
                                                 operation, tidx,
                                                 myTime)
        else:
            self.parser.AddOperationReplyTimestamp(logfile_obj, turnNumber,
                                                   server_name,
                                                   server_host,
                                                   port_number,
                                                   operation, tidx,
                                                   reply_status, myTime)
        return i

    # There was no user versioning before 1.0.
    
    def parseLine(self, line, type):
        line =  string.strip(line)
	if line and string.split(line)[0] == "LOG_VERSION:":
	    # We've found the user version.
	    return "interpLogVersion"
	else:
	    return Version0Parser.parseLine(self, line, type)	

ParserDict = {"1.0": Version1Point0Parser}
