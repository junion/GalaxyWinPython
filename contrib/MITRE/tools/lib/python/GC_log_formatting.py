# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This file implements lots of the formatting for the XML
# tags used in Communicator logs.

import xml_tree, string, time, os
    
class FormattableXMLObject:
    def __init__(self):
	self.interesting = None
	self.basic = None
	self.log = None
    def AddLogAndFormatClasses(self, log):
	pass
    def Extract(self):
	res = []
	if self.interesting is not None:
	    res = res + self.interesting
	for dp in self.children:
	    if isinstance(dp, FormattableXMLObject):
		res = res + dp.Extract()
	return res	    
    def Format(self, allow_interesting = None, indent = 0):
	# First, we see if the log formatter wants to
	# "customize" us, or if we have interesting
	# data that wants to format us.
	return (self.log and self.log.formatter and \
		self.log.formatter._Format(self, allow_interesting,
					   indent)) or \
	       self._DefaultFormat(allow_interesting, indent)
    def Summarize(self, indent = 0):
	return self.Format(1, indent)
    # Use the default XML renderer as the default formatter.
    def _DefaultFormat(self, allow_interesting, indent):
	if indent:
	    return self.render(pprint = 1, indent = indent)
	else:
	    return self.render()
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	if self.log and self.log.formatter:
	    return self.log.formatter._FormatHeader(self, allow_interesting,
						    indent)
	else:
	    return None
    def _FormatTime(self, allow_interesting = None, indent = 0):
	if self.log and self.log.formatter:
	    return self.log.formatter._FormatTime(self, allow_interesting,
						  indent)
	else:
	    return None
    def _FormatKeys(self, allow_interesting = None, indent = 0):
	if self.log and self.log.formatter:
	    return self.log.formatter._FormatKeys(self, allow_interesting,
						  indent)
	else:
	    return None
    def _FormatKey(self, allow_interesting = None, indent = 0):
	if self.log and self.log.formatter:
	    return self.log.formatter._FormatKey(self, allow_interesting,
						 indent)
	else:
	    return None
    def _FormatData(self, allow_interesting = None, indent = 0):
	if self.log and self.log.formatter:
	    return self.log.formatter._FormatData(self, allow_interesting,
						  indent)
	else:
	    return None
    def _FormatAnchor(self, allow_interesting = None, indent = 0):
	if self.log and self.log.formatter:
	    return self.log.formatter._FormatAnchor(self, allow_interesting,
						    indent)
	else:
	    return None
    def _FormatFooter(self, allow_interesting = None, indent = 0):
	if self.log and self.log.formatter:
	    return self.log.formatter._FormatFooter(self, allow_interesting,
						    indent)
	else:
	    return None

class FormattableTag(xml_tree.Tag, FormattableXMLObject):
    def __init__(self, *args, **kw):
	apply(xml_tree.Tag.__init__, (self,) + args, kw)
	FormattableXMLObject.__init__(self)
    def Extract(self):
	if self.interesting is None:
	    # Make sure that this instance has been updated
	    # for the interesting classes.
	    interesting = []
	    for key, value in self.properties.items():
		# If the value in the dict is None, then it
		# means that any key matches.
		new_i = GetInterestingFormatClass(key, value)
		if new_i is not None:
		    interesting.append(new_i(self, key, value))
	    self.interesting = interesting
	return FormattableXMLObject.Extract(self)
    def AddLogAndFormatClasses(self, log):
	global BasicFormatClasses
	self.log = log
	if BasicFormatClasses.has_key(self.name):
	    self.basic = BasicFormatClasses[self.name](self)
	for c in self.children:
	    if isinstance(c, FormattableTag):
		c.AddLogAndFormatClasses(log)
    def Copy(self):
	c = xml_tree.Tag.Copy(self)
	c.basic = self.basic
	c.interesting = self.interesting
	c.log = self.log
	return c
    def Finish(self):
	# In order to finish data tags correctly,
	# we need to make sure that GC_DATA always has
	# a data child.
	xml_tree.Tag.Finish(self)
	if self.name == "GC_DATA" and \
	   (not self.children):
	    self.AddChild(xml_tree.Data(""))
    def _PrepareForDeletion(self):
	self.basic = None
	self.interesting = None
	xml_tree.Tag._PrepareForDeletion(self)
		
class FormattableXMLDocument(xml_tree.XMLDocument, FormattableXMLObject):
    def __init__(self):
	xml_tree.XMLDocument.__init__(self)
	FormattableXMLObject.__init__(self)
    def AddLogAndFormatClasses(self, log):
	self.log = log
	# Just recurses
	self.basic = SessionFormatClass(self)
	for c in self.children:
	    if isinstance(c, FormattableTag):
		c.AddLogAndFormatClasses(log)
    def _PrepareForDeletion(self):
	self.basic = None
	xml_tree.XMLDocument._PrepareForDeletion(self)

def xml_to_formattable_tree(file):
    return xml_tree.xml_to_tree(file, {"XMLDocument": FormattableXMLDocument,
				       "Tag": FormattableTag})

def xdr_to_formattable_tree(str):
    return xml_tree.xdr_to_xml(str, {"XMLDocument": FormattableXMLDocument,
				     "Tag": FormattableTag})

# Basic format classes.

# SAM 08/30/00: It is possible that it will not be possible to
# derive a location, because GC_ANNOT can appear directly under
# GC_SESSION. This may be a problem in unified files.

def GetLocation(original_dp):	
    while original_dp:
	if not hasattr(original_dp, "properties"):
	    return (None, None)
	props = original_dp.properties
	try:
	    if original_dp.name == "GC_TURN" and \
	       props.has_key("id"):		    
		turnid = props["id"]
	    else:
		# It had better be in turnid otherwise.
		# That's where it would be always except
		# for backward compatibility issues.
		turnid = props["turnid"]
	except:
	    turnid = None
	try: tidx = props["tidx"]
	except: tidx = None
	if (turnid is not None) or (tidx is not None):
	    return (turnid, tidx)
	else:
	    original_dp = original_dp.parent
    return (None, None)

def GetOriginalTimes(original_dp):
    # Fix for log misformatting. If you want
    # times out of GC_DATA, climb up until you
    # find a legal object to get times out of. Much
    # in the spirit of GetLocation().
    while isinstance(original_dp, xml_tree.Tag) and \
	  original_dp.name not in InterestingEvent.legal_tags:
	original_dp = original_dp.parent
    if not isinstance(original_dp, xml_tree.Tag):
	return (None, None)
    else:
	props = original_dp.properties
	if props.has_key("time"):
	    return (string.atof(props["time"]), string.atof(props["time"]))
	elif props.has_key("etime") and props.has_key("stime"):
	    return (string.atof(props["stime"]), string.atof(props["etime"]))
	elif props.has_key("stime"):
	    return (string.atof(props["stime"]), string.atof(props["stime"]))
	else:
	    return (None, None)

class FormatClass:
    def __init__(self, original_dp):
	self.original_dp = original_dp	
    def GetLocation(self):
	# Search through parents of original dp to find tidx or
	# turn id. If you get all the way up to GC_TURN, you
	# should check the "id" prop for backward compatibility.
	return GetLocation(self.original_dp)

class BasicFormatClass(FormatClass):
    pass

class OperationFormatClass(BasicFormatClass):
    def _Format(self, allow_interesting = None, indent = 0):
	str_list = ["=============================="]
	str_list.append(self.original_dp._FormatHeader(allow_interesting, indent))
	str_list.append(self.original_dp._FormatTime(allow_interesting, indent))
	str_list.append(self.original_dp._FormatKeys(allow_interesting, indent))
	str_list.append(self.original_dp._FormatFooter(allow_interesting, indent))
	# filter None gets the false (empty strings) out.
	return string.joinfields(filter(None, str_list), "\n")    
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	properties = self.original_dp.properties
	if not properties.has_key("tidx"):
	    tidx_string = "unknown"
	else:
	    tidx_string = properties["tidx"]
	if properties.has_key("reply_status"):
	    reply_string = ", reply status " + properties["reply_status"]
	else:
	    reply_string = ""
	return "Datapoint for server %s, operation %s, tidx %s%s:" % \
	       (self._FormatServer(properties), properties["name"],
		tidx_string, reply_string)
    def _FormatServer(self, properties):
	return "%s(%s)" % (properties["server"], properties["location"])
    def _FormatTime(self, allow_interesting = None, indent = 0):
	properties = self.original_dp.properties
	if properties.has_key("reply_status") and \
	   properties["reply_status"] == "asynchronous":
	    return "Time: %s" % properties["stime"]
	else:
	    return "Start time: %s End time: %s" % \
		   (properties["stime"], properties["etime"])
    def _FormatKeys(self, allow_interesting = None, indent = 0):
	in_list = []
	out_list = []
	for data in self.original_dp.children:
	    # These are all data tags.
	    if data.properties.has_key("direction"):
		if data.properties["direction"] == "out":
		    out_list.append(data)
		else:
		    in_list.append(data)
	    else:
		in_list.append(data)
	str_list = []
	if in_list:
	    str_list.append("In keys:")
	    for dp in in_list:
                str_list.append(dp.Format(allow_interesting, indent))
	if out_list:
	    str_list.append("Out keys:")
	    for dp in out_list:
                str_list.append(dp.Format(allow_interesting, indent))
	return string.joinfields(str_list, "\n")

    def _FormatFooter(self, allow_interesting = None, indent = 0):
	return None

class KeyDataFormatClass(OperationFormatClass):
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	return "Key datapoint:"
    def _FormatKeys(self, allow_interesting = None, indent = 0):
	# There will be a single child, which will be a GC_DATA
	# with no value.
	return "Key: " + self.original_dp.children[0].properties["key"]

class EventFormatClass(OperationFormatClass):
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	properties = self.original_dp.properties
	try:
	    name = properties["name"]
	except KeyError:
	    name = "None"
	try:
	    tidx = properties["tidx"]
	except KeyError:
	    tidx = "None"
	return "Hub event %s, tidx %s, type %s" % \
	       (properties["etype"], tidx, name)
    def _FormatTime(self, allow_interesting = None, indent = 0):
	properties = self.original_dp.properties
	return "Time: %s" % properties["time"]

class MessageFormatClass(OperationFormatClass):
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	properties = self.original_dp.properties
	try:
	    name = properties["name"]
	except KeyError:
	    name = "None"
	try:
	    tidx = properties["tidx"]
	except KeyError:
	    tidx = "None"
	if properties.has_key("direction") and \
	   properties["direction"] == "server_to_hub":
	    return "Incoming Hub message %s from server %s, tidx %s:" % \
		   (name, self._FormatServer(properties), tidx)
	else:
	    return "Outgoing Hub message %s to server %s, tidx %s (%s):" % \
		   (name, self._FormatServer(properties),
		    tidx, properties["reply_status"])
    def _FormatTime(self, allow_interesting = None, indent = 0):
	properties = self.original_dp.properties
	return "Time: %s" % properties["time"]

class DataFormatClass(BasicFormatClass):
    def _Format(self, allow_interesting = None, indent = 0):
	str_list = [self.original_dp._FormatKey(allow_interesting, indent),
		    self.original_dp._FormatData(allow_interesting, indent)]
	return string.joinfields(str_list, "\n")
    def _FormatKey(self, allow_interesting = None, indent = 0):
	return self.original_dp.properties["key"]
    def _FormatData(self, allow_interesting = None, indent = 0):
	# There will be a single child.
	if not self.original_dp.children:
	    return ""
	else:
	    child = self.original_dp.children[0]
	    if isinstance(child, FormattableTag):
		return child.Format(None, indent)
	    elif isinstance(child, xml_tree.Data):
		# Use the type attribute to format correctly.
		# It will already be a string, so we shouldn't
		# need to do anything here.
		# We don't want to call "render" here, because
		# that's used specifically to print the document,
		# which means CGI substitutions happen on the
		# data. We want to leave that for later in this case.
		return (indent * " ") + child.data
	    else:
		return ""
    def _FormatAnchor(self, allow_interesting = None, indent = 0):
	# First, get the data, without the interesting dimension.
	# This will almost certainly call the method above.
	d = self.original_dp._FormatData(None, indent)
	# Next, chop off the path reference.
	return os.path.split(d)[1]

class UtteranceFormatClass(BasicFormatClass):
    def _Format(self, allow_interesting = None, indent = 0):
	str_list = [self.original_dp._FormatHeader(allow_interesting, indent)]
	# Make sure that you can extract the interesting elements
	# if allow_interesting is 1.
	if allow_interesting:
	    children = self.original_dp.Extract()
	else:
	    children = self.original_dp.children
	for dp in children:
	    str_list.append(dp.Format(allow_interesting, indent))
	str_list.append(self.original_dp._FormatFooter(allow_interesting, indent))
	return string.joinfields(str_list, "\n")
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	str_list = ["\n==============================\n"]
	str_list.append("    Utterance %s" % self.GetLocation()[0])
	str_list.append("\n==============================\n")
	return string.joinfields(str_list, "\n")
    def _FormatFooter(self, allow_interesting = None, indent = 0):
	return ""
    # SAM 4/24/00: Changing the default generation of turn ID to
    # turnid, to match everything else. So I need to add an
    # access method which will collapse the to.

class LogFormatClass(BasicFormatClass):
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	return ""
    def _FormatFooter(self, allow_interesting = None, indent = 0):
	return ""
    def _Format(self, allow_interesting = None, indent = 0):
	str_list = [self.original_dp._FormatHeader()]
	for c in self.original_dp.children:
	    str_list.append(c.Format())
	str_list.append(self.original_dp._FormatFooter())
	return string.joinfields(str_list, "\n")

class SessionFormatClass(BasicFormatClass):
    def _Format(self, allow_interesting = None, indent = 0):
	str_list = []
	for c in self.original_dp.children:
	    str_list.append(c.Format())
	return string.joinfields(str_list, "\n")

# And don't forget, we're going to have to deal with formatting
# the frames and lists, too. Sigh. This will look an AWFUL lot
# like the frame formatting in Galaxy. I'm half tempted to just
# build a Galaxy frame object and print it, but that would be evil.

# The dtype key is not obligatory, so we'll just have to wing it.

class GalaxyFormatClass(BasicFormatClass):
    def _FormatGalaxyValue(self, v, allow_interesting = None, indent = 0):
	# The element will be either a Data element or
	# a FormattableTag.
	only_child = v.children[0]
	if isinstance(only_child, xml_tree.Data):
	    try:
		dtype = v.properties["dtype"]
	    except:
		dtype = None
	    # Format based on type.
	    if dtype == "integer":
		return only_child.data
	    elif dtype == "float":
		return "%e" % string.atof(only_child.data)
	    elif dtype == "string":
		val = only_child.data
		# Argh. Must escape quotes. Sigh.
		# Normal string or symbol.
		# Always put quotes.
		if not val:
		    return '""'
		if val[0] == '"' and val[-1] == '"':
		    val = val[1:-1]
		if "\"" not in val and "\\" not in val:
		    return '"'+val+'"'
		else:
		    # Escape the damn thing.
		    res = []
		    for char in val:
			if char in ['"', '\\']:
			    res.append('\\')
			    res.append(char)
		    return '"'+string.joinfields(res, "")+'"'
	    else:
		return only_child.data
	else:
	    return only_child.Format(allow_interesting, indent)

class FrameFormatClass(GalaxyFormatClass):
    def _Format(self, allow_interesting = None, indent = 0):
	properties = self.original_dp.properties
	ftype = properties["frame_type"]
	if ftype == "topic":
	    t = "q"
	elif ftype == "pred":
	    t = "p"
	elif ftype == "clause":
	    t = "c"
	else:
	    t = ""
	str = "{" + t + " " + properties["name"]
	kindent = indent + 2 + len(t)
	for data in self.original_dp.children:
	    key = data.properties["key"]
	    vindent = kindent + len(key) + 1
	    str = str + "\n" + (" " * kindent)
	    str = str + key + " " + self._FormatGalaxyValue(data,
							    allow_interesting,
							    vindent)
	str = str + " }"
	return str

class ListFormatClass(GalaxyFormatClass):
    def _Format(self, allow_interesting = None, indent = 0):
	lindent = indent + 2
	str = "( "
	first = 1
	for elt in self.original_dp.children:
	    if not first:
		str = str + "\n" + (lindent * " ")
	    else:
		first = 0
	    str = str + self._FormatGalaxyValue(elt, allow_interesting,
						lindent)
	return str + " ) "    

BasicFormatClasses = {"GC_LOG": LogFormatClass,
		      "GC_DATA": DataFormatClass,
		      "GC_SESSION": SessionFormatClass,
		      "GC_OPERATION": OperationFormatClass,
		      "GC_MESSAGE": MessageFormatClass,
		      "GC_EVENT": EventFormatClass,
		      "GC_TURN": UtteranceFormatClass,
		      "GC_FRAME": FrameFormatClass,
		      "GC_LIST": ListFormatClass}

# And now we take care of the "interesting" elements.
# We need a table of the "type_" elements in order to determine
# which "interesting" class to add.

# The classes should describe what key and value they refer to, what
# their scope is (global or per turn), how many are permitted per turn
# (min and max), and how to format. Also, which turn they should be in.

BadInterestingFormat = "BadInterestingFormat"

# SAM 1/8/00: These classes should have some basic methods associated
# with them for accessing stuff from the class. You should be
# able to access the data, the interesting value (for data), the tidx and turnid
# of the closest parent, the times (for events).

class InterestingFormatClass(FormatClass):
    def __init__(self, original_dp, key, value):	
	if ((key, value) not in self.key_value_pairs) and \
	   ((key, None) not in self.key_value_pairs):
	    raise BadInterestingFormat, ("illegal pair", key, value)
	self.true_kv = (key, value)
	FormatClass.__init__(self, original_dp)
    def GetTimes(self):
	return GetOriginalTimes(self.original_dp)
    def GetData(self):
	return None
    def SetData(self):
	pass
    def GetValue(self):
	return self.true_kv[1]
    def SetValue(self, val):
	self.true_kv = (self.true_kv[0], val)
	self.original_dp.properties[self.true_kv[0]] = val

# An interesting event may have been hung off a GC_DATA tag.
# Perhaps the validator should catch this, or maybe the 
# rules?

class InterestingEvent(InterestingFormatClass):
    legal_tags = ["GC_OPERATION", "GC_MESSAGE", "GC_EVENT"]
    def _Format(self, allow_interesting = None, indent = 0):
	return self.original_dp._FormatTime(allow_interesting or self, indent) + ": " + \
	       self.original_dp._FormatHeader(allow_interesting or self, indent)
    def _FormatTime(self, allow_interesting = None, indent = 0):
	props = self.original_dp.properties
	stime, etime = self.GetTimes()
	if stime is None and etime is None:
	    return ""
	elif stime == etime:
	    return self._FormatTimeFloat(stime)
	else:
	    return self._FormatTimeFloat(stime) + \
		   " to " + self._FormatTimeFloat(etime)
    def _FormatTimeFloat(self, fval):
	f_time = string.splitfields(("%.2f" % (fval % 1)), ".")[1]
	cs = time.ctime(int(fval))
	[day, month, date, min, year] = string.split(cs)
	return string.join([day, month, date, year, "at", min+"."+f_time])

def GetOriginalData(dp):
    if not dp.children:
	return None
    elif isinstance(dp.children[0], xml_tree.Data):
	s = dp.children[0].data
	if dp.properties.has_key("dtype"):
	    dtype = dp.properties["dtype"]
	    if dtype == "integer":
		return string.atoi(s)
	    elif dtype == "float":
		return string.atof(s)
	    else:
		return s
	else:
	    return s
    else:
	# Return the original tag object.
	return dp.children[0]    

class InterestingData(InterestingFormatClass):    
    legal_tags = ["GC_DATA"]
    def GetData(self):
	return GetOriginalData(self.original_dp)
    def SetData(self, data):
	dp = self.original_dp
	if not dp.children:
	    return
	elif isinstance(dp.children[0], xml_tree.Data):
	    s = dp.children[0]
	    if type(data) is type(0):
		dp.properties["dtype"] = "integer"
		s.data = `data`
	    elif type(data) is type(0.0):
		dp.properties["dtype"] = "float"
		s.data = `data`
	    elif type(data) is type(""):
		s.data = data
	elif isinstance(data, xml_tree.Tag):
	    dp.children = []
	    dp.AddChild(data)
	    
    def _Format(self, allow_interesting = None, indent = 0):
	return self.original_dp._FormatKey(allow_interesting or self, indent) + \
	       ": " + \
	       self.original_dp._FormatData(allow_interesting or self, indent)
    def _FormatKey(self, allow_interesting = None, indent = 0):	
	return self._header_internal_()
    def _header_internal_(self):
	return ""

# As far as I can tell, the scope of the start and end utts is
# inside a turn, and that's a problem, because there can be
# many of them in a given turn. There should be exactly as
# many audio ends as audio starts, and a transcription for
# each audio, and perhaps an audio file, etc. I need to redo
# the min-max stuff. Probably, I need to do it in methods.

# The problem is that any functions in the scope of a class
# are interpreted as methods. So I need an instance lying around
# all the time in order to check whether there are enough
# elements of a given type; but sometimes there may be no
# instances, which may be the problem. So I'm just going to
# have to create an instance if it's not there, over in
# the validation stuff. Sigh.

class UserAudioStart(InterestingEvent):
    scope = "user"
    key_value_pairs = [("type_start_utt", "user")]
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	return "User started speaking."
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	if len(self_entries) > 0:
	    return 1, None
	else:
	    return 0, "At least one required."

class UserAudioEnd(InterestingEvent):
    scope = "user"
    key_value_pairs = [("type_end_utt", "user")]
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	return "User finished speaking."
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	try:
	    num_audio = len(num_dict[UserAudioStart])
	except:
	    num_audio = 0
	if len(self_entries) == num_audio:
	    return 1, None
	else:
	    return 0, "Number must match audio start."

class SystemAudioStart(InterestingEvent):
    scope = "system"
    key_value_pairs = [("type_start_utt", "system")]
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	return "System started speaking."
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	if len(self_entries) > 0:
	    return 1, None
	else:
	    return 0, "At least one required."
    
class SystemAudioEnd(InterestingEvent):
    scope = "system"
    key_value_pairs = [("type_end_utt", "system")]
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	return "System finished speaking."
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	try:
	    num_audio = len(num_dict[SystemAudioStart])
	except:
	    num_audio = 0
	if len(self_entries) == num_audio:
	    return 1, None
	else:
	    return 0, "Number must match audio start."

class StartTask(InterestingEvent):
    scope = "global"
    key_value_pairs = [("type_start_task", "total"),
		       ("type_start_task", "task"),
		       ("type_start_task", "true")]
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	if self.true_kv[1] == "true":
	    return "Task-specific portion and overall task started."
	elif self.true_kv[1] == "total":
	    return "Overall task started."
	else:
	    return "Task-specific portion started."
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	kv_values = map(lambda x: x.GetValue(), self_entries)
	if ("total" in kv_values) and ("true" in kv_values):
	    return 0, "Both total and true specified."
	if ("task" in kv_values) and ("true" in kv_values):
	    return 0, "Both task and true specified."
	if ("total" not in kv_values) and ("true" not in kv_values):
	    return 0, "No specification for overall portion."	
	if ("task" not in kv_values) and ("true" not in kv_values):
	    return 0, "No specification for task-specific portion."
	return 1, None
	
class EndTask(InterestingEvent):
    scope = "global"
    key_value_pairs = [("type_end_task", "true"),
		       ("type_end_task", "total"),
		       ("type_end_task", "task")]
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	if self.true_kv[1] == "true":
	    return "Task-specific portion and overall task ended."
	elif self.true_kv[1] == "total":
	    return "Overall task ended."
	else:
	    return "Task-specific portion ended."
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	kv_values = map(lambda x: x.GetValue(), self_entries)
	if len(kv_values) == 0:
	    return 0, "No end task found."
	if ("total" in kv_values) and ("true" in kv_values) and \
	   ("task" in kv_values):
	    return 0, "All of total, task and true specified."
	if ("total" in kv_values) and ("true" in kv_values):
	    return 0, "Both total and true specified."
	if ("task" in kv_values) and ("true" in kv_values):
	    return 0, "Both task and true specified."
	if ("total" not in kv_values) and ("true" not in kv_values):
	    return 0, "No specification for overall portion."	
	if ("task" not in kv_values) and ("true" not in kv_values):
	    return 0, "No specification for task-specific portion."
	return 1, None

class NewTurn(InterestingEvent):
    scope = "global"
    key_value_pairs = [("type_new_turn", "user"),
		       ("type_new_turn", "system")]
    def _Format(self, allow_interesting = None, indent = 0):
	return "\n" + InterestingEvent._Format(self, allow_interesting, indent) + "\n"
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	return "New %s turn began." % self.true_kv[1]
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	if len(self_entries) > 0:
	    return 1, None
	else:
	    return 0, "At least one required."

class UserTranscription(InterestingData):
    # A turn may have an arbitrary number of user inputs.
    # One transcription per user input. You'd need to use
    # tidxes to tie the transcription to the input in
    # the multiple input case.
    scope = "user"
    key_value_pairs = [("type_utt_text", "transcription")]
    def _header_internal_(self):
	return "User said"
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	try:
	    num_audio = len(num_dict[UserAudioStart])
	except:
	    num_audio = 0
	if len(self_entries) == num_audio:
	    return 1, None
	else:
	    return 0, "Number must match audio start."

class UserSR(InterestingData):
    # A turn may have an arbitrary number of user inputs.
    # You'd need to use tidxes to tie the transcription
    # to them, then.
    scope = "user"
    key_value_pairs = [("type_utt_text", "asr")]
    def _header_internal_(self):
	return "Recognizer heard"
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	try:
	    num_audio = len(num_dict[UserAudioStart])
	except:
	    num_audio = 0
	if len(self_entries) == num_audio:
	    return 1, None
	else:
	    return 0, "Number must match audio start."

class SystemText(InterestingData):
    # A turn can have an arbitrary number of system outputs.
    # How should it match up to the number of audio outputs?
    # Let's start with one-to-one.
    scope = "system"
    key_value_pairs = [("type_utt_text", "system")]
    def _header_internal_(self):
	return "System said"
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	try:
	    num_audio = len(num_dict[SystemAudioStart])
	except:
	    num_audio = 0
	if len(self_entries) == num_audio:
	    return 1, None
	else:
	    return 0, "Number must match audio start."

class HelpMessage(InterestingData):
    scope = "system"
    key_value_pairs = [("type_help_msg", "true")]
    def _header_internal_(self):
	return "System utterance type"
    def _FormatData(self, allow_interesting = None, indent = 0):
	return "help."
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	return 1, None

class ErrorMessage(InterestingData):
    scope = "system"
    key_value_pairs = [("type_error_msg", "true")]
    def _header_internal_(self):
	return "System utterance type"
    def _FormatData(self, allow_interesting = None, indent = 0):
	return "error."
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	return 1, None

# The prompts have a special case in that the value of the key
# is not restricted in any way.

class SystemPrompt(InterestingEvent):
    scope = "system"
    key_value_pairs = [("type_prompt", None)]
    def _FormatHeader(self, allow_interesting = None, indent = 0):
	return "System prompted for %s." % self.GetValue()
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	return 1, None

class TaskCompletion(InterestingData):
    scope = "global"
    key_value_pairs = [("type_task_completion", "1"),
		       ("type_task_completion", "0")]
    def _header_internal_(self):
	return "Task completion status"
    def _FormatData(self, allow_interesting = None, indent = 0):
	if self.GetValue() == "1":
	    return "completed."
	else:
	    return "not completed."
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	if len(self_entries) == 1:
	    return 1, None
	else:
	    return 0, "Exactly one required."

# Optional ones.

class UserAudioFile(InterestingData):
    scope = "user"
    key_value_pairs = [("type_audio_file", "user")]
    def _header_internal_(self):
	return "User audio file"
    def _FormatData(self, allow_interesting = None, indent = 0):
	return self.original_dp._FormatAnchor(allow_interesting, indent)
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	# If there are any at all, there should be as
	# many as there are audio starts.
	# This is actually a class method.
	try:
	    num_audio = len(num_dict[UserAudioStart])
	except:
	    num_audio = 0
	if len(self_entries) == 0:
	    return 1, None
	elif len(self_entries) == num_audio:
	    return 1, None
	else:
	    return 0, "If any are present, number must match audio start."

class SystemAudioFile(InterestingData):
    scope = "system"
    key_value_pairs = [("type_audio_file", "system")]
    def _header_internal_(self):
	return "System audio file"
    def _FormatData(self, allow_interesting = None, indent = 0):
	return self.original_dp._FormatAnchor(allow_interesting, indent)
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	# If there are any at all, there should be as
	# many as there are audio starts.
	# This is actually a class method.
	try:
	    num_audio = len(num_dict[SystemAudioStart])
	except:
	    num_audio = 0
	if len(self_entries) == 0:
	    return 1, None
	elif len(self_entries) == num_audio:
	    return 1, None
	else:
	    return 0, "If any are present, number must match audio start."

class SRHypothesis(InterestingData):
    scope = "user"
    key_value_pairs = [("type_utt_text", "failed_sr"),
		       ("type_utt_text", "successful_sr")]
    def _header_internal_(self):
	if self.true_kv[1] == "failed_sr":
	    return "Recognizer hypothesis (unparsed)"
	else:
	    return "Recognizer hypothesis (parsed)"
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	return 1, None

# ChosenSRHypothesis is now UserSR.

class InputParaphrase(InterestingData):
    scope = "user"
    key_value_pairs = [("type_utt_text", "input_paraphrase")]
    def _header_internal_(self):
	return "Input paraphrase"
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	try:
	    num_audio = len(num_dict[UserAudioStart])
	except:
	    num_audio = 0
	# There should be no more than there are user audio inputs.
	if len(self_entries) <= num_audio:
	    return 1, None
	else:
	    return 0, "Number must not exceed audio start."

class ConceptInput(InterestingData):
    scope = "user"
    key_value_pairs = [("type_utt_concept", "user")]
    def _header_internal_(self):
	return "User frame"
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	# There should be no more than there are user audio inputs.
	try:
	    num_audio = len(num_dict[UserAudioStart])
	except:
	    num_audio = 0
	if len(self_entries) <= num_audio:
	    return 1, None
	else:
	    return 0, "Number must not exceed audio start."

class ConceptAnnotation(InterestingData):
    scope = "user"
    key_value_pairs = [("type_annotation", "concept")]
    def _header_internal_(self):
	return "User frame annotation"
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	# There should be no more than there are user audio inputs.
	try:
	    num_audio = len(num_dict[UserAudioStart])
	except:
	    num_audio = 0
	if len(self_entries) <= num_audio:
	    return 1, None
	else:
	    return 0, "Number must not exceed audio start."

# HACK HACK HACK.

class TransducerInput(InterestingData):
    scope = "user"
    key_value_pairs = [("type_transducer", "input")]
    def _header_internal_(self):
	return "Transducer input"
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):
	# There should be no more than there are user audio inputs.
	try:
	    num_audio = len(num_dict[UserAudioStart])
	except:
	    num_audio = 0
	if len(self_entries) <= num_audio:
	    return 1, None
	else:
	    return 0, "Number must not exceed audio start."

class TransducerOutput(InterestingData):
    scope = "user"
    key_value_pairs = [("type_transducer", "output")]
    def _header_internal_(self):
	return "Transducer output"
    # This is actually a class method.
    def EnsureMinMax(self, self_entries, num_dict):	
	# There should be no more than there are user audio inputs.
	try:
	    num_audio = len(num_dict[UserAudioStart])
	except:
	    num_audio = 0
	if len(self_entries) <= num_audio:
	    return 1, None
	else:
	    return 0, "Number must not exceed audio start."

# END HACK HACK HACK 

# Note - when we complete this, we have to be able to get
# the io in here to do the notifications.

def LoadLandmarks(f):
    d = xml_tree.xml_to_tree(f)
    if string.lower(d.children[0].name) != "landmarks":
	raise "BadFile", "Toplevel tag must be LANDMARKS"
    for child in d.children[0].children:
	if (not isinstance(child, Tag)) or \
	   (string.lower(child.name) not in ["data", "event"]):
	    print "Ignoring %s element" % child.__class__.__name__
	    continue
	elif string.lower(child.name) == "data":
	    parent_class = "InterestingData"
	else:
	    parent_class = "InterestingEvent"
	for pair_child in child.children:	    
	    if (not isinstance(child, Tag)) or \
	       (string.lower(child.name) != "pair"):
		raise "BadFile", "Children of DATA and EVENT must be PAIR"

# These are also the ones in the "global" category.

InterestingFormatClasses = [StartTask, EndTask, NewTurn,
			    TaskCompletion,
			    SystemText, UserSR, UserTranscription,
			    HelpMessage, ErrorMessage, SystemPrompt,
			    SystemAudioStart, SystemAudioEnd,
			    UserAudioStart, UserAudioEnd,
			    SystemAudioFile, UserAudioFile,
			    SRHypothesis, InputParaphrase,
			    ConceptInput, ConceptAnnotation,
			    TransducerInput, TransducerOutput]

# One of the values may be None, which indicates that 
# anything matches.

InterestingFormatClassDict = {}
for elt in InterestingFormatClasses:
    key_list = elt.key_value_pairs
    for key, value in key_list:
	if InterestingFormatClassDict.has_key(key):
	    InterestingFormatClassDict[key][value] = elt
	else:
	    InterestingFormatClassDict[key] = {value: elt}

def GetInterestingFormatClass(key, value):
    try:
	return InterestingFormatClassDict[key][value]
    except KeyError:
	try:
	    return InterestingFormatClassDict[key][None]
	except KeyError:
	    return None

# Here's the default formatter, which overrides the basic
# or interesting format classes.

# I would like to simply specialize the class of the
# existing instance, but that might be a little tricky. The
# reason is that I need to define the classes, but I don't know
# which of the different event types they'll inherit from. 

# So I'll just substitute the interesting event, and have
# it point to the original datapoint, and when we ask for
# a regular ol' format, we'll get the original datapoint, but
# when we ask for an interesting format, we get the special one.
# We'll set up the interesting information class in the
# same file which has the logs, and we'll have the formatter
# send the cached datapoint to the formatter when there's
# an interesting datapoint.

# We do a complete class walk.

class DefaultFormatter:
    def __init__(self, logfile):
	self.logfile = logfile
	self.interface_table = {}
    def AddInterface(self, pclass, mname, method):
	if not self.interface_table.has_key(pclass):
	    self.interface_table[pclass] = {}
	self.interface_table[pclass][mname] = method
    def RemoveInterface(self, pclass, mname):
	try:
	    del self.interface_table[pclass][mname]
	except KeyError:
	    pass
    def __DoFormat(self, dp, mname, allow_interesting, indent):
	# We might as well do inheritance. Find the most
	# specific interface based on the input class,
	# either the interesting class or the root class.
	# do a depth-first left-to-right search.
	# SAM 12/1299: Finally realized that a datapoint can
	# be interesting for more than one reason. So I will now
	# PASS IN the interesting datapoint as the allow_interesting
	# argument.
	meth = None
	basic = None
	r = None
	if allow_interesting:
	    meth = self.__FindMethod(allow_interesting.__class__, mname)
	if meth is None and hasattr(dp, "basic") and dp.basic:
	    meth = self.__FindMethod(dp.basic.__class__, mname)
	    if meth is None: basic = dp.basic
	if meth:
	    r = meth(dp, allow_interesting, indent)
	if r is not None:
	    return r
	elif hasattr(allow_interesting, mname):
	    r = apply(getattr(allow_interesting, mname),
		      (allow_interesting, indent))
	if r is not None:
	    return r
	elif basic:
	    r = apply(getattr(basic, mname),
		      (allow_interesting, indent))
	return r
    def __FindMethod(self, pclass, mname):
	try:
	    return self.interface_table[pclass][mname]
	except KeyError:
	    for p in pclass.__bases__:
		return self.__FindMethod(p, mname)
	return None
    def _Format(self, dp, allow_interesting = None, indent = 0):	
	return self.__DoFormat(dp, "_Format",
			       allow_interesting, indent)
    def _FormatHeader(self, dp, allow_interesting = None, indent = 0):
	return self.__DoFormat(dp, "_FormatHeader",
			       allow_interesting, indent)    
    def _FormatTime(self, dp, allow_interesting = None, indent = 0):
	return self.__DoFormat(dp, "_FormatTime",
			       allow_interesting, indent)
    def _FormatKeys(self, dp, allow_interesting = None, indent = 0):
	return self.__DoFormat(dp, "_FormatKeys",
			       allow_interesting, indent)
    def _FormatKey(self, dp, allow_interesting = None, indent = 0):
	return self.__DoFormat(dp, "_FormatKey",
			       allow_interesting, indent)
    def _FormatData(self, dp, allow_interesting = None, indent = 0):
	return self.__DoFormat(dp, "_FormatData",
			       allow_interesting, indent)
    def _FormatAnchor(self, dp, allow_interesting = None, indent = 0):
	return self.__DoFormat(dp, "_FormatAnchor",
			       allow_interesting, indent)
    def _FormatFooter(self, dp, allow_interesting = None, indent = 0):
	return self.__DoFormat(dp, "_FormatFooter",
			       allow_interesting, indent)
    def FormatString(self, str):
	return str
    # And these functions format tables.
    def FormatSeparator(self):
	return "\n"
    def FormatTitle(self, title):
	return title + "\n"
    def FormatParagraph(self, text):
	return text + "\n"
    def FormatTable(self, headers, rows):
	strs = []
	if headers:
	    numcols = len(headers)
	    widths = map(len, headers)
	else:
	    numcols = len(rows[0])
	    widths = [0] * numcols
	# Cols determines the width. If any row is too wide or
	# too narrow, columns are added or dropped.
	for row in rows:
	    for i in range(numcols):
		try:
		    elt = row[i]
		except: elt = ""
		if len(elt) > widths[i]:
		    widths[i] = len(elt)
	# Now I should have a set of widths.
	if headers:
	    header_str = map(string.ljust, headers, widths)
	    strs.append(string.join(header_str) + "\n")
	    sep_str = map(lambda x: "-" * x, widths)
	    strs.append(string.join(sep_str) + "\n")
	for row in rows:
	    if len(row) > numcols:
		row = row[:numcols]
	    elif len(row) < numcols:
		row = row + ((numcols - len(row)) * [""])
	    row_str = map(string.ljust, row, widths)
	    strs.append(string.join(row_str) + "\n")
	return string.joinfields(strs, "")
