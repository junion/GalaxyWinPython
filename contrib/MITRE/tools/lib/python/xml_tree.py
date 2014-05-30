# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# In this class, I will build a tree representation of the XML.
# Once we have the tree, we can use it to do any number of things,
# for instance, interpret rules which describe the "interesting"
# information.

import xmllib, string, sys, url_directory, xdrlib, cgi

# I *can* use XDR as a cache encoding, but it turns out to be
# quite slow for encoding, and not considerably faster for
# reading. Sigh. Just parse the damn XML.

XMLXDRClassTable = {1: "Comment",
		    3: "Data",
		    4: "Tag",
		    5: "XMLDocument"}

def _find_class_ID(cname):
    for key, value in XMLXDRClassTable.items():
	if value == cname:
	    return key
    return None    

class Comment:
    xdr_class = _find_class_ID("Comment")
    def __init__(self, data = None):
	self.parent = None
	self.Instantiate(data)
    def Instantiate(self, data):
	self.data = data
    def render(self, pprint = 0, indent = 0, long = 1):
	if long:
	    s = "<!--" + self.data + "-->"
	else:
	    s = "(comment)"
	if pprint:
	    return (indent * " " ) + s
	else:
	    return s
    def Copy(self, class_table = None):
	c = self.__class__
	try:
	    if class_table:
		c = class_table["Comment"]
	except:
	    pass
	return c(self.data)
    def Finish(self):
	pass
    def Pack(self, packer):
	packer.pack_int(self.xdr_class)
	packer.pack_string(self.data)
    def Unpack(self, unpacker, class_table):
	self.Instantiate(unpacker.unpack_string())
    def _PrepareForDeletion(self):
	self.parent = None

class Data:
    xdr_class = _find_class_ID("Data")
    def __init__(self, data = None):
	self.parent = None
	self.Instantiate(data)
    def Instantiate(self, data):
	self.data = data
    def _Augment(self, more_data):
	if self.data is None:
	    self.data = more_data
	else:
	    self.data = self.data + more_data
    def render(self, pprint = 0, indent = 0, long = 1):
	if long:
	    s = cgi.escape(self.data)
	else:
	    s = "(data)"
	if pprint:
	    return (indent * " " ) + s
	else:
	    return s
    def Copy(self, class_table = None):
	c = self.__class__
	try:
	    if class_table:
		c = class_table["Data"]
	except:
	    pass
	return c(self.data)
    def Finish(self):
	self.data = string.strip(self.data)
    def Pack(self, packer):
	packer.pack_int(self.xdr_class)
	packer.pack_string(self.data)
    def Unpack(self, unpacker, class_table):
	self.Instantiate(unpacker.unpack_string())    
    def _PrepareForDeletion(self):
	self.parent = None

class _RecursiveElement:
    
    def Instantiate(self, children = None):
	self.parent = None
	self.children = []
	if children:
	    for c in children:
		self.AddChild(c)

    def Finish(self):
	for child in self.children:
	    child.Finish()

    def AddChild(self, child):
	child.parent = self
	self.children.append(child)

    def AddChildAtLocation(self, child, index = None,
			   ref_child = None, after_ref = 1):
	child.parent = self
	if index is not None:
	    self.children[index:index] = [child]
	elif (ref_child is not None) and \
	     (ref_child in self.children):
	    i = self.children.index(ref_child)
	    if after_ref:
		self.children[i+1:i+1] = [child]
	    else:
		self.children[i:i] = [child]
	else:
	    self.children.append(child)
    
    def _PrepareForDeletion(self):
	self.parent = None
	for elt in self.children:
	    elt._PrepareForDeletion()

class Tag(_RecursiveElement):
    xdr_class = _find_class_ID("Tag")
    def __init__(self, name = None, properties = None, \
		 children = None):
	self.Instantiate(name, properties, children)
    
    def Instantiate(self, name, properties, children):
	self.name = name
	if not properties:
	    self.properties = {}
	else: self.properties = properties
	_RecursiveElement.Instantiate(self, children)

    def Copy(self, class_table = None):
	new_props = {}
	for key, val in self.properties.items():
	    new_props[key] = val
	c = self.__class__
	try:
	    if class_table:
		c = class_table["Tag"]
	except:
	    pass
	return c(self.name, new_props,
		 map(lambda x, t = class_table: x.Copy(t), self.children))

    def render_properties(self):
	if not self.properties:
	    return ""
	else:
	    strlist = [""]
	    for key, value in self.properties.items():
		if type(value) is not type(""):
		    value = `value`
		strlist.append(cgi.escape(key)+"=\""+cgi.escape(value)+"\"")
	    return string.join(strlist)

    def __render_tag(self, short = 0, end_tag = 0, indent = 0):
	if end_tag: end_t = "/>"
	else: end_t = ">"
	indent_str = (indent * " ")
	if not short:
	    return indent_str + ("<%s%s%s" % (self.name, self.render_properties(), end_t))
	else:
	    return indent_str + ("<%s...%s" % (self.name, end_t))

    # If the children of the tag aren't tags themselves, it will
    # be important to add indents and newlines. Any sequence of
    # non-Tag stuff should be handled that way. 
	
    def render(self, pprint = 0, indent = 0, long = 1):
	if self.children:
	    strlist = [self.__render_tag(short = not long,
					 indent = indent,
					 end_tag = 0)]
	    for element in self.children:
		if type(element) is type(""):
		    if not long: element = "(data)"
		    if pprint:
			strlist.append(((indent + 2) * " ") + cgi.escape(element))
		    else:
			strlist.append(cgi.escape(element))
		elif type(element) is type(self):
		    # if it's an instance, it better have a
		    # render method.
		    if pprint:
			strlist.append(element.render(pprint = pprint,
						      indent = indent + 2,
						      long = long))
		    else:
			strlist.append(element.render(pprint = pprint,
						      indent = 0,
						      long = long))
	    if pprint:
		strlist.append((indent * " ") + "</%s>" % self.name)
		sep = "\n"
	    else:
		strlist.append("</%s>" % self.name)
		sep = ""
	    return string.joinfields(strlist, sep)
	else:
	    return self.__render_tag(short = not long, end_tag = 1, indent = indent)
    def Pack(self, packer):
	# First, we pack the type.
	packer.pack_int(self.xdr_class)
	# Then we pack the name.
	packer.pack_string(self.name)
	# Then we pack the properties.
	packer.pack_int(len(self.properties))
	for key, value in self.properties.items():
	    packer.pack_string(key)
	    packer.pack_string(value)
	# Then we pack the children.
	packer.pack_int(len(self.children))
	for child in self.children:
	    child.Pack(packer)
    def Unpack(self, unpacker, class_table):
	# First, we unpack the name.
	name = unpacker.unpack_string()
	# Then we unpack the properties.
	num_props = unpacker.unpack_int()
	props = {}
	for i in range(num_props):
	    key = unpacker.unpack_string()
	    value = unpacker.unpack_string()
	    props[key] = value
	# Then we unpack the children.
	num_children = unpacker.unpack_int()
	children = []
	for i in range(num_children):
	    child_class = unpacker.unpack_int()
	    cname = XMLXDRClassTable[child_class]
	    c = _choose_class(cname, class_table)
	    i = c()
	    i.Unpack(unpacker, class_table)
	    children.append(i)
	self.Instantiate(name, props, children)
    
class XMLDocument(_RecursiveElement):
    def __init__(self):
	_RecursiveElement.Instantiate(self, None)
    def render(self, pprint = 0, indent = 0, long = 1):
	return string.joinfields(map(lambda x, p = pprint, i = indent, l = long: x.render(p, i, l), self.children), "\n")
    def describe(self):
	return string.joinfields(map(lambda x: x.render(pprint = 1, long = 0), self.children), "\n")
    def Pack(self, packer):
	packer.pack_int(len(self.children))
	for child in self.children:
	    child.Pack(packer)
    def Unpack(self, unpacker, class_table):
	num_children = unpacker.unpack_int()	
	self.children = []
	for i in range(num_children):
	    child_class = unpacker.unpack_int()
	    cname = XMLXDRClassTable[child_class]
	    c = _choose_class(cname, class_table)
	    i = c()
	    i.Unpack(unpacker, class_table)
	    self.AddChild(i)
    def DocumentProperty(self, prop):
	# This better have exactly one child.
	# Actually, it seems that comments don't count.
	for gc_log_element in self.children:
	    if isinstance(gc_log_element, Tag):
		try:
		    return gc_log_element.properties[prop]
		except KeyError:
		    return None
    def SetDocumentProperty(self, prop, val):
	# This better have exactly one child.
	# Actually, it seems that comments don't count.
	for gc_log_element in self.children:
	    if isinstance(gc_log_element, Tag):
		gc_log_element.properties[prop] = val
		return
    def _PrepareForDeletion(self):
	# In order to truly free this element, we need to
	# remove all the circular references, for instance,
	# the parent links.
	for elt in self.children:
	    elt._PrepareForDeletion()	

XMLClassTable = {"Comment": Comment,
		 "Data": Data,
		 "Tag": Tag,
		 "XMLDocument": XMLDocument}

def _choose_class(class_name, class_table):
    global XMLClassTable
    if class_table and class_table.has_key(class_name):
	return class_table[class_name]
    else:
	return XMLClassTable[class_name]

class TreeBuildingParser(xmllib.XMLParser):
    def __init__(self, verbose = 0, class_table = None):
	xmllib.XMLParser.__init__(self)
	self.class_table = class_table
	self.cur_tag = [self.choose_class("XMLDocument")()]
	self.tree_verbose = verbose
    def close(self):
	xmllib.XMLParser.close(self)
	# Now, make sure we "finish" all the tags.
	self.cur_tag[0].Finish()
    def choose_class(self, class_name):
	return _choose_class(class_name, self.class_table)
    def handle_starttag(self, tag, method, attrs):
	# No need to call parent, since I'm not actually
	# formatting or parsing.
	# htmllib.HTMLParser.handle_starttag(self, tag, method, attrs)
	# Add a tag as the child.
	new_tag = self.choose_class("Tag")(tag, attrs)
	if self.tree_verbose: print "Pushing", tag, attrs
	self.cur_tag.append(new_tag)
    def unknown_starttag(self, tag, attrs):
	self.handle_starttag(tag, None, attrs)
    def unknown_endtag(self, tag):
	self.handle_endtag(tag, None)
    def handle_endtag(self, tag, method):	
	# No need to call parent, since I'm not actually
	# formatting or parsing.
	# htmllib.HTMLParser.handle_endtag(self, tag, method)
	# Just pop.
	if self.tree_verbose:
	    print "Popping", tag, "at end"
	self.cur_tag[-2].AddChild(self.cur_tag[-1])
	self.cur_tag[-1:] = []
    def handle_comment(self, data):
	# No need to call parent method.
	self.cur_tag[-1].AddChild(self.choose_class("Comment")(data))
    # Python 1.5.2 has a bug in it where handle_entityref is
    # never called. So we don't need to worry about it. And
    # handle_charref just unpacks the data, which is what we want.
    def handle_data(self, data):
	# Just add the data to the children
	# If the data is entirely whitespace, I think I'll skip it.
	# Unless the previous child was data too. I should collapse
	# contiguous data.
	# 1/6/00: Oops. Stripping whitespace isn't such a
	# good idea, since some people like their whitespace.
	# What I'll do is accumulate all the data, and then
	# strip at the END. I still need to strip leading space, at
	# least, due to the indenting during pretty printing.
	if self.cur_tag[-1].children and \
	   isinstance(self.cur_tag[-1].children[-1], Data):
	    self.cur_tag[-1].children[-1]._Augment(data)
	elif string.strip(data):
	    self.cur_tag[-1].AddChild(self.choose_class("Data")(data))

# We'll adopt the convention that "-" is stdin.

NoXMLFile = "NoXMLFile"

def xml_to_tree(filename, class_table = None):    
    p = TreeBuildingParser(class_table = class_table)
    if isinstance(filename, url_directory.BasePath):
	try:
	    lines = filename.Readlines()
	except IOError:
	    raise NoXMLFile, filename.PathString()
	s = string.joinfields(lines, "")
	path = filename
    elif filename == "-":
	s_list = []
	s = sys.stdin.read()
	while s:
	    s_list.append(s)
	    s = sys.stdin.read()
	s = string.joinfields(s_list, "")
	path = None
    else:
	try:
	    fp = open(filename, "r")
	except IOError:
	    raise NoXMLFile, filename
	s = fp.read()
	fp.close()
	path = url_directory.FilePath(filename)
    p.feed(s)
    p.close()
    p.cur_tag[0].path = path
    return p.cur_tag[0]

import xdrlib

def xml_to_xdr(doc):
    p = xdrlib.Packer()
    doc.Pack(p)
    return p.get_buffer()

def xdr_to_xml(str, class_table):
    p = xdrlib.Unpacker(str)
    c = _choose_class("XMLDocument", class_table)
    i = c()
    i.Unpack(p, class_table)
    return i
