# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# In this file, I put all the things which allow me to
# list and walk arbitrary URLs.

# We also want to be able to search remote hosts via HTTP.
# So I'll use urllib and urlparse. There's a problem with proxies
# with urllib and urlparse, but I can fix that.

# 1.5 has some bugs in FancyURLopener, some of which are fixed
# in 1.5.2, but not all.

import urllib, urlparse, htmllib, formatter, os, string, stat, errno

# This is mine.

import io_device

# This will work for Apache.

NotDirectoryError = "NotDirectoryError"
PathError = "PathError"

# We're going to complicate the match a little bit in
# order to do the modification time checking for HTTP.
# But not right now.

class DirListParser(htmllib.HTMLParser):
    def __init__(self, dir_path, *args, **kw):	
	apply(htmllib.HTMLParser.__init__, (self,) + args, kw)
	self.dir_path = dir_path
	self.dir_list = []
    def DigestDirList(self, contents):
	self.dir_list = []
	self.found_index = 0
	self.feed(contents)
	if not self.found_index:
	    raise NotDirectoryError, self.dir_path.PathString()
	return self.dir_list
    def start_title(self, attrs):
	self.save_bgn()
    def end_title(self):
	buf = string.strip(self.save_end())
	if string.find(buf, "Index of") != 0:
	    raise NotDirectoryError, self.dir_path.PathString()
	self.found_index = 1
    def anchor_bgn(self, href, name, type):
	self.cur_anchor = href
	self.save_bgn()
    def anchor_end(self):
	buf = string.strip(self.save_end())
	# I can't just compare the buf and the current anchor,
	# because the Apache server truncates. So I need to find
	# the truncation and compare the prefix.	
	truncation_loc = string.find(buf, "..>")
	if truncation_loc > -1:
	    # Match the truncation.
	    if self.cur_anchor[:truncation_loc] == buf[:truncation_loc]:
		# If it's a prefix
		self.dir_list.append(self.cur_anchor)
	elif buf == self.cur_anchor:
	    # It has to be an exact match
	    self.dir_list.append(buf)

# In 1.5.2, there's a new data argument to the error codes.
# This won't cause a problem for 1.5. 

# urllib.__version__ was 1.9 for 1.5, 1.10 for 1.5.2.
# It seems that we only need to override some of these methods for 
# 1.5. In 1.5.1, the version was already bumped to 1.10, but I'm 
# not sure all the relevant changes had been made by then.

class CacheingURLopener(urllib.FancyURLopener):
    def __init__(self, proxies, io):
	self.io = io
	urllib.FancyURLopener.__init__(self, proxies)
    if urllib.__version__ == "1.9":
	def http_error_401(self, url, fp, errcode, errmsg, headers):
	    if headers.has_key('www-authenticate'):
		stuff = headers['www-authenticate']
		import re
		match = re.match('[ \t]*([^ \t]+)[ \t]+realm="([^"]*)"', stuff)
		if match:
		    # This was group() in 1.5.
		    scheme, realm = match.groups()
		    if string.lower(scheme) == 'basic':
			return self.retry_http_basic_auth(url, realm)
	def retry_http_basic_auth(self, url, realm):
	    # 1.5 doesn't handle the special case for proxies.
	    if type(url) == type(""):
		host, selector = urllib.splithost(url)
	    else:
		host, selector = url
	    i = string.find(host, '@') + 1
	    host = host[i:]
	    user, passwd = self.get_user_passwd(host, realm, i)
	    if not (user or passwd): return None
	    if type(url) == type(""):
		host = user + ':' + passwd + '@' + host
		newurl = '//' + host + selector
		return self.open_http(newurl)
	    else:	    
		# If this is a proxy URL.
		urltype, rest = urllib.splittype(selector)
		realhost, rest = urllib.splithost(rest)
		user_passwd, realhost = urllib.splituser(realhost)
		# Reconstruct using encoded host/passwd.
		selector = "http://%s:%s@%s%s" % (user, passwd, realhost, rest)
		proxy_host, ignore = url
	    return self.open_http((proxy_host, selector))
    def get_user_passwd(self, host, realm, i):
	# Here, we cache the user and passwd for reuse. This
	# will only work if each search is a child of the
	# previous one, which is true in our case.
	if not (hasattr(self, "user") and hasattr(self, "passwd")):
	    self.user, self.passwd = urllib.FancyURLopener.get_user_passwd(self, host, realm, i)
	return self.user, self.passwd
    def prompt_user_passwd(self, host, realm):
	# Override this in a GUI environment!
	# Not clear how to check if stty works, since
	# subshell seems to screw up the checking anyway.	
	try:
	    # Making sure that prompt is printed, even under process
	    # control.
	    user = self.io.Prompt("Enter username for %s at %s: " % (realm, host))
	    self.io.EchoOff()
	    try:
		passwd = self.io.Prompt("Enter password for %s in %s at %s (clear text): " % (user, realm, host))
	    finally:
		self.io.EchoOn()
		return user, passwd
	except KeyboardInterrupt:
	    return None, None
	except io_device.PromptFailed:
	    return None, None

class BasePath:
    protocol = None
    def __init__(self, path, io = None, prefix = None):
	if not io:
	    self.io_device = io_device.DefaultIODevice()
	else:
	    self.io_device = io
	# Provisionally assign fullpath so that Isabs()
	# will work.
	self.path = self.fullpath = path
	self.prefix_obj = None
	if prefix and not self.Isabs():
	    if not isinstance(prefix, BasePath):
		prefix = Path(prefix)
	    self.fullpath = os.path.join(prefix.fullpath, self.path)
	    self.prefix_obj = prefix

    def Join(self, path):
	# Don't grab the fullpath yet.
	if isinstance(path, BasePath):
	    path = path.path
	return self.__class__(path, self.io_device, prefix = self)

    def Split(self):
	dir, file = os.path.split(self.fullpath)
	dpath = self.__class__(dir, self.io_device)
	return dpath, file
	    
    def Walk(self, fn, args):
	# Stolen from posixpath.py. We'll
	# let PathError be raised here.
	try:
	    names = self.Listdir()
	except NotDirectoryError, dir:
	    raise PathError, (dir + ": Not directory")
	except os.error, (num, msg):
	    raise PathError, (self.PathString() + ": " + msg)
	fn(args, self, names)
	exceptions = ('.', '..')
	for name in names:
	    if name.path not in exceptions:
		if name.Isdir() and not name.Islink():
		    name.Walk(fn, args)

    def Components(self):
	components = []
	split = None
	p = self.fullpath
	while p != split:
	    head, tail = os.path.split(p)
	    if tail:
		components.append(tail)
	    split = p
	    p = head
	# Add the root element.
	components.append(p)
	components.reverse()
	return components

    def Isabs(self):
	raise PathError, "Isabs() ot implemented"

    def Isdir(self):
	raise PathError, "Isdir() not implemented"

    def Islink(self):
	raise PathError, "Islink() not implemented"

    def Readlines(self):
	raise PathError, "Realines() not implemented"

    def WriteStream(self):
	# WriteStream should create the directories
	# if possible.
	raise PathError, "WriteStream() not implemented"

    def Listdir(self):
	raise PathError, "Listdir() not implemented"

    def Exists(self):
	raise PathError, "Exists() not implemented"

    def Listdirnames(self):
	# The reason this is so convoluted is that in the default
	# case, we need to cache some information on the children
	# (so, for instance, HTTP directory listings tell you
	# if the child is a directory itself or not).
	files = self.Listdir()
	return map(lambda x: x.Split()[1], files)

    def LastModified(self):
	raise PathError, "LastModified() not implemented"

    def PathString(self):
	# This is for pretty printing. Usually just
	# the fullpath.
	return self.fullpath

class FilePath(BasePath):
    protocol = "file"
    def __init__(self, path, io = None, prefix = None):
	# If the path has a file: prefix, we had better
	# strip it.
	path = urlparse.urlparse(path)[2]
	BasePath.__init__(self, path, io, prefix)
    def Listdir(self):
	try:
	    names = os.listdir(self.fullpath) 
	    return map(lambda x, p = self: FilePath(x, prefix = p), names)
	except os.error, (num, msg):
	    if msg == 20:
		raise NotDirectoryError, self.fullpath
	    else:
		raise PathError, (self.fullpath + ": " + msg)
    def Isdir(self):
	return os.path.isdir(self.fullpath)
    def Islink(self):
	return os.path.islink(self.fullpath)
    def Isabs(self):
	return os.path.isabs(self.fullpath)
    def Exists(self):
	return os.path.exists(self.fullpath)
    def Readlines(self):
	if self.Isdir():
	    raise PathError, "Not file"
	# open the file and read the contents.
	fp = open(self.fullpath, "r")
	lines = fp.readlines()
	fp.close()
	return lines
    def WriteStream(self, recurse = 1):	    
	# WriteStream should create the directories
	# if possible.
	try:
	    return open(self.fullpath, "w")
	except IOError, (num, desc):
	    if recurse == 0:
		raise PathError, "Can't open write stream"
	    if num == errno.ENOENT:
		# Not found, try creating the directory
		# and doing it again.
		dir, file = os.path.split(self.fullpath)
		if dir == self.fullpath:
		    # Special case, we're at the root.
		    raise PathError, "Can't open write stream"
		else:
		    _recursive_mkdir(dir)
		    return self.WriteStream(recurse = 0)
	    elif num == errno.EACCES:
		# Permission denied, fail.
		raise PathError, "Permission denied"
	    else:
		# Unknown error, fail.
		raise PathError, "Can't open write stream"
	except:
	    raise PathError, "Can't open write stream"
    def LastModified(self):
	return os.stat(self.fullpath)[stat.ST_MTIME]

# Create a directory. If it doesn't work, try recursing.
# If the recursion was successful, try creating again.
# If I ever do another protocol which supports writing and
# creating directories (i.e., FTP), I'll generalize this.

# First, set the umask.

def _recursive_mkdir(dir):
    try:
	oldmask = os.umask(0)
	__mkdir(dir, recurse = 1)
    finally:
	os.umask(oldmask)

def __mkdir(dir, recurse = 1):
    try:
	os.mkdir(dir)
    except os.error, (inum, desc):
	if recurse == 0:
	    raise PathError, "Can't create " + dir
	if inum == errno.ENOENT:
	    # No directory, recurse.
	    lastdir = dir
	    dir, file = os.path.split(dir)
	    if dir == lastdir:
		raise PathError, "Can't create " + dir
	    else:
		__mkdir(dir, recurse = 1)
		__mkdir(lastdir, recurse = 0)
	elif inum == errno.EACCES:
	    # Permission denied, fail.
	    raise PathError, "Can't create " + dir + " (permission denied)"
	else:
	    # Unknown error, fail.
	    raise PathError, "Can't create " + dir

class HttpPath(BasePath):
    protocol = "http"
    def __init__(self, path, io = None, prefix = None, proxies = None):
	p = urlparse.urlparse(path)
	self.host = p[1]
	path = p[2]
	BasePath.__init__(self, path, io, prefix)
	if not proxies:	    
	    self.proxies = {"http": "http://gatekeeper.mitre.org"}
	else:
	    self.proxies = proxies	    
	self.url_accessor = CacheingURLopener(self.proxies, self.io_device)
	self.elements = None
	self.listed = 0
	self.checked = 0
	self.isdir = 0

    def Split(self):
	dpath, file = BasePath.Split(self)
	dpath.url_accessor = self.url_accessor
	dpath.host = self.host
	return dpath, file

    def Join(self, path):
	new_elt = BasePath.Join(self, path)
	new_elt.url_accessor = self.url_accessor
	new_elt.host = self.host
	return new_elt

    def Readlines(self):
	if self.Isdir():
	    raise PathError, "Not file"
	# open the file and read the contents.
	fp = self.url_accessor.open(self.PathString())
	lines = fp.readlines()
	fp.close()
	return lines
	
    def Listdir(self):
	if self.listed:
	    if self.elements is None:
		raise NotDirectoryError, self.PathString()
	    return self.elements
	if self.checked:
	    if not self.isdir:
		raise NotDirectoryError, self.PathString()
	fp = self.url_accessor.open(self.PathString())
	contents = fp.read()
	# lines = fp.readlines()
	# contents = string.joinfields(lines, "")
	fp.close()
	self.listed = 1
	# This may raise NotDirectoryError
	d = DirListParser(self, formatter.NullFormatter())
	new_elts = d.DigestDirList(contents)
	final = []
	for elt in new_elts:
	    local_isdir = 0
	    if elt[-1] == "/":
		# The Apache server returns a trailing slash
		# for each directory element. So we can use
		# that here.
		local_isdir = 1
		elt = elt[:-1]
	    new_elt = self.Join(elt)
	    new_elt.checked = 1
	    new_elt.isdir = local_isdir
	    final.append(new_elt)
	self.elements = final
	return self.elements
    def Isdir(self):
	if self.checked: return self.isdir
	try:
	    self.Listdir()
	    self.checked = 1
	    self.isdir = 1
	    return 1
	except NotDirectoryError:
	    self.checked = 1
	    self.isdir = 0
	    return 0
    def Islink(self):
	return 0
    def Isabs(self):
	# We'll take a look at whether there's a leading
	# slash in the path.
	if self.fullpath[0] == "/":
	    return 1
	else:
	    return 0    
    def Exists(self):
	# This is awfully expensive to check, so I'll just punt.
	return 1
    def WriteStream(self):
	# WriteStream should create the directories
	# if possible.
	raise PathError, "Can't open write stream"
    def PathString(self):
	return urlparse.urlunparse(("http", self.host,
				    self.fullpath, None,
				    None, None))    
    def LastModified(self):
	# Right now, we don't know this yet.
	return 0

# urlparse returns (scheme, netloc, path, params, query, fragment).
# <scheme>://<netloc>/<path>;<params>?<query>#<fragment>
# I'll be primarily interested in the scheme, netloc and path. 
# If the scheme is file and there's a netloc, the scheme is 
# actually ftp. We'll live. Windows disk names are misrecognized
# as schemes, currently.

def Path(path, proxies = None, io = None, prefix = None):
    t = urlparse.urlparse(path)
    scheme = t[0]
    netloc = t[1]
    if (scheme == "file") and netloc:
	scheme = "ftp"	
    if (scheme == "") or (scheme == "file"):
	return FilePath(path, io, prefix)
    elif t[0] == "http":	
	return HttpPath(path, io, prefix, proxies)
    else:
	raise PathError, "Unsupported protocol " + t[0]

def PathFromComponents(protocol, components, absolute = 1, io = None,
		       proxies = None):
    path = apply(os.path.join, tuple(components))
    if protocol == "file":
	if absolute == 1 and path[0] != "/":
	    path = "/" + path
	return FilePath(path, io, None)
    elif protocol == "http":
	# Always absolute.
	path = "http://" + path
	return HttpPath(path, io, None, proxies)
    else:
	raise PathError, "Unsupported protocol " + protocol
