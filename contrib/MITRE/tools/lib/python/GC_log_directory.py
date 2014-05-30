# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# In this file, I create the appropriate infrastructure for a 
# single log directory, including the cache, the designated
# files, and the listing for other files eligible for anchoring. 
# Much of this infrastructure was taken from the original
# MIT_log.py structure.

# This directory should
# contain no directories besides a directory called .cache. It must
# have SOME logfile to start from, either a raw text log, a raw XML 
# log, or an annotated log. There will be a default pattern for each:

import re, string, os
from types import StringType

RAW_TEXT_LOG_PATS = [re.compile("^.+-\d{8,8}-\d{3,3}-hublog[.]txt$")]

RAW_XML_LOG_PATS = [re.compile("^.+-\d{8,8}-\d{3,3}-hublog-raw[.]xml$")]

ANN_XML_LOG_PATS = [re.compile("^.+-\d{8,8}-\d{3,3}-hublog-annotated[.]xml$")]

HUMAN_XML_LOG_PATS = [re.compile("^.+-\d{8,8}-\d{3,3}-hublog-human[.]xml$"),
		      re.compile("^.+-\d{8,8}-\d{3,3}-human[.]xml$")]

# It is an error for any pattern to match more than one file 
# in the directory.

# The cache will contain raw.cache, annotated.cache, human.cache.

import url_directory, io_device

import xml_visualization, xml_validation, xml_interleave

LogDirectoryError = "LogDirectoryError"
InvalidLogDirectory = "InvalidLogDirectory"

# The conversion step is the method by which you produce
# this document. If there's no conversion step, and no document,
# and no cache, there's nothing you can do. 

class FileSpec:
    def __init__(self, pattern, log_dir, cache_name, conversion_step):
	self.conversion_step = conversion_step
	self.file = None
	self.write_file = None
	self.doc = None
	self.log_dir = log_dir
	self.cache = None
	self.cache_name = cache_name
	if type(pattern) is not type([]):
	    patterns = [pattern]
	else: patterns = pattern
	self.patterns = []
	for pat in patterns:
	    if type(pat) is StringType:
		self.patterns.append(re.compile(pat))
	    else:
		self.patterns.append(pat)
    def MaybeCache(self, file, maybe_cache):
	if self.cache_name == maybe_cache:
	    self.cache = file
	    return 1
	return 0
    def MaybeSave(self, file, maybe_name):
	m = None
	for p in self.patterns:
	    m = p.match(maybe_name)
	    if m: break	
	if m is not None:
	    if self.file is not None:
		raise LogDirectoryError, \
		      ("%s: patterns %s match too many files" % (self.log_dir.path.PathString(), string.joinfields(map(lambda x: '"'+x.pattern+'"', self.patterns), ", ")))
	    else:
		self.file = self.write_file = file
		return 1
	else: return 0
    def _describe(self):
	if self.file:
	    self.log_dir.io.ReportOutput("File: " + self.file.fullpath)
	else:
	    self.log_dir.io.ReportOutput("No file.")
	if self.cache:
	    self.log_dir.io.ReportOutput("Cache: " + self.cache.fullpath)
	else:
	    self.log_dir.io.ReportOutput("No cache.")
    def Generate(self, force = 0, read_cache = 1, write_cache = 0,
		 write_file = 0, **kw):
	# First, if it's forced and there's a conversion step,
	# ignore what there is and run the conversion step.
	read_from_cache = 0
	read_from_file = 0
	if (not force) and self.doc:
	    return self.doc
	elif force and self.conversion_step and \
	     self.conversion_step.Enabled():
	    self.doc = self.conversion_step.Convert(force, read_cache,
						    write_cache, write_file,
						    kw)
	elif read_cache and self.cache and self.file and \
	     self.cache.LastModified() > self.file.LastModified():
	    # Don't read from the cache unless there's a
	    # file to compare it against.
	    self.doc = self.LoadDocumentFromCache()
	    read_from_cache = 1
	elif self.file:
	    self.doc = apply(self.LoadDocumentFromFile, (), kw)
	    read_from_file = 1
	elif self.conversion_step:
	    self.doc = self.conversion_step.Convert(force, read_cache,
						    write_cache, write_file,
						    kw)
	if self.doc and write_cache and \
	   self.cache_name and (not read_from_cache):
	    self.WriteDocumentToCache()
	if self.doc and write_file and \
	   self.write_file and (not read_from_file):
	    self.WriteDocumentToFile()
	return self.doc
    def LoadDocumentFromCache(self):
	return None
    def LoadDocumentFromFile(self, **arg_dict):
	return None
    def WriteDocumentToCache(self):
	pass
    def WriteDocumentToFile(self, catch_errors = 1):
	pass
    def Generatable(self):
	# A spec is generatable if it has either a
	# file or a doc, or if it has a conversion step and
	# the conversion step is enabled.
	return self.doc or self.file or \
	       (self.conversion_step and self.conversion_step.Enabled())
    def _Flush(self):
	if self.doc:
	    # It's not enough to just del it. You have to make sure that
	    # there are no circular references, so that the reference
	    # count really goes to 0.
	    self.doc._PrepareForDeletion()
	    del self.doc
	    self.doc = None

import GC_log_formatting

class XMLFileSpec(FileSpec):
    def Generate(self, force = 0, read_cache = 1, write_cache = 0,
		 write_file = 0, **kw):
	doc = apply(FileSpec.Generate, \
		    (self, force, read_cache, write_cache, write_file), kw)
	if doc is not None:
	    doc.AddLogAndFormatClasses(self.log_dir)
	return doc
    def LoadDocumentFromCache(self):
	self.log_dir.io.ReportStatus("Reading cache: " + self.cache.PathString())
	cache_contents = string.joinfields(self.cache.Readlines(), "")
	d = GC_log_formatting.xdr_to_formattable_tree(cache_contents)
	self.log_dir.io.ReportStatus("...read.")
	return d
    def LoadDocumentFromFile(self, **arg_dict):
	return GC_log_formatting.xml_to_formattable_tree(self.file)
    def WriteDocumentToCache(self):
	# First, make sure there's a cache document name.
	if not self.cache:
	    self.cache = self.log_dir.path.Join(".cache").Join(self.cache_name)
	self.log_dir.io.ReportStatus("Writing cache: " + self.cache.PathString())
	try:	    
	    fp = self.cache.WriteStream()
	    str = xml_tree.xml_to_xdr(self.doc)
	    fp.write(str)
	    fp.close()
	    self.log_dir.io.ReportStatus("...written.")
	except url_directory.PathError, desc:
	    self.log_dir.io.ReportStatus("...failed (%s)" % desc)
    def WriteDocumentToFile(self, catch_errors = 1):
	self.log_dir.io.ReportStatus("Writing file: " + self.write_file.PathString())
	try:
	    fp = self.write_file.WriteStream()
	    fp.write(self.doc.render(pprint = 1))
	    fp.close()
	except url_directory.PathError, desc:
	    if not catch_errors:
		raise url_directory.PathError, desc
	    else:
		self.log_dir.io.ReportStatus("...failed (%s)." % desc)

class RawXMLFileSpec(XMLFileSpec):
    def __init__(self, pattern, log_dir, cache_name, conversion_step):
	if not pattern:
	    pattern = RAW_XML_LOG_PATS
	FileSpec.__init__(self, pattern, log_dir, cache_name, conversion_step)
    def LoadDocumentFromFile(self, **arg_dict):
	self.log_dir.io.ReportStatus("Reading raw XML log: " + self.file.PathString())
	d = apply(XMLFileSpec.LoadDocumentFromFile, (self,), arg_dict)
	self.log_dir.io.ReportStatus("...read.")
	return d	    

class HumanXMLFileSpec(XMLFileSpec):
    def __init__(self, pattern, log_dir, cache_name, conversion_step):
	if not pattern:
	    pattern = HUMAN_XML_LOG_PATS
	FileSpec.__init__(self, pattern, log_dir, cache_name, conversion_step)
    def LoadDocumentFromFile(self, **arg_dict):
	self.log_dir.io.ReportStatus("Reading human XML annotations: " + self.file.PathString())
	d = apply(XMLFileSpec.LoadDocumentFromFile, (self,), arg_dict)
	self.log_dir.io.ReportStatus("...read.")
	return d

class AnnotatedXMLFileSpec(XMLFileSpec):
    def __init__(self, pattern, log_dir, cache_name, conversion_step):
	if not pattern:
	    pattern = ANN_XML_LOG_PATS
	FileSpec.__init__(self, pattern, log_dir, cache_name, conversion_step)
    def LoadDocumentFromFile(self, **arg_dict):
	self.log_dir.io.ReportStatus("Reading annotated XML log: " + self.file.PathString())
	d = apply(XMLFileSpec.LoadDocumentFromFile, (self,), arg_dict)
	self.log_dir.io.ReportStatus("...read.")
	return d

# We need to use the MIT log parsing stuff.

import MIT_log

class RawTextFileSpec(FileSpec):
    def __init__(self, pattern, log_dir, cache_name, conversion_step):
	if not pattern:
	    pattern = RAW_TEXT_LOG_PATS
	FileSpec.__init__(self, pattern, log_dir, cache_name, conversion_step)
    def LoadDocumentFromFile(self, **arg_dict):
	try:
	    default_user_version = arg_dict["default_user_version"]
	except: default_user_version = None
	d = MIT_log.MIT_Logfile(self.file, self.log_dir.io, default_user_version = default_user_version)
	d.LoadDatapoints()
	return d    

from GC_log_formatting import DefaultFormatter
import GC_human_annotations

class LogDir:
    def __init__(self, path, raw_txt_pat = None, \
		 raw_xml_pat = None, ann_xml_pat = None, \
		 human_xml_pat = None, io = None,
		 formatter = None, repository = None):	
	self.path = path
	self.repository = repository
	if not isinstance(self.path, url_directory.BasePath):
	    self.path = url_directory.Path(self.path)
	try:
	    files = self.path.Listdir()
	except url_directory.NotDirectoryError, dir:	
	    raise InvalidLogDirectory, (dir + ": Not a directory")
	self.other_paths = {}

	self.io = io
	if not self.io:
	    self.io = io_device.DefaultIODevice()
	if not formatter:
	    self.formatter = DefaultFormatter(self)
	else:
	    self.formatter = formatter
	
	# Here we set up the relevant files.
	
	self.raw_txt = RawTextFileSpec(raw_txt_pat,
				       self, None, None)
	self.raw_xml = RawXMLFileSpec(raw_xml_pat,
				      self, "raw.cache",
				      XmlizationConversionStep(self.raw_txt))
	self.annotated_xml = AnnotatedXMLFileSpec(ann_xml_pat,
						  self, "annotated.cache",
						  AnnotationConversionStep(self.raw_xml))
	self.human_xml = HumanXMLFileSpec(human_xml_pat,
					  self, "human.cache", None)
	self.log_summary = None

	# Now we search the directories and see what 
	# matches. 
	for f in files:
	    if f.Isdir():
		if f.Split()[1] == ".":
		    continue
		if f.Split()[1] != ".cache":
		    raise InvalidLogDirectory, \
			  "Directory contains non-cache subdirs"
		else:
		    # Check to see what was annotated.
		    contents = f.Listdir()
		    name_contents = map(lambda x: x.Split()[1], contents)
		    for i in range(len(contents)):
			entry = name_contents[i]
			file = contents[i]
			self.raw_xml.MaybeCache(file, entry) or \
			self.annotated_xml.MaybeCache(file, entry) or \
			self.human_xml.MaybeCache(file, entry)
	    else:
		# Here, we check all the patterns. We save away
		# the special stuff and the other stuff we cache
		# for later anchoring.
		name = f.Split()[1]
		self.raw_txt.MaybeSave(f, name) or \
		self.raw_xml.MaybeSave(f, name) or \
		self.annotated_xml.MaybeSave(f, name) or \
		self.human_xml.MaybeSave(f, name)

		self.other_paths[name] = f

	# Now, we try to assign write files.

	# If you provide a raw text file, the appropriate patterns
	# will be generated -raw.xml, -annotated.xml, -human.xml, unless
	# you provide alternate patterns. If you provide raw XML, the 
	# appropriate patterns for annotated or human will be generated
	# as follows: suffix -annotated, -human to the filename if the
	# raw file doesn't end in -raw, substitute for -raw otherwise.
	# If you provide annotated XML, the same rules apply.

	# Some of these files may not exist because the patterns
	# for some of the earlier files need to be reused.
		
	if self.annotated_xml.file:
	    dir, file = self.annotated_xml.file.Split()
	    base, ext = os.path.splitext(file)
	    if re.search("-annotated$", base):
		base = base[:-10]
	    if (not self.human_xml.file):
		new_name = base + "-human.xml"
		if self.other_paths.has_key(new_name):
		    self.human_xml.file = self.other_paths[new_name]
		    del self.other_paths[new_name]
		# Some tools can generate the human annotation file,
		# or its stub.
	        else:
		    self.human_xml.write_file = dir.Join(new_name)
	elif self.raw_xml.file:
	    dir, file = self.raw_xml.file.Split()
	    base, ext = os.path.splitext(file)
	    if re.search("-raw$", base):
		base = base[:-4]
	    # We know there's no annotated XML.
	    new_name = base + "-annotated.xml"
	    if self.other_paths.has_key(new_name):
		self.annotated_xml.file = self.other_paths[new_name]
		del self.other_paths[new_name]
	    else:
		self.annotated_xml.write_file = dir.Join(new_name)
	    if (not self.human_xml.file):
		new_name = base + "-human.xml"
		if self.other_paths.has_key(new_name):
		    self.human_xml.file = self.other_paths[new_name]
		    del self.other_paths[new_name]
		# Some tools can generate the human annotation file,
		# or its stub.
	        else:
		    self.human_xml.write_file = dir.Join(new_name)
	elif self.raw_txt.file:	    
	    dir, file = self.raw_txt.file.Split()
	    base, ext = os.path.splitext(file)
	    # We know there's no raw or annotated XML.
	    new_name = base + "-raw.xml"
	    if self.other_paths.has_key(new_name):
		self.raw_xml.file = self.other_paths[new_name]
		del self.other_paths[new_name]
	    else:
		self.raw_xml.write_file = dir.Join(new_name)
	    new_name = base + "-annotated.xml"
	    if self.other_paths.has_key(new_name):
		self.annotated_xml.file = self.other_paths[new_name]
		del self.other_paths[new_name]
	    else:
		self.annotated_xml.write_file = dir.Join(new_name)
	    if (not self.human_xml.file):
		new_name = base + "-human.xml"
		if self.other_paths.has_key(new_name):
		    self.human_xml.file = self.other_paths[new_name]
		    del self.other_paths[new_name]
		# Some tools can generate the human annotation file,
		# or its stub.
	        else:
		    self.human_xml.write_file = dir.Join(new_name)

	# Now that we've captured all the paths, we should
	# see if there's at least one thing we can work with.

	if (not self.raw_txt.file) and \
	   (not self.raw_xml.file) and \
	   (not self.annotated_xml.file):
	    raise InvalidLogDirectory, \
		  "Directory doesn't contain any logs"

    def __repr__(self):
	return "<Log directory at %s>" % self.path.fullpath

    def _GenerateLogSummary(self, force = 0, read_cache = 1, \
			    write_cache = 0, write_file = 0, \
			    without_annotated = 0, without_human = 0, **kw):
	if self.log_summary and (force == 0):
	    return self.log_summary
	if not without_annotated:
	    anno_d = apply(self.annotated_xml.Generate,
			   (force, read_cache, write_cache, write_file), kw)
	else:
	    anno_d = None
	# The annotated version may be marked to include human annotations.
	if (anno_d is not None) and \
	   (anno_d.DocumentProperty("human_annotations_included") is not None):
	    without_human = 1
	if not without_human:
	    human_d = apply(self.human_xml.Generate,
			    (force, read_cache, write_cache, write_file), kw)
	else:
	    human_d = None
	self.log_summary = xml_interleave.LogSummary(anno_d, human_d, self)
	return self.log_summary

    def _describe(self):
	self.io.ReportOutput("For raw text:")
	self.raw_txt._describe()
	self.io.ReportOutput("For raw XML:")
	self.raw_xml._describe()
	self.io.ReportOutput("For annotated XML:")
	self.annotated_xml._describe()
	self.io.ReportOutput("For human XML:")
	self.human_xml._describe()

    def _Flush(self):
	# In order to reclaim memory during a large batch
	# run, I'm setting it up so that the docs can be deleted
	# by the repository.
	self.raw_txt._Flush()
	self.raw_xml._Flush()
	self.annotated_xml._Flush()
	self.human_xml._Flush()
	if self.log_summary:
	    del self.log_summary
	    self.log_summary = None	
	
    # This next set of methods has to do with the
    # backchaining to enable validation, scoring,
    # summarization, annotation, etc. There are five
    # "goals" you can have, and each one of them
    # has a different set of dependencies. They work
    # like this:

    # There are several stages of processing available:
    # (1) xml conversion
    # (2) annotation
    # (3) summarization
    # (4) validation
    # (5) scoring
    # The order is (1) -> (2) -> (3), (4), (5). A request for any of 
    # (3), (4), (5) will force (1) and (2) if not already done; etc.

    # The various operations are implemented elsewhere. At some points,
    # there may be a cache you can read or write, and you should
    # be able to enable or disable reading or writing the cache
    # for these stages for any instance of processing. The cache
    # code will take care of checking dates, etc., too. By default,
    # caches are both written and read from. You should also be
    # able to force reprocessing at each stage.

    # Some of these dependencies are supported by the conversion
    # steps and the documents, others directly by conversion steps.

    def Xmlize(self, force = 0, read_cache = 1, write_cache = 0,
	       write_file = 0, print_output = 0, **kw):
	d = apply(self.raw_xml.Generate,
		  (force, read_cache, write_cache, write_file), kw)
	if print_output:
	    if self.raw_xml.doc:
		self.io.ReportOutput(self.raw_xml.doc.render(pprint = 1))
	return d

    def Annotate(self, force = 0, read_cache = 1, write_cache = 0,
		 write_file = 0, print_output = 0, **kw):
	# Multiple rules files are permitted, and the first one
	# which is permissible is used.
	d = apply(self.annotated_xml.Generate,
		  (force, read_cache, write_cache, write_file), kw)
	if print_output:
	    if self.annotated_xml.doc:
		self.io.ReportOutput(self.annotated_xml.doc.render(pprint = 1))

    def Visualize(self, force = 0, read_cache = 1, write_cache = 0,
		  write_file = 0, print_output = 0, **kw):
	log_sum = apply(self._GenerateLogSummary,
			(force, read_cache, write_cache, write_file), kw)
	vtype = "raw"
	if kw.has_key("vtype"):
	    vtype = kw["vtype"]
	xml_visualization.xml_visualize(log_sum, vtype = vtype,
					task_range = self.repository._InterpretRange(kw))

    def Validate(self, force = 0, read_cache = 1, write_cache = 0,
		 write_file = 0, print_output = 0, **kw):
	log_sum = apply(self._GenerateLogSummary,
			(force, read_cache, write_cache, write_file), kw)
	xml_validation.xml_validate(log_sum)

    def Unify(self, force = 0, read_cache = 1, write_cache = 0,
	      write_file = 0, print_output = 0, **kw):
	log_sum = apply(self._GenerateLogSummary,
			(force, read_cache, write_cache, write_file), kw)
	xml_interleave.xml_unify_files(log_sum)
	if write_file:
	    self.annotated_xml.WriteDocumentToFile()
	if print_output:
	    if self.annotated_xml.doc:
		self.io.ReportOutput(self.annotated_xml.doc.render(pprint = 1))

    def Score(self, force = 0, read_cache = 1, write_cache = 0,
	      write_file = 0, print_output = 0, **kw):
	log_sum = apply(self._GenerateLogSummary,
			(force, read_cache, write_cache, write_file), kw)
	if kw.has_key("score_set"):
	    score_set = kw["score_set"]
	else:
	    self.io.ReportStatus("No score set.")
	    return
	score_set.Score(log_sum, task_range = self.repository._InterpretRange(kw, score_set.default_task_range))

    def HumanStubber(self, force = 0, read_cache = 1, write_cache = 0,
		     write_file = 0, print_output = 0, **kw):
	log_sum = apply(self._GenerateLogSummary,
			(force, read_cache, write_cache, write_file, 0, 1), kw)
	range = self.repository._InterpretRange(kw)
	self.io.ReportOutput(GC_human_annotations.GenerateHumanAnnotationStub(log_sum, range))

# The xmlization involves calling Xmlize() on the dependent document.

class XmlizationConversionStep:
    def __init__(self, raw_doc):
	self.raw_doc = raw_doc
    def Convert(self, force, read_cache, write_cache, write_file, kw):
	d = apply(self.raw_doc.Generate,
		  (force, read_cache, write_cache, write_file), kw)
	if d:
	    return d.Xmlize()
	else: return None
    def Enabled(self):
	# The step is enabled if the inputs are available. If the
	# raw_doc has an actual file or doc, we're in business.
	return self.raw_doc.Generatable()

import xml_tree, xml_transducer

class AnnotationConversionStep:
    def __init__(self, raw_xml):
	self.raw_xml = raw_xml
	self.rule_bases = []
	
    def Convert(self, force, read_cache, write_cache, write_file, kw):
	d = apply(self.raw_xml.Generate,
		  (force, read_cache, write_cache, write_file), kw)
	if not d:
	    return None
	r = self.raw_xml.log_dir.repository	
	if r and r.rule_bases:
	    self.rule_bases = []
	    for i in range(len(r.rule_bases)):
		if r.rule_bases[i] is not None:
		    try:
			new_r = xml_transducer.create_rule_base_instance(r.rule_bases[i], self.raw_xml.log_dir.io)
			r.rule_bases[i] = new_r
			self.rule_bases.append(new_r)
		    except xml_tree.NoXMLFile, file:
			self.raw_xml.log_dir.io.ReportStatus("Can't read XML from rules file: " + file)
			r.rule_bases[i] = None
	elif kw.has_key("rule_bases") and kw["rule_bases"] is not None:
	    self.rule_bases = []
	    for i in range(len(kw["rule_bases"])):
		if kw["rule_bases"][i] is not None:
		    try:
			new_r = xml_transducer.create_rule_base_instance(kw["rule_bases"][i], self.raw_xml.log_dir.io)
			self.rule_bases.append(new_r)
		    except xml_tree.NoXMLFile, file:
			self.raw_xml.log_dir.io.ReportStatus("Can't read XML from rules file: " + file)
			kw["rule_bases"][i] = None

	else:
	    self.raw_xml.log_dir.io.ReportStatus("No rules to apply.")
	if not self.rule_bases:
	    self.raw_xml.log_dir.io.ReportStatus("No rules to apply.")
	
	for rule_base in self.rule_bases:
	    try:
		rule_base.Reset()
		self.raw_xml.log_dir.io.ReportStatus("Applying rules: " + rule_base.doc.path.PathString())
		new_d = rule_base.apply_rules(d)
		self.raw_xml.log_dir.io.ReportStatus("...succeeded.")
		return new_d
	    except xml_transducer.InapplicableVersion:
		self.raw_xml.log_dir.io.ReportStatus("...wrong version.")

	# Just return the original document if nothing happened.
	return d

    def Enabled(self):
	return self.raw_xml.Generatable()

#######################################################
#
# The cache is written in XDR format, for fast reading. The root tag is
# always a single tag (or should always be). We start with the
# root tag and write out a string for the tag name, an integer for the
# number of attribute-value pairs in the tag, then each attribute-
# value pair as strings, then the number of child tags, then 
# recurse. 
#
# Every class should know how to cache itself. See xml_tree.py.
# 
#######################################################
