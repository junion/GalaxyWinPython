# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# In this file, we look for the log repository. 

# First thing we do is find all the eligible directories which 
# may have the appropriate elements. There are a number of constraints
# on these directories. First, there may be several roots provided.
# Next, there may be start and end directories, if the default patterns
# are used. Most of this will be taken from the evaluator class. 
# Finally, we need to be able to read any one of the files over
# an HTTP connection (so they all must use url_directory). 

# PROCESSING

# There are several stages of processing available:
# (1) xml conversion
# (2) annotation
# (3) summarization
# (4) validation
# (5) scoring
# The order is (1) -> (2) -> (3), (4), (5). A request for any of 
# (3), (4), (5) will force (1) and (2) if not already done; etc.
# The scoring is an agglomerative phase over the entire repository;
# the other phases devolve to the individual log directory.

import url_directory, GC_log_directory, io_device, xml_transducer
import DMA_evaluation, xml_interleave

RepositoryError = "RepositoryError"

class Repository:
    
    def __init__(self, roots = None, start = None, end = None,
		 raw_txt_pat = None, raw_xml_pat = None,
		 ann_xml_pat = None, human_xml_pat = None,
		 io = None, rule_bases = None):
	
	# First, we canonicalize the roots and start and end
	# as paths.
	self.pats = (raw_txt_pat, raw_xml_pat, ann_xml_pat, human_xml_pat)
	self.io = io
	if not self.io:
	    self.io = io_device.DefaultIODevice()
	r = []
	if type(roots) is not type(r):
	    roots = [roots]
	if roots is not None:
	    for root in roots:
		if isinstance(root, url_directory.BasePath):
		    root.io = self.io
		    r.append(root)
		else:
		    r.append(url_directory.Path(root, io = self.io))
	    roots = r
		
	if not roots:
	    roots = []
	self.dir_roots = roots
	self.start_root = self.end_root = None
	self.UpdateStartAndEnd(start, end)

	# And now, rule bases.

	self.rule_bases = rule_bases
	if self.rule_bases is None:
	    self.rule_bases = []

	self.log_directories = []
	self.directories_found = 0

    def UpdateStartAndEnd(self, start, end):
	if (start or end) and len(self.dir_roots) > 1:
	    raise RepositoryError, "Can only have start or end with a single root"
	if (start is not None):
	    if not isinstance(start, url_directory.BasePath):
		start = url_directory.Path(start, io = self.io)
	    else:
		start.io = self.io
	if (end is not None):
	    if not isinstance(end, url_directory.BasePath):
		end = url_directory.Path(end, io = self.io)
	    else:
		end.io = self.io
	if start and (not start.Isabs()):
	    if not self.dir_roots:
		raise RepositoryError, "Wrong number of roots for relative start path"
	    start = self.dir_roots[0].Join(start)
	if end and (not end.Isabs()):
	    if not self.dir_roots:
		raise RepositoryError, "Wrong number of roots for relative end path"
	    end = self.dir_roots[0].Join(end)
	
	# If there are no roots, but there's a start and an
	# end and they're absolute paths, then establish a
	# single root which is the common prefix of both.
	
	if (not self.dir_roots) and start and end and \
	   start.Isabs() and end.Isabs():
	    if start.protocol != end.protocol:
		raise RepositoryError, \
		      "Start and end access protocols don't match"	    
	    # Can't use commonprefix, since it doesn't
	    # respect directory name integrity.
	    s = start.Components()
	    e = end.Components()
	    r = []
	    for i in range(min(len(s), len(e))):
		if s[i] == e[i]:
		    r.append(s[i])
		else: break
	    self.dir_roots = [url_directory.PathFromComponents(start.protocol, r,
							       io = self.io)]
	
	# These two will only be set if there's
	# no more than a single root.
	if start:
	    self.start_root = start.Components()
	if end:
	    self.end_root = end.Components()

	# Force relook for the directories.
	self.directories_found = 0
	self.log_directories = []
        
    def FindLogDirectories(self):
	if not self.directories_found:
	    for root in self.dir_roots:
		# The roots might be cacheing URL openers, and
		# I want to get the url_accessor from one and transfer
		# it to each successive root in order not to prompt
		# for the password more than once.
		try:
		    root.Walk(self.__FindFiles, ())
		except url_directory.PathError, err:
		    self.io.ReportStatus("Error: " + err)
	    self.directories_found = 1
        
    def __FindFiles(self, args, dir, files):
	# Next, if there's a start and end, each component of the
	# candidate must be >= (<=) the corresponding component of
	# start or end. Well, not quite. If it's > (<), then stop
	# checking. If it's =, keep going.
	if self.start_root or self.end_root:
	    comps = dir.Components()
	    if self.start_root:
		for i in range(len(self.start_root)):
		    if i > (len(comps) - 1):
			# Nothing's barfed. Keep going.
			break
		    if comps[i] > self.start_root[i]:
			break
		    if comps[i] != self.start_root[i]:
			# Don't just return; don't go any farther.
			files[:] = []
			return
	    if self.end_root:
		for i in range(len(self.end_root)):		    
		    if i > (len(comps) - 1):
			# Nothing's barfed. Keep going.
			break
		    if comps[i] < self.end_root[i]:
			break
		    if comps[i] != self.end_root[i]:
			# Don't just return; don't go any farther.
			files[:] = []
			return
	# Actually, we can just do this per directory.
	self.io.ReportStatus("Checking: " + dir.PathString())
	try:
	    candidate = apply(GC_log_directory.LogDir,
			      (dir,) + self.pats, {"io": self.io,
						   "repository": self})
	except GC_log_directory.InvalidLogDirectory:
	    return
	except GC_log_directory.LogDirectoryError, err:
	    self.io.ReportStatus("Error: " + err)
	    return
	self.log_directories.append(candidate)

    def Xmlize(self, force = 0, read_cache = 1, write_cache = 0,
	       write_file = 0, print_output = 0, **kw):
	apply(self.ApplyOperation,
	      (GC_log_directory.LogDir.Xmlize, force, read_cache, write_cache,
	       write_file, print_output), kw)

    def UpdateRuleBases(self, kw):
	if kw.has_key("rule_bases") and kw["rule_bases"] is not None:
	    self.rule_bases = kw["rule_bases"]

    def _InterpretRange(self, kw, default = xml_interleave.WITHIN_TOTAL_TASK):
	if (not kw.has_key("range")) or (not kw["range"]):
	    return default
	elif kw["range"] == "overall_task":
	    return xml_interleave.WITHIN_TOTAL_TASK
	elif kw["range"] == "all_turns":
	    return xml_interleave.WITHIN_ALL_TURNS
	elif kw["range"] == "on_task":
	    return xml_interleave.WITHIN_ON_TASK
	else:
	    return default
    
    def ApplyOperation(self, op, force = 0, read_cache = 1, write_cache = 0,
		       write_file = 0, print_output = 0, **kw):
	self.FindLogDirectories()
	self.UpdateRuleBases(kw)
	for l in self.log_directories:
	    # SAM 5/18/00: If you never delete the directory
	    # representations, Python starts to eat memory like
	    # you wouldn't believe. It ought to be possible to
	    # simple delete l when you're done. I've reset everything
	    # so that it's all self-contained.
	    apply(op, (l, force, read_cache, write_cache, write_file, print_output), kw)
	    l._Flush()

    def Annotate(self, force = 0, read_cache = 1, write_cache = 0,
		 write_file = 0, print_output = 0, **kw):
	apply(self.ApplyOperation,
	      (GC_log_directory.LogDir.Annotate, force, read_cache, write_cache,
	       write_file, print_output), kw)
    
    def Visualize(self, force = 0, read_cache = 1, write_cache = 0,
		  write_file = 0, print_output = 0, **kw):
	apply(self.ApplyOperation,
	      (GC_log_directory.LogDir.Visualize, force, read_cache, write_cache,
	       write_file, print_output), kw)

    def Unify(self, force = 0, read_cache = 1, write_cache = 0,
	      write_file = 0, print_output = 0, **kw):
	apply(self.ApplyOperation,
	      (GC_log_directory.LogDir.Unify, force, read_cache, write_cache,
	       write_file, print_output), kw)

    def Validate(self,  force = 0, read_cache = 1, write_cache = 0,
		 write_file = 0, print_output = 0, **kw):
	apply(self.ApplyOperation,
	      (GC_log_directory.LogDir.Validate, force, read_cache, write_cache,
	       write_file, print_output), kw)

    def Score(self, force = 0, read_cache = 1, write_cache = 0,
	      write_file = 0, print_output = 0, **kw):
	if not kw.has_key("score_set"):
	    score_set = DMA_evaluation.DMAScoreSet(self)
	    kw["score_set"] = score_set
	else:
	    score_set = kw["score_set"]
	if kw.has_key("ref_postprocess"):
	    score_set.ref_post = DMA_evaluation.PostProcessor(kw["ref_postprocess"])
	else:
	    # Always postprocess the transcription. If no postprocess
	    # is specified, use the NIST guidelines as a default.
	    score_set.ref_post = DMA_evaluation.NISTTranscriptionPostProcessor()
	apply(self.ApplyOperation,
	      (GC_log_directory.LogDir.Score, force, read_cache, write_cache,
	       write_file, print_output), kw)
	if not kw.has_key("format"):
	    format = "html"
	else:
	    format = kw["format"]	
	# Currently, we can only use HTML to output.
	import xml_visualization
	if format == "html":
	    formatter = xml_visualization.HTMLFormatter(self)
	elif format == "csv":
	    formatter = CSVFormatter(self)
	else:
	    formatter = xml_visualization.HTMLFormatter(self)
	score_set.Present(formatter)
	score_set.ref_post.Close()

    def HumanStubber(self, force = 0, read_cache = 1, write_cache = 0,
		     write_file = 0, print_output = 0, **kw):
	apply(self.ApplyOperation,
	      (GC_log_directory.LogDir.HumanStubber, force, read_cache, write_cache,
	       write_file, print_output), kw)


# CSV formatter for comma-delimited output.

import GC_log_formatting, string

class CSVFormatter(GC_log_formatting.DefaultFormatter):
    def FormatTable(self, headers, rows):
	strs = [string.joinfields(headers,",")]
	for row in rows:
	    strs.append(string.joinfields(row, ","))
	return string.joinfields(strs, "\n") + "\n"
