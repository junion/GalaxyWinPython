# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import sys, os, string, time

sys.path.insert(0, os.path.join(os.environ["GC_HOME"],
				"contrib", "MITRE", "templates"))

import GC_py_init

sys.path.insert(0, os.path.join(os.environ["GC_HOME"],
				"contrib", "MITRE", "tools",
				"lib", "python"))

# This is a general-purpose collector which gathers the contents
# of various landmarks. The question becomes: how to output
# the landmarks? How do we allow different groups to incorporate
# different strippers, for instance? Provide a shell function to 
# process each line? 

# We also want to indicate how we want things split up: in a single
# file or in a file per session.

import GC_log_directory, GC_log_formatting, xml_interleave, url_directory

from DMA_evaluation import PostProcessor

def GatherLandmarks(repository, force = 0, read_cache = 1, write_cache = 0,
		    write_file = 0, print_output = 0, **kw):
    landmarks = kw["landmark"]
    # Figure out the outdir.
    if kw.has_key("outdir"):
	outdir = kw["outdir"]    
    # If there's a single root, then use it
    elif len(repository.dir_roots) == 1:
	outdir = repository.dir_roots[0].PathString()
    else:
	outdir = os.getcwd()
    if kw.has_key("outprefix"):
	outprefix = kw["outprefix"]
	outprefix = os.path.splitext(outprefix)[0]
    else:
	outprefix = "landmark-" + `int(time.time())`
    # outform is either "single" or "session"
    if kw.has_key("outform"):
	outform = kw.has_key["outform"]
    else:
	outform = "single"
    # Now, we attempt to infer from the landmark list
    # which are the relevant interesting classes.
    class_list = []
    postprocess_list = []
    for lm in landmarks:
	try:
	    [key, value] = string.splitfields(lm, "=")
	    l = string.splitfields(value, ":", 1)
	    value = l[0]
	    postprocess = None
	    if len(l) > 1:
		postprocess = l[1]		
	    c = GC_log_formatting.GetInterestingFormatClass(key, value)
	    if c is None:
		repository.io.ReportStatus("Don't recognize landmark %s=%s. Skipping." % (key, value))
	    else:
		class_list.append((c, key, value))
		postprocess_list.append(postprocess)
	except:
	    repository.io.ReportStatus("Encountered an error processing landmark %s. Skipping." % lm)
    kw["class_list"] = class_list
    kw["postprocess_list"] = postprocess_list
    kw["outform"] = outform
    kw["outprefix"] = outprefix
    kw["outdir"] = outdir
    kw["range"] = repository._InterpretRange(kw)
    apply(repository.ApplyOperation,
	  (GatherSessionLandmarks, force, read_cache, write_cache,
	   write_file, print_output), kw)

def GatherSessionLandmarks(log_dir, force = 0, read_cache = 1, write_cache = 0,
			   write_file = 0, print_output = 0, **kw):
    anno_d = apply(log_dir.annotated_xml.Generate,
		   (force, read_cache, write_cache, write_file), kw)
    human_d = apply(log_dir.human_xml.Generate,
		    (force, read_cache, write_cache, write_file), kw)
    log_summary = xml_interleave.LogSummary(anno_d, human_d, log_dir)
    class_list = kw["class_list"]
    postprocess_list = kw["postprocess_list"]
    task_range = kw["range"]
    postprocess_dict = {}
    classes = []
    restrictions = {}
    for i in range(len(class_list)):
	eclass, key, value = class_list[i]
	postprocess_dict[eclass] = PostProcessor(postprocess_list[i])
	classes.append(eclass)
	restrictions[eclass] = {key: value}	    
    global_d, turn_d = log_summary.FindInterestingElements(classes, turn_segmentation = xml_interleave.COLLAPSED_TURNS, task_range = task_range, restrictions = restrictions)
    path = url_directory.Path(os.path.join(kw["outdir"], kw["outprefix"] + "-" + os.path.splitext(log_dir.annotated_xml.write_file.Split()[1])[0]) + ".txt")
    fp = path.WriteStream()
    for eclass, d in global_d:	
	if issubclass(eclass, GC_log_formatting.InterestingEvent):
	    tok = "%s" % (d.GetTimes(),)
	else:
	    tok = "%s" % d.original_dp._FormatData()
	tok = postprocess_dict[eclass].Process(tok) + "\n"
	fp.write(tok)
    for [turn_summary, lms] in turn_d:
	turn = turn_summary.turn_anchor
	for eclass, l in lms:
	    if issubclass(eclass, GC_log_formatting.InterestingEvent):
		tok = "%s" % (l.GetTimes(),)
	    else:
		tok = "%s" % l.original_dp._FormatData()
	    tok = postprocess_dict[eclass].Process(tok) + "\n"
	    fp.write(tok)
    for postprocess in postprocess_dict.values():
	postprocess.Close()
    fp.close()

# Toplevel.

from GC_log_shell import GC_DigestArgs, GC_Xmlize_Args, GC_Annotate_Args, GC_SR_Batch_Args, GC_Landmark_Args, GC_Range_Args

repository, processing_args = GC_DigestArgs("xml_extract_landmarks", GC_Xmlize_Args, GC_Annotate_Args, GC_SR_Batch_Args, GC_Landmark_Args, GC_Range_Args)

apply(GatherLandmarks, (repository,), processing_args)
