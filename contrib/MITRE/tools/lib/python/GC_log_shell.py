# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This file supports the shell calls in src/xmlize.

GC_Processing_Args = [("read_cache", 0, 0, None),
		      ("write_cache", 0, 0, None),
		      ("force", 0, 0, None),
		      ("write_file", 0, 0, None),
		      ("print_output", 0, 0, None),
		      ("help", 0, 0, None)]

GC_Repository_Args = [("start", 0, 0,
		       ("first_dir", type(""), None)),
		      ("end", 0, 0,
		       ("last_dir", type(""), None)),
		      ("raw_txt_pat", 0, 0,
		       ("regexp", type(""), None)),
		      ("raw_xml_pat", 0, 0,
		       ("regexp", type(""), None)),
		      ("ann_xml_pat", 0, 0,
		       ("regexp", type(""), None)),
		      ("human_xml_pat", 0, 0,
		       ("regexp", type(""), None))]

GC_Xmlize_Args = [("default_user_version", 0, 0,
		   ("version_string", type(""), None))]

GC_Annotate_Args = [("rule_base", 0, 1,
		     ("rule_file", type(""), None))]

# Visualization will just be the raw summary at the shell.
# GC_Visualize_Args = [("vtype", 0, 0,
#		      ("[raw | tabular]", type(""), "raw"))]

GC_Range_Args = [("range", 0, 0,
		  ("[all_turns | overall_task | on_task]", type(""),
		   "overall_task"))]

GC_Score_Args = [("format", 0, 0,
		  ("[html | csv]", type(""), "html")),
		 ("range", 0, 0,
		  ("[all_turns | overall_task | on_task]", type(""),
		   None)),
		 ("ref_postprocess", 0, 0,
		  ("script", type(""), None)),
		 ("score_type", 0, 0,
		  ("[DMA | SR | DMA-no-SR]", type(""), "DMA-no-SR"))]

GC_SR_Batch_Args = [("outdir", 0, 0,
		     ("file_location", type(""), None)),
		    ("outprefix", 0, 0,
		     ("file_prefix", type(""), None))]

GC_SR_Batch_Postprocess_Args = [("wav_postprocess", 0, 0,
				 ("script", type(""), None)),
				("hyp_postprocess", 0, 0,
				 ("script", type(""), None)),
				("ref_postprocess", 0, 0,
				 ("script", type(""), None))]

GC_Landmark_Args = [("landmark", 1, 1,
		     ("attr=val[:script]", type(""), None)),
		    ("outform", 0, 0,
		     ("[single | session]", type(""), None))]

import GC_log_repository, url_directory, io_device, GC_arglist, sys

# We also need to suppress status reporting, or route it elsewhere.

class StderrStatusIODevice(io_device.DefaultIODevice):
    def ReportStatus(self, str, add_newline = 1):
	if add_newline: str = str + "\n"
	sys.stderr.write(str)
	sys.stderr.flush()

def Usage(arg_processor, exec_name):
    print "Usage:", exec_name, arg_processor.Usage(), "log_root..."
    sys.exit(1)

# Arg processing.

from GC_arglist import ArgProcessor

def GC_DigestArgs(exec_name, *arg_specs):
    c = ArgProcessor()
    c.AddArgspec(GC_Repository_Args)
    c.AddArgspec(GC_Processing_Args)
    for spec in arg_specs:
	c.AddArgspec(spec)

    try:
	repository_args = c.ArgsForSpec(GC_Repository_Args)
	processing_args = c.ArgsForSpec(GC_Processing_Args)
	spec_arg_dicts = []
	for spec in arg_specs:
	    spec_arg_dicts.append(c.ArgsForSpec(spec))
	remainder = c.Remainder()
    except:
	Usage(c, exec_name)

    # Keys for creating a repository are identical to the 
    # repository args. The processing args and the xmlize args
    # can be combined, since presence is 1.

    if processing_args.has_key("help"):
	Usage(c, exec_name)

    for spec_dict in spec_arg_dicts:
	for key, value in spec_dict.items():
	    processing_args[key] = value

    # Little patch...
    if processing_args.has_key("rule_base"):
	processing_args["rule_bases"] = processing_args["rule_base"]
	del processing_args["rule_base"]

    # The remainder is the roots.

    if not remainder:
	Usage(c, exec_name)

    io = StderrStatusIODevice()
    roots = []
    for rem in remainder:
	r = url_directory.Path(rem, io = io)
	if not r.Exists():
	    io.ReportStatus("Error: " + r.PathString() + ": no such file or directory.")
	    continue
	if not r.Isdir():
	    r = r.Split()[0]
	roots.append(r)

    repository_args["io"] = io
    repository = apply(GC_log_repository.Repository, (roots,), repository_args)
    return repository, processing_args
