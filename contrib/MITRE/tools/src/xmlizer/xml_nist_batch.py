# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

import sys, os

sys.path.insert(0, os.path.join(os.environ["GC_HOME"],
				"contrib", "MITRE", "templates"))

import GC_py_init

sys.path.insert(0, os.path.join(os.environ["GC_HOME"],
				"contrib", "MITRE", "tools",
				"lib", "python"))

# The batcher deals with SR output.

import DMA_evaluation, GC_log_directory
    
def BatchSR(repository, force = 0, read_cache = 1, write_cache = 0,
	    write_file = 0, print_output = 0, **kw):
    # Figure out the outdir.
    if kw.has_key("outdir"):
	outdir = kw["outdir"]    
    # If there's a single root, then use it
    elif len(repository.dir_roots) == 1:
	outdir = repository.dir_roots[0]
    else:
	outdir = os.getcwd()
    outprefix = None
    if kw.has_key("outprefix"):
	outprefix = kw["outprefix"]
    wav_post = ref_post = hyp_post = None
    if kw.has_key("wav_postprocess"):
	wav_post = kw["wav_postprocess"]
    if kw.has_key("ref_postprocess"):
	ref_post = kw["ref_postprocess"]
    if kw.has_key("hyp_postprocess"):
	hyp_post = kw["hyp_postprocess"]
    score_set = DMA_evaluation.NISTSRCollector(repository, outdir, outprefix)
    kw["score_set"] = score_set
    apply(repository.ApplyOperation,
	  (GC_log_directory.LogDir.Score, force, read_cache, write_cache,
	   write_file, print_output), kw)
    # Now, we save the files.    
    score_set.SaveSRFiles(wav_postprocess = wav_post,
			  ref_postprocess = ref_post,
			  hyp_postprocess = hyp_post)

# Toplevel.

from GC_log_shell import GC_DigestArgs, GC_Xmlize_Args, GC_Annotate_Args, GC_SR_Batch_Args, GC_SR_Batch_Postprocess_Args, GC_Range_Args

repository, processing_args = GC_DigestArgs("xml_nist_batch", GC_Xmlize_Args, GC_Annotate_Args, GC_SR_Batch_Args, GC_SR_Batch_Postprocess_Args, GC_Range_Args)

apply(BatchSR, (repository,), processing_args)
