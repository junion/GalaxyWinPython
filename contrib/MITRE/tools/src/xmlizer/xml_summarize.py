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

# Currently, there are no special arguments for summarization.

from GC_log_shell import GC_DigestArgs, GC_Xmlize_Args, GC_Annotate_Args, GC_Range_Args

repository, processing_args = GC_DigestArgs("xml_summarize", GC_Xmlize_Args, GC_Annotate_Args, GC_Range_Args)

processing_args["vtype"] = "raw"

apply(repository.Visualize, (), processing_args)
