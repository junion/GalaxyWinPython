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

# No special arguments for validate.

from GC_log_shell import GC_DigestArgs, GC_Xmlize_Args, GC_Annotate_Args

repository, processing_args = GC_DigestArgs("xml_unify", GC_Xmlize_Args, GC_Annotate_Args)

apply(repository.Unify, (), processing_args)
