# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This function should be imported by everybody who uses
# Python code. It's designed to be platform-independent.

import sys, string, os

def _ReadCsh(csh):
    fp = os.popen(csh, "r")
    val = string.strip(fp.read())
    fp.close()
    return val

def _ArchOs(root_path):
    if os.name in ['dos', 'nt']:
	return "x86-nt"
    else:
	os_val = _ReadCsh(os.path.join(root_path, "templates", "set_os.csh"))
	arch_val = _ReadCsh(os.path.join(root_path, "templates", "set_arch.csh"))
	return arch_val + "-" + os_val

def InitializeGalaxyCommunicator():
    root_path = os.environ["GC_HOME"]
    binding_dir = os.path.join(root_path, "contrib", "MITRE", "bindings",
			       "python")
    c_binding_dir = os.path.join(binding_dir, _ArchOs(root_path))
    if binding_dir not in sys.path:
	sys.path.insert(0, binding_dir)
    if c_binding_dir not in sys.path:
	sys.path.insert(0, c_binding_dir)

InitializeGalaxyCommunicator()
