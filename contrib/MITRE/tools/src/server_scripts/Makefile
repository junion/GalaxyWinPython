# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Set the MITRE_ROOTDIR to the directory which dominates
# the examples/ directory. Obligatory.

MITRE_ROOTDIR = $(shell cd ../../..; pwd)

# Clear the environment. Obligatory.

include $(MITRE_ROOTDIR)/templates/env_clear.make

# If you provide scripts, you should also declare whatever
# SCRIPTVARS you use in the scripts, and you should provide
# values for those vars. For each var <var>, the script may 
# contain a reference to MF_<var>, which will be resolved using
# an automatically generated sed script. The script may
# refer to ROOTDIR, since it's already defined.

SCRIPTS = run_silent_server run_server

SCRIPTVARS = ARCHOS_PREFIX MITRE_ROOTDIR

# If you want to augment the command which produces the scripts
# in any way, add it here. The command should assume that it's
# getting its input from stdin, and should write its output
# to stdout. Optional.

# SCRIPTCMD += 

# Add other things you want to make. Optional.

# ALL +=

# Optional augmentations. 

# CLEAN +=

# VERYCLEAN +=

# DISTCLEAN += 

EXECDIR = ../../bin/

DIRS += $(EXECDIR)

include $(MITRE_ROOTDIR)/templates/dir.make
include $(MITRE_ROOTDIR)/templates/script.make
