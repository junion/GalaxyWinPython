# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Set the MITRE_ROOTDIR to the directory which dominates
# the examples/ directory. Obligatory.

MITRE_ROOTDIR = $(shell cd ..; /bin/pwd)

# Clear the environment. Obligatory.

include $(MITRE_ROOTDIR)/templates/env_clear.make

include $(MITRE_ROOTDIR)/templates/common.make

# Set your list of subdirectories. Obligatory.

SUBDIRS = binding_support java python clisp

# ALL += 

# Optional augmentations.

# CLEAN +=

# VERYCLEAN +=

# DISTCLEAN +=

include $(MITRE_ROOTDIR)/templates/subdir.make

# Now include dependencies. Optional.

python clisp: binding_support
