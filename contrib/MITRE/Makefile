# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Set the MITRE_ROOTDIR to the directory which dominates
# the examples/ directory. Obligatory.

MITRE_ROOTDIR = $(shell pwd)

# Clear the environment. Obligatory.

include $(MITRE_ROOTDIR)/templates/env_clear.make

include $(MITRE_ROOTDIR)/templates/common.make

# Set your list of subdirectories. Obligatory.

# Phasing out the MITRE bindings.

SUBDIRS = templates utilities examples \
	  bindings demos tools/src

# Add other things you want to make. Optional.

# ALL += 

# Optional augmentations.

# CLEAN += 

# VERYCLEAN +=

include $(MITRE_ROOTDIR)/templates/subdir.make

# Now include dependencies. Optional.

examples: utilities templates

