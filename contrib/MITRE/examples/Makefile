# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Set the MITRE_ROOTDIR to the directory which dominates
# the examples/ directory. Obligatory.

MITRE_ROOTDIR = $(shell cd ..; pwd)

# Clear the environment. Obligatory.

include $(MITRE_ROOTDIR)/templates/env_clear.make

# Set your list of subdirectories. Obligatory.

SUBDIRS = simple_mainloop audio double binary_data \
	  many_conns local_makefile

# Add other things you want to make. Optional.

# ALL +=

# Optional augmentations.

# CLEAN +=

# VERYCLEAN +=

# DISTCLEAN += 

include $(MITRE_ROOTDIR)/templates/subdir.make

# Now include dependencies. Optional.
