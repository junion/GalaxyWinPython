# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Set the ROOTDIR to the directory which dominates
# MITRE/ and the GalaxyCommunicator templates/ directories.
# If you don't set GALAXY_HOME, NL_HOME, or SLS_HOME,
# the Makefile will look for them here too. Obligatory.

ROOTDIR = $(shell cd ../../..; pwd)
MITRE_ROOTDIR = $(ROOTDIR)/contrib/MITRE

# Clear the environment. Obligatory.

include $(MITRE_ROOTDIR)/templates/env_clear.make

# Set your list of subdirectories. Obligatory.

SUBDIRS = src

# Add other things you want to make. Optional.

# ALL +=

# Optional augmentations.

CLEAN += include/\#* include/.\#* include/*~ include/core

# VERYCLEAN +=

# DISTCLEAN += 

include $(MITRE_ROOTDIR)/templates/subdir.make

# Now include dependencies. Optional.
