# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Set the MITRE_ROOTDIR to the directory which dominates
# the examples/ directory. Obligatory.

MITRE_ROOTDIR = $(shell cd ../..; pwd)

# Clear the environment. Obligatory.

include $(MITRE_ROOTDIR)/templates/env_clear.make

# Set your list of subdirectories. Obligatory.

# Phasing out the MITRE bindings.

EXECDIR_ROOT = $(MITRE_ROOTDIR)/tools/bin

EXECDIR = $(EXECDIR_ROOT)/

export EXECDIR

DIRS += $(EXECDIR_ROOT)

SUBDIRS = process_monitor server_scripts \
	  unit_test xmlizer hub_gui

# Add other things you want to make. Optional.

# ALL += 

# Optional augmentations.

CLEAN += ../lib/python/*.pyc ../lib/python/\#* ../lib/python/*~ ../lib/python/core

# VERYCLEAN +=

include $(MITRE_ROOTDIR)/templates/dir.make
include $(MITRE_ROOTDIR)/templates/subdir.make
