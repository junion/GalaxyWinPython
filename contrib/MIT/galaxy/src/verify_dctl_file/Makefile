# This file (c) Copyright 1998 - 2000 M.I.T.
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

MAKEFILE = Makefile

# Modify this to point to the root directory
ROOT_DIR = $(shell cd ../..; pwd)

# init.make sets ARCH, OS, and other variables to be used
# with makefile conditionals (ifdef, ifndef, ifeq, ifneq)
# include $(ROOT_DIR)templates/$(ARCHOS)/config.make

include $(ROOT_DIR)/../../../templates/GC_HOME.make
TEMPLATES = $(GC_HOME)/templates
include $(TEMPLATES)/archos.make

# Add any special CPP flags here.
CPPFLAGS = 
LDFLAGS  = 
# SPECIAL_LIBS = -lGalaxyEnhancements
# LIBDEPS  = $(ROOT_DIR)/lib/$(ARCHOS)/libGalaxyEnhancements$(LIBEXT)
SPECIAL_LIBS_D =
SPECIAL_LIBS_P =

# NOTE: ONLY SPECIFY ONE OF THE FOLLOWING THREE TYPES OF TARGETS:
# LIBTARGET, EXECTARGETS, SERVER

# If you're making executables, list them here.
EXECTARGETS = verify_dctl_file

# List all .c files here, including those in subdirectories:
SOURCES = \
	verify_dctl_file.c

# Include the rules file (make variable settings and targets)
include $(ROOT_DIR)/templates/rules.make

# Include automatically generated source dependency file
include $(MAKEFILE).depend
