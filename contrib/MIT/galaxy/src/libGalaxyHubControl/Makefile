# This file (c) Copyright 1998 - 2000 M.I.T.
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

MAKEFILE = Makefile

# Modify this to point to the root directory
ROOT_DIR = $(shell cd ../..; pwd)

include $(ROOT_DIR)/../../../templates/GC_HOME.make
TEMPLATES = $(GC_HOME)/templates
include $(TEMPLATES)/archos.make

# Add any special CPP flags here.
CPPFLAGS = -I$(GC_HOME)/src/HUB -I$(TEMPLATES)/$(ARCHOS)
LDFLAGS  = 
SPECIAL_LIBS = ${COMMON_LIBS}
LIBDEPS  = 
SPECIAL_LIBS_D = ${COMMON_LIBS}
SPECIAL_LIBS_P = 

# NOTE: ONLY SPECIFY ONE OF THE FOLLOWING THREE TYPES OF TARGETS:
# LIBTARGET, EXECTARGETS, SERVER

# If you're making a library, give the name here.
LIBTARGET = libGalaxyHubControl.a

# List all .c files here, including those in subdirectories like so:
SOURCES = \
	mit_control.c \
	read_hub_program.c \
	print_hub_program.c

SUBDIRS = .

# Include the rules file (make variable settings and targets)

# Here we set the values for INCLUDEPATH, LIBPATH, etc.

# This may be suboptimal, because the MIT folks might be interested
# in having the targets.make floating around as well. So we move
# it to its own templates directory.

include $(ROOT_DIR)/templates/rules.make

# Include automatically generated source dependency file
include ${MAKEFILE}.depend
