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
CPPFLAGS = -I$(TEMPLATES)/$(ARCHOS) -I$(GC_HOME)/src/HUB -I$(GALAXY_HOME)/src/libGalaxyHubControl
LDFLAGS  = 
SPECIAL_LIBS =
LIBDEPS  = 
SPECIAL_LIBS_D =
SPECIAL_LIBS_P =

# NOTE: ONLY SPECIFY ONE OF THE FOLLOWING THREE TYPES OF TARGETS:
# LIBTARGET, EXECTARGETS, SERVER

# If you're making executables, list them here.
EXECTARGETS = verify_program

# List all .c files here, including those in subdirectories:
# HUB_FILES = read_hub_program.c print_hub_program.c hub_program.h hub_program_tags.h hub_error_tags.h mit_control.h
# LIB_FILES = read_program.c print_program.c program_tags.c error_tags.c
HUB_CONTROL_FILES = read_hub_program.c print_hub_program.c
# SAM 4/3/02: I'd rather not add alarm.c, but it's self-contained
# and the code which finalizes rules relies on it.
HUB_FILES = hub_namespace.c external_api.c alarm.c

SOURCES = \
	verify_program.c \
	$(HUB_CONTROL_FILES) \
	$(HUB_FILES)

# Include the rules file (make variable settings and targets)
include $(ROOT_DIR)/templates/rules.make

$(HUB_CONTROL_FILES):
	$(RM) $@
	ln -s ../libGalaxyHubControl/$@ .

$(HUB_FILES):
	$(RM) $@
	ln -s $(GC_HOME)/src/HUB/$@ .

clean: link_clean

link_clean:
	$(RM) $(HUB_FILES) $(HUB_CONTROL_FILES)

# Include automatically generated source dependency file
include $(MAKEFILE).depend
