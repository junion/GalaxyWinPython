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
TEMPLATES = $(ROOT_DIR)/templates
include $(ROOT_DIR)/templates/archos.make
include $(ROOT_DIR)/templates/GC_HOME.make
include $(ROOT_DIR)/templates/$(ARCHOS)/config.make

# Add any special CPP flags here. Any values that 
# config.make might set, such as CPPFLAGS, needs to 
# be careful to do a += here.
CPPFLAGS += -I$(TEMPLATES)/$(ARCHOS)
# LDFLAGS  = 
# SPECIAL_LIBS = 
# LIBDEPS  = 
# SPECIAL_LIBS_D =
# SPECIAL_LIBS_P =

ifdef SCRIPTING_LIBRARY
  LDFLAGS += -L$(SCRIPTING_LIBDIR)
  LIBDEPS += $(SCRIPTING_LIBDIR)/lib$(SCRIPTING_LIBRARY)$(LIBEXT)
  SPECIAL_LIBS += -l$(SCRIPTING_LIBRARY)
endif

# NOTE: ONLY SPECIFY ONE OF THE FOLLOWING THREE TYPES OF TARGETS:
# LIBTARGET, EXECTARGETS, SERVER

# If you're making executables, list them here.
EXECTARGETS = hub

# List all .c files here, including those in subdirectories:
SOURCES = \
	builtin.c \
	hub.c \
	hub_init.c \
	hub_namespace.c \
	hub_process.c \
	hub_report_status.c \
	hub_special.c \
	hub_util.c \
	logfile.c \
	alarm.c \
	session.c \
	external_api.c \
	external_runtime_api.c \
	db.c

# Include the rules file (make variable settings and targets)
include $(ROOT_DIR)/templates/rules.make

# Include automatically generated source dependency file
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
include $(MAKEFILE).depend
endif

hub$(OTHREADLIB)$(OEXE): $(SCRIPTING_LIBDIR)/lib$(SCRIPTING_LIBRARY)$(OTHREADLIB)$(OLIB)$(LIBEXT)

$(SCRIPTING_LIBDIR)/lib$(SCRIPTING_LIBRARY)$(OTHREADLIB)$(OLIB)$(LIBEXT): force_lib_check
	cd $(SCRIPTING_SRCDIR); $(MAKE) $(MAKECMDGOALS)

.PHONY: force_lib_check
