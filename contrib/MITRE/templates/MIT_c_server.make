# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

MAKEFILE = MF_MAKEFILE

# Modify this to point to the package root directory
ROOT_DIR = MF_ROOTDIR
MITRE_ROOTDIR = MF_MITRE_ROOTDIR
TEMPLATES = $(ROOT_DIR)/templates

# archos.make sets ARCH, OS, and ARCHOS for makefile conditionals.
include $(TEMPLATES)/archos.make

# Add any special CPP/LD flags here.
CPPFLAGS += -I$(MITRE_ROOTDIR)/utilities/include -I$(TEMPLATES)/$(ARCHOS)
LDFLAGS  += -L$(MITRE_ROOTDIR)/utilities/lib/$(ARCHOS)

# The order of objects for linking is as follows:
#  - object files (from SOURCES)
#  - SPECIAL_LIBS (defined below)
#    SPECIAL_LIBS are libraries with variants (_debug, _profile)
#    which are particular to the current executable.  If any of the
#    variants are non-standard, set SPECIAL_LIBS_D and/or SPECIAL_LIBS_P.
#  - LIBS (defined in <package>.make)
#    LIBS are libraries with variants which every executable in the
#    package requires.
#  - COMMON_LIBS (defined below and in <package>.make)
#    COMMON_LIBS are third party libraries without variants.
#  - SYSLIBS (defined in sysdep.make and <package>.make)
#    SYSLIBS are operating system libraries.
COMMON_LIBS +=
SPECIAL_LIBS += -lMITRE_galaxy
LIBDEPS += $(MITRE_ROOTDIR)/utilities/lib/$(ARCHOS)/libMITRE_galaxy$(LIBEXT)

# NOTE: ONLY SPECIFY ONE OF THE FOLLOWING THREE TYPES OF TARGETS:
# LIBTARGET, EXECTARGETS, SERVER

# If you're making a library, give the name here.
#LIBTARGET = libutil.a

# If you're making executables, list them here.
#EXECTARGETS = exec_name

MF_THREAD_SAFE

# If making an executable that uses the ServerStubs mechanism, 
# give the name here.
SERVER = MF_SERVER
# This variable specifies the file from which to read server operations.
# The default is $(GALAXY_HOME)/System/servers.
# SAM 8/11/99: No longer used, now using headers.
# OPERATIONS_FILE = MF_OPERATIONS_FILE
# ifndef OPERATIONS_FILE
#   OPERATIONS_FILE = operations.pgm
# endif
# If the server operations take server data, uncomment the following line.
# SAM 8/11/99: ditto
# SERVER_DATA = -server_data
# If the server uses the dialogue control mechanism, uncomment the following line.
#USE_DCTL = 1
# This variable specifies the dialogue control file for generating the
# dialogue function map.
# The default is $(GALAXY_HOME)/System/$(SERVER).dctl
#DCTL_FILE = 
# This variable specifies the structure passed to the dialogue functions.
# The default structure is GAL_DOMAIN_SVR
#DCTL_STRUCT = GAL_DB
#SFUNC.H = server.MF_SERVER.h

ifndef EXECDIR
EXECDIR = bin/
endif

MORE_CLEAN += bin

# List subdirectories below so that OBJ subdirectories get made.
SUBDIRS = .

# List all .c files here, including those in subdirectories:
SOURCES = MF_SOURCES

# Include the rules file (make variable settings and targets)
include $(TEMPLATES)/rules.make

# Include automatically generated source dependency file
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
include $(MAKEFILE).depend
endif

include $(MITRE_ROOTDIR)/templates/utilities_targets.make
