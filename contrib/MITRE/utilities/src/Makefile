# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# (c) Copyright 1998 - 2000 M.I.T.
# Permission is hereby granted, without written agreement or royalty fee, to 
# use, copy, modify, and distribute this software and its documentation for 
# any purpose, provided that the above copyright notice and the following 
# three paragraphs appear in all copies of this software.
# IN NO EVENT SHALL M.I.T. BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, 
# SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF 
# THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF M.I.T. HAS BEEN ADVISED OF 
# THE POSSIBILITY OF SUCH DAMAGE.
# M.I.T. SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING, BUT NOT LIMITED 
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
# PURPOSE, AND NON-INFRINGEMENT.
# THE SOFTWARE IS PROVIDED ON AN "AS IS" BASIS AND M.I.T. HAS NO OBLIGATION 
# TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.


# SAM 9/27/99: Modified to handle the new Makefile format.

MAKEFILE = Makefile

# Modify this to point to the package root directory
MITRE_ROOTDIR = $(shell cd ../..; pwd)
include $(MITRE_ROOTDIR)/templates/GC_HOME.make
ROOT_DIR = $(GC_HOME)

# init.make sets ARCH, OS, and other variables to be used
# with makefile conditionals (ifdef, ifndef, ifeq, ifneq)
#TEMPLATES = $(ROOT_DIR)/templates
#include $(TEMPLATES)/archos.make

# Add any special CPP/LD flags here.
CPPFLAGS = -I../include -I$(TEMPLATES)/$(ARCHOS)
LDFLAGS  = 

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
COMMON_LIBS =
SPECIAL_LIBS =
LIBDEPS = 
SPECIAL_LIBS_D =
SPECIAL_LIBS_P =

# NOTE: ONLY SPECIFY ONE OF THE FOLLOWING THREE TYPES OF TARGETS:
# LIBTARGET, EXECTARGETS, SERVER, APPLET

# If you're making a library, give the name here.
LIBTARGET = libMITRE_galaxy.a

LIBDIR = ../lib/

# If you're making executables, list them here.
#EXECTARGETS = exec_name

# If making an executable that uses the ServerStubs mechanism, 
# give the name here.
#SERVER = server_name

# If you're making a Java applet, give the name here.
#APPLET = AppletName

# If this is a C application which is thread safe, uncomment
# this line in order to be able to do "make thread".

THREAD_SAFE = 1

# List subdirectories below so that OBJ subdirectories get made.

# List all .c files here, including those in subdirectories:
SOURCES = stdin_utility.c frame_utility.c broker_utility.c \
          binary_utility.c mgutil_init.c

# Include the package rules file (make variable settings and targets)
# in the package templates directory (See rules.make.stub)
include ${ROOT_DIR}/templates/rules.make

MORE_DISTCLEAN += $(LIBDIR)

# Include automatically generated source dependency file
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
include $(MAKEFILE).depend
endif
