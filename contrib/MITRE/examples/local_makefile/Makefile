# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This example doesn't use either the MIT or the MITRE
# Makefile templates, as an illustration.

GC_HOME = $(shell cd ../../../..; pwd)

MITRE_ROOTDIR = $(shell cd ../..; pwd)

TEMPLATES = $(GC_HOME)/templates

# I need to get the architecture to find the MIT libraries.
# But I need to load archos.make to find out what the ARCH prefix
# is, and $(ARCHOS)/config.make to find out what ARCHOS_PREFIX is.

include $(TEMPLATES)/archos.make
include $(TEMPLATES)/$(ARCHOS)/config.make

SEDCMD = sed -e "s!MF_GC_HOME!$(GC_HOME)!"

CPPFLAGS = -I$(GC_HOME)/include

LIBS = -L$(GC_HOME)/lib/$(ARCHOS) -lGalaxy 

# Depending on whether there are shared libraries or not,
# we want to pick the right one library extension, etc.

include $(TEMPLATES)/shared_libs.make

ifdef SHARED_LIBS
  ifdef XLINKER_RTFLAG
    LIBS += -Xlinker $(XLINKER_RTFLAG) -Xlinker $(GC_HOME)/lib/$(ARCHOS)
  endif
endif

ifeq ($(OS),solaris)
LIBS += -lsocket -lnsl -lrt
endif

LIBDEPS = $(GC_HOME)/lib/$(ARCHOS)/libGalaxy$(LIBEXT)

# I list all the targets because I don't want the recursive
# make coming in from above to fail.

all opt debug insure profile purify thread thread-debug thread-insure thread-profile thread-purify: double headerless_double double.csh

SOURCES =  ../double/double.c ../double/double_core.c

double: ../double/double.c $(LIBDEPS)
	$(CC) -g $(CPPFLAGS) -I../double -o double.$(ARCHOS).bin $(SOURCES) $(LIBS)

HSOURCES = ../double/headerless_double.c ../double/double_core.c

headerless_double: ../double/headerless_double.c $(LIBDEPS)
	$(CC) -g $(CPPFLAGS) -I../double -o headerless_double.$(ARCHOS).bin $(HSOURCES) $(LIBS)

double.csh: double.csh.in
	cat double.csh.in | $(SEDCMD) > double.csh
	chmod +x double.csh

distclean veryclean clean:
	/bin/rm -rf double.*.bin double.csh \#* .\#* *~ core headerless_double.*.bin
