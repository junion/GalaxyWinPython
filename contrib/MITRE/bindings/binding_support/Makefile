# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This example doesn't use either the MIT or the MITRE
# Makefile templates, as an illustration.

ROOTDIR = $(shell cd ../../../..; /bin/pwd)

MITRE_ROOTDIR = $(shell cd ../..; /bin/pwd)

TEMPLATES = $(ROOTDIR)/templates

# I need to get the architecture to find the MIT libraries.
# But I need to load archos.make to find out what the ARCH prefix
# is, and $(ARCHOS)/config.make to find out what ARCHOS_PREFIX is.

include $(MITRE_ROOTDIR)/templates/common.make

CPPFLAGS = -I$(ROOTDIR)/include -I.

# Depending on whether there are shared libraries or not,
# we want to pick the right one library extension, etc.
# Actually, it's worse than that. We need to make sure that if 
# there are no shared libraries, we use all the object files
# from the nonshared library. Urk. I thought we could get
# these from the .a using "ar", but I forgot that ar only 
# includes the file name, not the directory name. So we'll
# use "make".

include $(TEMPLATES)/shared_libs.make

ifdef SHARED_LIBS
  GC_LIB = $(ROOTDIR)/lib/$(ARCHOS)/libGalaxy$(LIBEXT)
else
  GC_LIB = $(shell OOBJ=O $(MAKE) --no-print-directory -s -C $(ROOTDIR)/src/libGalaxy print-objs)
endif

ifeq ($(OS),solaris)
LIBS += -lsocket -lnsl -lrt
endif

LIB_TARGET = ../lib/$(ARCHOS)/libGalaxyBindingSupport.so

$(ALL_BUILD_TARGETS): $(LIB_TARGET)

SOURCES = binding_support.c

DIRS += ../lib/$(ARCHOS)

$(LIB_TARGET): ../lib/$(ARCHOS) $(SOURCES) binding_support.h $(GC_LIB)
	$(CC) -shared -fPIC $(CPPFLAGS) -o $(LIB_TARGET) $(SOURCES) $(GC_LIB) $(LIBS)

veryclean clean:
	/bin/rm -rf $(LIB_TARGET) \#* .\#* *~ core 

distclean: veryclean
	/bin/rm -rf ../lib

include $(MITRE_ROOTDIR)/templates/dir.make

include $(ROOTDIR)/templates/targets.make
