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

ifdef PYTHONBIN

ifdef PYINCLUDE

PYENABLED = 1

PYTARGETS = $(ARCHOS)/cGalaxymodule.so

PYWRAPPERS = $(PYTARGETS:$(ARCHOS)/%module.so=%_wrap.c)

SUPPORT_LIB = $(shell cd ..; /bin/pwd)/lib/$(ARCHOS)/libGalaxyBindingSupport.so

# PYINCLUDE should be specified in the config.make file.

CPPFLAGS += -I$(ROOTDIR)/include $(PYINCLUDE) -I$(shell cd ../binding_support; /bin/pwd)

# Depending on whether there are shared libraries or not,
# we want to pick the right one library extension, etc.

DIRS += $(ARCHOS)

$(ALL_BUILD_TARGETS): $(PYTARGETS)

$(PYTARGETS): $(ARCHOS)/%module.so : %_wrap.c $(SUPPORT_LIB)
	$(CC) -shared -fPIC $(CPPFLAGS) -DHAVE_CONFIG_H $< $(SUPPORT_LIB) -o $@

distclean veryclean clean:
	/bin/rm -rf \#* .\#* *~ core *.doc *.o *.pyc $(ARCHOS)

include $(MITRE_ROOTDIR)/templates/dir.make

$(SUPPORT_LIB):
	cd ../binding_support; $(MAKE)

# If PYWRAPPERS isn't present, make swig. 

include ../swig/swig.make

SWIGFILES = $(PYWRAPPERS:%_wrap.c=../swig/%.i)

swig: $(SWIGFILES)
	for x in $(SWIGFILES:../swig/%.i=%); do \
	  ( $(SWIG_BIN) -python -o `echo $$x`_wrap.c ../swig/$$x.i ) ; \
	done

endif

endif

# Now, if PYENABLED isn't set, we have substitute targets.

ifndef PYENABLED

all $(ALL_BUILD_TARGETS):

include $(MITRE_ROOTDIR)/templates/clean.make

endif
