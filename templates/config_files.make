# This file (c) Copyright 1998 - 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

GC_HOME = $(shell cd ..; pwd)
TEMPLATES = $(shell pwd)

include $(TEMPLATES)/archos.make

include $(TEMPLATES)/$(ARCHOS)/config.make

all: libext.make fat_binary.csh

fat_binary.csh: % : %.in $(ARCHOS)/config.make
	sed "s%<gc_home>%$(GC_HOME)%g" $@.in > $@.temp
	sed "s%<archos_prefix>%$(ARCHOS_PREFIX)%g" $@.temp > $@
	rm -rf $@.temp

# We use the setting for SHARED_LIBS.

# Moved the computation of XLINKER_RTFLAG to configure.

$(ARCHOS)/shared_libs.make: $(ARCHOS)/config.make shared_libs.make.in
	cat shared_libs.make.in | sed "s%MF_SHARED_LIBS%$(SHARED_LIBS)%" | sed "s%MF_XLINKER_RTFLAG%$(XLINKER_RTFLAG)%" > $@
