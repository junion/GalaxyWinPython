# This file (c) Copyright 1998 - 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This Makefile is based extensively on the original galaxy/src/Makefile
# from MIT.

LIBS = libGalaxy

EXECS = HUB 

COMMON_TARGETS = depend headers clean veryclean distclean dependclean

default: opt
all: opt debug

opt: libs hub
debug: libs-debug hub-debug
purify: hub-purify libs-debug
insure: hub-insure libs-insure
profile: hub-profile libs-profile
thread: libs-thread hub-thread
thread-debug: libs-thread-debug hub-thread-debug
thread-purify: hub-thread-purify libs-thread-debug
thread-insure: hub-thread-insure libs-thread-insure
thread-profile: hub-thread-profile libs-thread-profile

libs: $(LIBS)
libs-debug: $(LIBS:%=%.debug)
libs-insure: $(LIBS:%=%.insure)
libs-profile: $(LIBS:%=%.profile)
libs-thread: $(LIBS:%=%.thread)
libs-thread-debug: $(LIBS:%=%.thread-debug)
libs-thread-insure: $(LIBS:%=%.thread-insure)
libs-thread-profile: $(LIBS:%=%.thread-profile)

hub: $(EXECS)
hub-debug: $(EXECS:%=%.debug)
hub-purify: $(EXECS:%=%.purify)
hub-insure: $(EXECS:%=%.insure)
hub-profile: $(EXECS:%=%.profile)

hub-thread: $(EXECS:%=%.thread)
hub-thread-debug: $(EXECS:%=%.thread-debug)
hub-thread-purify: $(EXECS:%=%.thread-purify)
hub-thread-insure: $(EXECS:%=%.thread-insure)
hub-thread-profile: $(EXECS:%=%.thread-profile)

EXTS = debug insure profile thread thread-debug \
       thread-insure thread-profile

EX_EXTS = $(EXTS) purify thread-purify

LIB_EXTS = $(foreach ext,$(EXTS),$(LIBS:%=%.$(ext)))

EXEC_EXTS = $(foreach ext,$(EX_EXTS),$(EXECS:%=%.$(ext)))

$(LIBS) $(LIB_EXTS) $(EXECS) $(EXEC_EXTS): 
	@if (test -d $(basename $@)) then \
	  ( cd $(basename $@); $(MAKE) $(subst .,,$(suffix $@)) ) \
	fi

$(EXECS): libs
$(EXECS:%=%.debug): libs-debug
$(EXECS:%=%.purify): libs-debug
$(EXECS:%=%.insure): libs-insure
$(EXECS:%=%.profile): libs-profile
$(EXECS:%=%.thread): libs-thread
$(EXECS:%=%.thread-debug): libs-thread-debug
$(EXECS:%=%.thread-purify): libs-thread-debug
$(EXECS:%=%.thread-insure): libs-thread-insure
$(EXECS:%=%.thread-profile): libs-thread-profile


$(COMMON_TARGETS): %: %-all

$(COMMON_TARGETS:%=%-all):
	@for i in $(LIBS) $(EXECS); do \
	  (if (test -d $$i) then (cd $$i; $(MAKE) $(@:%-all=%)) fi); \
	done

clean veryclean: local_clean

local_clean:
	rm -rf *~ core

distclean: local_distclean

local_distclean:
	-find . -name '.\#*' -exec rm -rf {} \; -print
	-find . -name '.nfs*' -exec rm -rf {} \; -print
	-find . -name OBJ -exec /bin/rm -rf {} \; -print

.PHONY: default all libs hub
.PHONY: libs-debug hub-debug hub-purify local_clean local_distclean hub-insure libs-insure
.PHONY: $(EXECS) $(EXEC_EXTS)
.PHONY: $(LIBS) $(LIB_EXTS)
.PHONY: $(COMMON_TARGETS)
