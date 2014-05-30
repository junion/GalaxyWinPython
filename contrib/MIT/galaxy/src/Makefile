# This file (c) Copyright 1998 - 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

LIBS =

MORE_LIBS = \
	libGalaxyEnhancements \
	libGalaxyHubControl

ALL_LIBS = $(LIBS) $(MORE_LIBS)

EXECS = verify_dctl_file \
	verify_program

ALL_EXECS = $(EXECS)

TARGETS = headers depend clean veryclean dependclean distclean

default all: opt

opt: libs hub
debug: libs-debug hub-debug

libs: $(ALL_LIBS)
libs-debug: $(ALL_LIBS:%=%_debug)
libs-insure: $(ALL_LIBS:%=%_insure)

$(ALL_LIBS):
	@echo ""
	@echo "MAKING LIB $@"
	@if (test -d $@) then (cd $@; $(MAKE)) fi

$(ALL_LIBS:%=%_debug):
	@echo ""
	@echo "MAKING LIB $@"
	@if (test -d $(@:%_debug=%)) then (cd $(@:%_debug=%); $(MAKE) debug) fi

$(ALL_LIBS:%=%_insure):
	@echo ""
	@echo "MAKING LIB $@"
	@if (test -d $(@:%_insure=%)) then (cd $(@:%_insure=%); $(MAKE) insure) fi

$(ALL_EXECS): libs
	@echo ""
	@echo "MAKING $@"
	@if (test -d $@) then (cd $@; $(MAKE)) fi

$(ALL_EXECS:%=%_debug): libs-debug
	@echo ""
	@echo "MAKING $@"
	@if (test -d $(@:%_debug=%)) then (cd $(@:%_debug=%); $(MAKE) debug) fi

$(ALL_EXECS:%=%_insure): libs-insure
	@echo ""
	@echo "MAKING $@"
	@if (test -d $(@:%_insure=%)) then (cd $(@:%_insure=%); $(MAKE) insure) fi

hub: $(EXECS)
hub-debug: $(EXECS:%=%_debug)
hub-insure: $(EXECS:%=%_insure)

$(TARGETS): %: %-all

$(TARGETS:%=%-all):
	@echo; echo "        !!! MAKING $@ FOR ENTIRE SRC TREE !!!"
	@for i in $(ALL_LIBS) $(ALL_EXECS); do \
		(if (test -d $$i) then (cd $$i; $(MAKE) $(@:%-all=%)) fi); \
	done
	@echo "        !!! DONE MAKING $@ !!!"; echo

clean veryclean:
	rm -rf *~ core

distclean: 
	-find . -type l -exec rm -rf {} \; -print
	-find . -name '.\#*' -exec rm -rf {} \; -print
	-find . -name '.nfs*' -exec rm -rf {} \; -print
	-find . -name OBJ -exec /bin/rm -rf {} \; -print
	-find . -name functions.h -exec rm -rf {} \; -print
	-find . -name server.h -exec rm -rf {} \; -print
	@echo "        !!! DONE MAKING DISTCLEAN !!!"; echo

.PHONY: default all includes libs hub
.PHONY: libs-debug hub-debug
.PHONY: libs-insure hub-insure
.PHONY: $(ALL_EXECS) $(ALL_EXECS:%=%_debug) $(ALL_EXECS:%=%_insure)
.PHONY: $(ALL_LIBS) $(ALL_LIBS:%=%_debug) $(ALL_LIBS:%=%_insure)
.PHONY: $(TARGETS)
