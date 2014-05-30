# This file (c) Copyright 1998 - 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This is the toplevel Makefile for the GalaxyCommunicator2 distribution.
# The first thing that needs to happen is that the contrib subdirectories
# need to be augmented with GC_HOME.make.

# This Makefile is based extensively on the MIT toplevel Makefile.

# The next thing to happen is that if contrib/MIT is around and
# it's enabled, we want to generate a links.setenv file appropriate
# to the configuration.

GC_HOME = $(shell pwd)

TEMPLATES = $(GC_HOME)/templates

include $(TEMPLATES)/archos.make

include $(TEMPLATES)/$(ARCHOS)/config.make

MITRE_DEPS =

CONTRIB_CORE = $(filter-out CVS,$(notdir $(wildcard contrib/*)))

CONTRIBS = $(CONTRIB_CORE:%=contrib-%)

ifndef COMPILE_MIT
OLD_CONTRIBS := $(CONTRIBS)
CONTRIBS = $(filter-out contrib-MIT,$(OLD_CONTRIBS))
else
MITRE_DEPS += contrib-MIT
endif

ifndef COMPILE_MITRE
OLD_CONTRIBS := $(CONTRIBS)
CONTRIBS = $(filter-out contrib-MITRE,$(OLD_CONTRIBS))
endif

COMMON_TARGETS = depend headers clean veryclean

default: all

all opt: libs hub contrib
debug: libs-debug hub-debug contrib-debug
purify: libs-debug hub-purify contrib-purify
insure: libs-insure hub-insure contrib-insure
profile: libs-profile hub-profile contrib-profile

thread: libs-thread hub-thread contrib-thread
thread-debug: libs-thread-debug hub-thread-debug contrib-thread-debug
thread-purify: libs-thread-debug hub-thread-purify contrib-thread-purify
thread-insure: libs-thread-insure hub-thread-insure contrib-thread-insure
thread-profile: libs-thread-profile hub-thread-profile contrib-thread-profile

init site-init: 
	(cd templates; $(MAKE) site-init)

partial-init partial-site-init:
	(cd templates; $(MAKE) partial-site-init)

hub contrib: libs
hub-debug contrib-debug hub-purify contrib-purify: libs-debug
hub-insure contrib-insure: libs-insure
hub-profile contrib-profile: libs-profile
hub-thread contrib-thread: libs-thread
hub-thread-debug contrib-thread-debug hub-thread-purify contrib-thread-purify: libs-thread-debug
hub-thread-insure contrib-thread-insure: libs-thread-insure
hub-thread-profile contrib-thread-profile: libs-thread-profile

$(COMMON_TARGETS): init
	for subdir in `echo src docs $(subst -,/,$(CONTRIBS))`; do \
	  (cd $$subdir; $(MAKE) $@) ; \
	done

EXTS = debug insure profile thread thread-debug \
       thread-insure thread-profile 

EXEC_EXTS = $(EXTS) purify thread-purify

CONTRIB_EXTS = $(foreach ext,$(EXEC_EXTS),$(CONTRIBS:%=%.$(ext)))

LIB_EXTS = $(EXTS:%=libs-%)

HUB_EXTS = $(EXEC_EXTS:%=hub-%)

hub libs $(HUB_EXTS) $(LIB_EXTS): init
	cd src; $(MAKE) $@

$(CONTRIBS) $(CONTRIB_EXTS):
	cd $(subst -,/,$(basename $@)); $(MAKE) $(subst .,,$(suffix $@))

contrib: init $(CONTRIBS)
contrib-debug: init $(CONTRIBS:%=%.debug)
contrib-purify: init $(CONTRIBS:%=%.purify)
contrib-insure: init $(CONTRIBS:%=%.insure)
contrib-profile: init $(CONTRIBS:%=%.profile)
contrib-thread: init $(CONTRIBS:%=%.thread)
contrib-thread-debug: init $(CONTRIBS:%=%.thread-debug)
contrib-thread-purify: init $(CONTRIBS:%=%.thread-purify)
contrib-thread-insure: init $(CONTRIBS:%=%.thread-insure)
contrib-thread-profile: init $(CONTRIBS:%=%.thread-profile)

contrib-MITRE: $(MITRE_DEPS)
contrib-MITRE.purify: $(MITRE_DEPS:%=%.purify)
contrib-MITRE.debug: $(MITRE_DEPS:%=%.debug)
contrib-MITRE.profile: $(MITRE_DEPS:%=%.profile)
contrib-MITRE.insure: $(MITRE_DEPS:%=%.insure)
contrib-MITRE.thread: $(MITRE_DEPS:%=%.thread)
contrib-MITRE.thread-purify: $(MITRE_DEPS:%=%.thread-purify)
contrib-MITRE.thread-debug: $(MITRE_DEPS:%=%.thread-debug)
contrib-MITRE.thread-profile: $(MITRE_DEPS:%=%.thread-profile)
contrib-MITRE.thread-insure: $(MITRE_DEPS:%=%.thread-insure)

# regenerate template files
templates:
	(cd templates; $(MAKE) remake)

include $(TEMPLATES)/template_targets.make

# remove all non-source files and directories
distclean: init extraneous_clean
	for subdir in `echo src docs $(subst -,/,$(CONTRIBS))`; do \
	  (cd $$subdir; $(MAKE) distclean) ; \
	done
ifndef COMPILE_MITRE
	cd contrib/MITRE; $(MAKE) distclean
endif

clean veryclean distclean: $(GC_HOME_MAKEFILES)

extraneous_clean:
	@$(MAKE) --no-print-directory findclean	
	$(RM) bin lib
	@echo; echo "To remove site customizations, make distclean in templates"

FINDNAMES = \( -name '*~' -o -name '\#*' -o -name '.\#*' -o -name 'tca.map' -o -name 'tca.log' -o -name '.inslog*' -o -name 'core' \)

# find and remove cores and byproducts of emacs, cvs, and insure++
findclean:
	find . $(FINDNAMES) -exec $(RM) {} \; -print

tags:
	find . \( -name '*.cpp' -o -name '*.[ch]' \) | etags -

.PHONY: default all opt debug init site-init templates findclean
.PHONY: $(COMMON_TARGETS) contrib contrib-debug contrib-purify
.PHONY: hub hub-debug hub-purify libs libs-debug $(CONTRIB_EXTS)
.PHONY: $(LIB_EXTS) $(HUB_EXTS)
.PHONY: hub-insure libs-insure contrib-template-distclean
.PHONY: extraneous_clean gc_home_clean purify clean distclean veryclean
