# This file (c) Copyright 1998 - 2000 M.I.T.
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

GC_HOME = $(shell cd ..; /bin/pwd)
TEMPLATES = $(GC_HOME)/templates

# SAM 11/20/01: New way of doing the cross compilation. I've ditched the
# traditional way of computing host/build/target using config.guess 
# because I want to build the appropriate subdirectory before I ever
# get into the main configure script. So now, GC_CROSS is just a 
# /bin/sh file which sets the appropriate variables: host, CC, CXX, AR. 
# I no longer need to include it here. The only other place that
# GC_CROSS is used is in archos.make, and I've written a script which
# extracts the appropriate archos value from the GC_CROSS file.

include archos.make

SUBDIRS = MITRE

GC_HOME_MAKEFILES = \
	$(SUBDIRS:%=$(GC_HOME)/contrib/%/templates/GC_HOME.make) \
	$(GC_HOME)/templates/GC_HOME.make

GCSED = sed -e "s!MF_GC_HOME!$(GC_HOME)!"

INIT_TARGETS = $(GC_HOME_MAKEFILES) config.make $(ARCHOS)/shared_libs.make \
	       $(ARCHOS)/config.values

# shared_libs.make is for those folks who aren't using the
# Galaxy Communicator makefile templates, but still need to know
# what library dependencies exist.

DEP_TARGETS = \
	fat_binary.csh

all: $(INIT_TARGETS) $(DEP_TARGETS)

site-init: $(INIT_TARGETS)

# This target is for cached config.make files.

# I think the first dependency is guaranteed to be handled first.

partial-site-init: $(GC_HOME_MAKEFILES) config.make.headeronly
	touch $(ARCHOS)/config.make
	$(MAKE) $(ARCHOS)/shared_libs.make $(ARCHOS)/config.values

templates remake: $(INIT_TARGETS)
	rm -rf $(DEP_TARGETS)
	$(MAKE)	

$(ARCHOS)/.initialized:
	if test \! -d $(ARCHOS); then mkdir $(ARCHOS); fi
	touch $(ARCHOS)/.initialized

config.make: $(ARCHOS)/config.make $(ARCHOS)/config.values

config.make.headeronly: $(ARCHOS)/.initialized configure.main configure GC_config.h.in
	@echo; echo GC_config.h not found, creating from GC_config.h.in using configure.
	./configure --without-config-output --with-cached-args

$(ARCHOS)/config.make $(TEMPLATES)/$(ARCHOS)/GC_config.h: $(ARCHOS)/.initialized configure.main configure GC_config.h.in
	@echo; echo $@ not found, creating from $(notdir $@.in) using configure.
	./configure --with-cached-args
	@echo "Exiting to allow user configuration of $(ARCHOS)/config.make in toplevel templates/ directory."
	exit 1

$(GC_HOME_MAKEFILES): GC_HOME.make.in
	cat GC_HOME.make.in | $(GCSED) > $@

configure.main: configure.in
	autoconf configure.in > configure.main

# I have to segregate the PERL generation, because 
# trying to load config.make here causes all sorts of
# strange things to happen (for instance, make distclean will
# actually run configure...)

# I'm going to touch the file, just in case. We can get into
# some odd situations. This needs to recurse to make sure that 
# config.make exists. Ditto for libext.make

fat_binary.csh: % : %.in $(ARCHOS)/config.make
	$(MAKE) -f config_files.make $@
	touch $@

$(ARCHOS)/config.values: $(ARCHOS)/config.make
	$(MAKE) -f extract_values.make

$(ARCHOS)/shared_libs.make: $(ARCHOS)/config.make shared_libs.make.in
	$(MAKE) -f config_files.make $@
	touch $@

clean veryclean:
	rm -rf *~ .\#* \#*
	rm -rf $(DEP_TARGETS)
	rm -rf *.temp

# Since x86-nt will never have a config.make in it, the rm -rf line
# here shouldn't affect it. However, we ought to make doubly sure.

DIRS_TO_REMOVE = $(filter-out x86-nt/,$(dir $(wildcard */config.make)))

distclean: clean
	rm -rf $(INIT_TARGETS) $(DEP_TARGETS) $(SITE_INIT)
	rm -rf $(DIRS_TO_REMOVE) config.log config.status

.PHONY: default all remake site-init clean veryclean distclean
.PHONY:	config.make config.make.headeronly partial-site-init

# for Emacs...
# Local Variables:
# mode: makefile
# comment-column: 40
# fill-column: 77
# End:
