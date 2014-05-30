# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef ALL_PROVIDED
ALL_PROVIDED = 1
all: default
default: opt
endif

opt: $(SUBDIRS)
debug: $(SUBDIRS:%=%.debug)
purify: $(SUBDIRS:%=%.purify)
profile: $(SUBDIRS:%=%.profile)
insure: $(SUBDIRS:%=%.insure)
thread: $(SUBDIRS:%=%.thread)
thread-debug: $(SUBDIRS:%=%.thread-debug)
thread-purify: $(SUBDIRS:%=%.thread-purify)
thread-profile: $(SUBDIRS:%=%.thread-profile)
thread-insure: $(SUBDIRS:%=%.thread-insure)


SUBDIR_EXTS = $(SUBDIRS:%=%.debug) $(SUBDIRS:%=%.purify) \
	      $(SUBDIRS:%=%.insure) $(SUBDIRS:%=%.profile) \
	      $(SUBDIRS:%=%.thread) $(SUBDIRS:%=%.thread-debug) \
	      $(SUBDIRS:%=%.thread-purify) $(SUBDIRS:%=%.thread-profile) \
	      $(SUBDIRS:%=%.thread-insure) $(SUBDIRS:%=%.clean) \
	      $(SUBDIRS:%=%.veryclean) $(SUBDIRS:%=%.distclean)

$(SUBDIR_EXTS) $(SUBDIRS):
	@cd $(basename $@); \
	echo "Making $(subst .,,$(suffix $@)) in $(basename $@)"; \
	$(MAKE) $(subst .,,$(suffix $@))

clean: $(SUBDIRS:%=%.clean)

veryclean: $(SUBDIRS:%=%.veryclean)

distclean: $(SUBDIRS:%=%.distclean)

include $(MITRE_ROOTDIR)/templates/clean.make

.PHONY: $(SUBDIRS) $(SUBDIR_EXTS)
