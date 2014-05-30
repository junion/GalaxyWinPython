# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef DIRS_PROVIDED
DIRS_PROVIDED = 1

ifndef ALL_PROVIDED
ALL_PROVIDED = 1
all: default
default: opt
endif

# This assumes the presence of a variable DIRS.

$(ALL_BUILD_TARGETS): $(DIRS:%=%/.created)

# We need to make sure that directories are deleted only
# if they're created via the Makefiles. So when it's time to try
# to create a directory, I'll recursively add it and if it 
# already exists, I'll make sure I remove the file which 
# indicates that it was created by the Makefile. 
# This is ugly, but I can't see another way of doing it.
	
DISTCLEAN_SEEDS = $(filter %.mcreated,$(foreach dir,$(DIRS),$(dir)/$(findstring .mcreated,$(shell ls -a $(dir)))))

DISTCLEAN += $(DIRS:%=%/.created) $(foreach f,$(DISTCLEAN_SEEDS),$(dir $(f)))

# .created ensures the directory exists. .mcreated says that it was
# created by the Makefile.

$(DIRS:%=%/.created):
	@if [ ! -d $(dir $@) ] ; then \
	  ( mkdir -p $(dir $@); echo "Creating $(dir $@)" ; \
            touch $(dir $@)/.mcreated ) ; \
	fi
	touch $@

$(filter-out %.created,$(ALL)) $(filter-out %.created,$(DEBUG)): $(DIRS:%=%/.created)

.PHONY: $(ALL_BUILD_TARGETS)

endif
