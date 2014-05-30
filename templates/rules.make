# This file (c) Copyright 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# In this file, I will sort out whether there's a single target
# or multiple targets. If there are multiple targets, I want to recurse
# so that there's exactly one. This makes it a lot easier to
# deconstruct things like thread-insure.

ifndef __RULES_MAKE__
__RULES_MAKE__ = 1

ifndef TEMPLATES
TEMPLATES = $(ROOT_DIR)/templates
endif

# First, establish a default goal.

ifeq ($(words $(MAKECMDGOALS)),0)
  MAKECMDGOALS = opt
endif

ifneq ($(words $(MAKECMDGOALS)),1)
.PHONY: $(MAKECMDGOALS)

# We may have been loading from a file here, so we better
# make sure we do that.

# We define MAKEFILE in various places. If it's
# defined, be sure to use it, otherwise the
# recursion will be too broad.

ifdef MAKEFILE
MAKE += -f $(MAKEFILE)
endif

$(MAKECMDGOALS):
	$(MAKE) $@
else
# Otherwise, load the single goal handler.
include $(TEMPLATES)/rules_single.make
endif

endif
# __RULES_MAKE__
