# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef __COMMON_MAKE__

__COMMON_MAKE__ = 1

include $(MITRE_ROOTDIR)/templates/GC_HOME.make
ROOTDIR = $(GC_HOME)

ifndef TEMPLATES
TEMPLATES = $(ROOTDIR)/templates
endif

# I've moved the Python and CL settings to $(ARCHOS)/config.make.
# But archos.make must be loaded first.

include $(TEMPLATES)/archos.make
include $(TEMPLATES)/$(ARCHOS)/config.make

# These are for global use where conditionalizations occur.
# It's also meant as a checklist for what targets need to
# be handled in the contrib/MITRE subdirectory.

ALL_BUILD_TARGETS = opt debug insure purify profile \
		    thread thread-insure thread-purify thread-profile thread-debug
ALL_CLEAN_TARGETS = clean veryclean distclean

endif
