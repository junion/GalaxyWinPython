# This file (c) Copyright 1998 - 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

include $(TEMPLATES)/archos.make
include $(TEMPLATES)/GC_HOME.make

# INCLUDEPATH and LIBPATH are added to CPPFLAGS and LDFLAGS.
# templates/$(ARCHOS) contains the config.h header.

INCLUDEPATH += $(GC_HOME)
LIBPATH += $(GC_HOME)/lib/$(ARCHOS)

# CLEAN_HEADERS is used to substitute variables for path names
# when generating makefile dependencies.

CLEAN_HEADERS += GC_HOME
TARGETS_FILE += $(TEMPLATES)/targets.make

include $(TEMPLATES)/Galaxy.make
