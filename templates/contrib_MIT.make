# This file (c) Copyright 1998 - 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

include $(GC_HOME)/templates/shared_libs.make

LIBS += -lGalaxyEnhancements
LIBDEPS += $(GC_HOME)/contrib/MIT/galaxy/lib/$(ARCHOS)/libGalaxyEnhancements$(LIBEXT)

ifdef SHARED_LIBS
  ifdef XLINKER_RTFLAG
    LDFLAGS += -Xlinker $(XLINKER_RTFLAG) -Xlinker $(GC_HOME)/contrib/MIT/galaxy/lib/$(ARCHOS)
  endif
endif
