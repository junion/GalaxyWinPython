# This file (c) Copyright 1998 - 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef NO_GALAXY

include $(GC_HOME)/templates/shared_libs.make

LIBS     += -lGalaxy
LIBDEPS  += $(GC_HOME)/lib/$(ARCHOS)/libGalaxy$(LIBEXT)

ifdef SHARED_LIBS
  ifdef XLINKER_RTFLAG
    LDFLAGS += -Xlinker $(XLINKER_RTFLAG) -Xlinker $(GC_HOME)/lib/$(ARCHOS)
  endif
endif

endif