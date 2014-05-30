# This file (c) Copyright 1998 - 2000 M.I.T.
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

#      PURPOSE: Targets for automatically compiling SLS libraries on demand
#		The library is created if it does not exist and is specified
#		as a dependency
#

ifndef __GC_TARGETS_MAKE__
__GC_TARGETS_MAKE__ = 1

ifdef GC_HOME
GC_LIBS = $(GC_HOME)/lib/$(ARCHOS)/libGalaxy$(OTHREADLIB)$(OLIB)$(LIBEXT)

$(GC_LIBS): force_lib_check
	cd $(GC_HOME)/src/libGalaxy; $(MAKE) $(MAKECMDGOALS)

.PHONY: force_lib_check

endif

# end of ifndef __GC_TARGETS_MAKE__
endif

# for Emacs...
# Local Variables:
# mode: makefile
# comment-column: 40
# fill-column: 77
# End:






