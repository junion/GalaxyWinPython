# This file (c) Copyright 1998 - 2000 M.I.T.
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef __TARGETS_MAKE__
__TARGETS_MAKE__ = 1

.PHONY: force_lib_check

ifdef GALAXY_HOME
GALAXY_LIBDIRS = libGalaxyEnhancements libGalaxyHubControl
 
GALAXY_LIBS = $(GALAXY_LIBDIRS:%=$(GALAXY_HOME)/lib/$(ARCHOS)/%$(OTHREADLIB)$(OLIB).a)

$(GALAXY_LIBS): force_lib_check
	cd $(@:$(GALAXY_HOME)/lib/$(ARCHOS)/%$(OTHREADLIB)$(OLIB).a=$(GALAXY_HOME)/src/%); $(MAKE) $(MAKECMDGOALS)
endif

# end of ifndef __TARGETS_MAKE__
endif

# for Emacs...
# Local Variables:
# mode: makefile
# comment-column: 40
# fill-column: 77
# End:
