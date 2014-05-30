# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef __UTILITIES_TARGETS_MAKE__
__UTILITIES_TARGETS_MAKE__ = 1

ifdef MITRE_ROOTDIR

UTILITIES_LIBS = $(MITRE_ROOTDIR)/utilities/lib/$(ARCHOS)/libMITRE_galaxy$(OTHREADLIB)$(OLIB)$(LIBEXT)

$(UTILITIES_LIBS): force_lib_check
	cd $(MITRE_ROOTDIR)/utilities/src; $(MAKE) $(MAKECMDGOALS)

.PHONY: force_lib_check

endif

# end of ifndef __UTILITIES_TARGETS_MAKE__
endif

# for Emacs...
# Local Variables:
# mode: makefile
# comment-column: 40
# fill-column: 77
# End:
