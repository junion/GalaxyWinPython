# This file (c) Copyright 1998 - 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# I'm passing responsibility for compilation all the way down
# to src/galaxy, since it's the only thing that needs compiling.

TARGETS = all opt debug purify profile insure \
	  thread thread-debug thread-purify thread-profile thread-insure \
          clean distclean veryclean depend headers

$(TARGETS):
	cd galaxy; $(MAKE) $@
