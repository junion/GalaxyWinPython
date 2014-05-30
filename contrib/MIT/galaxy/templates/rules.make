# This file (c) Copyright 1998 - 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

TEMPLATES = $(shell cd $(ROOT_DIR)/../../../templates; /bin/pwd)
PACKAGE += contrib_MIT

# These should not be set anywhere else in the Makefiles
GALAXY_HOME = $(shell cd $(ROOT_DIR)/../galaxy; /bin/pwd)

include $(TEMPLATES)/rules.make

# for Emacs...
# Local Variables:
# mode: makefile
# comment-column: 40
# fill-column: 77
# End:
