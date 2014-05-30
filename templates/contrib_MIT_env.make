# This file (c) Copyright 1998 - 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This file contains the particular elements required to configure
# the MIT code.

include $(TEMPLATES)/archos.make

INCLUDEPATH += $(GALAXY_HOME)

LIBPATH += $(GALAXY_HOME)/lib/$(ARCHOS)

CLEAN_HEADERS += GALAXY_HOME

TARGETS_FILE += $(GC_HOME)/contrib/MIT/galaxy/templates/targets.make

include $(TEMPLATES)/contrib_MIT.make
