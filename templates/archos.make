# This file (c) Copyright 1998 - 2000 M.I.T.
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef __ARCHOS_MAKE__
__ARCHOS_MAKE__ = 1

ifndef TEMPLATES
TEMPLATES = $(ROOT_DIR)/templates
endif

# GC_CROSS can be set on the command line of make in order
# to configure cross-compilation. See Makefile in this directory.

ifdef GC_CROSS
  CONFIG_SUB := $(subst -, ,$(shell $(TEMPLATES)/set_archos_cross.sh $(GC_CROSS)))
  ARCH       := $(word 1, $(CONFIG_SUB))
  OS         := $(word 2, $(CONFIG_SUB))
  ARCHOS    := $(ARCH)-$(OS)
else
  ARCH   := $(shell $(TEMPLATES)/set_arch.csh)
  OS     := $(shell $(TEMPLATES)/set_os.csh)
  ARCHOS := $(ARCH)-$(OS)
endif

endif 
# __ARCHOS_MAKE__
