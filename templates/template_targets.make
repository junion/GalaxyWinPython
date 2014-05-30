# This file (c) Copyright 1998 - 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# And here are the targets for the template files.

ifndef __GC_TEMPLATE_TARGETS_MAKE__
__GC_TEMPLATE_TARGETS_MAKE__ = 1

ifdef GC_HOME

ifndef TEMPLATES
  TEMPLATES = $(GC_HOME)/templates
endif

include $(TEMPLATES)/archos.make

GC_SUBDIRS = MITRE

GC_HOME_MAKEFILES = \
	$(GC_SUBDIRS:%=$(GC_HOME)/contrib/%/templates/GC_HOME.make) \
	$(GC_HOME)/templates/GC_HOME.make

$(GC_HOME_MAKEFILES): $(TEMPLATES)/GC_HOME.make.in
	cd $(TEMPLATES); $(MAKE) $@

$(TEMPLATES)/fat_binary.csh:
	cd $(TEMPLATES); $(MAKE) fat_binary.csh

$(TEMPLATES)/$(ARCHOS)/config.make: $(TEMPLATES)/config.make.in $(TEMPLATES)/configure.main
	cd $(TEMPLATES); $(MAKE) config.make

$(TEMPLATES)/shared_libs.make: $(TEMPLATES)/$(ARCHOS)/shared_libs.make

$(TEMPLATES)/$(ARCHOS)/shared_libs.make: $(TEMPLATES)/$(ARCHOS)/config.make
	cd $(TEMPLATES); $(MAKE) $(ARCHOS)/shared_libs.make

endif 

endif
