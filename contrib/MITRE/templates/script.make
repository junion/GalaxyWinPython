# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef __SCRIPT_MAKE__
__SCRIPT_MAKE__ = 1

ifdef SCRIPTVARS
SEDCMD = $(foreach s,$(SCRIPTVARS),sed -e "s!MF_$(s)!`echo $($(s))`!" |)
SCRIPTCMD += $(SEDCMD) cat
else
SCRIPTCMD += cat
endif

# Mst precede any reference to ARCHOS.

include $(MITRE_ROOTDIR)/templates/common.make

SCRIPT_DEPS = $(MITRE_ROOTDIR)/templates/env.csh

ifndef EXECDIR
EXECDIR = ./
endif

ifndef ALL_PROVIDED
ALL_PROVIDED = 1
all: default
default: opt
endif

ifndef ONLY_CLEAN
$(ALL_BUILD_TARGETS): $(SCRIPTS:%=$(EXECDIR)%) $(SCRIPT_DEPS)
endif

VERYCLEAN += $(SCRIPTS:%=$(EXECDIR)%)

# If EXECDIR isn't created, there will be a problem...

IN_DEPS = $(SCRIPT_DEPS)

ifneq ($(EXECDIR),./)
IN_DEPS += $(EXECDIR)
endif

$(SCRIPTS:%=$(EXECDIR)%): $(EXECDIR)% : %.in $(IN_DEPS)
	cat $(notdir $@).in | $(SCRIPTCMD) > $@
	chmod a+rx $@

$(SCRIPT_DEPS):
	cd $(MITRE_ROOTDIR)/templates; $(MAKE) $(notdir $@)

include $(MITRE_ROOTDIR)/templates/clean.make

$(MITRE_ROOTDIR)/templates/env.csh: $(MITRE_ROOTDIR)/templates/env.csh.in

.PHONY: $(ALL_BUILD_TARGETS)

endif
