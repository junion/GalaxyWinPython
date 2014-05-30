# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef __ALLEGRO_MAKE__
__ALLEGRO_MAKE__ = 1

# If there's no Allegro, don't do anything.

include $(MITRE_ROOTDIR)/templates/common.make
include $(MITRE_ROOTDIR)/templates/GC_HOME.make
ROOTDIR = $(GC_HOME)

# Make sure that cleans happen no matter whether Allegro is enabled.

CLEAN += $(ACLSCRIPTS:%=%.in)

# Veryclean adds the scripts.

SCRIPTS += $(ACLSCRIPTS)

ifdef ALLEGROBIN

SCRIPTCMD += sed "s!MF_CLFILE!`echo $(shell pwd)/$@.cl`!" |

ACLSCRIPTVARS := $(SCRIPTVARS)

SCRIPTVARS += ROOTDIR MITRE_ROOTDIR

# Make sure to substitute the ARCHOS_PREFIX first!

SCRIPTCMD += sed "s!MF_GC_HOME!$(GC_HOME)!" | \
	     sed "s!MF_ARCHOS_PREFIX!$(ARCHOS_PREFIX)!g" | \
	     sed "s!MF_ARCHOS!$(ARCHOS)!" | \

include $(MITRE_ROOTDIR)/templates/script.make

ACLSEDCMD = sed "s!MF_SETENV!$(foreach s,$(ACLSCRIPTVARS),setenv $(s) MF_$(s);)!"

$(ACLSCRIPTS:%=%.in): $(MITRE_ROOTDIR)/templates/acl.csh
	cat $(MITRE_ROOTDIR)/templates/acl.csh | $(ACLSEDCMD) > $@

else

ONLY_CLEAN = 1
include $(MITRE_ROOTDIR)/templates/script.make

endif

endif
