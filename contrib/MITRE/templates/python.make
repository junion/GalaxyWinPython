# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef __PYTHON_MAKE__
__PYTHON_MAKE__ = 1

# If there's no python, don't do anything.

include $(MITRE_ROOTDIR)/templates/common.make
include $(MITRE_ROOTDIR)/templates/GC_HOME.make
ROOTDIR = $(GC_HOME)

# Make sure happens is enabled no matter whether Python is enabled.

CLEAN += *.pyc $(PYSCRIPTS:%=%.in) $(BAREPYSCRIPTS:%=%.in)

# veryclean adds the scripts.

SCRIPTS += $(BAREPYSCRIPTS)

ALLPYSCRIPTS += $(BAREPYSCRIPTS)

# If PYTHONBIN is defined, we know we can get at least the
# executables which don't use the libGalaxy bindings. We can
# only get the libGalaxy bindings if we have PYINCLUDE.

ifdef PYTHONBIN

SCRIPTCMD += sed "s!MF_PYFILE!$(shell pwd)/$(notdir $@).py!" | \
             sed "s!MF_GC_HOME!$(GC_HOME)!" | \
	     sed "s!MF_PYTHONBIN!$(PYTHONBIN)!" | \
	     sed "s!MF_ARCHOS_PREFIX!$(ARCHOS_PREFIX)!g" | \

PYSCRIPTVARS := $(SCRIPTVARS)

SCRIPTVARS += ROOTDIR MITRE_ROOTDIR

ifdef PYINCLUDE

SCRIPTS += $(PYSCRIPTS)
ALLPYSCRIPTS += $(PYSCRIPTS)

endif

include $(MITRE_ROOTDIR)/templates/script.make

PYSEDCMD = sed "s!MF_SETENV!$(foreach s,$(PYSCRIPTVARS),setenv $(s) MF_$(s);)!"

$(ALLPYSCRIPTS:%=%.in): $(MITRE_ROOTDIR)/templates/python.csh
	cat $(MITRE_ROOTDIR)/templates/python.csh | $(PYSEDCMD) > $@

else

ONLY_CLEAN = 1
include $(MITRE_ROOTDIR)/templates/script.make

endif

endif