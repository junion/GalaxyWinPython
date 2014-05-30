# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# I'm not going to use the standard templates, because I
# need to worry about whether Python is installed or not.

# Set the MITRE_ROOTDIR to the directory which dominates
# the examples/ directory. Obligatory.

MITRE_ROOTDIR = $(shell cd ../../..; pwd)

# Clear the environment. Obligatory.

include $(MITRE_ROOTDIR)/templates/env_clear.make

# SAM 3/23/98: The settings here have to be customized
# for whether PYTHONBIN is set or not. So first I need to
# load common.make.

include $(MITRE_ROOTDIR)/templates/common.make

EXECDIR = ../../bin/

MONITORS = $(EXECDIR)process_monitor $(EXECDIR)csh_process_monitor

DIRS += $(EXECDIR)

include $(MITRE_ROOTDIR)/templates/dir.make

$(ALL_BUILD_TARGETS): monitors

PY_SED = sed -e "s!MF_PYFILE!$(MITRE_ROOTDIR)/tools/src/process_monitor/process_monitor.py!" | \
	 sed -e "s!MF_PYTHONBIN!$(PYTHONBIN)!" | \
	 sed -e "s!MF_GC_HOME!$(GC_HOME)!" | \
	 sed -e "s!MF_ARCHOS_PREFIX!$(ARCHOS_PREFIX)!g" | \
	 sed -e "s!MF_SETENV!!"

ifdef PYTHONBIN
$(EXECDIR)process_monitor: process_monitor.py $(EXECDIR) $(GC_HOME)/templates/$(ARCHOS)/config.make
	cat $(MITRE_ROOTDIR)/templates/python.csh | $(PY_SED) > $@
	chmod a+rx $@
else
$(EXECDIR)process_monitor: process_monitor.csh.in $(EXECDIR) $(GC_HOME)/templates/$(ARCHOS)/config.make
	cat process_monitor.csh.in | sed -e "s!MF_MITRE_ROOTDIR!$(MITRE_ROOTDIR)!" > $@
	chmod a+rx $@
endif

$(EXECDIR)csh_process_monitor: process_monitor.csh.in $(EXECDIR)
	cat process_monitor.csh.in | sed -e "s!MF_MITRE_ROOTDIR!$(MITRE_ROOTDIR)!" > $@
	chmod a+rx $@

monitors: $(MONITORS)

clean: 
	/bin/rm -rf *~ \#* core $(CLEAN) *.pyc

distclean veryclean: clean
	/bin/rm -rf $(EXECDIR)process_monitor $(DISTCLEAN)

.PHONY: monitors $(ALL_BUILD_TARGETS)
