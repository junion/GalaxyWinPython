# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This Makefile is strictly for producing the templates
# for the environments.

include GC_HOME.make

ROOTDIR = $(GC_HOME)
MITRE_ROOTDIR = $(shell cd ..; pwd)

# First, I need archos. I can also get init settings
# by loading $(ARCHOS)/config.make.

TEMPLATES = $(ROOTDIR)/templates

include common.make

all default $(ALL_BUILD_TARGETS): env.csh

ENVCMD = sed -e "s!MF_GC_HOME!$(ROOTDIR)!" | \
	 sed -e "s!MF_MITRE_ROOT!$(MITRE_ROOTDIR)!" | \
	 sed -e "s!MF_ARCHOS_PREFIX!$(ARCHOS_PREFIX)!g"

env.csh: env.csh.in GC_HOME.make
	cat env.csh.in | $(ENVCMD) > env.csh

clean: 
	/bin/rm -rf *~ core \#* .\#*

distclean veryclean: clean
	/bin/rm -rf env.csh *.pyc

.PHONY: $(ALL_BUILD_TARGETS) clean distclean veryclean
