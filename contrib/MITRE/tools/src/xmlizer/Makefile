# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Set the MITRE_ROOTDIR to the directory which dominates
# the examples/ directory. Obligatory.

MITRE_ROOTDIR = $(shell cd ../../..; pwd)
TOOL_DIR = $(MITRE_ROOTDIR)/tools/lib/python

# Clear the environment. Obligatory.

include $(MITRE_ROOTDIR)/templates/env_clear.make

include $(MITRE_ROOTDIR)/templates/common.make

# Set the scripts. Obligatory. For each script, a file
# with the corresponding .py extension will be used.

PYSCRIPTS = xmlize xml_annotate xml_summarize \
	    xml_log_validate xml_score xml_human_stubber \
	    xml_nist_batch xml_extract_landmarks xml_unify

# If you want to augment the command which produces the scripts
# in any way, add it here. The command should assume that it's
# getting its input from stdin, and should write its output
# to stdout. Optional.

# SCRIPTCMD += 

# Add other things you want to make. Optional.

# ALL +=

# Optional augmentations. 

# CLEAN +=

# VERYCLEAN +=

# DISTCLEAN += 

EXECDIR = ../../bin/

DIRS += $(EXECDIR)

include $(MITRE_ROOTDIR)/templates/dir.make
include $(MITRE_ROOTDIR)/templates/python.make
