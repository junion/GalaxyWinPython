# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Set the MITRE_ROOTDIR to the directory which dominates
# the examples/ directory. Obligatory.

MITRE_ROOTDIR = $(shell cd ../../..; pwd)

# Clear the environment. Obligatory.

include $(MITRE_ROOTDIR)/templates/env_clear.make

# A MITRE example will contain both servers and scripts.
# Each script will expect to find its corresponding .in file.
# If you provide scripts, you should also declare whatever
# SCRIPTVARS you use in the scripts, and you should provide
# values for those vars. For each var <var>, the script may 
# contain a reference to MF_<var>, which will be resolved using
# an automatically generated sed script. The script may
# refer to ROOTDIR, since will be defined.

#SCRIPTS = sample_hub_gui.csh

SCRIPTVARS = MITRE_ROOTDIR

# For each server you intend to define, a Makefile will be
# generated automatically. For each server, you should provide
# a list of sources, named SRCS.<servername>. Obligatory. The
# MIT Makefile templates will be used.

SERVERS = sample_hub_gui

SRCS.sample_hub_gui = sample_hub_gui.c

THREAD_SAFE.sample_hub_gui = 1

EXECDIR = ../../bin/

# If there are any subdirectories which need to be compiled, then 
# they should be included here. Examples in various languages are
# a good case.

#SUBDIRS = 

export EXECDIR

include $(MITRE_ROOTDIR)/templates/MITRE_example.make
