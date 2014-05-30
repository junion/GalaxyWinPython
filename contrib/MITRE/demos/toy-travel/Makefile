# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# Set the MITRE_ROOTDIR to the directory which dominates
# the examples/ directory. Obligatory.

MITRE_ROOTDIR = $(shell cd ../..; pwd)

# Clear the environment. Obligatory.

include $(MITRE_ROOTDIR)/templates/env_clear.make

# A MITRE example will contain both servers and scripts.
# Each script will expect to find its corresponding .in file.
# If you provide scripts, you should also declare whatever
# SCRIPTVARS you use in the scripts, and you should provide
# values for those vars. For each var <var>, the script may 
# contain a reference to MF_<var>, which will be resolved using
# an automatically generated sed script. The script may
# refer to ROOTDIR, since it's already defined.

# SCRIPTS = 

# SCRIPTVARS = 

# For each server you intend to define, a Makefile will be
# generated automatically. For each server, you should provide
# a list of sources, named SRCS.<servername>. Obligatory. The
# MIT Makefile templates will be used, and the headers will
# generated from a file operations.pgm, which you should provide.

SERVERS = Parser Audio Recognizer Dialogue \
          Generator Backend Synthesizer UI IOMonitor

SRCS.IOMonitor = IOMonitor.c

SRCS.Parser = Parser.c component_engine.c

SRCS.Audio = Audio.c component_engine.c

SRCS.Recognizer = Recognizer.c component_engine.c

SRCS.Dialogue = Dialogue.c component_engine.c

SRCS.Generator = Generator.c component_engine.c

SRCS.Backend = Backend.c component_engine.c

SRCS.Synthesizer = Synthesizer.c component_engine.c

SRCS.UI = UI.c component_engine.c

# In order to get all the values for $(ALL) recognized, I have to make
# sure that the default: dependency is set AFTER both .make files
# are loaded. I can do that by setting the DEFAULT_PROVIDED variable
# and then providing the default later.

# This is created when we run the demo because of logging.

CLEAN += sls

include $(MITRE_ROOTDIR)/templates/MITRE_example.make

