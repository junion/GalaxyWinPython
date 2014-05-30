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

# Set the servers. Obligatory. For each server, a script
# loading that server will be created. For each server, you should 
# provide a list of sources, named SRCS.<servername>. These should be
# .java files. Finally, for each server, you should provide the 
# name of the Java class which hosts the mainloop in MAIN.<servername>.

SERVER = audio_binary

SRCS = BinaryData.java MainBinary.java

MAIN = MainBinary

JAR_FILE = binary_example.jar

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

include $(MITRE_ROOTDIR)/templates/java.make
