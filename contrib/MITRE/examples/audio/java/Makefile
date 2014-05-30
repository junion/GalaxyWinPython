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

SERVERS = testaudio_send testaudio_receive

SRCS.testaudio_send = AudioOut.java MainAudioOut.java

SRCS.testaudio_receive = AudioIn.java MainAudioIn.java AudioInBroker.java 

MAIN.testaudio_receive = MainAudioIn
MAIN.testaudio_send = MainAudioOut

# Pack it all into a single jar file.

JAR_FILE = audio_example.jar

JAVAFLAGS = -g -deprecation
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
