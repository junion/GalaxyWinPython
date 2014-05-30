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

SCRIPTS = audio.csh client_double_audio.csh \
	  double_audio.csh env_audio.csh 

SCRIPTVARS = ROOTDIR MITRE_ROOTDIR

# For each server you intend to define, a Makefile will be
# generated automatically. For each server, you should provide
# a list of sources, named SRCS.<servername>. Obligatory.  The
# MIT Makefile templates will be used.

SERVERS = testaudio_send testaudio_receive\
	  testaudio_abort_send

SRCS.testaudio_send = testaudio_send.c

THREAD_SAFE.testaudio_send = 1

SRCS.testaudio_receive = testaudio_receive.c

THREAD_SAFE.testaudio_receive = 1

SRCS.testaudio_abort_send = testaudio_abort_send.c

# If there are any subdirectories which need to be compiled, then 
# they should be included here. Examples in various languages are
# a good case.

SUBDIRS = python acl java

include $(MITRE_ROOTDIR)/templates/subdir.make
include $(MITRE_ROOTDIR)/templates/MITRE_example.make
