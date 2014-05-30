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

SCRIPTS = mainloop_double.csh basic_double.csh mainloop_audio.csh

SCRIPTVARS = ROOTDIR MITRE_ROOTDIR

# For each server you intend to define, a Makefile will be
# generated automatically. For each server, you should provide
# a list of sources, named SRCS.<servername>. Obligatory. The
# MIT Makefile templates will be used.

SERVERS = basic_double mainloop_double \
          mainloop_testaudio_send mainloop_testaudio_receive

SRCS.basic_double = basic_double.c double_core.c

SRCS.mainloop_double = mainloop_double.c simple_mainloop_elr.c \
                       simple_mainloop.c double_core.c

SRCS.mainloop_testaudio_send = testaudio_send.c simple_mainloop_elr.c \
			       simple_mainloop.c

SRCS.mainloop_testaudio_receive = testaudio_receive.c \
                                  simple_mainloop_elr.c \
                                  simple_mainloop.c

# If there are any subdirectories which need to be compiled, then 
# they should be included here. Examples in various languages are
# a good case.

SUBDIRS = 

DOUBLE_LINKS = double_core.c double_server.h

TESTAUDIO_LINKS = testaudio_send.c testaudio_receive.c testaudio.h \
	          testaudio_send_server.h testaudio_receive_server.h

LINKS = $(DOUBLE_LINKS) $(TESTAUDIO_LINKS)

all default opt: $(LINKS)

include $(MITRE_ROOTDIR)/templates/subdir.make
include $(MITRE_ROOTDIR)/templates/MITRE_example.make

$(DOUBLE_LINKS): % : ../double/%
	/bin/rm -rf $@
	ln -s ../double/$@ .

$(TESTAUDIO_LINKS): % : ../audio/%
	/bin/rm -rf $@
	ln -s ../audio/$@ .

clean distclean veryclean: link_clean

link_clean:
	/bin/rm -rf $(LINKS)
