# This file (c) Copyright 1998 - 2000 M.I.T.
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

MAKEFILE = Makefile

# Modify this to point to the root directory
ROOT_DIR = $(shell cd ../..; pwd)
TEMPLATES = $(ROOT_DIR)/templates
include $(ROOT_DIR)/templates/archos.make

# Add any special CPP flags here.
CPPFLAGS = -DSLS_VERBOSE_DEFAULT_BW -I$(TEMPLATES)/$(ARCHOS)
LDFLAGS  = 
SPECIAL_LIBS = ${COMMON_LIBS}
LIBDEPS  = 
SPECIAL_LIBS_D = ${COMMON_LIBS}
SPECIAL_LIBS_P = 

# NOTE: ONLY SPECIFY ONE OF THE FOLLOWING THREE TYPES OF TARGETS:
# LIBTARGET, EXECTARGETS, SERVER

# If you're making a library, give the name here.
LIBTARGET = libGalaxy.a

#	galaxy/dialogue.c \
#	galaxy/domain.c \

SUBDIRS = galaxy io ServerStub util

TEMPLATES = $(ROOT_DIR)/templates
include $(TEMPLATES)/archos.make
include $(TEMPLATES)/$(ARCHOS)/config.make

THREAD_SAFE = 1

# The util/compat subdirectory is provided to add missing OS 
# functionality. The GalaxyCommunicator and sls_util libraries
# both include this code, and must be able to link with each other.
# See util/compat/README.

SYSDEP_FILES =

# Support for SunOS, Alpha OSF, HPUX removed.

# No modifications needed for Irix.

# strerror() exists in Linux, but appears not to be thread-safe. However,
# strerror_r() also seems to exist, and I'm getting header clashes with
# my own definition in ppc linux. There are no extras for Linux.

# List all .c files here, including those in subdirectories like so:

SOURCES = \
	galaxy/dispatch_function.c \
	galaxy/dynamic_buffer.c \
	galaxy/error_tags.c \
	galaxy/grovel.c \
	galaxy/gthread_init.c \
	galaxy/local_memory.c \
	galaxy/name_barrier.c \
	galaxy/nfio.c \
	galaxy/nframe.c \
	galaxy/plist.c \
	galaxy/pr_util.c \
	galaxy/program_tags.c \
	galaxy/read_program.c \
	galaxy/print_program.c \
	galaxy/signal.c \
	galaxy/stream_util.c \
	galaxy/string_util.c \
	galaxy/sym.c \
	galaxy/test_conditions.c \
	galaxy/timed_tasks.c \
	galaxy/tobj.c \
	galaxy/uucode.c \
	galaxy/vlist.c \
	galaxy/debug_memory.c \
	io/sockqueue.c \
	io/binary_io.c \
	io/broker_data.c \
	io/hub_server.c \
	io/ip_util.c \
	io/pointer_queue.c \
	io/xdr_buffer.c \
	ServerStub/broker_proxy.c \
	ServerStub/continuation.c \
	ServerStub/galaxy_elr.c \
	ServerStub/generic-server.c \
	ServerStub/generic-server-main.c \
	ServerStub/generic-server-toplevel.c \
	ServerStub/init_server_wrapper.c \
	ServerStub/frame_util.c \
	ServerStub/print_usage.c \
	ServerStub/server_locations.c \
	ServerStub/SS_default_init.c \
	util/gthread.c \
	util/oa.c \
	util/sls_verbose.c \
	util/sls_verbose_color.c \
	util/sock.c \
	util/mkdirp.c

# Some operating systems have older versions of XDR.

ifndef HAVE_XDR_SIZEOF
  SOURCES += util/compat/win32/xdr_sizeof.c
  SUBDIRS += util/compat/win32
endif

ifndef HAVE_STRTOK_R
  SOURCES += util/compat/sunos/strtok_r.c
  SUBDIRS += util/compat/sunos
endif

SOURCES += $(SYSDEP_FILES)

# Include the rules file (make variable settings and targets)

# Here we set the values for INCLUDEPATH, LIBPATH, etc.

# This may be suboptimal, because the MIT folks might be interested
# in having the targets.make floating around as well. So we move
# it to its own templates directory.

include $(ROOT_DIR)/templates/rules.make

# Include automatically generated source dependency file. Make
# sure we don't try to generate .depend when we're doing print-objs,
# because (a) I don't need it and (b) it will be reported in the
# output. Grr. 

ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
  ifneq ($(findstring print-objs,$(MAKECMDGOALS)),print-objs)
    include $(MAKEFILE).depend
  endif
endif

# This is for contrib/MITRE/binding-support/Makefile, which
# needs to know the object pathnames when it's building
# a shared object support library when SHARED_LIBS isn't set.

print-objs:
	echo $(ARCHOBJS:%=$(ROOT_DIR)/src/libGalaxy/%)

.PHONY: print-objs

