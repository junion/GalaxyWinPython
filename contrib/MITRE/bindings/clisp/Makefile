# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# In this file, I try to compile the files.

MITRE_ROOTDIR = $(shell cd ../..; /bin/pwd)

ROOTDIR = $(shell cd ../../../..; /bin/pwd)

include $(MITRE_ROOTDIR)/templates/common.make

LISP_OBJS = cGalaxy.fasl SLSUtil.fasl Galaxy.fasl GalaxyIO.fasl 

LISP_SRCS = $(LISP_OBJS:%.fasl=%.cl)

SO_FILE = so_path.make

DIRS += $(ARCHOS)

ACLLIB = $(ARCHOS)/libGalaxyACLSupport.so

SUPPORT_LIB = $(shell cd ..; /bin/pwd)/lib/$(ARCHOS)/libGalaxyBindingSupport.so

ifdef ALLEGROBIN
$(ALL_BUILD_TARGETS): lisp_objs
else
$(ALL_BUILD_TARGETS): 
endif

.PHONY: lisp_objs

lisp_objs: $(LISP_SRCS) $(ACLLIB)
	$(ALLEGROBIN) -L $(ACLLIB) $(foreach clfile,$(filter %.cl,$^),-C $(clfile) -L $(clfile:%.cl=%.fasl)) -kill

clean veryclean:
	/bin/rm -rf $(LISP_OBJS) *~ \#* \.#* core $(SO_FILE) $(ACLLIB)

distclean: veryclean
	/bin/rm -rf $(ARCHOS)

so_path.make:
	/bin/rm -f so_path.make
	$(ALLEGROBIN) -e "(with-open-file (f \"so_path.make\" :direction :output) (format f \"~a~a\" (translate-logical-pathname \"sys:\") (excl::get-shared-library-name)))" -kill

include $(MITRE_ROOTDIR)/templates/dir.make

# To create the library, we need to link together the binding
# support library with the Lisp library. Lisp itself can tell us 
# where to find the Allegro shared library path.

$(ACLLIB): $(SUPPORT_LIB) so_path.make acl_callbacks.c $(ARCHOS)
	$(CC) -g -shared -fPIC -D__$(ARCH)__ -D__$(OS)__ -I$(dir $(shell cat so_path.make))misc -I$(ROOTDIR)/include -I$(ROOTDIR)/templates/$(ARCHOS) acl_callbacks.c $(shell cat so_path.make) $(SUPPORT_LIB) -o $(ACLLIB)

$(SUPPORT_LIB): force_lib_check
	cd ../binding_support; $(MAKE)

.PHONY: $(ALL_BUILD_TARGETS) $(ALL_CLEAN_TARGETS) force_lib_check
