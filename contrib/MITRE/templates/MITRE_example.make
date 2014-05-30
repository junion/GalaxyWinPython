# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef __MITRE_EXAMPLE_MAKE__
__MITRE_EXAMPLE_MAKE__ = 1

# We use MIT_c_server.make as a template to generate
# the makefile. Variables which must be filled are:
# MF_MAKEFILE
# MF_SERVER
# MF_SOURCES

include $(MITRE_ROOTDIR)/templates/GC_HOME.make

ROOTDIR = $(GC_HOME)

MAKEFILE_SEDSCRIPT = sed -e "s!MF_ROOTDIR!$(ROOTDIR)!" | \
		     sed -e "s!MF_GC_HOME!$(GC_HOME)!" | \
		     sed -e "s!MF_MITRE_ROOTDIR!$(MITRE_ROOTDIR)!" | \
                     sed -e "s!MF_MAKEFILE!$@!" | \
                     sed -e "s!MF_SERVER!$(basename $@)!" | \
                     sed -e "s!MF_SOURCES!$(SRCS.$(basename $@))!" | \
		     sed -e "s!MF_THREAD_SAFE!$$mf_thread!"

DISTCLEAN += $(SERVERS:%=%.make)

SCRIPTVARS += GC_HOME

include $(MITRE_ROOTDIR)/templates/script.make

ifndef ALL_PROVIDED
ALL_PROVIDED = 1
all: default
default: opt
endif

opt: $(SERVERS)
debug: $(SERVERS:%=%.debug)
purify: $(SERVERS:%=%.purify)
insure: $(SERVERS:%=%.insure)
profile: $(SERVERS:%=%.profile)
thread: $(SERVERS:%=%.thread)
thread-debug: $(SERVERS:%=%.thread-debug)
thread-purify: $(SERVERS:%=%.thread-purify)
thread-insure: $(SERVERS:%=%.thread-insure)
thread-profile: $(SERVERS:%=%.thread-profile)


SERVER_BUILDS = $(SERVERS:%=%.debug) $(SERVERS:%=%.purify) \
	        $(SERVERS:%=%.insure) $(SERVERS:%=%.profile) \
		$(SERVERS:%=%.thread) $(SERVERS:%=%.thread-debug) \
		$(SERVERS:%=%.thread-profile) $(SERVERS:%=%.thread-purify) \
		$(SERVERS:%=%.thread-insure)

SERVER_CLEANS = $(SERVERS:%=%.clean) $(SERVERS:%=%.veryclean) \
	        $(SERVERS:%=%.distclean)

$(SERVERS): % : %.make
$(SERVERS:%=%.debug): %.debug : %.make
$(SERVERS:%=%.profile): %.profile : %.make
$(SERVERS:%=%.purify): %.purify : %.make
$(SERVERS:%=%.insure): %.insure : %.make
$(SERVERS:%=%.thread): %.thread : %.make
$(SERVERS:%=%.thread-debug): %.thread-debug : %.make
$(SERVERS:%=%.thread-profile): %.thread-profile : %.make
$(SERVERS:%=%.thread-purify): %.thread-purify : %.make
$(SERVERS:%=%.thread-insure): %.thread-insure : %.make

$(SERVERS) $(SERVER_BUILDS):
	$(MAKE) -f $(basename $@).make $(subst .,,$(suffix $@))

$(SERVERS:%=%.make): $(MITRE_ROOTDIR)/templates/MIT_c_server.make $(MITRE_ROOTDIR)/templates/GC_HOME.make
	if [ -n "$(THREAD_SAFE.$(basename $@))" ] ; then export mf_thread; mf_thread="THREAD_SAFE = 1"; fi ; \
	cat $(MITRE_ROOTDIR)/templates/MIT_c_server.make | $(MAKEFILE_SEDSCRIPT) > $@

# It may very well be the case that someone has already done
# a make distclean, and there is no more Makefile. Check this
# before you compile.

$(SERVER_CLEANS):
	@if [ -f $(basename $@).make ] ; then \
	  ( echo "Cleaning $(basename $@).make"; \
            $(MAKE) -f $(basename $@).make $(subst .,,$(suffix $@)) ) ; \
        else echo "Can't find $(basename $@).make; skipping $(subst .,,$(suffix $@))" ; \
        fi

clean: $(SERVERS:%=%.clean)

veryclean: $(SERVERS:%=%.veryclean)

distclean: $(SERVERS:%=%.distclean)

.PHONY: $(SERVERS) $(SERVER_BUILDS) $(SERVER_CLEANS)

endif
