# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef __JAVA_MAKE__
__JAVA_MAKE__ = 1

# If there's no Java, don't do anything.

include $(MITRE_ROOTDIR)/templates/common.make
include $(MITRE_ROOTDIR)/templates/GC_HOME.make
ROOTDIR = $(GC_HOME)

# Step 1: recurse to get down to a single target.

# First, establish a default goal.

ifeq ($(words $(MAKECMDGOALS)),0)
  MAKECMDGOALS = opt
endif

ifneq ($(words $(MAKECMDGOALS)),1)
.PHONY: $(MAKECMDGOALS)

ifdef MAKEFILE
MAKE += -f $(MAKEFILE)
endif

$(MAKECMDGOALS):
	$(MAKE) $@
else

# Step 2: Now that we're down to a single goal, figure out whether
# we're cleaning or not. If we're cleaning, then set up the 
# cleaning stuff. Whether we have multiple servers or a single
# server isn't relevant here.

ifeq ($(findstring $(MAKECMDGOALS),$(ALL_CLEAN_TARGETS)),$(MAKECMDGOALS))

# We're cleaning.

# There are sometimes $ in the class names, which are 
# handled poorly by the shell.

CLASSES_TO_CLEAN := $(shell find . -name '*.class')

CLEAN += $(CLASSES_TO_CLEAN:%='%') $(SERVERS:%=%.in) $(SERVER).in \
	 jar_substitute_$(SERVER) $(SERVERS:%=jar_substitute_%) \
	 $(OTHER_FILES_TO_CLEAN)

DISTCLEAN += $(JAR_FILE) $(foreach x,$(SERVERS),$(JAR_FILE.$(x)))

# veryclean adds the scripts. Note that we add both SERVERS
# and SERVER, so we don't need to figure out which one we have right now.

SCRIPTS += $(SERVERS) $(SERVER)

ONLY_CLEAN = 1
include $(MITRE_ROOTDIR)/templates/script.make

else 

# We're not cleaning. At this point, JDK_HOME must be defined.

ifdef JDK_HOME

# If there are SERVERS defined, we should expect to find
# SRCS.<server>, MAIN.<server>, and optionally JAR_FILE.<server>
# for each one, and we recurse. Otherwise, we expect to find
# SERVER, SRCS, MAIN, and JAR_FILE.

# But what we need to check for is the absence of SERVER,
# rather than the presence of SERVERS, which would lead to
# an infinite recursion.

ifndef SERVER

# If there's only a single JAR_FILE, use it for everyone.

ifndef JAR_FILE
JAR_FILE = $($(@:%.$(MAKECMDGOALS)=JAR_FILE.%))
endif

$(MAKECMDGOALS): $(SERVERS:%=%.$(MAKECMDGOALS))

$(SERVERS:%=%.$(MAKECMDGOALS)):
	$(MAKE) SERVER=$(@:%.$(MAKECMDGOALS)=%) MAIN=$($(@:%.$(MAKECMDGOALS)=MAIN.%)) SRCS="$($(@:%.$(MAKECMDGOALS)=SRCS.%))" JAR_FILE=$(JAR_FILE) $(MAKECMDGOALS)

else

MITRE_JAVA_JAR_FILE = $(MITRE_ROOTDIR)/bindings/java/lib/galaxy.jar

SCRIPTS += $(SERVER)

JAVAFLAGS = -deprecation

# += Appends a "space" between "words", this should fix that
empty := 
space := $(empty) $(empty)
colon := $(empty):$(empty)

VALUE_FILE = $(GC_HOME)/templates/$(ARCHOS)/config.values

SCRIPTCMD += sed "s!MF_JAVACLASS!$(MAIN)!" | \
	     sed "s!MF_CLASSPATH!$(CLASSPATH)!" | \
	     sed "s!MF_GC_HOME!$(GC_HOME)!" | \
	     sed "s!MF_ARCHOS_PREFIX!$(ARCHOS_PREFIX)!g" | \

JAVASCRIPTVARS := $(SCRIPTVARS)

SCRIPTVARS += ROOTDIR MITRE_ROOTDIR

# If there's a jar file specified for this server, then 
# add it to the class path. Otherwise, set up a dependency
# for the source files.

# I don't want to put . in the CLASSPATH list in case the
# script ends up somewhere else.

ifneq ($(JAR_FILE),)
CLASSPATH += $(shell pwd)/$(JAR_FILE)
BUILD_DEPS = jar_$(JAR_FILE)
else
CLASSPATH += $(shell pwd)
BUILD_DEPS = jar_substitute_$(SERVER)
CLEAN += jar_substitute_$(SERVER)
endif

CLASSPATH += $(MITRE_JAVA_JAR_FILE)

CLASSPATH := $(subst $(space),$(colon),$(CLASSPATH))

include $(MITRE_ROOTDIR)/templates/script.make

$(ALL_BUILD_TARGETS): $(BUILD_DEPS)

JAVASEDCMD = sed "s!MF_SETENV!$(foreach s,$(JAVASCRIPTVARS),setenv $(s) MF_$(s);)!"

$(SERVER).in: $(MITRE_ROOTDIR)/templates/java.csh
	cat $(MITRE_ROOTDIR)/templates/java.csh | $(JAVASEDCMD) > $@

# If we already have a jar file, update it. Otherwise, create it.
# Remember, we may be reusing a jar file for the entire pile of examples.

jar_$(JAR_FILE): $(SRCS) $(MITRE_JAVA_JAR_FILE)
	$(JAVAC) -classpath $(CLASSPATH) $(JAVAFLAGS) $(filter-out $(MITRE_JAVA_JAR_FILE),$?)
	if test -f $(JAR_FILE); \
	  then $(JDK_HOME)/bin/jar uf $(JAR_FILE) $(SRCS:%.java=%*.class) $(OTHER_FILES_TO_JAR) ; \
	else $(JDK_HOME)/bin/jar cf $(JAR_FILE) $(SRCS:%.java=%*.class) $(OTHER_FILES_TO_JAR) ; \
	fi

jar_substitute_$(SERVER): $(SRCS) $(MITRE_JAVA_JAR_FILE)
	$(JAVAC) -classpath $(CLASSPATH) $(JAVAFLAGS) $(filter-out $(MITRE_JAVA_JAR_FILE),$?)
	$(TOUCH) jar_substitute_$(SERVER)

$(MITRE_JAVA_JAR_FILE): force_lib_check
	cd $(MITRE_ROOTDIR)/bindings/java; $(MAKE) all

.PHONY: force_lib_check jar_$(JAR_FILE)

endif
# ifdef SERVERS

else

ONLY_CLEAN = 1
include $(MITRE_ROOTDIR)/templates/script.make

endif
# ifdef JDK_HOME

endif
# ifeq ($findstring($(MAKECMDGOALS),$(ALL_CLEAN_TARGETS)),$(MAKECMDGOALS))

endif
# ifneq ($(words $(MAKECMDGOALS)),1)

endif
# ifndef __JAVA_MAKE__
