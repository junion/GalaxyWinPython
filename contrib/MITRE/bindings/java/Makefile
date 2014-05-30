# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# $Id: Makefile,v 1.11 2002/03/22 15:36:23 wohlever Exp $

MITRE_ROOTDIR = $(shell cd ../..; pwd)

include $(MITRE_ROOTDIR)/templates/env_clear.make

include $(MITRE_ROOTDIR)/templates/common.make

# Only make these things if this is set.
ifdef JDK_HOME

JAVA = $(JDK_HOME)/bin/java
JAR = $(JDK_HOME)/bin/jar

ifndef JAVADOC
JAVADOC = $(JDK_HOME)/bin/javadoc
endif

# If we are using an older JDK (i.e. jdk1.1.x)
ifdef SWING_HOME
CLASSPATH= .:$(JDK_HOME)/lib/classes.zip:lib:$(SWING_HOME)/swingall.jar
endif

# JAVAFLAGS = -deprecation -g

all $(ALL_BUILD_TARGETS): dirs_time_stamp lib/galaxy.jar

GALAXY_SRC = galaxy/lang/*.java galaxy/io/*.java galaxy/util/*.java galaxy/server/*.java galaxy/server/ui/*.java 


lib/galaxy.jar: $(GALAXY_SRC)
	CLASSPATH=$(CLASSPATH) $(JAVAC) $(JAVAFLAGS) -d lib $?
	cp galaxy/server/ui/*.gif lib/galaxy/server/ui
	cd lib;$(JAR) cf ../$@ galaxy

javadocs:
	-mkdir docs
	$(JAVADOC)  -sourcepath $(CLASSPATH) -d docs  galaxy.server galaxy.io galaxy.lang galaxy.util galaxy.server.ui

dirs_time_stamp:
	-mkdir lib
	touch $@

clean_doc:
	-/bin/rm -rf docs/*.html docs/galaxy/*/*.html


.PHONY: clean_doc javadocs

else
all $(ALL_BUILD_TARGETS):

endif

# No matter whether it's enabled or not, I want to clean.

$(ALL_CLEAN_TARGETS): 
	-/bin/rm -rf lib dirs_time_stamp \#~ *~ core

.PHONY: $(ALL_BUILD_TARGETS) $(ALL_CLEAN_TARGETS)

