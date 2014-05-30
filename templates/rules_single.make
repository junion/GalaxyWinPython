# Portions of this file (c) Copyright 1998 - 2000 M.I.T.
# Portions of this file (c) Copyright 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

#      PURPOSE: This file contains variable definitions, rules, and dependencies
#		used by Makefiles via an 'include' statement. See Makefile.stub 
#		in this directory for an example. The variables provide for two 
#		features:
#
#		-the ability to separate generated object files, libraries, and
#		 executables by architecture and OS of the compiling system
#		-the ability to generate and seperate object files, libraries,
#		 and executables by compile type. (e.g. optimized, debug, etc)
#		
#  		Rules are divided into 3 sets: LIBTARGET, EXECTARGETS, and
#               SERVER, depending on whether the Makefile is creating a library, 
#		executable, or server which uses the serverstub mechanism.
#
#      REQUIRES: One of LIBTARGET, EXECTARGETS, or SERVER to be defined
#

# SAM 6/30/00: Modified this file to assume a single target (see
# rules.make). This allows me to deconstruct the dependencies
# much more easily.

ifndef __RULES_SINGLE_MAKE__
__RULES_SINGLE_MAKE__ = 1

#********************************************************************************
#
# 	VARIABLE DEFINES
#
#********************************************************************************

# SAM 8/23/99: Modified to handle situation without any package 
# definition, and without default settings for GALAXY_HOME, SLS_HOME,
# and NL_HOME. The details of these should be handled in 
# contrib/MIT/templates/rules.make.

# ROOT_DIR is *not* GC_HOME. It's the root directory of the 
# package, under which you can find templates/, include/ and src/ and where
# you'll put lib/. 

ifndef TEMPLATES
TEMPLATES = $(ROOT_DIR)/templates
endif

# If ROOT_DIR is equal to GC_HOME, there's a chance
# that the problem is that the user set it up with pwd
# rather than /bin/pwd (this will mess up the template
# dependencies). Fix it, just in case.

ifneq ($(ROOT_DIR),$(GC_HOME))
  ROOT_TMP := $(ROOT_DIR)
  ROOT_DIR = $(shell cd $(ROOT_TMP); /bin/pwd)
endif

# INCLUDEPATH, LIBPATH and CLEAN_HEADERS are initialized
# in galaxy_preamble.make. 
# previously if they are so desired. PACKAGE_FILE will
# be loaded last.

ifdef PACKAGE
include $(PACKAGE:%=$(TEMPLATES)/%_env.make)
endif

include $(TEMPLATES)/GC_env.make

#
# Default compilation flags
#

# Now, I can set up INCLUDEPATH and LIB_PATH.

# -D__$(ARCH)__ -D__$(OS)__ is now added in config.make.
CPPFLAGS += -I. $(INCLUDEPATH:%=-I%/include)
LDFLAGS  += $(LIBPATH:%=-L%)

include $(TEMPLATES)/archos.make

# MIT site.make. sysdep.make, init.make all rolled into
# $(ARCHOS)/config.make. I will not distribute MIT site.make.in, etc.
# But I have to make sure I inherit updates so that their
# compilations continue to work.

include $(TEMPLATES)/$(ARCHOS)/config.make

CPPFLAGS += $(WARNINGS)

ifndef WARNINGS
# Enable all warnings for GCC
WARNINGS  = -Wall
endif

# For shared libs.

# So here's the next problem. If gcc isn't configured to use
# GNU ld, there's no guessing what the right option is. 
# On Solaris, it's probably -R; elsewhere, I give up. 
# 2/11/02: moved this into configure.

ifdef SHARED_LIBS
ifdef XLINKER_RTFLAG

# For compatibility with the MIT makefile structure, the
# -Xlinker entries for shared libraries are added directly to
# the LDFLAGS, rather than via some indirect mechanism. So
# I need to add only the directories in LDFLAGS which aren't
# already -Xlinker'ed.

# First we munge the XLINKER_RTFLAG together with its pathname, and then
# filter everything else out.

TMP_RPATH := $(filter $(XLINKER_RTFLAG)/%,$(subst $(XLINKER_RTFLAG) -Xlinker /,$(XLINKER_RTFLAG)/,$(LDFLAGS)))

# These are the elements which are already -Xlinker'ed.

XLINKS := $(TMP_RPATH:$(XLINKER_RTFLAG)%=%)

# So we get all the dirs out of the LDFLAGS and filter out
# all the XLINKS.

NEW_LDIRS := $(filter-out $(XLINKS),$(patsubst -L%,%,$(filter -L%,$(LDFLAGS))))

# Now we add the remainder.

LDFLAGS += $(NEW_LDIRS:%=-Xlinker $(XLINKER_RTFLAG) -Xlinker %)

endif
endif

ifdef SHARED_LIBS
  LIBEXT = .so
else
  LIBEXT = .a
endif

ifndef LIBDIR
# LIBDIR is the directory in which to install libraries.
# ARCHLIBDIR is the ARCH-OS specific subdirectory.
LIBDIR      = $(ROOT_DIR)/lib/
endif
ARCHLIBDIR  = $(LIBDIR)$(ARCHOS)/

# SRCDIR is the root of the source subtree
SRCDIR      = $(ROOT_DIR)/src/

# EXECDIR is the directory to install executables and ARCHEXECDIR
# is an ARCH-OS specific subdirectory
ifndef EXECDIR
EXECDIR     = $(ROOT_DIR)/bin/
endif

ARCHEXECDIR = $(EXECDIR)$(ARCHOS)/

# Determine the list of object files from the list of source files
# which should be defined in the Makefile which includes this file.
OBJS     = $(filter %.o,$(SOURCES:%.c=%.o) $(SOURCES:%.cpp=%.o))

# Determine the directories to put .o files.
# Start at ./OBJ and add a subdirectory ARCHOS
OBJDIR   = OBJ/$(ARCHOS)/

# ARCHLINK is defined in templates/plat/config_settings.<archos>

ifdef ARCHLINK
ARCHOBJLINK   = OBJ/$(ARCHLINK)
ARCHLIBLINK   = $(LIBDIR)$(ARCHLINK)
ARCHEXECLINK  = $(EXECDIR)$(ARCHLINK)
endif

# Then additional subdirectories to reflect compile flags

SPECIAL_TARGETS = default all opt debug profile purify insure \
                  thread thread-debug thread-profile thread-purify thread-insure
OBJopt = O
LIBopt = 
EXEopt =
CFLAGSopt = $(CFLAGS_O)

OBJdebug = D
LIBdebug = _debug
EXEdebug = _debug
CFLAGSdebug = $(CFLAGS_D)

OBJprofile = P
LIBprofile = _profile
EXEprofile = _profile
CFLAGSprofile = $(CFLAGS_P)

OBJpurify = D
LIBpurify = _debug
EXEpurify = _purify
CFLAGSpurify = $(CFLAGS_D)

OBJinsure = I
LIBinsure = _insure
EXEinsure = _insure
CFLAGSinsure = $(CFLAGS_I) $(CFLAGS_D)

THREAD_CFLAGS += -DGAL_THREADS -D_REENTRANT

# If the goal is one of the special targets, introduce the
# main dependency and set up the environment. Otherwise, 
# let it go.

ifdef THREAD_SAFE
ifdef THREADS_SUPPORTED
THREADS_OK = 1
endif
endif

ifeq ($(MAKECMDGOALS),$(filter $(SPECIAL_TARGETS),$(MAKECMDGOALS)))

  # First, if the target is "thread", treat it as thread-opt.

  ifeq ($(MAKECMDGOALS),thread)
    ifdef THREADS_OK
      OTHREAD = T
      OTHREADLIB = _thread
      COMMON_LIBS += -lpthread  
      CFLAGS += $(THREAD_CFLAGS)
    endif
    BARE_TARGET = opt
  else

    # Otherwise, if the target is default or all, treat it as opt.

    ifeq ($(MAKECMDGOALS),$(filter default all,$(MAKECMDGOALS)))
      BARE_TARGET = opt
    else

      # Otherwise, if the target has "thread" in it, set up thread safety.

      ifeq (thread,$(findstring thread,$(MAKECMDGOALS)))
 
        # If THREADS_OK is set, then set up a threaded target.
        # Otherwise, default to the bare target.

        ifdef THREADS_OK
          OTHREAD = T
          OTHREADLIB = _thread
          COMMON_LIBS += -lpthread
          CFLAGS += $(THREAD_CFLAGS)
        endif
      endif
      # Strip off the thread- prefix.
      BARE_TARGET = $(patsubst thread-%,%,$(MAKECMDGOALS))
    endif
  endif

  OOBJ = $(OBJ$(BARE_TARGET))
  OLIB = $(LIB$(BARE_TARGET))
  OEXE = $(EXE$(BARE_TARGET))

  # First, always add -fPIC, just in case someone later comes
  # along and tries to compile a .so file when there's already a .a.
  # But only if the bare target isn't profile. And if the bare 
  # target is profile, make sure the LIBEXT is .a, since you
  # can't do .so without -fPIC.

  ifneq (profile,$(BARE_TARGET))
    CFLAGS += -fPIC
  else
    LIBEXT = .a
  endif

  CFLAGS += $(CFLAGS$(BARE_TARGET))

.PHONY: target $(MAKECMDGOALS)
$(MAKECMDGOALS): target

#endif ifeq ($(MAKECMDGOALS),$(filter $(SPECIAL_TARGETS),$(MAKECMDGOALS)))
endif

# At this point, the target is canonicalized.

TRUE_OBJDIR = $(OBJDIR)$(OTHREAD)$(OOBJ)/
OBJEXIST = $(TRUE_OBJDIR).created $(SUBDIRS:%=$(TRUE_OBJDIR)%/.created)
ARCHOBJS = $(OBJS:%=$(TRUE_OBJDIR)%)

ifdef LIBTARGET
# library target

LIBTARGET_BASE = $(basename $(LIBTARGET))

ARCHLIBTARG = $(LIBTARGET_BASE:%=$(ARCHLIBDIR)%$(OTHREADLIB)$(OLIB))

# library target directory
TARGEXIST     = $(ARCHLIBDIR).created
# ARCHOS directory links
ARCHLINKS     = $(ARCHOBJLINK) $(ARCHLIBLINK)

# end of ifdef LIBTARGET
endif

ifdef SERVER
EXECTARGETS = $(SERVER)
endif

# If the target is an executable, generate the right suffix and 
# TWO directories. One is a symbolic link in EXECDIR to fat_binary,
# the other the actual executable in the ARCHEXECDIR
ifdef EXECTARGETS

EXT_EXECTARGETS = $(EXECTARGETS:%=%$(OTHREADLIB)$(OEXE))

ARCHEXECTARGS    = $(EXT_EXECTARGETS:%=$(ARCHEXECDIR)%)

# If there's a definition of the special libs we want, use it.
ifneq ($(SPECIAL_LIBS_$(OTHREAD)$(OOBJ)),)
  TRUE_SPECIAL_LIBS = $(SPECIAL_LIBS_$(OTHREAD)$(OOBJ))
else
  TRUE_SPECIAL_LIBS = $(SPECIAL_LIBS:%=%$(OTHREADLIB)$(OLIB))
endif

# If there's a definition of the libdeps we want, use it.
ifneq ($(LIBDEPS_$(OTHREAD)$(OOBJ)),)
  TRUE_LIBDEPS = $(LIBDEPS_$(OTHREAD)$(OOBJ))
else
  TRUE_LIBDEPS = $(LIBDEPS:%$(LIBEXT)=%$(OTHREADLIB)$(OLIB)$(LIBEXT))
endif

TRUE_LIBS = $(TRUE_SPECIAL_LIBS) $(LIBS:%=%$(OTHREADLIB)$(OLIB)) $(COMMON_LIBS)

# executable target directories
TARGEXIST          = $(ARCHEXECDIR).created
# ARCHOS directory links
ARCHLINKS     = $(ARCHOBJLINK) $(ARCHEXECLINK)

# fat_binary is a shell which EXECTARGS link to, which determines
# the correct ARCHEXECTARGS to invoke
FAT_BINARY = $(EXECDIR)fat_binary
FAT_BINARY_ORIG	= $(TEMPLATES)/fat_binary.csh

# end of ifdef EXECTARGETS
endif

#********************************************************************************
#
# 	TARGETS AND DEPENDENCIES
#
# The first target specified is the default (i.e. what gets made if a target
# is not specified on the make command line). We don't actually have to
# call it "default", but...
#
#********************************************************************************
ifdef TARGEXIST

ifeq ($(BARE_TARGET),insure)
  OBJCC = $(INSURE_CC)
  OBJCXX = $(INSURE_CXX)
  OBJLINKCC = $(INSURE_LINKCC)
else
  ifeq ($(BARE_TARGET),purify)
    OBJCC = $(CC)
    OBJCXX = $(CXX)
    OBJLINKCC = $(PURIFY) $(LINKCC)
  else
    OBJCC = $(CC)
    OBJCXX = $(CXX)
    OBJLINKCC = $(LINKCC)
  endif
endif 

#
# dependencies for object files
#
$(ARCHOBJS): $(OBJEXIST)

#
# General compilation rules for c files
#
$(TRUE_OBJDIR)%.o: %.c
	$(OBJCC) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<

#
# General compilation rules for c++ files
#
$(TRUE_OBJDIR)%.o: %.cpp
	$(OBJCXX) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<

#
# Rules to make directories --- we use .created as a flag file to eliminate
# unnecessary recompilation (it's a long story)
#

$(OBJEXIST):
	@echo; echo "Making Object Directory"
	test -d $(dir $@) || $(MKDIR) $(dir $@)
	touch $@

$(TARGEXIST):
	@echo; echo "Making Target Directory"
	test -d $(dir $@) || $(MKDIR) $(dir $@)
	touch $@

ifdef ARCHOBJLINK
${ARCHOBJLINK}:
	$(RM) ${ARCHOBJLINK}
	ln -s ${ARCHOS} ${ARCHOBJLINK}
endif

# Copy fat binary shell script into target directory
$(FAT_BINARY): $(FAT_BINARY_ORIG) $(TARGEXIST)
	@echo; echo "MAKING fat_binary"
	$(CP) $(FAT_BINARY_ORIG) $@
	touch $@
	$(CHMOD) a+rx $@
	@echo

# Remove core files and object files
ifdef SUBDIRS
SUBDIR_CLEAN = subdirclean
SUBDIR_DISTCLEAN = subdirdistclean
endif

clean: $(SUBDIR_CLEAN)
	$(RM) core \#* *.o *.a *.so $(OBJDIR)* $(SERVERCLEAN) $(MORE_CLEAN)

subdirclean:
	@for i in $(SUBDIRS); do \
		(cd $$i; $(RM) core \#* *.o *.a *.so $(MORE_CLEAN)); \
	done

subdirdistclean:
	@for i in $(SUBDIRS); do \
		(cd $$i; $(RM) .\#* *~); \
	done

# Remove the header dependencies from the end of the Makefile
dependclean:
	@$(RM) $(MAKEFILE).depend

odependclean:
	@cat $(MAKEFILE) | sed '/## Automatic header dependencies -- DO NOT DELETE THIS COMMENT ##/,$$d' > tmp
	@mv tmp $(MAKEFILE)

fixdepend:
	@cat $(MAKEFILE) | sed '/## Automatic header dependencies -- DO NOT DELETE THIS COMMENT ##/,$$d' > tmp
	@mv tmp $(MAKEFILE)
	@cat $(MAKEFILE) | sed '/# Include automatically generated source dependency file/,$$d' > tmp
	@mv tmp $(MAKEFILE)
	echo "# Include automatically generated source dependency file" >> $(MAKEFILE)
	echo 'include $${MAKEFILE}.depend' >> $(MAKEFILE)

# Determine dependencies and append to makefile. Support target "headers" for
# backwards compatibility with old SLS code.
depend headers:
	$(RM) $(MAKEFILE).depend
	@$(MAKE) -s $(MAKEFILE).depend

# The sources must exist, and they may change.

ifndef DEPENDFLAGS
  DEPENDFLAGS = -MM -MG
endif

$(MAKEFILE).depend: $(SOURCES)
	@echo "Making $@: $(SOURCES)"
	@$(RM) $@
	@for i in $(SOURCES); do \
	    ( $(TEMPLATES)/depend.csh $(ARCHOS) $(words $(CLEAN_HEADERS)) $(foreach s,$(CLEAN_HEADERS),$(s) $($(s))) `$(CC) $(DEPENDFLAGS) $(CPPFLAGS) $(CFLAGS) $$i` >> $@  ) ; \
	done
	@test -s $@ || $(RM) $@

#
# We specify a .PHONY target for two reasons. First, if by some fluke a
# file gets created with the phony target name, we want the compile to
# execute the commands for that target anyway. Second, it improves
# performance because make knows not to look for an existing file or
# dependencies for it.
#
.PHONY: clean veryclean distclean dependclean depend headers odependclean odepend oheaders

# end of ifdef TARGEXIST
endif

#********************************************************************************
#
# 	LIBRARY SPECIFIC TARGETS
# 
#********************************************************************************
ifdef LIBTARGET

#
# Rules to create the libraries
#
$(ARCHLIBTARG).a: $(ARCHOBJS) $(OBJECTS)
	$(AR) $@ $?
	$(RANLIB) $@

$(ARCHLIBTARG).so: $(ARCHOBJS) $(OBJECTS)
	$(CC) -shared $^ -o $@ 

# If INSURE isn't defined, but we're trying to compile, 
# then barf.

ifndef INSURE
  ifeq ($(BARE_TARGET),insure)
    FAIL_LIBTARGET = INSURE
  endif
endif

ifdef FAIL_LIBTARGET
target:
	@echo "$(FAIL_LIBTARGET) not defined in templates/$(ARCHOS)/config.make"
	@exit 1
else
# Compilation targets
target: $(OBJEXIST) $(TARGEXIST) $(ARCHLIBTARG)$(LIBEXT) $(ARCHLINKS)
endif

veryclean: clean
	$(RM) $(LIBTARGET:%$(LIBEXT)=$(ARCHLIBDIR)%*)

distclean: veryclean dependclean $(SUBDIR_DISTCLEAN)
	$(RM) OBJ
	$(RM) .\#* *~ $(MORE_DISTCLEAN)
	$(RM) $(ARCHLIBDIR) ${ARCHLIBLINK}

ifdef ARCHLIBLINK
${ARCHLIBLINK}:
	$(RM) ${ARCHLIBLINK}
	ln -s ${ARCHOS} ${ARCHLIBLINK}
endif

# end of ifdef TARGEXIST
endif

#********************************************************************************
#
# 	EXECUTABLE SPECIFIC TARGETS
# 
#********************************************************************************
ifdef EXECTARGETS

MAKEDEPS += $(MAKEFILE)

# These are for the EXECTARGETS (when EXECDIR is not ./)
ifneq ($(EXECDIR),./)
$(EXT_EXECTARGETS): %: $(FAT_BINARY) $(EXECDIR)%
.PHONY: $(EXT_EXECTARGETS)
endif

#
# Rules to create the architecture-independent executables (symbolic links to fat_binary)
#
$(EXT_EXECTARGETS:%=$(EXECDIR)%): $(EXECDIR)%: $(FAT_BINARY) $(ARCHEXECDIR)%
	@echo; echo "MAKING EXECTARGS link to fat_binary"
	test -h $@ || ln -s $(notdir $(FAT_BINARY)) $@
	-touch $@
	@echo

#
# Rules to create the architecture-dependent executables (binaries)
#

# We want to strip the executables when the target is opt,
# but I'm not quite sure how
# I want to do that yet.

$(ARCHEXECTARGS): $(ARCHOBJS) $(MAKEDEPS) $(TRUE_LIBDEPS) ${TARGEXIST}
	@echo
	$(OBJLINKCC) $(CFLAGS) $(LDFLAGS) -o $@ $(ARCHOBJS) $(TRUE_LIBS) $(SYSLIBS)
ifeq ($(BARE_TARGET),opt)
	-$(STRIP) $@
endif

ifndef INSURE
  ifeq ($(BARE_TARGET),insure)
    FAIL_EXECTARGET = "INSURE"
  endif
endif

ifndef PURIFY
  ifeq ($(BARE_TARGET),purify)
    FAIL_EXECTARGET = "PURIFY"
  endif
endif

ifdef FAIL_EXECTARGET
insure:
	@echo "$(FAIL_EXECTARGET) not defined in templates/$(ARCHOS)/config.make"
	@exit 1
else
# If there are multiple EXECTARGETS, make the first one by default
target: $(EXECDIR)$(word 1, $(EXT_EXECTARGETS)) ${ARCHLINKS}
endif

# remove automatically generated header files, binaries and executables

veryclean: clean
	$(RM) $(EXECTARGETS:%=$(ARCHEXECDIR)%*)

distclean: veryclean dependclean $(SUBDIR_DISTCLEAN)
	$(RM) OBJ
	$(RM) .\#* *~ $(MORE_DISTCLEAN)
	$(RM) $(EXT_EXECTARGETS:%=$(EXECDIR)%)
	$(RM) $(FAT_BINARY)
	$(RM) $(ARCHEXECDIR) ${ARCHEXECLINK}

ifdef ARCHEXECLINK
${ARCHEXECLINK}:
	$(RM) ${ARCHEXECLINK}
	ln -s ${ARCHOS} ${ARCHEXECLINK}
endif

# automatic library targets

include $(TARGETS_FILE)

# end of ifdef EXECTARGETS
endif

#********************************************************************************
#
# 	CREATING DIRECTORIES AND LINKS
# 
#********************************************************************************
ifdef DIRECTORIES
DIRSEXIST = $(DIRECTORIES:%=%/.created)

target: $(DIRSEXIST)

$(DIRSEXIST):
	@echo; echo "Making Directory"
	test -d $(dir $@) || $(MKDIR) $(dir $@)
	touch $@

dirsclean:
	$(RM) $(DIRSEXIST) $(DIRECTORIES)

.PHONY: dirsclean

#end of ifdef DIRECTORIES
endif

ifdef LINKS
target: $(LINKS)
clean veryclean distclean: linksclean

ifdef DIRECTORIES
$(LINKS): $(DIRSEXIST)
endif

links:
	$(RM) $(LINKS)
	@$(MAKE) --no-print-directory $(LINKS)

linksclean:
	$(RM) $(LINKS)

.PHONY: links linksclean

#end of ifdef LINKS
endif

#********************************************************************************
#
# 	GENERIC TARGETS
# 
#********************************************************************************

# if no rules.make target (library, server, executable, applet) is being made,
# the default target does nothing.
default:

src:
	cd $(SRCDIR); $(MAKE) -k

src-debug:
	cd $(SRCDIR); $(MAKE) -k debug

include $(TEMPLATES)/template_targets.make

.PHONY: src src-debug

# end of ifndef __RULES_SINGLE_MAKE__
endif

# for Emacs...
# Local Variables:
# mode: makefile
# comment-column: 40
# fill-column: 77
# End:
