# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# We've encountered problems with stuff being inherited from
# environments (such as the value of SCRIPTS). So I'll include
# a file which clears the environment if it hasn't yet been
# cleared.

ifndef __ENV_CLEAR__
__ENV_CLEAR__ = 1

ifeq ($(origin LIBS),environment)
  LIBS = 
endif

ifeq ($(origin CPPFLAGS),environment)
  CPPFLAGS =
endif

ifeq ($(origin DISTCLEAN),environment)
  DISTCLEAN =
endif

ifeq ($(origin MORE_CLEAN),environment)
  MORE_CLEAN =
endif

ifeq ($(origin LDFLAGS),environment)
  LDFLAGS =
endif

ifeq ($(origin COMMON_LIBS),environment)
  COMMON_LIBS =
endif

ifeq ($(origin SPECIAL_LIBS),environment)
  SPECIAL_LIBS =
endif

ifeq ($(origin SCRIPTCMD),environment)
  SCRIPTCMD =
endif

ifeq ($(origin SCRIPTVARS),environment)
  SCRIPTVARS =
endif

ifeq ($(origin CLEAN),environment)
  CLEAN =
endif

ifeq ($(origin SCRIPTS),environment)
  SCRIPTS =
endif

ifeq ($(origin VERYCLEAN),environment)
  VERYCLEAN =
endif

# Also want to unset some of the values in $(ARCHOS)/config.make.

ifeq ($(origin JDK_HOME),environment)
  JDK_HOME =
endif

ifeq ($(origin ORACLE_HOME),environment)
  ORACLE_HOME = 
endif

ifeq ($(origin TT_BASE),environment)
  TT_BASE = 
endif

endif
