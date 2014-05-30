# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifeq ($(ARCHOS),sparc-solaris)
#  SWIG_BIN = /afs/rcf/project/ai-contrib/packages/SWIG1.1-883/@sys/bin/swig -DSWIG11
  SWIG_BIN = /afs/rcf/project/ai-contrib/packages/SWIG-1.3.7/@sys/bin/swig -DSWIG13
endif
