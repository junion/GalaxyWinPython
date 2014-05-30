#!/bin/csh -f

# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.


source MF_GC_HOME/contrib/MITRE/templates/env.csh

setenv VALUE_FILE $GC_HOME/templates/$MF_ARCHOS_PREFIXARCHOS/config.values

eval `$GC_HOME/templates/extract_values.csh $VALUE_FILE csh JDK_HOME`

MF_SETENV

exec $JDK_HOME/bin/java -classpath MF_CLASSPATH MF_JAVACLASS $*:q
