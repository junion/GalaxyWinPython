# This file (c) Copyright 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

# This file is used to create a table of fully-expanded
# values for all the settings in config.make. It calls
# make recursively to retrieve the values using -p and 
# to remove the existing values file. Then it echoes
# all the variables and their values into the new file 
# in a neutral format. It's then possible to extract the 
# values you want in a non-neutral format at run-time
# using the script extract_values.csh.

GC_HOME = $(shell cd ..; /bin/pwd)
ROOT_DIR = $(GC_HOME)
include $(GC_HOME)/templates/archos.make

include $(GC_HOME)/templates/$(ARCHOS)/config.make

ifneq ($(MAKECMDGOALS),vals)
RAW_VALS = $(shell $(MAKE) -s -p -f $(GC_HOME)/templates/extract_values.make vals)

# First, we find the = and := entries and remove the leading space.
# That gives us all the variables. Then we filter out everything
# which doesn't end with a =, and then we remove the =. This gives
# us a list of the variables. Then, we append the origin, so we 
# can filter on it. Finally, we filter out all the ones which 
# don't have a :file suffix, and remove the :file suffix.

empty :=

TARGET_VALS = $(patsubst %:file,%,$(filter %:file,$(foreach var, $(patsubst %=,%,$(filter %=,$(subst $(empty) =,=, $(subst $(empty) :=,=,$(RAW_VALS))))),$(var):$(origin $(var)))))

all: $(TARGET_VALS)

$(TARGET_VALS):
	@echo $@ $($@) >> $(GC_HOME)/templates/$(ARCHOS)/config.values

endif

vals:
	$(RM) -f $(GC_HOME)/templates/$(ARCHOS)/config.values
