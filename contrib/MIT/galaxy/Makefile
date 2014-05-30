# Portions of this file (c) Copyright 1998 - 2000 M.I.T.
# Portions of this file (c) Copyright 2000 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

default all: opt

opt debug libs libs-debug hub hub-debug:
	cd src; $(MAKE) $@

headers depend:
	cd src; $(MAKE) $@

clean veryclean:
	cd src; $(MAKE) $@
	rm -rf \#* .\#* *~

distclean:
	cd src; $(MAKE) $@
	rm -rf \#* .\#* *~
	rm -rf lib bin

remove-logs:
	rm -rf logs

.PHONY: default opt debug all includes libs libs-debug hub hub-debug
.PHONY: headers depend clean veryclean distclean
