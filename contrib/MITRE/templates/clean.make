# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

ifndef CLEANS_PROVIDED
CLEANS_PROVIDED = 1

clean: localclean

localclean:
	/bin/rm -rf core $(CLEAN) .\#* \#* *~

veryclean: localclean localveryclean

localveryclean:
	@if [ -n "$(VERYCLEAN)" ]; then \
	  (echo /bin/rm -rf $(VERYCLEAN) ; /bin/rm -rf $(VERYCLEAN)) ; \
	fi

distclean: localclean localveryclean
	@if [ -n "$(DISTCLEAN)" ]; then \
	  (echo /bin/rm -rf $(DISTCLEAN) ; /bin/rm -rf $(DISTCLEAN)) ; \
	fi

.PHONY: clean veryclean distclean localclean localveryclean
endif
