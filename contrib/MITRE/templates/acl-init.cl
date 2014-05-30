;; This file (c) Copyright 1998 - 2002 The MITRE Corporation
;; 
;; This file is part of the Galaxy Communicator system. It is licensed
;; under the conditions described in the file LICENSE in the root 
;; directory of the Galaxy Communicator system.

(in-package "USER")

;; This should be a defsystem, but I'm a little too lazy.
;; I should also have a compilation phase for CL. Later.

;; I don't want it telling me that it's loading things,
;; so I'll open a stream to /dev/null.

(defun choose-file (prefix name)
  (let ((stem (concatenate 'string prefix "/" name)))
    (if (probe-file (concatenate 'string stem ".fasl"))
	(concatenate 'string stem ".fasl")
      (concatenate 'string stem ".cl"))))

(defun load-shared-library (prefix)
  (load (concatenate 'string prefix "/" *galaxy-archos* "/libGalaxyACLSupport.so")))

(let ((prefix (concatenate 'string (sys:getenv "GC_HOME") "/contrib/MITRE/bindings/clisp")))
  (with-open-file (*standard-output* "/dev/null"
		   :direction :output :if-exists :overwrite)
    (load-shared-library prefix)
    (load (choose-file prefix "SLSUtil"))
    (load (choose-file prefix "cGalaxy"))
    (load (choose-file prefix "Galaxy"))
    (load (choose-file prefix "GalaxyIO"))
    (load (choose-file prefix "MGalaxy"))))

;; I need to have the scheduler started.

(mp:start-scheduler)

;; I need to shut off printing in the read-eval-print loop. 

(setq top-level:*print* #'(lambda (&rest args )))

;; I need to eliminate the prompt. This needs to be a
;; format string which takes eight arguments.

(setq top-level::*prompt* "~8*~&")

;; I need to eliminate the startup message.

(setq excl:*print-startup-message* nil)
