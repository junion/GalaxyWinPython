;; This file (c) Copyright 1998 - 2002 The MITRE Corporation
;; 
;; This file is part of the Galaxy Communicator system. It is licensed
;; under the conditions described in the file LICENSE in the root 
;; directory of the Galaxy Communicator system.

(in-package "USER")

(defvar *Factor* 1)

(defun multiply (env frame)
  (let ((i (galaxy::get-object frame ":int")))
    (if (< (/ most-positive-fixnum i) *Factor*)
	;; If we're about to overflow...
	(error "multiply would overflow most-positive-fixnum"))
    (galaxy::set-prop frame ":int" (* i *factor*))
    frame))

(defun welcome (env frame)
  (let ((factor (galaxy::get-object frame ":factor")))
    (if factor
        (setq *Factor* factor))
    nil))

(defun main ()
  (let* ((s (make-instance 'galaxy-io::server
             :name "multiply"
             :argv (sys:command-line-arguments)
             :default-port 2900)))
    (galaxy-io::add-dispatch-function s "reinitialize" #'welcome)
    (galaxy-io::add-dispatch-function s "multiply" #'multiply)
    (galaxy-io::run-server s)))

(main)
