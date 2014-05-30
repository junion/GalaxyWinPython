;; This file (c) Copyright 1998 - 2002 The MITRE Corporation
;; 
;; This file is part of the Galaxy Communicator system. It is licensed
;; under the conditions described in the file LICENSE in the root 
;; directory of the Galaxy Communicator system.

(in-package "USER")

(defparameter *initial-increment* 1)

(defun welcome (env frame)
  (let ((prog-name (galaxy::get-object frame ":program")))
    (if (not prog-name)
        (setf prog-name "main"))
    (galaxy-io::write-frame
     env
     (make-instance 'galaxy::gal-clause-frame
       :name prog-name :data `((":int" . ,*initial-increment*))))
    nil))

(defun double (env frame)
  (let ((i (galaxy::get-object frame ":int"))
        (prog-name (galaxy::get-object frame ":program")))
    (if (not prog-name)
        (setf prog-name "main"))
    (if (< (/ most-positive-fixnum i) 2)
        ;; If we're about to overflow...
        (error "double would overflow most-positive-fixnum"))
    (galaxy-io::write-frame
     env (make-instance 'galaxy::gal-clause-frame :name prog-name
                         :data `((":int" . ,(* 2 i)))))
    nil))

(defun complex-double (env frame)
  (handler-case 
      (let* ((resframe (galaxy-io::dispatch-frame
                        env
                        (make-instance 'galaxy::gal-clause-frame
                          :name "multiply"
                          :data `((":int" . ,(galaxy::get-object frame ":int")))))))
        (setf (galaxy-io::env-continuation-data env) frame)
        (continue-complex-double
         env resframe galaxy-io::*GAL-REPLY-MSG-TYPE*))
    (galaxy-io::dispatch-error (e) nil)))

(defun continue-complex-double (env resframe msg-type)
  (if (= msg-type galaxy-io::*GAL-REPLY-MSG-TYPE*)
      (let ((i (galaxy::get-object resframe ":int"))
            (prog-name (galaxy::get-object (galaxy-io::env-continuation-data env) ":program")))
        (if (not prog-name)
            (setf prog-name "main"))
        (if (< (/ most-positive-fixnum i) 2)
            ;; If we're about to overflow...
            (error "double would overflow most-positive-fixnum"))
        (galaxy-io::write-frame
         env (make-instance
                  'galaxy::gal-clause-frame :name prog-name
                  :data `((":int" . ,(* 2 i)))))))
  nil)

(defun continuation-complex-double (env frame)
  (setf (galaxy-io::env-continuation-data env) frame)
  (galaxy-io::dispatch-frame-with-continuation
   env
   (make-instance 'galaxy::gal-clause-frame
                          :name "multiply"
                          :data `((":int" . ,(galaxy::get-object frame ":int"))))
   #'continue-complex-double))

(defun echo-frame (env frame)
  frame)

(defun main ()
  (let* ((argv (sys:command-line-arguments))
         (s (make-instance 'galaxy-io::server
             :usage-string "[-increment i]"
             :argv argv
             :name "double"
             :default-port 2800)))
    (do ((argv argv))
        ((null argv))
      (cond ((string= (car argv) "-increment")
             (if (not (cdr argv))
                 (progn
                   (galaxy-io::print-usage s)
                   (excl::exit 1)))            
             (setf *initial-increment*
               (read-from-string (elt argv 1)))
             (setf argv (cddr argv)))
            (t (setf argv (cdr argv)))))
    (galaxy-io::add-dispatch-function
     s "reinitialize" #'welcome
     `(nil ,galaxy::*GAL-OTHER-KEYS-NEVER*
           ,galaxy::*GAL-REPLY-NONE*
           nil ,galaxy::*GAL-OTHER-KEYS-NEVER*))    
    (galaxy-io::add-dispatch-function
     s "twice" #'double
     `(((":int" galaxy::gal-int ,galaxy::*GAL-KEY-ALWAYS*))
       ,galaxy::*GAL-OTHER-KEYS-NEVER*
       ,galaxy::*GAL-REPLY-NONE*
       nil ,galaxy::*GAL-OTHER-KEYS-NEVER*))
    (galaxy-io::add-dispatch-function
     s "complex_twice" #'complex-double
     `(((":int" galaxy::gal-int ,galaxy::*GAL-KEY-ALWAYS*))
       ,galaxy::*GAL-OTHER-KEYS-NEVER*
       ,galaxy::*GAL-REPLY-NONE*
       nil ,galaxy::*GAL-OTHER-KEYS-NEVER*))
    (galaxy-io::add-dispatch-function
     s "continuation_complex_twice" #'continuation-complex-double
     `(((":int" galaxy::gal-int ,galaxy::*GAL-KEY-ALWAYS*))
       ,galaxy::*GAL-OTHER-KEYS-NEVER*
       ,galaxy::*GAL-REPLY-NONE*
       nil ,galaxy::*GAL-OTHER-KEYS-NEVER*))
    (galaxy-io::add-dispatch-function
     s "echo_frame" #'echo-frame)
    (galaxy-io::run-server s)))

(main)


