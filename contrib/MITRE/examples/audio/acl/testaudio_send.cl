;; This file (c) Copyright 1998 - 2002 The MITRE Corporation
;; 
;; This file is part of the Galaxy Communicator system. It is licensed
;; under the conditions described in the file LICENSE in the root 
;; directory of the Galaxy Communicator system.

(in-package "USER")

;; The Allegro out broker can't fake streaming, because
;; there's no timer loop.

;; The broker method is one of the strings "original_env",
;; "original_comm", "proxy_obj", "proxy_stream", "proxy_original".

(defun welcome (env frame)
  (let ((filename (galaxy::get-object frame ":audiofile"))
	(broker-method (galaxy::get-object frame ":broker_method")))
    (if (not filename)
	(progn
	  (format *error-output* "No filename provided~%")
	  (return-from welcome nil)))
    (with-open-file (s (galaxy::get-object frame ":audiofile")
		     :direction :input :element-type 'unsigned-byte)
      (let ((buf (galaxy::binary-object 'galaxy::gal-binary
					:size (file-length s))))
	(lisp::read-sequence buf s)
	(cond ((or (null broker-method)
		   (member broker-method '("original_env" "original_comm")
			   :test #'string=))
	       (let* ((b (make-instance 'galaxy-io::broker-data-out
			   :connection (galaxy-io::env-connection env)
			   :timeout 10))
		      (f nil))
		 (galaxy-io::write-object b buf)
		 (galaxy-io::data-done b)
		 (setf f (make-instance 'galaxy::gal-clause-frame
			   :name "main"))
		 (galaxy-io::populate-frame b f ":binary_host" ":binary_port")
		 (galaxy-io::write-frame env f)
		 nil))
	      ((member broker-method '("proxy_obj" "proxy_stream"
				       "proxy_original")
		       :test #'string=)
	       (let ((p (make-instance 'galaxy-io::broker-proxy-out
			  :env env :timeout 10 :type 'galaxy::gal-binary)))
		 (galaxy-io::write-object p buf)
		 (galaxy-io::data-done p)
		 (setf f (make-instance 'galaxy::gal-clause-frame
			   :name "main"))
		 (galaxy::set-prop f ":binary_proxy" p)
		 (galaxy-io::write-frame env f))))))))

(defun notify (env frame)
  (format t "Audio send: ~a~%" (galaxy::get-object frame ":notification")))

(defun main ()
  (let* ((argv (sys:command-line-arguments :application t))
	 (s (make-instance 'galaxy-io::server
	      :name "testaudio_send"
	      :argv argv
	      :default-port 12345)))
    (galaxy-io::add-dispatch-function s "reinitialize" #'welcome)
    (galaxy-io::add-dispatch-function s "notify" #'notify)
    (galaxy-io::run-server s)))

(main)
