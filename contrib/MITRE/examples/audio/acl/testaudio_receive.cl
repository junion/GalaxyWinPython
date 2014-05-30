;; This file (c) Copyright 1998 - 2002 The MITRE Corporation
;; 
;; This file is part of the Galaxy Communicator system. It is licensed
;; under the conditions described in the file LICENSE in the root 
;; directory of the Galaxy Communicator system.

(in-package "USER")

;; Utilities.

(defun play-audio (data outstream)
  (format t "Length of array is ~d~%" (length data))
  (handler-case
      (with-open-file (s "/dev/audio" :direction :output :element-type
		       'unsigned-byte :if-exists :overwrite)
	(lisp::write-sequence data s)
	(force-output s)
	(format t "Length of array is ~d~%" (length data)))
    (file-error (gal-warn outstream "Couldn't open /dev/audio"))))

(defun env-notify (env msg)
  (galaxy-io::write-frame
   env
   (make-instance 'galaxy::gal-clause-frame
     :name "notify"
     :data `((":notification" . ,msg)))))

;; Base class.

(defclass audio-in-broker (galaxy-io::broker-data-in)
  ((audio-data :accessor audio-in-broker-audio-data)))

(defmethod initialize-instance :after ((b audio-in-broker) &rest args)
  (setf (audio-in-broker-audio-data b)
    (galaxy::binary-object 'galaxy::gal-binary)))

(defmethod galaxy-io::data-done-callback :before ((b audio-in-broker))
  (play-audio (audio-in-broker-audio-data b) (galaxy-io::ostream b))
  (broker-notify b "Audio received."))

(defmethod galaxy-io::abort-callback :before ((b audio-in-broker))
  (broker-notify b "Audio aborted."))

(defmethod broker-notify ((b audio-in-broker) msg)
  )

;; Class without environment.

(defclass comm-audio-in-broker (audio-in-broker) ())

(defmethod broker-notify ((b comm-audio-in-broker) msg)
  (galaxy-io::write-frame
   (galaxy-io::broker-data-connection b)
   (make-instance 'galaxy::gal-clause-frame
     :name "notify"
     :data `((":notification" . ,msg)))))

(defmethod galaxy-io::handle-binary ((b comm-audio-in-broker) obj)
  (galaxy::augment-binary-object
   (audio-in-broker-audio-data b) obj))

;; Class with environment.

(defclass env-audio-in-broker (audio-in-broker) ())

(defmethod broker-notify ((b env-audio-in-broker) msg)
  (env-notify (galaxy-io::broker-data-in-environment b) msg))

(defmethod galaxy-io::env-handle-binary ((b env-audio-in-broker) env obj)
  (galaxy::augment-binary-object
   (audio-in-broker-audio-data b) obj))

;; Proxy in stream.

(defclass audio-proxy-in-stream (galaxy-io::broker-proxy-in-stream) ())

(defmethod galaxy-io::handle-object ((b audio-proxy-in-stream) obj)
  (play-audio obj (galaxy-io::ostream b)))

(defmethod galaxy-io::data-done-callback ((b audio-proxy-in-stream))
  (env-notify (galaxy-io::broker-proxy-in-stream-env b) "Audio received."))

(defmethod galaxy-io::abort-callback ((b audio-proxy-in-stream))
  (env-notify (galaxy-io::broker-proxy-in-stream-env b) "Audio aborted."))

;; The broker method is one of the strings "original_env",
;; "original_comm", "proxy_obj", "proxy_stream", "proxy_original".

(defun receive-audio (env frame)
  (let ((broker-method (galaxy-io::connection-data
			(galaxy-io::env-connection env))))
    (cond ((or (null broker-method)
	       (string= broker-method "original_env"))
	   (make-instance 'env-audio-in-broker
	     :environment env
	     :host (galaxy::get-object frame ":binary_host")
	     :port (galaxy::get-object frame ":binary_port")
	     :frame frame))
	  ((string= broker-method "original_comm")
	   (make-instance 'comm-audio-in-broker
	     :connection (galaxy-io::env-connection env)
	     :host (galaxy::get-object frame ":binary_host")
	     :port (galaxy::get-object frame ":binary_port")
	     :frame frame))
	  ((string= broker-method "proxy_obj")
	   (let ((bp (galaxy::get-object frame ":binary_proxy")))
	     (multiple-value-bind (obj success)
		 (galaxy-io::unproxify-object bp env)
	       (cond ((and success
			   (eq (galaxy::get-object-type obj)
			       'galaxy::gal-binary))
		      (play-audio obj (galaxy-io::ostream env))
		      (env-notify env "Audio received."))
		     (t (env-notify env "Audio aborted."))))))
	  ((string= broker-method "proxy_stream")
	   (let ((bp (galaxy::get-object frame ":binary_proxy")))
	     (galaxy-io::unproxify
	      bp env
	      :proxy-stream-type 'audio-proxy-in-stream
	      :immediate nil)))
	  ((string= broker-method "proxy_original")
	   (let ((bp (galaxy::get-object frame ":binary_proxy")))
	     (make-instance 'env-audio-in-broker
	     :environment env
	     :proxy bp
	     :frame frame))))
    nil))

(defun welcome (env frame)
  (setf (galaxy-io::connection-data
	 (galaxy-io::env-connection env))
    (galaxy::get-object frame ":broker_method"))
  nil)

(defun main ()
  (let* ((argv (sys:command-line-arguments :application t))
         (s (make-instance 'galaxy-io::server
              :name "testaudio"
              :argv argv
              :default-port 12346)))
    (galaxy-io::add-dispatch-function s "receive_audio" #'receive-audio)
    (galaxy-io::add-dispatch-function s "reinitialize" #'welcome)
    (galaxy-io::run-server s)))

(main)
