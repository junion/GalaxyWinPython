;; This file (c) Copyright 1998 - 2002 The MITRE Corporation
;; 
;; This file is part of the Galaxy Communicator system. It is licensed
;; under the conditions described in the file LICENSE in the root 
;; directory of the Galaxy Communicator system.

(in-package "USER")

(defun welcome (conn frame)
  (let ((filename (galaxy::get-object frame ":audiofile")))
    (if (not filename)
	(progn
	  (format *error-output* "No filename provided~%")
	  (return-from welcome nil)))
    ;; Now construct return message.
    (with-open-file (s filename
		     :direction :input :element-type 'unsigned-byte)
      (let ((buf (galaxy::binary-object 'galaxy::gal-binary
					:size (file-length s))))
	(lisp::read-sequence buf s)
	(galaxy-io::write-frame
	 conn
	 (make-instance 'galaxy::gal-clause-frame
	   :name "main"
	   :data `((":audio_data" . ,buf))))
	nil))))

(defun receive_audio (conn frame)
  (let ((data (galaxy::get-object frame ":audio_data")))
    (handler-case
	(with-open-file (s "/dev/audio" :direction :output :element-type
			 'unsigned-byte :if-exists :overwrite)
	  (lisp::write-sequence data s))
      (file-error (format *error-output* "Couldn't open /dev/audio"))))
  nil)

(defun main ()
  (let* ((s (make-instance 'galaxy-io::server
	      :name "audio_binary"
	      :default-port 17800)))
    (setf (galaxy-io::server-debug-p s) t)
    (galaxy-io::add-dispatch-function s "reinitialize" #'welcome)
    (galaxy-io::add-dispatch-function s "receive_audio" #'receive_audio)
    (galaxy-io::run-server s)))

(main)
