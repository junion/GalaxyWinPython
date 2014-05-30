;; This file (c) Copyright 1998 - 2002 The MITRE Corporation
;; 
;; This file is part of the Galaxy Communicator system. It is licensed
;; under the conditions described in the file LICENSE in the root 
;; directory of the Galaxy Communicator system.

(in-package "GALAXY-IO")

(defconstant *documentation* "
GalIO_IPAddress():                    (ip-address)

GalIO_GetBrokerSocket(b):             [not implemented]
GalIO_GetBrokerCallerData(b):         [not needed; use child class]
GalIO_BrokerSetFinalizer(b, f, data): [not implemented]
GalIO_BrokerWriteFrame(b, f):         (write-object b f)
GalIO_BrokerWriteString(b, s):        (write-object b s)
GalIO_BrokerWriteBinary(b, data, n):  (write-object b data)
GalIO_BrokerWriteInt16(b, data, n):   (write-object b data)
GalIO_BrokerWriteInt32(b, data, n):   (write-object b data)
GalIO_BrokerWriteInt64(b, data, n):   (write-object b data)
GalIO_BrokerWriteFloat32(b, data, n): (write-object b data)
GalIO_BrokerWriteFloat64(b, data, n): (write-object b data)
GalIO_BrokerDataDone(b):              (data-done b)
GalIO_BrokerDataOutDone(b):           (data-done b)
GalIO_BrokerDataOutInit(...):         (make-instance 'broker-data-out :connection conn :port port :frame frame)
GalIO_CommBrokerDataInInit(...):          (make-instance 'broker-data-in :host host :port port :frame frame :connection conn)
GalIO_BrokerStructQueuePop(b):        [not needed; no broker queues in Allegro]
GalIO_SetBrokerActive(b):             [not needed; no broker queues in Allegro]
GalIO_BrokerStructQueueAppend(...):   [not needed; no broker queues in Allegro]
GalIO_GetBrokerFrame(...):            [not implemented]
GalIO_BrokerStructDequeue(...):       [not needed; no broker queues in Allegro]
GalIO_FrameSetBrokerCallID(f, call_id): [not implemented]

GalIO_DestroyCommStruct(conn):        [not needed]
GalIO_GetServerListenPort(s):         (server-port s)
GalIO_GetCommSocket(conn):            [not implemented]
GalIO_CloseCommSocket(conn):          [not implemented]
GalIO_GetCommHost(conn):              [not implemented]
GalIO_GetCommFrame(conn):             [not implemented]
GalIO_GetCommServerData(conn):        [not needed; use instance attributes]
GalIO_SetCommDone(conn):              [not implemented]
GalIO_ResetCommSockets(conn):         [not implemented]
GalIO_CommWriteFrame(c, f, block):    (write-frame c f)
GalIO_CommDispatchFrame(c, f, key):   (dispatch-frame c f)
GalIO_CommFlushOutQueue(conn):        [not implemented]
GalIO_CommReadReady(conn):            [not implemented]
GalIO_ClientInit(...):                [not implemented]
GalIO_OutHandler(conn):               (run-connection c) (approximately)
GalIO_InHandler(conn):                (run-connection c) (approximately)
GalIO_ConnectionPoll(conn):           [not implemented; polling by select]
GalIO_ServerHandler(conn):            (run-connection c) (approximately)
GalIO_HubHandler(conn):               [not implemented]
GalIO_ServerInit(port, require_port, ...): (make-instance 'server :name ... :default-port port :require-port require-port)
GalIO_SetServerDefaultPort(..):       (setf (server-default-port s) p)
GalIO_CommValidating(c):              (server-validate (connection-server c))
GalIO_DestroyServerStruct(s):         [not needed]
GalIO_EnableDispatchFnValidation(s):  (setf (server-validate s) t)
GalIO_ServerPoll(s):                  [not implemented; polling by select]
GalIO_SetCommData(c, data):           [not needed; use child class]
GalIO_ServerUsesTimedTasks(s):        [not implemented; polling by select]
GalIO_SetServerDone(s):               [not needed]
GalIO_GetServerDefaultPort(s):        (server-default-port s)
GalIO_GetServerData(s):               [not needed; use child class]
GalIO_GetCommServerName(c):           (server-name (connection-server c))
GalIO_SetServerData(s, data):         [not needed; use child class]
GalIO_SetServerMaxConnections(s, m):  (setf (server-maxconns s) m)
GalIO_GetError(fr):                   [not needed; catch dispatch-error]
GalIO_SetServerName(s, name):         (setf (server-name s) name)
GalIO_GetUniqueConnection(s):         [not implemented]
GalIO_GetServerMaxConnections(s):     (server-maxconns s)
GalIO_GetCommData(c):                 [not needed; use child class]
GalIO_GetServerName(s):               (server-name s)

GalSS_AddDispatchFunction(...):       (add-dispatch-function ...)
GalSS_StartAndRunServer(s):           (run-server s)
GalSS_ExtractServerArgs(...):         [not implemented]
GalSS_EnvError(e, errdesc):           [not needed; just raise error]
GalSS_EnvDestroyToken(e):             (destroy-token e)
GalSS_EnvGetCommData(e):              [not needed; use child class]
GalSS_InitializeServerDefaults(...):  [not implemented]
GalSS_EnvComm(e):                     (env-connection c)
GalSS_CmdlineInitializeServer(...):   [not implemented]
GalSS_EnvSetCommData(e, data):        [not needed; use child class]
GalSS_EnvWriteFrame(e, fr):           (write-frame e fr)
GalSS_InitializeServerFromServerArgs(...): (make-instance 'server ... )
GalSS_InitializeServer(...):          (make-instance 'server ... )
GalSS_EnvDispatchFrame(e, fr):        (dispatch-frame e fr)
")

;; These are an enum in io_msg_types.h.

(defconstant *GAL-OBJECT-MSG-TYPE* 0)
(defconstant *GAL-MESSAGE-MSG-TYPE* 1)
(defconstant *GAL-REPLY-MSG-TYPE* 2)
(defconstant *GAL-DESTROY-MSG-TYPE* 3)
(defconstant *GAL-BROKER-START-MSG-TYPE* 4)
(defconstant *GAL-BROKER-END-MSG-TYPE* 5)
(defconstant *GAL-ERROR-MSG-TYPE* 6)
(defconstant *GAL-DISCONNECT-MSG-TYPE* 7)
(defconstant *GAL-POSTPONE-MSG-TYPE* 8)

(defconstant *GAL-APPLICATION-ERROR* 0)
(defconstant *GAL-NO-OPNAME-ERROR* 1)
(defconstant *GAL-TRANSMISSION-ERROR* 2)
(defconstant *GAL-RECEPTION-ERROR* 3)
(defconstant *GAL-SERVER-DOWN-ERROR* 4)
(defconstant *GAL-NO-FRAME-ERROR* 5)
(defconstant *GAL-CONN-REJECTION-ERROR* 6)

(defconstant *GAL-SERVER-LISTENER-STARTUP-EVENT* 0)
(defconstant *GAL-SERVER-LISTENER-SHUTDOWN-EVENT* 1)
(defconstant *GAL-SERVER-CLIENT-POLL-STARTUP-EVENT* 2)
(defconstant *GAL-SERVER-DESTRUCTION-EVENT* 3)
(defconstant *GAL-SERVER-CONNECTION-CREATION-EVENT* 4)
(defconstant *GAL-CONNECTION-BROKER-OUT-STARTUP-EVENT* 5)
(defconstant *GAL-CONNECTION-BROKER-IN-STARTUP-EVENT* 6)
(defconstant *GAL-CONNECTION-BROKER-OUT-CREATION-EVENT* 7)
(defconstant *GAL-CONNECTION-BROKER-IN-CREATION-EVENT* 8)
(defconstant *GAL-CONNECTION-DISPATCH-FN-EVENT* 9)
(defconstant *GAL-CONNECTION-SHUTDOWN-EVENT* 10)
(defconstant *GAL-CONNECTION-DESTRUCTION-EVENT* 11)
(defconstant *GAL-BROKER-DATA-DONE-EVENT* 12)
(defconstant *GAL-BROKER-ABORT-EVENT* 13)
(defconstant *GAL-BROKER-DESTRUCTION-EVENT* 14)
(defconstant *GAL-BROKER-CONNECTION-EVENT* 15)

(defconstant *GAL-SERVER-READS-ONLY-FROM-SESSION* 1)
(defconstant *GAL-SERVER-WRITES-ONLY-TO-SERVER* 2)
(defconstant *GAL-SERVER-WRITES-ONLY-TO-SESSION* 4)
(defconstant *GAL-PERMANENT-LOCK* 8)

(defun ip-address ()
  (c-galaxy::ip-address))

;; 
;; Error reporting macro. In Python, I can reraise with
;; the same traceback, but not in Lisp.
;;

(defmacro maybe-handler-case ((flag) op &body cases)
  `(if (not ,flag)
       (handler-case ,op . ,cases)
     ,op))

(define-condition galaxy-io-unknown-type-error (simple-error)
  ((obj-type :initarg :obj-type))
  (:report
   (lambda (condition stream)
     (with-slots (obj-type) condition
       (format stream "Unknown type ~s" obj-type)))))

(define-condition connection-dead (simple-error)
  ((connection :initarg :connection :accessor connection-dead-connection))
  (:report
   (lambda (condition stream)
     (with-slots (connection) condition
       (format stream "Connection is dead: ~s" connection)))))

;; 
;; Brokering.
;;

(defclass broker-data ()
  ((ostream :accessor ostream)
   (c-broker :accessor broker-data-c-broker :initarg :c-broker :initform nil)
   (server :accessor broker-data-server :initform nil)
   (connection :accessor broker-data-connection :initarg :connection :initform nil)
   (status :accessor broker-data-status :initform :not-yet-connected)))
   
(defmethod initialize-instance :after ((b broker-data) &rest args)
  (cond ((broker-data-connection b)
	 (inherit-from-connection b))))

(defmethod inherit-from-connection ((b broker-data))
  (setf (ostream b) (ostream (broker-data-connection b)))
  (push b (connection-brokers (broker-data-connection b)))
  (setf (broker-data-server b) (connection-server (broker-data-connection b))))

(defmethod disconnect ((b broker-data))
  (disconnect-1 b t))

(defmethod disconnect-callback ((b broker-data))
  (disconnect-1 b))

(defmethod disconnect-1 ((b broker-data) &optional (force-destroy nil))
  (cond ((and force-destroy
	      (broker-data-c-broker b)
	      (not (zerop (broker-data-c-broker b))))
	 (data-done b)
	 (c-galaxy::destroy-broker-struct
	  (broker-data-c-broker b)))
	((not (eq (broker-data-status b) :disconnected))
	 (setf (broker-data-c-broker b) nil)
	 (setf (connection-brokers
		(broker-data-connection b))
	   (remove b (connection-brokers
		      (broker-data-connection b))))
	 (setf (broker-data-status b) :disconnected))))

;; Fn takes no arguments. Use lambdas.

(defmethod add-callback ((b broker-data) event fn)
  (c-galaxy::add-broker-callback
   (broker-data-c-broker b) event fn))

(defclass broker-data-out (broker-data)
  ((server-status :accessor broker-data-out-server-status
		  :initform :not-yet-connected)
   (timeout :accessor broker-data-out-timeout
	    :initform 0 :initarg :timeout)
   (port :accessor broker-data-out-port :initform 0)
   (call-id :accessor broker-data-out-call-id :initform nil)
   (done :accessor broker-data-out-done :initform nil)))

(define-condition broker-connected (condition) () )

(defmethod initialize-instance :after ((b broker-data-out) &rest ignore)
  (if (not (broker-data-connection b))
      (error 'broker-connected))
  (let* ((c-broker (c-galaxy::broker-data-out-init
		    (connection-c-connection
		     (broker-data-connection b))
		    (broker-data-out-timeout b))))
    (cond ((not (zerop c-broker))
	   (c-galaxy::add-broker-callback
	    c-broker *GAL-BROKER-DESTRUCTION-EVENT*
	    #'(lambda () (disconnect-callback b)))
	   (setf (broker-data-out-port b)
	     (c-galaxy::get-broker-listen-port c-broker))
	   (setf (broker-data-out-call-id b)
	     (c-galaxy::get-broker-call-id c-broker))
	   (setf (broker-data-c-broker b) c-broker)
	   (mp::process-run-function
	    "Broker data out loop"
	    #'run-broker b))
	  (t (error 'broker-connected)))))

(defmethod data-done ((b broker-data-out))
  (setf (broker-data-out-done b) t)
  (if (broker-data-c-broker b)
      (c-galaxy::broker-data-out-done
       (broker-data-c-broker b))))

(defmethod populate-frame ((b broker-data-out) f host-key port-key)
  (galaxy::set-prop
   f port-key (broker-data-out-port b))
  (galaxy::set-prop
   f ":call_id" (broker-data-out-call-id b))
  (galaxy::set-prop
   f host-key (ip-address)))

(define-condition galaxy-io-type-error (simple-error)
  ((obj-type :initarg :obj-type))
  (:report
   (lambda (condition stream)
     (with-slots (obj-type) condition
       (format stream "Unexpected type ~s" obj-type)))))

(defmethod write-object ((b broker-data-out) obj)
  (if (broker-data-c-broker b)
      (typecase obj
	((or galaxy::gal-binary galaxy::gal-int-16 galaxy::gal-int-32)
	 (let ((data (galaxy::binary-data-to-string obj)))
	   (typecase obj
	     (galaxy::gal-binary
	      (c-galaxy::broker-write-binary
	       (broker-data-c-broker b) data (length obj)))
	     (galaxy::gal-int-16
	      (c-galaxy::broker-write-int-16
	       (broker-data-c-broker b) data (length obj)))
	     (galaxy::gal-int-32
	      (c-galaxy::broker-write-int-32
	       (broker-data-c-broker b) data (length obj))))))
	((or galaxy::gal-float-32 galaxy::gal-float-64)
	 (typecase obj	     
	   (galaxy::gal-float-32
	    (c-galaxy::broker-write-float-32
	     (broker-data-c-broker b) obj (length obj)))
	   (galaxy::gal-float-64
	    (c-galaxy::broker-write-float-64
	     (broker-data-c-broker b) obj (length obj)))))
	(galaxy::gal-frame
	 (let ((c-frame (galaxy::lisp-to-c obj)))
	   (c-galaxy::broker-write-frame
	    (broker-data-c-broker b) c-frame)
	   (c-galaxy::free-frame c-frame)))
	(galaxy::gal-string
	 (c-galaxy::broker-write-string
	  (broker-data-c-broker b) obj))
	((or galaxy::gal-int galaxy::gal-list galaxy::gal-float
	  galaxy::gal-symbol)
	 (let ((c-obj (galaxy::val-to-object obj)))
	   (c-galaxy::broker-write-object
	    (broker-data-c-broker b) c-obj)
	   (c-galaxy::free-object c-obj)))
	(t (error 'galaxy-io-type-error
		  :obj-type (galaxy::get-object-type obj))))))

(defmethod expire ((b broker-data-out))
  (c-galaxy::force-broker-expiration (broker-data-c-broker b)))

;; This function should read a frame from the connection,
;; verify that it's the right message, and if so, do the writing
;; and signal that the connection is established, so
;; the broker connection process can exit.

;; Most of the rest of this stuff is for synchronization
;; of the state of the C object and this one.

;; Return values for broker-data-out-callback-handler are as follows:
;; 0: still polling
;; 1: it's done, and the broker has been destroyed.
;; -1: error, and the broker connection has shut down, but the
;;     server may still be running
;; -2: error, and the broker has been destroyed

;; If the result is 0, we need to make sure that
;; we're still polling on the right ports.

;; This should all be handled correctly in the callbacks.

;; How do I ensure that the synchronization is being
;; managed correctly?

(defmethod run-broker ((b broker-data-out))
  (loop
    (if (null (broker-data-c-broker b))
	(return-from run-broker))
    (mp::process-wait-with-timeout "Wait a while" 0.1 #'(lambda () nil))
    ;; Check again!
    (if (null (broker-data-c-broker b))
	(return-from run-broker))
    (c-galaxy::broker-data-out-callback-handler
     (broker-data-c-broker b))))

;; Incoming.

(defclass broker-data-in (broker-data)
  ((environment :accessor broker-data-in-environment
		:initform nil :initarg :environment)
   (callback-data :accessor broker-data-in-callback-data
		  :initform nil :initarg :callback-data)))

(defmethod initialize-instance :after ((b broker-data-in)
				       &key (host nil)
					    (port nil)
					    (frame nil)
					    (proxy nil)
				       &allow-other-keys)
  (cond ((and (broker-data-in-environment b)
	      (not (broker-data-connection b)))
	 (setf (broker-data-connection b)
	   (env-connection
	    (broker-data-in-environment b)))
	 (inherit-from-connection b)))
  (if (not (broker-data-connection b))
      (error 'broker-connected))
  (if (and proxy (null (broker-data-in-environment b)))
      (error "proxy requires environment"))
  (if (and (null proxy) (or (null port) (null host)))
      (error "no host and port for client"))
  (let ((c-broker nil))
    (if proxy
	(setf c-broker (c-galaxy::env-broker-proxy-in-init
			(env-c-env (broker-data-in-environment b))
			(broker-proxy-c-proxy proxy)
			#'(lambda (dt data)
			    (handle-broker-data b dt data))))
      (let ((c-frame (galaxy::lisp-to-c frame)))
	(setf c-broker
	  (cond ((broker-data-in-environment b)
		 (c-galaxy::env-broker-data-in-init
		  (env-c-env
		   (broker-data-in-environment b))
		  host port c-frame
		  #'(lambda (dt data)
		      (handle-broker-data b dt data))))
		((broker-data-connection b)
		 (c-galaxy::comm-broker-data-in-init
		  (connection-c-connection
		   (broker-data-connection b))
		  host port c-frame
		  #'(lambda (dt data)
		      (handle-broker-data b dt data))))
		(t (error "Neither connection nor environment for incoming broker"))))
	(c-galaxy::free-frame c-frame)))
    (if (not (zerop c-broker))
	(progn
	  (c-galaxy::add-broker-callback
	   c-broker *GAL-BROKER-DESTRUCTION-EVENT*
	   #'(lambda () (disconnect-callback b)))
	  (c-galaxy::add-broker-callback
	   c-broker *GAL-BROKER-DATA-DONE-EVENT*
	   #'(lambda () (data-done-callback b)))
	  (c-galaxy::add-broker-callback
	   c-broker *GAL-BROKER-ABORT-EVENT*
	   #'(lambda () (abort-callback b)))
	  (setf (broker-data-c-broker b) c-broker)
	  (if (broker-data-in-environment b)
	      (let ((new-env (copy-environment
			      (broker-data-in-environment b))))
		(setf (env-c-env new-env)
		  (c-galaxy::broker-get-environment c-broker))
		(env-lock-environment new-env)
		(setf (broker-data-in-environment b) new-env)))
	  (c-galaxy::set-broker-active
	   (broker-data-c-broker b))
	  (mp::process-run-function
	   "Broker data in loop"
	   #'run-broker b))
      (error 'broker-connected))))

(defmethod data-done-callback ((b broker-data-in))
  )

(defmethod abort-callback ((b broker-data-in))
  )

;; Return values are:
;; 1 if the broker is done and has been destroyed,
;; 0 if not done
;; -1 if error was encountered and the broker has been destroyed.

;; This should all happen in the callbacks now. 

(defmethod run-broker ((b broker-data-in))
  (let ((streams (c-galaxy::get-broker-socket
		  (broker-data-c-broker b))))
    (loop
      (if (or (eq (broker-data-status b) :disconnected)
	      (not (broker-data-c-broker b)))
	  (return-from run-broker))
      (mp::wait-for-input-available streams :timeout 0.1)
      ;; Check again!
      (if (or (eq (broker-data-status b) :disconnected)
	      (not (broker-data-c-broker b)))
	  (return-from run-broker))
      (c-galaxy::broker-data-in-callback-handler
       (broker-data-c-broker b) 0))))

(defmethod data-done ((b broker-data-in))
  (if (broker-data-c-broker b)
      (c-galaxy::broker-data-done
       (broker-data-c-broker b))))

;; The arguments here are all foreign pointers or integers. 

(defmethod handle-broker-data ((b broker-data-in) dt data)
  (if (zerop data)
      (error 'galaxy-io-type-error
             :obj-type dt))
  (let ((o (galaxy::object-to-val data)))
    (case (car (rassoc dt galaxy::*TYPE-TABLE*))
      (galaxy::gal-frame
       (handle-frame b o))
      (galaxy::gal-string
       (handle-string b o))
      (galaxy::gal-symbol
       (handle-symbol b o))      
      (galaxy::gal-float
       (handle-float b o))      
      (galaxy::gal-int
       (handle-int b o))
      (galaxy::gal-list
       (handle-list b o))
      (galaxy::gal-binary
       (handle-binary b o))
      (galaxy::gal-int-16
       (handle-int-16 b o))
      (galaxy::gal-int-32
       (handle-int-32 b o))
      (galaxy::gal-float-32
       (handle-float-32 b o))
      (galaxy::gal-float-64
       (handle-float-64 b o))
      (galaxy::gal-proxy
       (handle-proxy b o)))))

(define-condition broker-data-not-handled (simple-error)
  ((obj :initarg :obj)
   (broker :initarg :broker))
  (:report
   (lambda (condition stream)
     (with-slots (obj broker) condition
       (format stream "For broker ~a: ~a" broker obj)))))

(defmethod handle-frame ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-frame
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))

(defmethod env-handle-frame ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))  

(defmethod handle-string ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-string
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))

(defmethod env-handle-string ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

(defmethod handle-symbol ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-symbol
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))

(defmethod env-handle-symbol ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

(defmethod handle-float ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-float
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))

(defmethod env-handle-float ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

(defmethod handle-int ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-int
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))

(defmethod env-handle-int ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

(defmethod handle-list ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-list
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))

(defmethod env-handle-list ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

(defmethod handle-binary ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-binary
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))
  
(defmethod env-handle-binary ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

(defmethod handle-int-16 ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-int-16
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))
  
(defmethod env-handle-int-16 ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

(defmethod handle-int-32 ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-int-32
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))
  
(defmethod env-handle-int-32 ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

(defmethod handle-int-64 ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-int-64
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))
  
(defmethod env-handle-int-64 ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

(defmethod handle-float-32 ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-float-32
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))
  
(defmethod env-handle-float-32 ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

(defmethod handle-float-64 ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-float-64
       b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))
  
(defmethod env-handle-float-64 ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

(defmethod handle-proxy ((b broker-data-in) obj)
  (if (broker-data-in-environment b)
      (env-handle-proxy b (broker-data-in-environment b) obj)
    (error 'broker-data-not-handled :broker b :obj obj)))

(defmethod env-handle-proxy ((b broker-data-in) env obj)
  (error 'broker-data-not-handled :broker b :obj obj))

;;
;; Broker proxies.
;;

(defclass broker-proxy ()
  ((env :accessor broker-proxy-env :initarg :env :initform nil)
   ;; We don't want to use the presence of the c-proxy to
   ;; determine connected/disconnected, because the C proxy
   ;; will be freed when the element is garbage collected
   ;; (and may be needed AFTER the data is retrieved).
   (disconnected :accessor broker-proxy-disconnected
		 :initform nil)
   (c-proxy :accessor broker-proxy-c-proxy
	    :initarg :c-proxy :initform nil)))

(defmethod broker-proxy-register ((b broker-proxy))
  (push b (connection-proxies (env-connection (broker-proxy-env b)))))

(defsetf broker-proxy-c-proxy set-broker-proxy-c-proxy)

(defmethod set-broker-proxy-c-proxy ((b broker-proxy) new-c-proxy)
  (with-slots (c-proxy) b
    (setf c-proxy new-c-proxy))
  ;; Schedule a finalizer which frees.
  (if (and new-c-proxy (not (zerop new-c-proxy)))
      (excl::schedule-finalization 
       b #'free-broker-proxy)))

(defmethod free-broker-proxy ((b broker-proxy))
  (if (broker-proxy-c-proxy b)
      (c-galaxy::free-broker-proxy
       (broker-proxy-c-proxy b)))
  (setf (broker-proxy-c-proxy b) nil))

(defmethod add-callback ((b broker-proxy) event fn)
  (if (broker-proxy-c-proxy b)
      (c-galaxy::add-broker-callback
       (c-galaxy::broker-proxy-broker
	(broker-proxy-c-proxy b))
       event fn)))

(defmethod disconnect ((b broker-proxy))
  (if (member b (connection-proxies
		 (env-connection (broker-proxy-env b))))
      (remove b (connection-proxies
		 (env-connection (broker-proxy-env b)))))
  (setf (broker-proxy-disconnected b) t))

(defclass broker-proxy-out (broker-proxy)
  ((type :accessor broker-proxy-out-type :initarg :type :initform nil)
   (obj-found :accessor broker-proxy-out-obj-found :initform nil)
   (obj :accessor broker-proxy-out-obj :initarg :obj :initform nil)))

;; NIL for the type means -1.

;; :obj NIL needs to be an empty list which is 
;; being out-proxied. Therefore, I need to be testing if
;; out is present.

(defmethod initialize-instance :before ((b broker-proxy-out)
					&rest args
					&key obj type
				        &allow-other-keys)
  (multiple-value-bind (obj-present val car-list)
      (get-properties args '(:obj))
    (if obj-present
	(setf (broker-proxy-out-obj-found b) t))
    (if (and obj-present type
	     (not (eq type (galaxy::get-object-type obj))))
	(error "obj and type don't match"))
    (if (and obj-present (not type))
	(setf (broker-proxy-out-type b)
	  (galaxy::get-object-type obj)))
    ))
  

;; NIL for a type means any type. Note, also, the note 
;; above for :obj NIL.

(defmethod initialize-instance :after ((b broker-proxy-out)
				       &key (timeout 0)
				       &allow-other-keys)
  (let ((otype-num -1))
    (if (broker-proxy-out-type b)
	(setf otype-num (galaxy::get-object-integer
			 (broker-proxy-out-type b))))
    (setf (broker-proxy-c-proxy b)
      (c-galaxy::proxify-object-type
       (env-c-env (broker-proxy-env b))
       otype-num -1 timeout)))
  (if (zerop (broker-proxy-c-proxy b))
      (error "no proxy"))
  (broker-proxy-register b)
  (if (or (broker-proxy-out-obj b)
	  (broker-proxy-out-obj-found b))
      (if (eq 'galaxy::gal-list (broker-proxy-out-type b))
	  (dolist (x (broker-proxy-out-obj b))
	    (c-galaxy::proxy-write
	     (broker-proxy-c-proxy b)
	     (galaxy::val-to-object x) 1))
	(c-galaxy::proxy-write
	 (broker-proxy-c-proxy b)
	 (galaxy::val-to-object (broker-proxy-out-obj b)) 1)))
  (mp::process-run-function
   "Broker proxy out loop"
   #'run-broker-proxy b))

(defmethod write-object ((b broker-proxy-out) obj)
  (let ((res (c-galaxy::proxy-write
	      (broker-proxy-c-proxy b)
	      (galaxy::val-to-object obj) 1)))
    (cond ((< res 0)
	   (error "can't write obj"))
	  ((or (broker-proxy-out-obj b)
	       (broker-proxy-out-obj-found b))
	   (typecase (broker-proxy-out-obj b)
	     (galaxy::gal-list
	      (setf (broker-proxy-out-obj b)
		(append (broker-proxy-out-obj b) (list obj))))
	     ((or galaxy::gal-binary galaxy::gal-int-16
	       galaxy::gal-int-32 galaxy::gal-float-32
	       galaxy::gal-float-64)
	      (galaxy::augment-binary-object
	       (broker-proxy-out-obj b) obj)))))))

(defmethod data-done ((b broker-proxy-out))
  (c-galaxy::proxy-done
   (broker-proxy-c-proxy b)))

(defmethod self-terminates ((b broker-proxy-out))
  (eq (c-galaxy::proxy-self-terminates
       (broker-proxy-c-proxy b)) 1))

(defmethod expire ((b broker-proxy-out))
  (c-galaxy::force-proxy-expiration
   (broker-proxy-c-proxy b)))

(defmethod run-broker-proxy ((b broker-proxy-out))
  (loop
    (if (or (null (broker-proxy-c-proxy b))
	    (broker-proxy-disconnected b))
	(return-from run-broker-proxy))
    (mp::process-wait-with-timeout "Wait a while" 0.1 #'(lambda () nil))
    (if (or (null (broker-proxy-c-proxy b))
	    (broker-proxy-disconnected b))
	(return-from run-broker-proxy))
    (let ((res (c-galaxy::broker-proxy-out-callback-handler
		(broker-proxy-c-proxy b))))
      (cond ((= res 1)
	     (disconnect b)
	     (return-from run-broker-proxy))))))

(defclass broker-proxy-in-stream ()
  ((bp :accessor broker-proxy-in-stream-bp
       :initarg :bp :initform nil)
   (env :accessor broker-proxy-in-stream-env
	:initarg :env :initform nil)
   (ostream :accessor ostream
	    :initform nil)
   (type :accessor broker-proxy-in-stream-type
	 :initform nil)))

(defmethod initialize-instance :after ((bs broker-proxy-in-stream)
				       &rest ignore)
  (setf (ostream bs)
    (ostream (env-connection (broker-proxy-in-stream-env bs))))
  (setf (broker-proxy-in-stream-type bs)
    (car (rassoc (c-galaxy::broker-proxy-object-type
		  (broker-proxy-c-proxy
		   (broker-proxy-in-stream-bp bs)))
		 galaxy::*TYPE-TABLE*))))

(defmethod handle-object ((bs broker-proxy-in-stream) obj)
  )

(defmethod data-done-callback ((bs broker-proxy-in-stream))
  )

(defmethod abort-callback ((bs broker-proxy-in-stream))
  )

(defclass broker-proxy-in (broker-proxy)
  ((bp-stream :accessor broker-proxy-in-bp-stream :initform nil)
   (callback-data :accessor broker-proxy-in-callback-data
		  :initform nil :initarg :callback-data)
  ))

;; This object is only instantiated from the reader,
;; so the proxy argument will already be a C object.

(defmethod initialize-instance :after ((b broker-proxy-in)
				       &key proxy
				       &allow-other-keys)
  (setf (broker-proxy-c-proxy b)
    (c-galaxy::copy-broker-proxy proxy))
  (if (zerop (broker-proxy-c-proxy b))
      (error "no proxy")))

(defmethod object-type ((b broker-proxy-in))
  (car (rassoc (c-galaxy::broker-proxy-object-type
		(broker-proxy-c-proxy b))
	       galaxy::*TYPE-TABLE*)))

;; unproxify-object must return a separate success flag,
;; since a list of length 0 is nil.

(defmethod unproxify-object ((b broker-proxy-in) env)
  (setf (broker-proxy-env b) env)
  (let ((c-obj (c-galaxy::unproxify-object
		(env-c-env env) (broker-proxy-c-proxy b))))
    (if (zerop c-obj)
	(values nil nil)
      (let ((o (galaxy::object-to-val c-obj)))
	(c-galaxy::free-object c-obj)
	(values o t)))))

(defmethod unproxify ((b broker-proxy-in) env
		      &key (proxy-stream-type 'broker-proxy-in-stream)
			   (immediate 1))
  (setf (broker-proxy-in-bp-stream b)
    (make-instance proxy-stream-type :bp b :env env))
  (c-galaxy::unproxify
   (env-c-env env)
   (broker-proxy-c-proxy b)
   #'(lambda (obj)
       (handle-object
	(broker-proxy-in-bp-stream b)
	(galaxy::object-to-val obj)))
   #'(lambda ()
       (data-done-callback
	(broker-proxy-in-bp-stream b)))
   #'(lambda ()
       (abort-callback
	(broker-proxy-in-bp-stream b)))
   immediate)
  (setf (broker-proxy-env b) env)
  (broker-proxy-register b)
  (mp::process-run-function
   "Broker proxy in loop"
   #'run-broker-proxy b))

;; On the inbound side,

(defmethod run-broker-proxy ((b broker-proxy-in))
  (loop
    (if (or (null (broker-proxy-c-proxy b))
	    (broker-proxy-disconnected b))
	(return-from run-broker-proxy))
    (let ((streams (list (c-galaxy::get-broker-proxy-socket
			  (broker-proxy-c-proxy b)))))
      (mp::wait-for-input-available streams :timeout 0.1))      
    (if (or (null (broker-proxy-c-proxy b))
	    (broker-proxy-disconnected b))
	(return-from run-broker-proxy))
    (let ((res (c-galaxy::broker-proxy-in-callback-handler
		(broker-proxy-c-proxy b))))
      (cond ((= res 1)
	     (disconnect b)
	     (return-from run-broker-proxy))))))

;;
;; Connections. 
;;

(define-condition dispatch-error (simple-error)
  ((obj :initarg :obj))
  (:report
   (lambda (condition stream)
     (with-slots (obj) condition
       (format stream "~a" obj)))))

(define-condition write-error (simple-error)
  ((reason :initarg :reason))
  (:report
   (lambda (condition stream)
     (with-slots (reason) condition
       (format stream "~a" reason)))))

(defclass connection ()
  ((server :accessor connection-server :initarg :server)
   (ostream :accessor ostream)
   (brokers :accessor connection-brokers :initform nil)
   (proxies :accessor connection-proxies :initform nil)
   (data :accessor connection-data :initform nil)
   (c-connection :initarg :c-connection
		 :accessor connection-c-connection :initform nil)
   (disconnected :accessor connection-disconnected :initform nil)))

(defmethod initialize-instance :after ((c connection) &key server &allow-other-keys)
  (setf (ostream c) (ostream server))
  (c-galaxy::add-connection-callback
   (connection-c-connection c)
   *GAL-CONNECTION-SHUTDOWN-EVENT*
   #'(lambda () (disconnect-1 c)))
  (push (cons (connection-c-connection c) c)
	(server-conns (connection-server c)))
  ;; We also need to set up the process to run this connection.
  (mp::process-run-function
   "Run connection loop"
   #'run-connection c))

;; Return values are
;; -1 means an error was encountered and the connection has been destroyed.
;; 0 means we're in the midst of things.
;; 1 means we're done and the connection has been destroyed.

;; This should all be handled in the callbacks now.

;; I can't use a blocking callback handler because that will
;; hang the entire Lisp process. I need rather to do a
;; wait-for-input-available with a timeout. The timeout is
;; required to process anything in the connection queue
;; which is "stuck" there.

(defmethod run-connection ((c connection))
  (loop
    (if (connection-disconnected c)
	(return-from run-connection))
    (let ((streams (list (c-galaxy::get-comm-socket
			  (connection-c-connection c)))))
      (mp::wait-for-input-available streams :timeout 0.1))
    ;; Check again!
    (if (connection-disconnected c)
	(return-from run-connection))
    (c-galaxy::connection-callback-handler
     (connection-c-connection c) 0)))

;; Added provider (ignored) for consistent generic function signature.

(defmethod write-frame ((c connection) f &optional (provider nil))
  (if (null (connection-c-connection c))
      (error 'connection-dead :connection c)
    (let ((c-f (galaxy::lisp-to-c f)))
      (c-galaxy::comm-write-frame
       (connection-c-connection c) c-f 0)
      (c-galaxy::free-frame c-f))))

;; Ditto.

(defmethod dispatch-frame ((c connection) f &optional (provider nil))
  (if (null (connection-c-connection c))
      (error 'connection-dead :connection c)
    (let* ((c-f (galaxy::lisp-to-c f))
	   (res-array (c-galaxy::dispatch-via-hub
		       (connection-c-connection c) c-f))
	   (msg-type (c-galaxy::result-array-msg-type res-array))
	   (res-f (c-galaxy::result-array-frame res-array)))
      (c-galaxy::free-result-array res-array)
      (let ((result (postprocess-return c res-f msg-type)))
	(c-galaxy::free-frame c-f)
	result))))

(defmethod postprocess-return ((c connection) c-f msg-type)
  (let ((result nil))
    (if (not (zerop c-f))
	(setf result (galaxy::instantiate-frame-from-c c-f)))
    (if (= msg-type *GAL-ERROR-MSG-TYPE*)
	(error 'dispatch-error :obj result)
      result)))

(defmethod disconnect-1 ((c connection) &optional (force-destroy nil))
  (cond ((and force-destroy
	      (connection-c-connection c)
	      (not (zerop (connection-c-connection c))))
	 (c-galaxy::set-comm-done
	  (connection-c-connection c))
	 (c-galaxy::destroy-comm-struct
	  (connection-c-connection c)))
	((not (connection-disconnected c))
	 ;; Remove the connection from the server table.
	 (setf (server-conns (connection-server c))
	   (remove (assoc (connection-c-connection c)
			  (server-conns (connection-server c)))
		   (server-conns (connection-server c))))
	 (let ((c-conn (connection-c-connection c)))
	   (setf (connection-c-connection c) nil)
	   ;; Shut down the brokers. Later.
	   (dolist (b (connection-brokers c))
	     (disconnect b))
	   (dolist (b (connection-proxies c))
	     (disconnect b))
	   (setf (connection-disconnected c) t)))))

(defmethod disconnect ((c connection))
  (disconnect-1 c t))

(defmethod validating ((c connection))
  (if (connection-c-connection c)
      (if (= (c-galaxy::comm-validating (connection-c-connection c)) 1) t nil)))

;; Call environments.
;;

(defclass call-environment ()
  ((connection :accessor env-connection :initform nil :initarg :connection)
   (server :accessor env-server :initform nil)
   (c-environment :initarg :c-environment :accessor env-c-env :initform nil)
   (continuation-data :initform nil :accessor env-continuation-data)
   (continuation-fn :initform nil
		    :accessor env-continuation-fn :initarg :continuation-fn)
   (ostream :accessor ostream :initform nil)))

;; Used for brokers and continuations. Later, we set the environment and lock it.

(defmethod copy-environment ((e call-environment))
  (let ((new-e (make-instance
		   (server-env-class
		    (env-server e))
		 :connection (env-connection e))))
    (if (env-continuation-data e)
	(setf (env-continuation-data new-e)
	  (env-continuation-data e)))
    new-e))

(defmethod initialize-instance :after ((e call-environment) &key connection &allow-other-keys)
  ;; Lock the environment.
  (if (and (env-c-env e)
	   (not (zerop (env-c-env e))))
      (env-lock-environment e))
  (setf (ostream e) (ostream connection))
  (setf (env-server e) (connection-server connection)))

(defmethod env-lock-environment ((e call-environment))
  (c-galaxy::env-lock (env-c-env e))
  ;; Schedule a finalizer which unlocks.
  (excl::schedule-finalization 
   e #'(lambda (env)
	 (c-galaxy::env-unlock (env-c-env env))
	 (setf (env-c-env env) nil))))  

(defmethod get-session-id ((e call-environment))
  (if (env-c-env e)
      (c-galaxy::env-get-session-id
       (env-c-env e))))

(defmethod update-session-id ((e call-environment) id)
  (if (env-c-env e)
      (c-galaxy::env-update-session-id 
       (env-c-env e) id)))

(defmethod dispatch-frame ((e call-environment) f
			   &optional (provider nil))
  (let* ((c-f (galaxy::lisp-to-c f))
	 (res-array
	  (if (not (eq provider nil))
	      (c-galaxy::env-dispatch-frame-to-provider
	       (env-c-env e) c-f provider)
	    (c-galaxy::env-dispatch-frame
	     (env-c-env e) c-f)))
	 (msg-type (c-galaxy::result-array-msg-type res-array))
	 (res-f (c-galaxy::result-array-frame res-array)))
    (c-galaxy::free-result-array res-array)
    (let ((result (postprocess-return
		   (env-connection e) res-f msg-type)))
      (c-galaxy::free-frame res-f)
      result)))

(defmethod get-originating-provider ((e call-environment))
  (if (env-c-env e)
      (c-galaxy::env-get-originating-provider (env-c-env e))))

(defmethod inherit-token-timestamp ((e call-environment))
  (if (env-c-env e)
      (c-galaxy::env-inherit-token-timestamp (env-c-env e))))

(defmethod get-token-timestamp ((e call-environment))
  (if (env-c-env e)
      (c-galaxy::env-get-token-timestamp (env-c-env e))
    (coerce -1 'double-float)))

(defmethod dispatch-frame-with-continuation ((e call-environment)
					     f continuation-fn
					     &optional (provider nil))
  (let ((new-env (copy-environment e)))
    (setf (env-continuation-fn new-env) continuation-fn)
    (let* ((c-f (galaxy::lisp-to-c f))
	   (res
	    (if (not (eq provider nil))
		(c-galaxy::env-dispatch-frame-to-provider-with-continuation
		 (env-c-env e) c-f provider
		 #'(lambda (c-reply-f c-env msg-type)
		     (env-continue
		      new-env c-reply-f c-env msg-type)))
	      (c-galaxy::env-dispatch-frame-with-continuation
	       (env-c-env e) c-f
	       #'(lambda (c-reply-f c-env msg-type)
		   (env-continue
		    new-env c-reply-f c-env msg-type))))))
      (c-galaxy::free-frame c-f)
      (if (= res -1)
	  (error 'write-error :reason "Writing continuation failed"))
      nil)))

;; Note the similarity with call-dispatch-fn.

(defmethod env-continue ((env call-environment) c-frame c-env msg-type)
  (let ((frame (galaxy::instantiate-frame-from-c c-frame))
	(res-f nil))
    (setf (env-c-env env) c-env)
    (env-lock-environment env)
    (maybe-handler-case ((server-debug-p
			  (env-server env)))
        (setf res-f (funcall (env-continuation-fn env) env frame msg-type))
      (connection-dead (e)
       (disconnect (connection-dead-connection e))
       (setf res-f nil))
      (excl::interrupt-signal (e)
       (graceful-exit
	(env-server env)))
      (error (e)
       (c-galaxy::env-error
	c-env
	(format nil "~a: ~a: ~a"
		(server-name (env-server env))
		(type-of e) e))
       (setf res-f nil)))
    (if (null res-f)
	0
      (galaxy::lisp-to-c res-f))))

(defmethod write-frame ((e call-environment) f
			&optional (provider nil))
  (let* ((c-f (galaxy::lisp-to-c f))
	 (res
	  (if (not (eq provider nil))
	      (c-galaxy::env-write-frame-to-provider
	       (env-c-env e) c-f provider 0)
	    (c-galaxy::env-write-frame
	     (env-c-env e) c-f 0))))
    (c-galaxy::free-frame c-f)
    (if (= res -1)
	(error 'write-error :reason "Couldn't write frame"))
    nil))

(defmethod destroy-token ((e call-environment))
  (if (= (c-galaxy::env-destroy-token (env-c-env e)) -1)
      (error 'write-error :reason "Couldn't write destroy token"))
  nil)

(defmethod reply ((e call-environment) f)
  (let* ((c-f (galaxy::lisp-to-c f))
	 (res (c-galaxy::env-reply
	       (env-c-env e) c-f)))
    (c-galaxy::free-frame c-f)
    (if (= res -1)
	(error 'write-error :reason "Couldn't send reply"))
    nil))

(defmethod get-session-properties ((e call-environment) props)
  ;; The list should be converted to C and later freed. Sigh.
  (let ((c-f (c-galaxy::env-get-session-properties
	      (env-c-env e) (make-array (1+ (length props))
					:initial-contents
					(append props '(0))))))
    (if (not (zerop c-f))
	(let ((f (galaxy::instantiate-frame-from-c c-f)))
	  (c-galaxy::free-frame c-f)
	  f))))

(defmethod get-server-properties ((e call-environment) props)
  ;; The list should be converted to C and later freed. Sigh.
  (let ((c-f (c-galaxy::env-get-server-properties
	      (env-c-env e) (make-array (1+ (length props))
					:initial-contents
					(append props '(0))))))
    (if (not (zerop c-f))
	(let ((f (galaxy::instantiate-frame-from-c c-f)))
	  (c-galaxy::free-frame c-f)
	  f))))

(defmethod modify-session-properties((e call-environment)
				     &key (properties-to-set nil)
					  (properties-to-delete nil))
  (if (and (null properties-to-set)
	   (null properties-to-delete))
      nil
    (let ((c-f (if properties-to-set
		   (galaxy::lisp-to-c properties-to-set)
		 0))
	  (c-l (make-array (1+ (length properties-to-delete))
			   :initial-contents
			   (append properties-to-delete '(0)))))
      (c-galaxy::env-modify-session-properties
       (env-c-env e) c-f c-l))))

(defmethod modify-server-properties((e call-environment)
				    &key (properties-to-set nil)
					 (properties-to-delete nil))
  (if (and (null properties-to-set)
	   (null properties-to-delete))
      nil
    (let ((c-f (if properties-to-set
		   (galaxy::lisp-to-c properties-to-set)
		 0))	  
	  (c-l (make-array (1+ (length properties-to-delete))
			   :initial-contents
			   (append properties-to-delete '(0)))))
      (c-galaxy::env-modify-server-properties
       (env-c-env e) c-f c-l))))

(defmethod set-session ((e call-environment) session-name lock-info)
  (c-galaxy::env-set-session
   (env-c-env e) session-name lock-info))

;; For seeds.

(defmethod call-environment-seed ((c connection)
				  &optional
				  (e 'call-environment)
				  (session-id nil))
  (let* ((c-env (c-galaxy::env-create
		 (connection-c-connection c)))
	 (new-env (make-instance e
		    :c-environment c-env
		    :connection c)))
    (if session-id
	(c-galaxy::env-update-session-id
	 c-env session-id))
    new-env))

;;
;; Server.
;;

;; In Allegro, I should be able to use the IPC server stuff.

;; Here's how to do a simple echo server:

(defclass server ()
  ((ostream :accessor ostream :initform nil)
   (conns :accessor server-conns :initform nil)
   (num-conns :accessor server-num-conns :initform 0)
   (maxconns :accessor server-maxconns :initform 1 :initarg :maxconns)
   (fd :accessor server-fd :initform -1)
   (dispatch-fns :accessor server-dispatch-fns :initform nil)
   (disconnected :accessor server-disconnected :initform 0)
   (debug-p :accessor server-debug-p :initform nil)
   (require-port :accessor server-require-port :initform nil
		 :initarg :require-port)
   (conn-class :accessor server-conn-class :initform 'connection
	       :initarg :conn-class)
   (env-class :accessor server-env-class :initform 'call-environment
	      :initarg :env-class)
   (done :accessor server-done :initform nil)
   (validate :accessor server-validate :initform nil :initarg :validate)
   (usage-string :accessor server-usage-string :initarg :usage-string :initform "")
   (name :accessor server-name :initarg :name :initform "<unknown>")
   (c-server :accessor server-c-server :initform nil)
   ))

;; See galaxy_io.h.

(defconstant *GAL-CONNECTION-LISTENER* 1)
(defconstant *GAL-BROKER-LISTENER* 2)
(defconstant *GAL-HUB-CLIENT* 4)
(defconstant *GAL-SERVER-TYPE-MASK* 7)
(defconstant *GAL-HUB-CLIENT-CONNECT-FAILURE-MASK* 24)
(defconstant *GAL-HUB-CLIENT-CONNECT-FAILURE-RETRY* 0)
(defconstant *GAL-HUB-CLIENT-CONNECT-FAILURE-SHUTDOWN* 8)
(defconstant *GAL-HUB-CLIENT-CONNECT-FAILURE-NOOP* 16)
(defconstant *GAL-HUB-CLIENT-DISCONNECT-MASK* 96)
(defconstant *GAL-HUB-CLIENT-DISCONNECT-RETRY* 0)
(defconstant *GAL-HUB-CLIENT-DISCONNECT-SHUTDOWN* 32)
(defconstant *GAL-HUB-CLIENT-DISCONNECT-NOOP* 64)

(define-condition server-error (simple-error)
  ((connection :initarg :server :accessor server-error-server))
  (:report
   (lambda (condition stream)
     (with-slots (server) condition
       (format stream "Server error: ~s" server)))))

(defmethod print-usage ((s server))
  (format t "Usage: ~a [-port <port_num>] [-maxconns <n>] [-assert] [-debug] [-validate] [-verbosity <num>] [-contact_hub <host:port...>] [-session_id <id>] [-server_locations_file <file>] [-slf_name <name>] ~a"
	  (server-name s) (server-usage-string s)))

(defmethod initialize-instance :after ((s server)
				       &key (argv nil)
					    (verbosity -1)
					    (server-listen-status *GAL-CONNECTION-LISTENER*)
					    (client-pair-string nil)
					    (session-id nil)
					    (default-port 0)
					    (server-locations-file nil)
					    (slf-name nil)
				       &allow-other-keys)
  ;; We're going to surgically alter argv. The first element
  ;; will always be the program name (like C, unlike Python).
  (do ((i 0)
       (port-found nil)
       (contact-hub-found nil)
       (argc (length argv) (length argv)))
      ((= i argc)
       ;; If we've found only contact-hub, we want to
       ;; disable listening.
       (if (and contact-hub-found (not port-found))
	   (setf server-listen-status
	     (logand server-listen-status (lognot *GAL-CONNECTION-LISTENER*)))))
    (cond ((string= (elt argv i) "-port")
	   (cond ((= (1+ i) argc)
		  (print-usage s)
		  (excl::exit 1))
		 (t (setf default-port
		      (read-from-string (elt argv (1+ i))))
		    (if contact-hub-found
			(setf server-listen-status
			  (logior server-listen-status
				  *GAL-HUB-CLIENT* *GAL-CONNECTION-LISTENER*))
		      (setf server-listen-status
			(logior server-listen-status *GAL-CONNECTION-LISTENER*)))
		    (setf port-found t)
		    (setf argv (append (subseq argv 0 i)
				       (subseq argv (+ i 2)))))))
	  ((string= (elt argv i) "-verbosity")
	   (cond ((= (1+ i) argc)
		  (print-usage s)
		  (excl::exit 1))
		 (t (setf sls-util::*VERBOSE-LEVEL*
		      (read-from-string (elt argv (1+ i))))
		    (setf verbosity sls-util::*VERBOSE-LEVEL*)
		    (setf argv (append (subseq argv 0 i)
				       (subseq argv (+ i 2)))))))
	  ((string= (elt argv i) "-maxconns")
	   (cond ((= (1+ i) argc)
		  (print-usage s)
		  (excl::exit 1))
		 (t (setf (server-maxconns s)
		      (read-from-string (elt argv (1+ i))))
		    (setf argv (append (subseq argv 0 i)
				       (subseq argv (+ i 2)))))))
	  ((string= (elt argv i) "-debug")
	   (setf (server-debug-p s) t)
	   (setf argv (append (subseq argv 0 i)
			      (subseq argv (1+ i)))))
	  ((string= (elt argv i) "-assert")
	   (setf (server-require-port s) t)
	   (setf argv (append (subseq argv 0 i)
			      (subseq argv (1+ i)))))
	  ((string= (elt argv i) "-validate")
	   (setf (server-validate s) t)
	   (setf argv (append (subseq argv 0 i)
			      (subseq argv (1+ i)))))
	  ((string= (elt argv i) "-session_id")
	   (cond ((= (1+ i) argc)
		  (print-usage s)
		  (excl::exit 1))
		 (t (setf session-id (elt argv (1+ i)))
		    (setf argv (append (subseq argv 0 i)
				       (subseq argv (+ i 2)))))))
	  ((string= (elt argv i) "-contact_hub")
	   (cond ((= (1+ i) argc)
		  (print-usage s)
		  (excl::exit 1))
		 (t
		  (setf client-pair-string (elt argv (1+ i)))
		  (if port-found
		      (setf server-listen-status
			(logior server-listen-status
				*GAL-HUB-CLIENT* *GAL-CONNECTION-LISTENER*))
		    (setf server-listen-status
		      (logior server-listen-status *GAL-HUB-CLIENT*)))
		  (setf contact-hub-found t)
		  (setf argv (append (subseq argv 0 i)
				     (subseq argv (+ i 2)))))))
	  ((string= (elt argv i) "-server_locations_file")
	   (cond ((= (1+ i) argc)
		  (print-usage s)
		  (excl::exit 1))
		 (t (setf server-locations-file (elt argv (1+ i)))
		    (setf argv (append (subseq argv 0 i)
				       (subseq argv (+ i 2)))))))
	  ((string= (elt argv i) "-slf_name")
	   (cond ((= (1+ i) argc)
		  (print-usage s)
		  (excl::exit 1))
		 (t (setf slf-name (elt argv (1+ i)))
		    (setf argv (append (subseq argv 0 i)
				       (subseq argv (+ i 2)))))))
	  (t (incf i))))
  (setf (ostream s) (make-instance 'sls-util::ostream))
  ;; Now, we set up the server.
  (let ((arg-pkg
	 (c-galaxy::encapsulate-arguments
	  (server-name s)
	  default-port
	  (server-maxconns s)
	  (if (server-require-port s) 1 0)
	  (if (server-validate s) 1 0)
	  verbosity
	  server-listen-status
	  ;; 0 == NULL.
	  (or client-pair-string 0)
	  (or session-id 0)
	  (or server-locations-file 0)
	  (or slf-name 0))))
    (setf (server-c-server s)
      (c-galaxy::setup-server arg-pkg))
    (c-galaxy::free-arg-pkg arg-pkg)
    )
  (if (zerop (server-c-server s))
      (error 'server-error :server s))
  (c-galaxy::add-server-callback
   (server-c-server s)
   *GAL-SERVER-LISTENER-STARTUP-EVENT*
   #'(lambda () (setup-listener s)))
  (c-galaxy::add-server-callback
   (server-c-server s)
   *GAL-SERVER-CLIENT-POLL-STARTUP-EVENT*
   #'(lambda () (setup-client s)))
  ;; Set the server defaults. Make sure you get the
  ;; default port directly from the server object. 
  ;; Either the -port OR -server_locations_file arguments
  ;; could have set this already. 
  (c-galaxy::initialize-server-defaults
   (server-c-server s)
   (c-galaxy::strdup (server-name s))
   (c-galaxy::get-server-default-port (server-c-server s)))
  )

(defmethod add-service-type ((s server) stype)
  (c-galaxy::add-service-type (server-c-server s) stype))

(defmethod modify-properties ((s server) 
			      &key (properties-to-set nil)
				   (properties-to-delete nil))
  (if (and (null properties-to-set)
	   (null properties-to-delete))
      nil
    (let ((c-f (if properties-to-set
		   (galaxy::lisp-to-c properties-to-set)
		 0))	  
	  (c-l (make-array (1+ (length properties-to-delete))
			   :initial-contents
			   (append properties-to-delete '(0)))))
      (c-galaxy::server-modify-properties
       (server-c-server s) c-f c-l))))

;; Backward compatibility for setf functions for server properties.

(defmethod set-server-name ((s server) new-name)
  (if (server-c-server s)
      (c-galaxy::set-server-name
       (server-c-server s) (c-galaxy::strdup new-name)))
  (with-slots (name) s
    (setf name new-name))
  new-name)

(defsetf server-name set-server-name)

(defmethod server-port ((s server))
  (if (server-c-server s)
      (c-galaxy::get-server-listen-port s)))

(defmethod set-server-validate ((s server) validation)
  (cond ((eq validation t)
	 (with-slots (validate) s
	   (setf validate t))
	 (if (server-c-server s)
	     (c-galaxy::enable-dispatch-fn-validation s)))
	(t
	 (error "Can't disable server validation"))))

(defsetf server-validate set-server-validate)

(defmethod set-server-maxconns ((s server) new-maxconns)
  (if (server-c-server s)
      (c-galaxy::set-server-max-connections
       (server-c-server s) new-maxconns))
  (with-slots (maxconns) s
    (setf maxconns new-maxconns))
  new-maxconns)

(defsetf server-maxconns set-server-maxconns)

;; Adding dispatch functions. As in the Python bindings, I
;; use the C API to build up the key mappings, and then add
;; the dispatch functions in C. In Python, I used a lexical
;; closure to "hold" the actual dispatch function, and I can
;; do the same here.

(defmethod add-dispatch-function ((s server) name fn &optional (signature nil))
  (if (not signature)
      (setf signature
	(list nil galaxy::*GAL-OTHER-KEYS-MAYBE*
	      galaxy::*GAL-REPLY-UNKNOWN*
	      nil galaxy::*GAL-OTHER-KEYS-MAYBE*)))
  (destructuring-bind (in-keys in-other-keys reply-status out-keys out-other-keys)
      signature
    (let ((in-key-struct
	   (c-galaxy::create-empty-dispatch-fn-key-array (length in-keys)))
	  (out-key-struct
	   (c-galaxy::create-empty-dispatch-fn-key-array (length out-keys))))
      (dotimes (i (length in-keys))
	(destructuring-bind (key objtype oblig)
	    (nth i in-keys)
	  (c-galaxy::populate-dispatch-fn-key-array-cell
	   in-key-struct i key
	   (cdr (assoc objtype galaxy::*TYPE-TABLE*)) oblig)))
      (dotimes (i (length out-keys))
	(destructuring-bind (key objtype oblig)
	    (nth i out-keys)
	  (c-galaxy::populate-dispatch-fn-key-array-cell
	   out-key-struct i key
	   (cdr (assoc objtype galaxy::*TYPE-TABLE*)) oblig)))
      (c-galaxy::add-dispatch-function
       (server-c-server s) name
       #'(lambda (frame env)
	   (call-dispatch-fn s fn frame env))
       in-key-struct in-other-keys reply-status
       out-key-struct out-other-keys))))

(defmethod call-dispatch-fn ((s server) dispatch-fn c-frame c-env)
  (let* ((f (galaxy::instantiate-frame-from-c c-frame))
	 (c-conn (c-galaxy::env-comm c-env))
	 (conn (cdr (assoc c-conn (server-conns s))))
	 (res-f nil)
	 (new-env (make-instance (server-env-class s)
		    :connection conn
		    :c-environment c-env)))
    (maybe-handler-case ((server-debug-p
			  (connection-server conn)))
        (setf res-f (funcall dispatch-fn new-env f))
      (connection-dead (e)
	(disconnect (connection-dead-connection e))
	(setf res-f nil))
      (excl::interrupt-signal (e)
	(graceful-exit
	  (connection-server conn)))
      (error (e)
	(c-galaxy::env-error
	 (env-c-env new-env)
	 (format nil "~a: ~a: ~a"
		 (server-name (connection-server conn))
		 (type-of e) e))
	(setf res-f nil)))
    (if (null res-f)
	0
        (galaxy::lisp-to-c res-f))))

(defmethod server-is-client ((s server))
  (if (server-c-server s)
      (if (> (c-galaxy::server-is-client (server-c-server s)) 0) t nil)
    nil))

(defmethod server-is-listener ((s server))
  (if (server-c-server s)
      (if (> (c-galaxy::server-is-listener (server-c-server s)) 0) t nil)
    nil))

(defmethod poll-clients ((s server))
  (if (server-c-server s)
      (c-galaxy::server-check-hub-contacts (server-c-server s))))

(defmethod create-connection ((s server) c-conn)
  (make-instance (server-conn-class s)
    :c-connection c-conn
    :server s))

;; And now, the main loop stuff. If server-is-client, then
;; we want to start a loop to check the client connections.
;; Otherwise, we want to set up a loop for
;; the listener. We never shut down the loop.

(defmethod add-signal-behavior ((s server) signum)
  (let ((cur-sig-handler (assoc signum excl::*signals*)))
    (if cur-sig-handler
	(excl::add-signal-handler
	 signum #'(lambda (sig ignore)
		    (funcall (cdr cur-sig-handler) sig ignore)
		    (graceful-exit s)))
      (excl::add-signal-handler
       signum #'(lambda (sig ignore) (graceful-exit s))))))

(defmethod run-server ((s server))
  ;; SIGHUP  
  (add-signal-behavior s 1)
  ;; SIGINT
  (add-signal-behavior s 2)
  ;; SIGQUIT
  (add-signal-behavior s 3)
  ;; SIGTERM
  (add-signal-behavior s 15)
  (if (zerop (c-galaxy::server-start (server-c-server s)))
      (return-from run-server))
  ;; Add the connect callback.
  (c-galaxy::add-server-connect-callback
   (server-c-server s)
   #'(lambda (c-conn) (create-connection s c-conn)))
  ;; Add the destruct callback.
  (c-galaxy::add-server-callback
   (server-c-server s)
   *GAL-SERVER-DESTRUCTION-EVENT*
   #'(lambda () (setf (server-c-server s) nil)))
  (maybe-handler-case ((server-debug-p s))
      (unwind-protect
	  (handler-case
	      (mp::process-wait
	       "Toplevel wait"
	       #'(lambda (s) (null (server-c-server s)))
	       s)
	    ;; I always want to exit.
	    (excl::interrupt-signal (e)
	      (graceful-exit s)))
	(ipc::unix-close (server-fd s)))
      (excl::interrupt-signal (e)
	(graceful-exit s))
      (error (e)
	     (format t "~a~%" e))))

(defmethod setup-listener ((s server))
  ;; Don't check if it's a listener, because
  ;; it won't be until it starts up.
  (mp::process-run-function
   "Run server listener"
   #'run-listener-loop s)
  (c-galaxy::add-server-callback
   (server-c-server s)
   *GAL-SERVER-LISTENER-SHUTDOWN-EVENT*
   #'(lambda () (setf (server-done s) t))))

(defmethod setup-client ((s server))
  ;; Don't check if it's a client, because
  ;; it won't be until it starts up.
  (mp::process-run-function
   "Run client checker"
   #'run-client-loop s))

(defmethod run-client-loop ((s server))
  (loop
    (if (null (server-c-server s))
	(return))
    (poll-clients s)
    (sleep 1)))

;; Return values of GalIO_ServerCallbackHandler():
;; 1 if there's a new connection (creation callback will handle)
;; 0 if not
;; -1 if error and the listener has been shut down
;; -2 if error and server has been destroyed.

;; These should all be handled in the callback now.

;; I can't use a blocking callback handler because that will
;; hang the entire Lisp process. I need rather to do a
;; wait-for-input-available with a timeout. The timeout is
;; required to process anything in the connection queue
;; which is "stuck" there.

(defmethod run-listener-loop ((s server))
  (let ((streams (list (c-galaxy::get-server-listen-socket
			(server-c-server s)))))
    (loop
      (mp::wait-for-input-available streams :timeout 0.1)
      (if (or (null (server-c-server s))
	      (server-done s))
	  (progn
	    (setf (server-done s) t)
	    (return-from run-listener-loop)))
      (let* ((result (c-galaxy::server-callback-handler
		      (server-c-server s) 0)))
	(c-galaxy::free-result-array result)))))

(defmethod disconnect ((s server))
  (if (server-c-server s)
      (c-galaxy::destroy-server-struct (server-c-server s))))

;; Just in case.

(defmethod graceful-exit ((s server))
  (setf (server-done s) t)
  (disconnect s))

;; For backward compatibility.

(ff:def-foreign-call getpid
    (:void)
  :returning :int)

