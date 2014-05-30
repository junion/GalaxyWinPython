;; This file (c) Copyright 1998 - 2002 The MITRE Corporation
;; 
;; This file is part of the Galaxy Communicator system. It is licensed
;; under the conditions described in the file LICENSE in the root 
;; directory of the Galaxy Communicator system.

(defpackage c-galaxy
  (:use "COMMON-LISP"))

(in-package "C-GALAXY")

(if (not (member :galaxy-so *features*))
    (progn
      ;; (load "libGalaxyBindingSupport.so")
      (push :galaxy-so *features*)))

(ff:def-foreign-call (initialize-statics "Gal_InitializeStatics")
    (:void)
  :returning :void)

;; The only way to convert foreign string returns is
;; via excl:native-to-string.

(ff:def-foreign-call (object-type-string-1 "Gal_ObjectTypeString")
    ((objtype :int))
  :returning :foreign-address)

;; This is the "public" function.

(defun object-type-string (objtype)
  (excl:native-to-string (object-type-string-1 objtype)))

(ff:def-foreign-call (int-object "Gal_IntObject")
    ((obj :int))
  :returning :foreign-address)

(ff:def-foreign-call (float-object "Gal_FloatObject")
    ((obj :float))
  :returning :foreign-address)

(ff:def-foreign-call (empty-list-object "GBGal_EmptyListObject")
    (:void)
  :returning :foreign-address)

(ff:def-foreign-call (list-object-add "Gal_ListObjectAdd")
    ((objlist :foreign-address) (newobj :foreign-address))
  :returning :int)

(ff:def-foreign-call (gal-list-length "Gal_ListLength")
    ((objlist :foreign-address))
  :returning :int)

(ff:def-foreign-call (binary-object "Gal_BinaryObject")
    ((data (* :char)) (size :int))
  :returning :foreign-address)

;; For all of these, the memory has to be copied and managed.
;; * :char does NOT COPY.

(ff:def-foreign-call (copy-array "Gal_ACLCopyArray")
    ((data (* :char)) (size :int))
  :returning :foreign-address)

(ff:def-foreign-call (create-int-16-object "Gal_CreateInt16Object")
    ((data :foreign-address) (size :int) (manage-memory :int))
  :returning :foreign-address)

(ff:def-foreign-call (create-int-32-object "Gal_CreateInt32Object")
    ((data :foreign-address) (size :int) (manage-memory :int))
  :returning :foreign-address)

(ff:def-foreign-call (create-float-32-object "Gal_ACLCreateFloat32Object")
    ((data (* :float) (array single-float))
     (size :int))
  :returning :foreign-address)

(ff:def-foreign-call (create-float-64-object "Gal_ACLCreateFloat64Object")
    ((data (* :double) (array double-float))
     (size :int))
  :returning :foreign-address)

(ff:def-foreign-call (string-object "Gal_StringObject")
    ((string (* :char)))
  :returning :foreign-address)

(ff:def-foreign-call (frame-object "Gal_FrameObject")
    ((frame :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (symbol-object "Gal_SymbolObject")
    ((string (* :char)))
  :returning :foreign-address)

(ff:def-foreign-call (get-object-type "Gal_GetObjectType")
    ((obj :foreign-address))
  :returning :int)

(ff:def-foreign-call (get-detailed-type "Gal_GetDetailedType")
    ((obj :foreign-address))
  :returning :int)

(ff:def-foreign-call (frame-value "Gal_FrameValue")
    ((obj :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (int-value "Gal_IntValue")
    ((obj :foreign-address))
  :returning :int)

(ff:def-foreign-call (float-value "Gal_FloatValue")
    ((obj :foreign-address))
  :returning :float)

(ff:def-foreign-call (string-value-1 "Gal_StringValue")
    ((obj :foreign-address))
  :returning :foreign-address)

(defun string-value (obj)
  (excl:native-to-string (string-value-1 obj)))

(ff:def-foreign-call (get-list-object "Gal_GetListObject")
    ((obj :foreign-address) (i :int))
  :returning :foreign-address)

(ff:def-foreign-call (object-byte-count "Gal_ObjectByteCount")
    ((obj :foreign-address))
  :returning :int)

(ff:def-foreign-call (array-value "_Gal_ArrayValue")
    ((obj :foreign-address) (zero-pointer :int))
  :returning :foreign-address)

(ff:def-foreign-call (keyword-value-1 "Gal_KeywordValue")
    ((obj :foreign-address))
  :returning :foreign-address)

(defun keyword-value (obj)
  (excl::native-to-string (keyword-value-1 obj)))

(ff:def-foreign-call (object-to-string-1 "Gal_ObjectToString")
    ((obj :foreign-address))
  :returning :foreign-address)

(defun object-to-string (obj)
  (excl::native-to-string (object-to-string-1 obj)))

(ff:def-foreign-call (free-wrapper "Gal_FreeWrapper")
    ((obj :foreign-address))
  :returning :void)

(ff:def-foreign-call (read-frame-from-string "Gal_ReadFrameFromString")
    ((string (* :char)))
  :returning :foreign-address)

(ff:def-foreign-call (make-frame "Gal_MakeFrame")
    ((name (* :char)) (type :int))
  :returning :foreign-address)

(ff:def-foreign-call (set-prop "Gal_SetProp")
    ((frame :foreign-address) (key (* :char)) (obj :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (add-pred "Gal_AddPred")
    ((frame :foreign-address) (pred :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (get-frame-type "Gal_GetFrameType")
    ((frame :foreign-address))
  :returning :int)

(ff:def-foreign-call (frame-name-1 "Gal_FrameName")
    ((frame :foreign-address))
  :returning :foreign-address)

(defun frame-name (obj)
  (excl::native-to-string (frame-name-1 obj)))

(ff:def-foreign-call (get-properties-1 "GBGal_GetProperties")
    ((frame :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (num-properties-1 "Gal_PointerBufferSize")
    ((pointer-buffer :foreign-address))
  :returning :int)

(ff:def-foreign-call (nth-property "Gal_PointerBufferNthElement")
    ((pointer-buffer :foreign-address) (i :int))
  :returning :foreign-address)

(defun get-frame-properties (f)
  (let* ((prop-ptr (get-properties-1 f))
	 (num-props (num-properties-1 prop-ptr))
	 (prop-list nil))
    (dotimes (i num-props)
      (push (excl::native-to-string
	     (nth-property prop-ptr i)) prop-list))
    prop-list))

(ff:def-foreign-call (get-object "Gal_GetObject")
    ((f :foreign-address) (key (* :char)))
  :returning :foreign-address)

(ff:def-foreign-call (num-preds "Gal_NumPreds")
    ((f :foreign-address))
  :returning :int)

(ff:def-foreign-call (get-pred "Gal_GetPred")
    ((f :foreign-address) (i :int))
  :returning :foreign-address)

(ff:def-foreign-call (frame-to-string-1 "Gal_FrameToString")
    ((f :foreign-address) (how-to :int))
  :returning :foreign-address)

(defun frame-to-string (f how-to)
  (excl::native-to-string (frame-to-string-1 f how-to)))

(ff:def-foreign-call (free-frame "Gal_FreeFrame")
    ((f :foreign-address))
  :returning :void)

(ff:def-foreign-call (read-object-from-string "Gal_ReadObjectFromString")
    ((buf (* :char)))
  :returning :foreign-address)

(ff:def-foreign-call (free-object "Gal_FreeObject")
    ((obj :foreign-address))
  :returning :void)

;; And now, support for the I/O library.

(ff:def-foreign-call (setup-server "GBGalSS_SetupServer")
    ((arg-pkg :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (server-start "GalIO_ServerStart")
    ((server :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (free-arg-pkg "GalSS_FreeArgPkg")
    ((arg-pkg :foreign-address))
  :returning :void)

(ff:def-foreign-call (encapsulate-arguments
		      "_GBGalSS_EncapsulateArguments")
    ((server-name (* :char))
     (port :unsigned-short)
     (max-conns :int)
     (do-assert :int)
     (validate :int)
     (verbosity :int)
     (server-listen-status :int)
     (client-pair-string (* :char))
     (session-id (* :char))
     (server-locations-file (* :char))
     (slf-name (* :char)))
  :returning :foreign-address)

(ff:def-foreign-call (initialize-server-defaults
		      "GalSS_InitializeServerDefaults")
    ((server :foreign-address)
     (name (* :char))
     (port :unsigned-short))
  :returning :void)

(ff:def-foreign-call (server-is-client "GalIO_ServerIsClient")
    ((server :foreign-address))
  :returning :int)

(ff:def-foreign-call (server-is-listener "GalIO_ServerIsListener")
    ((server :foreign-address))
  :returning :int)

;; This will be a baaad thing on Windows, but we'll worry
;; about that later...

(ff:def-foreign-call (get-server-listen-socket "GalIO_GetServerListenSocket")
    ((server :foreign-address))
  :returning :int)

(ff:def-foreign-call (set-server-name "GalIO_SetServerName")
    ((server :foreign-address) (name (* :char)))
  :returning :void)

(ff:def-foreign-call (set-server-max-connections "GalIO_SetServerMaxConnections")
    ((server :foreign-address) (max :int))
  :returning :void)

(ff:def-foreign-call (destroy-server-struct "GalIO_DestroyServerStruct")
    ((server :foreign-address))
  :returning :void)

(ff:def-foreign-call (get-server-listen-port "GalIO_GetServerListenPort")
    ((server :foreign-address))
  :returning :unsigned-short)

(ff:def-foreign-call (get-server-default-port "GalIO_GetServerDefaultPort")
    ((server :foreign-address))
  :returning :unsigned-short)

(ff:def-foreign-call (enable-dispatch-fn-validation "GalIO_EnableDispatchFnValidation")
    ((server :foreign-address))
  :returning :void)

(ff:def-foreign-call (create-empty-dispatch-fn-key-array "_Gal_CreateEmptyDispatchFnKeyArray")
    ((size :int))
  :returning :foreign-address)

(ff:def-foreign-call (populate-dispatch-fn-key-array-cell "_Gal_PopulateDispatchFnKeyArrayCell")
    ((key-array :foreign-address)
     (index :int) (key (* :char))
     (object-type :int) (obligatory :int))
  :returning :void)

(ff:def-foreign-call (server-check-hub-contacts "GalIO_ServerCheckHubContacts")
    ((server :foreign-address))
  :returning :void)

(ff:def-foreign-call (server-callback-handler "GBGalIO_ServerCallbackHandler")
    ((server :foreign-address) (read-blocking :int))
  :returning :foreign-address)

(ff:def-foreign-call (env-destroy-token "GalSS_EnvDestroyToken")
    ((env :foreign-address))
  :returning :int)

(ff:def-foreign-call (env-reply "GalSS_EnvReply")
    ((env :foreign-address) (frame :foreign-address))
  :returning :int)

(ff:def-foreign-call (env-create "GalSS_EnvCreate")
    ((conn :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (env-update-session-id "GalSS_EnvUpdateSessionID")
    ((env :foreign-address) (id (* :char)))
  :returning :void)

(ff:def-foreign-call (env-get-session-id-1 "GalSS_EnvGetSessionID")
    ((env :foreign-address))
  :returning :foreign-address)

(defun env-get-session-id (env)
  (excl::native-to-string (env-get-session-id-1 env)))

;; Properties.

(ff:def-foreign-call (add-service-type "GalIO_AddServiceType")
    ((server :foreign-address) (type-name (* :char)))
  :returning :void)

'(
(ff:def-foreign-call (acl-server-modify-properties "GalIO_ServerModifyProperties")
    ((server :foreign-address)
     (frame :foreign-address)
     (property-list (* :int) (array integer)))
  :returning :void)

(defun server-modify-properties (server frame property-list)
  (let ((parray (make-array (1+ (length property-list))
			    :initial-contents
			    (append (mapcar #'excl::string-to-native property-list) '(0)))))
    (acl-server-modify-properties server frame parray)
    (dotimes (i (length property-list))
      (excl::aclfree (elt parray i)))))

(ff:def-foreign-call (acl-env-get-session-properties "GalSS_EnvGetSessionProperties")
    ((env :foreign-address)     
     (property-list (* :int) (array integer)))
  :returning :foreign-address)

(defun env-get-session-properties(env plist)
  (let* ((parray (make-array (1+ (length plist))
			    :initial-contents
			    (append (mapcar #'excl::string-to-native plist) '(0))))
	 (res (acl-env-get-session-properties env parray)))
    (dotimes (i (length plist))
      (excl::aclfree (elt parray i)))
    res))

(ff:def-foreign-call (acl-env-get-server-properties "GalSS_EnvGetServerProperties")
    ((env :foreign-address)
     (property-list (* :int) (array integer)))
  :returning :foreign-address)

(defun env-get-server-properties(env plist)
  (let* ((parray (make-array (1+ (length plist))
			    :initial-contents
			    (append (mapcar #'excl::string-to-native plist) '(0))))
	 (res (acl-env-get-server-properties env parray)))
    (format t "~s ~s~%" plist parray)
    (dotimes (i (length plist))
      (excl::aclfree (elt parray i)))
    res))

(ff:def-foreign-call (acl-env-modify-session-properties "GalSS_EnvModifySessionProperties")
    ((env :foreign-address)
     (frame :foreign-address)
     (property-list (* :int) (array integer)))
  :returning :void)

(defun env-modify-session-properties (env frame plist)
  (let ((parray (make-array (1+ (length plist))
			    :initial-contents
			    (append (mapcar #'excl::string-to-native plist) '(0)))))
    (acl-env-modify-session-properties env frame parray)
    (dotimes (i (length plist))
      (excl::aclfree (elt parray i)))))

(ff:def-foreign-call (acl-env-modify-server-properties "GalSS_EnvModifyServerProperties")
    ((env :foreign-address)
     (frame :foreign-address)
     (property-list (* :int) (array integer)))
  :returning :void)

(defun env-modify-server-properties (env frame plist)
  (let ((parray (make-array (1+ (length plist))
			    :initial-contents
			    (append (mapcar #'excl::string-to-native plist) '(0)))))
    (acl-env-modify-server-properties env frame parray)
    (dotimes (i (length plist))
      (excl::aclfree (elt parray i)))))
)

(ff:def-foreign-call (server-modify-properties "GalIO_ServerModifyProperties")
    ((server :foreign-address)
     (frame :foreign-address)
     (property-list (* (* :char)) (simple-array simple-string (*))))
  :returning :void)

(ff:def-foreign-call (env-get-session-properties "GalSS_EnvGetSessionProperties")
    ((env :foreign-address)     
     (property-list (* (* :char)) (simple-array simple-string (*))))
  :returning :foreign-address)

(ff:def-foreign-call (env-get-server-properties "GalSS_EnvGetServerProperties")
    ((env :foreign-address)
     (property-list (* (* :char)) (simple-array simple-string (*))))
  :returning :foreign-address)

(ff:def-foreign-call (env-modify-session-properties "GalSS_EnvModifySessionProperties")
    ((env :foreign-address)
     (frame :foreign-address)
     (property-list (* (* :char)) (simple-array simple-string (*))))
  :returning :void)

(ff:def-foreign-call (env-modify-server-properties "GalSS_EnvModifyServerProperties")
    ((env :foreign-address)
     (frame :foreign-address)
     (property-list (* (* :char)) (simple-array simple-string (*))))
  :returning :void)

(ff:def-foreign-call (env-set-session "GalSS_EnvSetSession")
    ((env :foreign-address)
     (session-name (* :char))
     (lock-info :int))
  :returning :void)

(ff:def-foreign-call (env-comm "GalSS_EnvComm")
    ((env :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (env-error "GalSS_EnvError")
    ((env :foreign-address)
     (description (* :char)))
  :returning :int)

(ff:def-foreign-call (env-dispatch-frame "GBGalSS_EnvDispatchFrame")
    ((env :foreign-address) (frame :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (env-write-frame "GalSS_EnvWriteFrame")
    ((env :foreign-address) (frame :foreign-address) (do-block :int))
  :returning :int)

(ff:def-foreign-call (env-dispatch-frame-to-provider "GBGalSS_EnvDispatchFrameToProvider")
    ((env :foreign-address) (frame :foreign-address) (provider (* :char)))
  :returning :foreign-address)

(ff:def-foreign-call (env-write-frame-to-provider "GalSS_EnvWriteFrameToProvider")
    ((env :foreign-address)
     (frame :foreign-address)
     (provider (* :char))
     (do-block :int))
  :returning :int)

(ff:def-foreign-call (env-get-originating-provider-1 "GalSS_EnvGetOriginatingProvider")
    ((env :foreign-address))
  :returning :foreign-address)

(defun env-get-originating-provider (env)
  (excl::native-to-string (env-get-originating-provider-1 env)))

(ff:def-foreign-call (env-inherit-token-timestamp "GalSS_EnvInheritTokenTimestamp")
    ((env :foreign-address))
  :returning :void)

(ff:def-foreign-call (env-get-token-timestamp "GalSS_EnvGetTokenTimestamp")
    ((env :foreign-address))
  :returning :double)

(ff:def-foreign-call (comm-write-frame "GalIO_CommWriteFrame")
    ((conn :foreign-address) (frame :foreign-address) (do-block :int))
  :returning :int)

(ff:def-foreign-call (dispatch-via-hub "GBGalIO_DispatchViaHub")
    ((conn :foreign-address) (frame :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (env-lock "GalSS_EnvLock")
    ((env :foreign-address))
  :returning :void)

(ff:def-foreign-call (env-unlock "GalSS_EnvUnlock")
    ((env :foreign-address))
  :returning :void)

(ff:def-foreign-call (connection-callback-handler "GalIO_ConnectionCallbackHandler")
    ((conn :foreign-address) (blocking :int))
  :returning :int)

(ff:def-foreign-call (result-array-frame "GBGal_ResultArrayFrame")
    ((result-array :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (result-array-status "GBGal_ResultArrayStatus")
    ((result-array :foreign-address))
  :returning :int)

(ff:def-foreign-call (result-array-comm-struct "GBGal_ResultArrayCommStruct")
    ((result-array :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (result-array-msg-type "GBGal_ResultArrayMsgType")
    ((result-array :foreign-address))
  :returning :int)

(ff:def-foreign-call (free-result-array "GBGal_FreeResultArray")
    ((result-array :foreign-address))
  :returning :void)

(ff:def-foreign-call (unset-disconnect-callback "GalIO_UnsetDisconnectCallback")
    ((conn :foreign-address))
  :returning :void)

(ff:def-foreign-call (destroy-comm-struct "GalIO_DestroyCommStruct")
    ((conn :foreign-address))
  :returning :void)

(ff:def-foreign-call (comm-validating "GalIO_CommValidating")
    ((conn :foreign-address))
  :returning :int)

(ff:def-foreign-call (get-comm-socket "GalIO_GetCommSocket")
    ((conn :foreign-address))
  :returning :int)

(ff:def-foreign-call (ip-address-1 "GalIO_IPAddress")
    (:void)
  :returning :foreign-address)

(defun ip-address ()
  (excl::native-to-string (ip-address-1)))

(ff:def-foreign-call (broker-unset-finalizer "GalIO_BrokerUnsetFinalizer")
    ((broker :foreign-address))
  :returning :void)

(ff:def-foreign-call (destroy-broker-struct "GalIO_DestroyBrokerStruct")
    ((broker :foreign-address))
  :returning :void)

(ff:def-foreign-call (broker-data-out-done "GalIO_BrokerDataOutDone")
    ((broker :foreign-address))
  :returning :void)

(ff:def-foreign-call (force-broker-expiration
		      "GalIO_ForceBrokerExpiration")
    ((broker :foreign-address))
  :returning :void)

(ff:def-foreign-call (broker-write-binary "GalIO_BrokerWriteBinary")
    ((broker :foreign-address) (data (* :char)) (bytes :int))
  :returning :int)

(ff:def-foreign-call (broker-write-int-16 "GalIO_BrokerWriteInt16")
    ((broker :foreign-address) (data (* :char)) (bytes :int))
  :returning :int)

(ff:def-foreign-call (broker-write-int-32 "GalIO_BrokerWriteInt32")
    ((broker :foreign-address) (data (* :char)) (bytes :int))
  :returning :int)

(ff:def-foreign-call (broker-write-float-32 "GalIO_ACLBrokerWriteFloat32")
    ((broker :foreign-address)
     (data (* :float) (array single-float))
     (bytes :int))
  :returning :int)

(ff:def-foreign-call (broker-write-float-64 "GalIO_ACLBrokerWriteFloat64")
    ((broker :foreign-address)
     (data (* :double) (array double-float))
     (bytes :int))
  :returning :int)

(ff:def-foreign-call (broker-write-frame "GalIO_BrokerWriteFrame")
    ((broker :foreign-address) (frame :foreign-address))
  :returning :int)

(ff:def-foreign-call (broker-write-string "GalIO_BrokerWriteString")
    ((broker :foreign-address) (data (* :char)))
  :returning :int)

(ff:def-foreign-call (broker-write-object "GalIO_BrokerWriteObject")
    ((broker :foreign-address) (obj :foreign-address))
  :returning :int)

(ff:def-foreign-call (broker-data-done "GalIO_BrokerDataDone")
    ((broker :foreign-address))
  :returning :void)

(ff:def-foreign-call (get-broker-listen-socket "GalIO_GetBrokerListenSocket")
    ((broker :foreign-address))
  :returning :int)

(ff:def-foreign-call (get-broker-listen-port "GalIO_GetBrokerListenPort")
    ((broker :foreign-address))
  :returning :unsigned-short)

(ff:def-foreign-call (get-broker-call-id-1 "GalIO_GetBrokerCallID")
    ((broker :foreign-address))
  :returning :foreign-address)

(defun get-broker-call-id (b)
  (excl::native-to-string (get-broker-call-id-1 b)))

(ff:def-foreign-call (broker-data-out-callback-handler "GalIO_BrokerDataOutCallbackHandler")
    ((broker :foreign-address))
  :returning :int)

(ff:def-foreign-call (get-broker-socket "GalIO_GetBrokerSocket")
    ((broker :foreign-address))
  :returning :int)

(ff:def-foreign-call (broker-data-in-callback-handler "GalIO_BrokerDataInCallbackHandler")
    ((broker :foreign-address) (blocking :int))
  :returning :int)    

(ff:def-foreign-call (frame-get-broker-port "GalIO_FrameGetBrokerPort")
    ((frame :foreign-address))
  :returning :int)

(ff:def-foreign-call (set-broker-active "GalIO_SetBrokerActive")
    ((broker :foreign-address))
  :returning :void)

;; We need some callback support here.

(ff:def-foreign-call (nth-native-arg "ACLGal_NthNativeArg")
    ((i :int) (pointers :foreign-address))
  :returning :foreign-address)

(ff:defun-foreign-callable do-funcall
    ((function :lisp)
     (arg-list :lisp)
     (native-args :unsigned-long)
     (native-arglen :long))
  (let ((local-native-args nil))
    (dotimes (i native-arglen)
      (push (nth-native-arg i native-args) local-native-args))
    (let ((args (append arg-list (reverse local-native-args))))
      (apply function args))))

;; Register the callable.

(defconstant *DO-FUNCALL-INDEX*
    (cadr (multiple-value-list (ff:register-foreign-callable 'do-funcall))))

(defconstant *NIL-INDEX*
    (ff:register-lisp-value nil))

(ff:def-foreign-call (create-callback "ACLGal_CreateACLCallback")
    ((funcall-index :int) (fn-index :int) (lisp-array-index :int))
  :returning :foreign-address)

;; And now, all the various callbacks themselves.

(ff:def-foreign-call (acl-add-server-connect-callback
		      "GalIO_ACLAddServerConnectCallback")
    ((server :foreign-address) (callback :foreign-address))
  :returning :void)

(defun add-server-connect-callback (c-server callback-fn)
  (let ((lisp-fn-index (ff:register-lisp-value callback-fn)))
    (acl-add-server-connect-callback
     c-server
     (create-callback *DO-FUNCALL-INDEX* lisp-fn-index *NIL-INDEX*))))

(ff:def-foreign-call (add-acl-dispatch-function "GalSS_ACLAddDispatchFunction")
    ((server :foreign-address)
     (name (* :char)) (callback :foreign-address)
     (in-keys :foreign-address) (other-in-keys :int)
     (reply-status :int)
     (out-keys :foreign-address) (other-out-keys :int))
  :returning :void)			       

(defun add-dispatch-function (c-server name callback-fn
			      in-key-struct in-other-keys
			      reply-status out-key-struct out-other-keys)
  (let ((lisp-fn-index (ff:register-lisp-value callback-fn)))
    (add-acl-dispatch-function
     c-server name
     (create-callback *DO-FUNCALL-INDEX* lisp-fn-index *NIL-INDEX*)
     in-key-struct in-other-keys
     reply-status out-key-struct out-other-keys)))

(ff:def-foreign-call (acl-add-server-callback
		      "GalIO_ACLAddServerCallback")
    ((server :foreign-address)
     (event :int)
     (callback :foreign-address))
  :returning :void)

(defun add-server-callback (c-server event callback)
  (let ((lisp-fn-index (ff:register-lisp-value callback)))
    (acl-add-server-callback
     c-server event
     (create-callback *DO-FUNCALL-INDEX* lisp-fn-index *NIL-INDEX*))))

(ff:def-foreign-call (acl-add-connection-callback
		      "GalIO_ACLAddConnectionCallback")
    ((connection :foreign-address)
     (event :int)
     (callback :foreign-address))
  :returning :void)

(defun add-connection-callback (c-connection event callback)
  (let ((lisp-fn-index (ff:register-lisp-value callback)))
    (acl-add-connection-callback
     c-connection event
     (create-callback *DO-FUNCALL-INDEX* lisp-fn-index *NIL-INDEX*))))

(ff:def-foreign-call (set-comm-done "GalIO_SetCommDone")
    ((conn :foreign-address))
  :returning :void)

(ff:def-foreign-call (broker-data-out-init "GalIO_ACLBrokerDataOutInit")
    ((conn :foreign-address)
     (timeout-secs :int))
  :returning :foreign-address)

(ff:def-foreign-call (acl-add-broker-callback "GalIO_ACLAddBrokerCallback")
    ((broker :foreign-address)
     (event :int)
     (callback :foreign-address))
  :returning :void)

(defun add-broker-callback(c-broker event callback)
  (let ((lisp-fn-index (ff:register-lisp-value callback)))
    (acl-add-broker-callback
     c-broker event
     (create-callback *DO-FUNCALL-INDEX* lisp-fn-index *NIL-INDEX*))))

(ff:def-foreign-call (acl-comm-broker-data-in-init
		      "GalIO_ACLCommBrokerDataInInit")
    ((connection :foreign-address)
     (host (* :char))
     (port :unsigned-short) (frame :foreign-address)
     (callback :foreign-address))
  :returning :foreign-address)		      

(defun comm-broker-data-in-init (conn host port c-frame callback)
  (let ((callback-index (ff:register-lisp-value callback)))
    (acl-comm-broker-data-in-init
     conn
     host port c-frame
     (create-callback *DO-FUNCALL-INDEX* callback-index *NIL-INDEX*))))

(ff:def-foreign-call (env-acl-broker-data-in-init
		      "GalSS_ACLEnvBrokerDataInInit")
    ((env :foreign-address)
     (host (* :char))
     (port :unsigned-short) (frame :foreign-address)
     (callback :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (broker-get-environment "GalSS_BrokerGetEnvironment")
    ((broker :foreign-address))
  :returning :foreign-address)

(defun env-broker-data-in-init (env host port c-frame callback)
  (let ((callback-index (ff:register-lisp-value callback)))
    (env-acl-broker-data-in-init
     env host port c-frame
     (create-callback *DO-FUNCALL-INDEX* callback-index *NIL-INDEX*))))

;; Broker proxies.

(ff:def-foreign-call (free-broker-proxy "GalSS_FreeBrokerProxy")
    ((proxy :foreign-address))
  :returning :void)

(ff:def-foreign-call (broker-proxy-broker "GalSS_BrokerProxyBroker")
    ((proxy :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (proxify-object-type "GalSS_ProxifyObjectType")
    ((env :foreign-address)
     (type :int)
     (poll-ms :int)
     (timeout :int))
  :returning :foreign-address)

(ff:def-foreign-call (proxy-write "GalSS_ProxyWrite")
    ((proxy :foreign-address)
     (obj :foreign-address)
     (manage-memory :int))
  :returning :int)

(ff:def-foreign-call (proxy-done "GalSS_ProxyDone")
    ((proxy :foreign-address))
  :returning :void)

(ff:def-foreign-call (proxy-self-terminates "GalSS_ProxySelfTerminates")
    ((proxy :foreign-address))
  :returning :int)

(ff:def-foreign-call (broker-proxy-out-callback-handler
		      "GalSS_BrokerProxyOutCallbackHandler")
    ((proxy :foreign-address))
  :returning :int)

(ff:def-foreign-call (force-proxy-expiration
		      "GalSS_ForceProxyExpiration")
    ((proxy :foreign-address))
  :returning :void)

(ff:def-foreign-call (broker-proxy-object-type
		      "GalSS_BrokerProxyObjectType")
    ((proxy :foreign-address))
  :returning :int)

(ff:def-foreign-call (copy-broker-proxy "GalSS_CopyBrokerProxy")
    ((proxy :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (unproxify-object
		      "GalSS_UnproxifyObject")
    ((env :foreign-address)
     (proxy :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (get-broker-proxy-socket
		      "GalSS_GetBrokerProxySocket")
    ((proxy :foreign-address))
  :returning :int)

(ff:def-foreign-call (broker-proxy-in-callback-handler
		      "GalSS_BrokerProxyInCallbackHandler")
    ((proxy :foreign-address))
  :returning :int)

(ff:def-foreign-call (create-proxy-object
		      "Gal_CreateProxyObject")
    ((proxy :foreign-address)
     (manage-memory :int))
  :returning :foreign-address)

(ff:def-foreign-call (proxy-value "Gal_ProxyValue")
    ((obj :foreign-address))
  :returning :foreign-address)

(ff:def-foreign-call (acl-unproxify
		      "GalSS_ACLUnproxify")
    ((env :foreign-address)
     (proxy :foreign-address)
     (op-callback :foreign-address)
     (data-done-callback :foreign-address)
     (abort-callback :foreign-address)
     (immediate :int))
  :returning :void)

(defun unproxify (env proxy op-callback data-done-callback
		  abort-callback immediate)
  (let ((op-callback-index (ff:register-lisp-value op-callback))
	(dd-callback-index (ff:register-lisp-value data-done-callback))
	(abort-callback-index (ff:register-lisp-value abort-callback))
	(acl-immediate (if immediate 1 0)))
    (acl-unproxify
     env proxy
     (create-callback *DO-FUNCALL-INDEX* op-callback-index *NIL-INDEX*)
     (create-callback *DO-FUNCALL-INDEX* dd-callback-index *NIL-INDEX*)
     (create-callback *DO-FUNCALL-INDEX* abort-callback-index *NIL-INDEX*)
     acl-immediate)))

;; acl-unproxify.

(ff:def-foreign-call (env-acl-broker-proxy-in-init
		      "GalSS_ACLEnvBrokerProxyInInit")
    ((env :foreign-address)
     (proxy :foreign-address)
     (callback :foreign-address))
  :returning :foreign-address)

(defun env-broker-proxy-in-init (env c-proxy callback)
  (let ((callback-index (ff:register-lisp-value callback)))
    (env-acl-broker-proxy-in-init
     env c-proxy
     (create-callback *DO-FUNCALL-INDEX* callback-index *NIL-INDEX*))))

;; and disconnect the proxies when the connection dies.

;; Continuations.

(ff:def-foreign-call (acl-env-dispatch-frame-with-continuation
		      "GalSS_ACLEnvDispatchFrameWithContinuation")
    ((env :foreign-address)
     (frame :foreign-address)
     (callback :foreign-address))
  :returning :int)

(ff:def-foreign-call (acl-env-dispatch-frame-to-provider-with-continuation
		      "GalSS_ACLEnvDispatchFrameToProviderWithContinuation")
    ((env :foreign-address)
     (frame :foreign-address)
     (provider (* :char))
     (callback :foreign-address))
  :returning :int)

(defun env-dispatch-frame-with-continuation(env frame continuation-fn)
  (let ((continuation-index (ff:register-lisp-value continuation-fn)))
    (acl-env-dispatch-frame-with-continuation
     env frame (create-callback *DO-FUNCALL-INDEX*
				continuation-index *NIL-INDEX*))))

(defun env-dispatch-frame-to-provider-with-continuation(env frame provider continuation-fn)
  (let ((continuation-index (ff:register-lisp-value continuation-fn)))
    (acl-env-dispatch-frame-to-provider-with-continuation
     env frame provider (create-callback *DO-FUNCALL-INDEX*
					 continuation-index *NIL-INDEX*))))

;; Because we need to make sure the memory is copied. 

(ff:def-foreign-call strdup
    ((str (* :char)))
  :returning :foreign-address)

;; Because we can't just load bytes in Allegro for floats.

(ff:def-foreign-call (read-single-float "Gal_ACLReadSingleFloat")
    ((i :int) (data :foreign-address))
  :returning :float)

(ff:def-foreign-call (read-double-float "Gal_ACLReadDoubleFloat")
    ((i :int) (data :foreign-address))
  :returning :double)
