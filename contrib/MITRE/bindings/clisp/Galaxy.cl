;; This file (c) Copyright 1998 - 2002 The MITRE Corporation
;; 
;; This file is part of the Galaxy Communicator system. It is licensed
;; under the conditions described in the file LICENSE in the root 
;; directory of the Galaxy Communicator system.

; SAM 11/17/98: I'm going to organize these very much like the Python
;; bindings: three files, Galaxy.cl, GalaxyIO.cl, MGalaxy.cl.

(defpackage Galaxy
  (:use "COMMON-LISP"))

(in-package "GALAXY")

(defpackage Galaxy-IO
  (:use "COMMON-LISP"))

(defconstant *documentation* "
Gal_ReadFrameFromString(s):            (read-frame-from-string s)
Gal_ReadFrameFromFile(fp):             [not implemented]
Gal_ReadObjectFromString(s):           (read-irp-value s) (approximately)
Gal_ReadObjectFromFile(fp):            [not implemented]
Gal_MakeFrame(name, type):             (make-instance 'gal-topic-frame :name name) OR (make-instance 'gal-clause-frame :name name) OR (make-instance 'gal-pred-frame :name name)
Gal_MakeTopicFrame(name):              (make-instance 'gal-topic-frame :name name)
Gal_MakePredFrame(name):               (make-instance 'gal-pred-frame :name name)
Gal_MakeClauseFrame(name):             (make-instance 'gal-clause-frame :name name)
Gal_FreeFrame(f):                      [not needed]
Gal_CopyFrame(f):                      (frame-copy f) [not yet implemented]
Gal_FrameEqual(f1, f2):                (frame-equal f1 f2) [not yet implemented[
Gal_MatchFrame(f1, f2):                (frame-match f1 f2) [not yet implemented]
Gal_SetFrameName(f, name):             (setf (frame-name f) name)
Gal_SetFrameType(f, type):             (setf (frame-type f) type)
Gal_FrameName(f):                      (frame-name f)
Gal_FrameNameEq(f, name):              (string= (frame-name f) name)
Gal_FrameNamesEq(f1, f2):              (string= (frame-name f1) (frame-name f2))
Gal_FrameIsType(f, type):              (eq (frame-type f) type)
Gal_AddPred(f, pred):                  (push pred (frame-preds f))
Gal_GetPred(f, i):                     (elt (frame-preds f) i)
Gal_GetPredByName(f, name):            (get-pred-by-name f name)
Gal_RemPred(f, i):                     (rem-pred f i)
Gal_RemPredByName(f, name):            (rem-pred-by-name f name)
Gal_DelPred(f, i):                     (rem-pred f i)
Gal_DelPredByName(f, name):            (rem-pred-by-name f name)
Gal_NumPreds(f):                       (length (frame-preds f))
Gal_ClearPreds(f):                     (setf (frame-preds f) nil)
Gal_SetProp(f, key, obj):              (set-prop f key obj)
Gal_GetObject(f, key):                 (get-object f key)
Gal_GetFrame(f, key):                  (get-value f key 'gal-frame)
Gal_GetTopicFrame(f, key):             (get-value f key 'gal-topic-frame)
Gal_TopicValue(...):                   [not implemented]
Gal_GetString(f, key):                 (get-value f key 'gal-string)
Gal_GetInt(f, key):                    (get-value f key 'gal-int)
Gal_GetFloat(f, key):                  (get-value f key 'gal-float)
Gal_GetList(f, key):                   (get-value f key 'gal-list)
Gal_GetBinary(f, key):                 (get-value f key 'gal-binary)
Gal_RemProp(f, key):                   (rem-prop f key)
Gal_DelProp(f, key):                   (del-prop f key)
Gal_NumProperties(f):                  (num-properties f)
Gal_GetProperties(f):                  (gal-get-properties f) [get-properties is a CL internal function]

Gal_PrFrame(f):                        (pr-frame f t)
Gal_PPFrame(f):                        (pp-frame f t)
GalUtil_PPFrame(verbose_level, fr):    (sls-util::gal-write-level o (pp-frame f nil) verbose-level)
GalUtil_CPPFrame(verbose_level, fr):   [not implemented]
Gal_PrFrameToString(f, buf, size):     (pr-frame f nil)
Gal_PPFrameToString(f, buf, size):     (pp-frame f nil)
Gal_PrFrameToFile(f, fp):              (pr-frame f fp)
Gal_PPFrameToFile(f, fp):              (pp-frame f fp)
Gal_PPObjectToFile(o, fp):             (pr-object o fp *GAL-PP-PRINT*)
Gal_PPObject(o):                       (pr-object o t *GAL-PR-PRINT*)
Gal_OutlineFrame(f, verbose_level):    (outline-frame f t) [not implemented yet]
Gal_OutlineObject(f, verbose_level):   [not implemented]
Gal_PrObject(obj):                     (pr-object obj t)
Gal_ObjectToString(obj):               (pr-object obj nil)
GalUtil_PrObject(v_level, o):          (sls-util::gal-write-level o (pr-object o nil) v-level)
GalUtil_PPObject(v_level, o):          (sls-util::gal-write-level o (pr-object o nil :pp) v-level)
GalUtil_CPPObject(v_level, o):         [not implemented]
Gal_PrObjectToFile(obj):               (pr-object obj fp)

Gal_GetObjectType(obj):                (get-object-type obj)
Gal_GetDetailedType(obj):              (get-detailed-type obj)
Gal_ObjectTypeString(type):            (object-type-string type)
Gal_GetObjectTypeString(type):         (object-type-string type)

# not implemented: grovel.c

Gal_FindKey(f, name):                  [not implemented]
Gal_MatchKeyValue(f, name, obj):       [not implemented]
Gal_FindPred(f, pred_name):            [not implemented]
Gal_FindTopic(f, name):                [not implemented]
Gal_DeletePreds(f, name):              [not implemented]
Gal_FindPredParent(f, name, parent, findpar, nth): [not implemented]

# end

# not implemented: signal.c

Gal_AddSignalHandler(...):             [not implemented]
Gal_SignalsInitialized():              [not implemented]
Gal_InitializeSignals():               [not implemented]

# not implemented: timed_tasks.c

Gal_AddTimedTask(task, val, ms):       [not implemented]
Gal_RemoveTimedTask(task, val):        [not implemented]
Gal_TimedTasksLoopHandler(tv):         [not implemented]
Gal_TimedTasksLoop():                  [not implemented]
Gal_TimedTasksLoopExit():              [not implemented]
Gal_TaskPkgBlocking(pkg):              [not implemented]
Gal_TaskPkgData(pkg):                  [not implemented]
Gal_EnableTimedTaskThreads():          [not implemented]
Gal_RemoveTask(...):                   [not implemented]
Gal_AddTask(...):                      [not implemented]
Gal_AddTimedTaskWithIO(...):           [not implemented]
Gal_ReAddTask(...):                    [not implemented]
Gal_TimedTaskThreadsEnabled():         [not implemented]

#end

# end

Gal_Predp(obj):                        (typep obj 'gal-pred-frame)
Gal_FreeObject(obj):                   [not needed]
Gal_FrameObject(f):                    [not needed]
Gal_StringObject(obj):                 [not needed]
Gal_ListObject(obj):                   [not needed]
Gal_ListObjectFromElements(...):       [not needed]
Gal_IntObject(obj):                    [not needed]
Gal_BinaryObject(data, size):          (binary-object 'gal-binary :data data)
Gal_PointerObject(obj):                [not implemented]
Gal_FloatObject(float):                [not needed]
Gal_ObjectEqual(obj1, obj2):           (object-equal obj1 obj2) [not yet implemented]
Gal_ObjectCaseEqual(obj1, obj2):       (object-equal obj1 obj2 :ignore-case 1) [not yet implemented]
Gal_Floatp(obj):                       (typep obj 'gal-float)
Gal_Topicp(obj):                       (typep obj 'gal-topic-frame)
Gal_TopicFramep(obj):                  (typep obj 'gal-topic-frame)
Gal_PredFramep(obj):                   (typep obj 'gal-pred-frame)
Gal_Clausep(obj):                      (typep obj 'gal-clause-frame)
Gal_ClauseFramep(obj):                 (typep obj 'gal-clause-frame)
Gal_Listp(obj):                        (typep obj 'gal-list)
Gal_Binaryp(obj):                      (typep obj 'gal-binary)
Gal_Framep(obj):                       (typep obj 'gal-frame)
Gal_Stringp(obj):                      (typep obj 'gal-string)
Gal_Intp(obj):                         (typep obj 'gal-int)
Gal_ListLength(obj):                   (length obj)
Gal_GetListValue(obj, n, otype):       (value-warn (elt obj n) otype)
Gal_StringValue(obj):                  (value-warn obj 'gal-string)
Gal_BinaryValue(obj):                  (value-warn obj 'gal-binary)
Gal_ListValue(obj):                    (value-warn obj 'gal-list)
Gal_FrameValue(obj):                   (value-warn obj 'gal-frame)
Gal_PredValue(obj):                    (value-warn obj 'gal-pred-frame)
Gal_ClauseValue(obj):                  (value-warn obj 'gal-clause-frame)
Gal_IntValue(obj):                     (value-warn obj 'gal-int)
Gal_FloatValue(obj):                   (value-warn obj 'gal-float)
Gal_BinarySize(obj):                   (length obj)
Gal_GetFrameType(obj):                 (frame-type obj)
Gal_CopyObject(obj):                   [not implemented]
Gal_GetListObject(obj, n):             (elt obj n)
Gal_Symbolp(obj):                      (typep obj 'gal-symbol)
Gal_FreeWrapper(obj):                  [not needed]

# dispatch_function.c

Gal_FreeDispatchFnKeyArray(entry):     [not needed]
Gal_CreateDispatchFnKeyArray(...):     [not needed]
")

(push :gc-version-3 *features*)

;; Initialize the static variables, just in case
;; it doesn't happen elsewhere.

(c-galaxy::initialize-statics)

;; Message types.

(defconstant *GAL-KEY-ALWAYS* 0)
(defconstant *GAL-KEY-SOMETIMES* 1)
(defconstant *GAL-OTHER-KEYS-MAYBE* 2)
(defconstant *GAL-OTHER-KEYS-NEVER* 3)
(defconstant *GAL-REPLY-PROVIDED* 4)
(defconstant *GAL-REPLY-NONE* 5)
(defconstant *GAL-REPLY-UNKNOWN* 6)

;; Print types.

(defconstant *GAL-PP-PRINT* 0)
(defconstant *GAL-PR-PRINT* 1)

;; For the object types, we'll rely on the Lisp type system extensively.

(defclass gal-frame ()
  ((name :accessor frame-name :initform "" :initarg :name)
   (preds :accessor frame-preds :initform nil :initarg :preds)
   (data :accessor frame-data :initform nil :initarg :data)))

;; Frame types.

(defconstant *FRAME-TYPE-TABLE*
    '((1 . gal-topic-frame)
      (2 . gal-clause-frame)
      (3 . gal-pred-frame)))

(defun instantiate-frame-from-c (c-frame)
  (let* ((dtype (cdr (assoc (c-galaxy::get-frame-type c-frame)
			    *FRAME-TYPE-TABLE*)))
	 (new-f (make-instance dtype)))
    (instantiate-from-c new-f c-frame)
    new-f))

(defmethod instantiate-from-c ((f gal-frame) c-frame)
  (setf (frame-name f) (c-galaxy::frame-name c-frame))
  (dolist (p (c-galaxy::get-frame-properties c-frame))
    (set-prop f p (object-to-val (c-galaxy::get-object c-frame p))))
  (setf (frame-preds f) nil)
  (dotimes (i (c-galaxy::num-preds c-frame))
    (push (object-to-val (c-galaxy::get-pred c-frame i))
	  (frame-preds f))))

(defmethod frame-type ((f gal-frame))
  nil)

(defclass gal-clause-frame (gal-frame) ())

;; Type-of is not portable, so we need to do this.

(defmethod frame-type ((f gal-clause-frame))
  'gal-clause-frame)

(defclass gal-topic-frame (gal-frame) ())
	  
(defmethod frame-type ((f gal-topic-frame))
  'gal-topic-frame)

(defclass gal-pred-frame (gal-frame) ())

(defmethod frame-type ((f gal-pred-frame))
  'gal-pred-frame)

;; And now, the binary types.

(deftype gal-binary () '(array (unsigned-byte 8) *))

(deftype gal-int-16 () '(array (unsigned-byte 16) *))

(deftype gal-int-32 () '(array (unsigned-byte 32) *))

;; In Allegro, 64 bit integers are a problem, since I can't
;; determine the type by looking at the array. 

(deftype gal-float-32 () '(array single-float *))

(deftype gal-float-64 () '(array double-float *))

(defun binary-object (gal-t &key data (size 0))
  (let ((true-gal-t
	 (case gal-t
	   (gal-binary '(unsigned-byte 8))
	   (gal-int-16 '(unsigned-byte 16))
	   (gal-int-32 '(unsigned-byte 32))
	   (gal-float-32 'single-float)
	   (gal-float-64 'double-float))))
    (if data
	(make-array (max (length data) size)
		    :adjustable t
		    :element-type true-gal-t
		    :initial-contents data)
      (make-array size :adjustable t :element-type true-gal-t))))

(defun augment-binary-object (a new-data)
  (let* ((curlen (length a))
	 (newlen (+ curlen (length new-data))))
    (adjust-array a newlen)
    (dolist (elem (coerce new-data 'list))
      (setf (aref a curlen) elem)
      (incf curlen))))

;; This has to be efficient too. So I'll do the same trick here.

(defun binary-data-from-c-data (true-gal-t data b-length)
  (multiple-value-bind (byte-num array-type)
      (case true-gal-t
	(gal-binary (values 1 '(unsigned-byte 8)))
	(gal-int-16 (values 2 '(unsigned-byte 16)))
	(gal-int-32 (values 4 '(unsigned-byte 32)))
	(gal-float-32 (values 4 'single-float))
	(gal-float-64 (values 8 'double-float)))
    (cond ((member true-gal-t '(gal-binary gal-int-16 gal-int-32))
	   (let* ((s (if (zerop data)
			 ""
		       (excl::native-to-string data :length b-length)))
		  (all-vals
		   (make-array (/ b-length byte-num)
			       :adjustable t :element-type array-type)))
	     (do ((i 0 (1+ i)))
		 ((= i (/ b-length byte-num))
		  all-vals)
	       (do ((j 0 (1+ j))
		    (new-val 0))
		   ((= j byte-num)	   
		    (setf (aref all-vals i) new-val))
		 #+little-endian
		 (setf new-val (logior (char-code
					(aref s (+ (* i byte-num)
						   (- byte-num j))))
				       (ash new-val 8)))
		 #-little-endian
		 (setf new-val (logior (char-code 
					(aref s (+ (* i byte-num) j)))
				       (ash new-val 8)))))))
	  ((eq true-gal-t 'gal-float-32)
	   (let ((all-vals
		  (make-array (/ b-length 4)
			       :adjustable t :element-type 'single-float)))
	     ;; Can't just load bytes for floats.
	     (do ((i 0 (1+ i)))
		 ((= i (/ b-length 4))
		  all-vals)
	       (setf (aref all-vals i)
		 (c-galaxy::read-single-float i data)))))
	  (t
	   (let ((all-vals
		  (make-array (/ b-length 8)
			      :adjustable t :element-type 'double-float)))
	     ;; Can't just load bytes for floats.
	     (do ((i 0 (1+ i)))
		 ((= i (/ b-length 8))
		  all-vals)
	       (setf (aref all-vals i)
		 (c-galaxy::read-double-float i data))))))))

;; I need to do this far more efficiently. So what I'll
;; do is allocate a string ahead of time, of the appropriate
;; size (which is the length of the array * byte-num.

(defun binary-data-to-string (seed-int-array)
  (let* ((byte-num (typecase seed-int-array
		     (gal-binary 1)
		     (gal-int-16 2)
		     (gal-int-32 4)
		     (gal-float-32 4)
		     (gal-float-64 8)))
	 (storage (make-string (* (length seed-int-array) byte-num))))
    (do ((i 0 (1+ i)))
	((= i (length seed-int-array))
	 storage)
      (let ((val (aref seed-int-array i)))
	(dotimes (j byte-num)
	  #-little-endian
	  ;; I read the bytes from least to most significant,
	  ;; so I store them in reverse order (right to left).
	  (setf (aref storage (+ (* i byte-num) (- byte-num j 1)))
	    (code-char (logand #b11111111 val)))
	  #+little-endian
	  ;; I read the bytes from least to most significant,
	  ;; so I store them in order (left to right).
	  (setf (aref storage (+ (* i byte-num) j))
	    (code-char (logand #b11111111 val)))
	  (setf val (ash val -8)))))))

;; The types are: gal-frame, gal-float, gal-string, gal-symbol,
;; gal-list, gal-int, and the binary types. gal-frame will be the
;; name of the class.

(deftype gal-float () 'float)
(deftype gal-int () 'integer)
(deftype gal-list () 'list)
(deftype gal-symbol () '(and symbol (not null)))
(deftype gal-string () 'string)
(deftype gal-proxy () 'galaxy-io::broker-proxy)

(defun get-object-type (obj)
  (typecase obj
    (gal-frame 'gal-frame)
    (gal-float 'gal-float)
    (gal-string 'gal-string)
    (gal-symbol 'gal-symbol)
    (gal-list 'gal-list)
    (gal-int 'gal-int)
    (gal-binary 'gal-binary)
    (gal-int-16 'gal-int-16)
    (gal-int-32 'gal-int-32)
    ;; (gal-int-64 'gal-int-64)
    (gal-float-32 'gal-float-32)
    (gal-float-64 'gal-float-64)
    (gal-proxy 'gal-proxy)
    (otherwise nil)
    ))

;; These are from galaxy.h.

(defconstant *TYPE-TABLE* 
    '((gal-free . 0)
      (gal-frame . 1)
      (gal-string . 2)
      (gal-int . 3)
      (gal-float . 4)
      (gal-symbol . 5)
      (gal-list . 6)
      (gal-topic-frame . 8)
      (gal-clause-frame . 9)
      (gal-pred-frame . 10)
      (gal-binary . 11)
      (gal-int-16 . 12)
      (gal-int-32 . 13)
      ;; (gal-int-64 . 14)
      (gal-float-32 . 15)
      (gal-float-64 . 16)
      (gal-proxy . 20)
      ))

(defun get-object-integer (objtype)
  (cdr (assoc objtype *TYPE-TABLE*)))

(defun get-detailed-type (obj)
  (let ((x (get-object-type obj)))
    (case x
      (gal-frame (frame-type obj))
      (otherwise x))))

(define-condition galaxy-unknown-type-error (simple-error)
  ((obj-type :initarg :obj-type))
  (:report
   (lambda (condition stream)
     (with-slots (obj-type) condition
       (format stream "Unknown type ~s" obj-type)))))

(defun object-type-string (obj-type)
  (c-galaxy::object-type-string
   (get-object-integer obj-type)))

(define-condition galaxy-type-error (simple-error)
  ((obj :initarg :obj) (expected-type :initarg :expected-type))
  (:report
   (lambda (condition stream)
     (with-slots (obj expected-type) condition
       (format stream "Expected type ~s for object ~a" expected-type obj)))))

(defun value-warn (obj otype)
  (if (not (typep obj otype))
      (error 'galaxy-type-error :obj obj :expected-type otype)
    obj))

;; For the moment, I think I'll treat the data as a plist
;; instead of an a-list. Don't see the difference, frankly...

(defmethod get-object ((o gal-frame) key)
  (cdr (assoc key (frame-data o) :test #'string=)))

(defmethod get-pred-by-name ((o gal-frame) key)
  (find-if #'(lambda (x) (string= (frame-name x) key)) (frame-preds o)))

(defmethod rem-pred ((o gal-frame) index)
  (if (zerop index)
      (pop (frame-preds o))    
    (let ((p (elt (frame-preds o) index)))
      (delete p (frame-preds o))
      p)))

(defmethod rem-pred-by-name ((o gal-frame) key)
  (let ((p (get-pred-by-name o key)))
    (setf (frame-preds o) (delete p (frame-preds o)))
    p))

(defmethod set-prop ((o gal-frame) key value)
  (let ((entry (assoc key (frame-data o) :test #'string=)))
    (if entry
	(setf (cdr entry) value)
      (push (cons key value) (frame-data o)))))

(defmethod get-value ((o gal-frame) key type)
  (let ((p (get-object o key)))
    (if p (value-warn p type))))

(defmethod rem-prop ((o gal-frame) key)
  (let ((p (assoc key (frame-data o) :test #'string=)))
    (setf (frame-data o) (delete p (frame-data o)))
    (cdr p)))

(defmethod del-prop ((o gal-frame) key)
  (let ((p (assoc key (frame-data o) :test #'string=)))
    (setf (frame-data o) (delete p (frame-data o)))
    nil))

(defmethod num-properties ((o gal-frame))
  (length (frame-data o)))

;; This is not guaranteed to stay the same!

(defmethod gal-get-properties ((o gal-frame))
  (mapcar #'car (frame-data o)))

;; And finally, frame reading and printing.

(defun read-frame-from-string (s)
  (let ((c-frame (c-galaxy::read-frame-from-string s)))
    (instantiate-frame-from-c c-frame)))

(defun read-irp-value (s)
  (cond ((eq 6 (mismatch "NULLObject" s))
	 (values nil ""))
	(t (let* ((temp (c-galaxy::read-object-from-string s))
		  (val (object-to-val temp)))
	     (c-galaxy::free-object temp)
	     (values val "")))))

;; Printing

(defun pr-frame (frame to-where)
  (print-frame frame to-where *GAL-PR-PRINT*))

(defun print-frame (frame to-where how-to)
  (let* ((c-f (lisp-to-c frame))
	 (s (c-galaxy::frame-to-string c-f how-to)))
    (c-galaxy::free-frame c-f)
    (format to-where s)))

(defun pp-frame (frame to-where)
  (print-frame frame to-where *GAL-PP-PRINT*))

;; Object printing.

(defun pr-object (value to-where &optional (how-to *GAL-PR-PRINT*))
  (typecase value
    (gal-frame
     (print-frame value to-where how-to))
    (gal-symbol
     (format to-where (symbol-name value)))
    ((or gal-int gal-float gal-list gal-string gal-binary)
     (let* ((temp (val-to-object value))
	    (s (c-galaxy::object-to-string temp)))
       (c-galaxy::free-wrapper temp)
       (format to-where s)))
    (otherwise 
     (error 'galaxy-frame-printing-error
	    :msg (format nil "Can't print ~a" value)))))

;;
;;  Object conversion
;;

(define-condition galaxy-object-conversion-error (simple-error)
  ((msg :initarg :msg))
  (:report
   (lambda (condition stream)
     (with-slots (msg) condition
       (format stream msg)))))

(defun val-to-object (val)
  (typecase val
    (gal-int
     (c-galaxy::int-object val))
    (gal-float
     (c-galaxy::float-object val))
    (gal-list
     (let ((p (c-galaxy::empty-list-object)))
       (dolist (v val)
	 (c-galaxy::list-object-add p (val-to-object v)))
       p))
    (gal-binary
     (let ((s (binary-data-to-string val)))
       (c-galaxy::binary-object s (length s))))
    (gal-int-16
     (let ((s (binary-data-to-string val)))
       (c-galaxy::create-int-16-object
	(c-galaxy::copy-array s (length s)) (length val) 1)))
    (gal-int-32
     (let ((s (binary-data-to-string val)))
       (c-galaxy::create-int-32-object
	(c-galaxy::copy-array s (length s)) (length val) 1)))
    (gal-float-32
     (c-galaxy::create-float-32-object val (length val)))
    (gal-float-64
     (c-galaxy::create-float-64-object val (length val)))
    (gal-string
     (c-galaxy::string-object val))
    (gal-frame
     (c-galaxy::frame-object (lisp-to-c val)))
    (gal-symbol
     (c-galaxy::symbol-object (symbol-name val)))
    (gal-proxy
     (c-galaxy::create-proxy-object
      (galaxy-io::broker-proxy-c-proxy val) 0))
    (otherwise
     (error 'galaxy-object-conversion-error
	    :msg (format nil "Can't convert ~a (~a) to C" val (type-of val))))))

(defmethod lisp-to-c ((f gal-frame))
  (let ((c-f (c-galaxy::make-frame
	      (frame-name f)
	      (car (rassoc (frame-type f) *FRAME-TYPE-TABLE*)))))
    (dolist (d (frame-data f))
      (c-galaxy::set-prop c-f (car d) (val-to-object (cdr d))))
    (dolist (p (frame-preds f))
      (c-galaxy::add-pred c-f (lisp-to-c p)))
    c-f))

(defun object-to-val (obj)
  (case (car (rassoc (c-galaxy::get-object-type obj) *TYPE-TABLE*))
    (gal-frame
     (instantiate-frame-from-c (c-galaxy::frame-value obj)))
    (gal-int
     (c-galaxy::int-value obj))
    (gal-float
     (c-galaxy::float-value obj))
    (gal-string
     (c-galaxy::string-value obj))
    (gal-list
     (let ((len (c-galaxy::gal-list-length obj))
	   (res ()))
       (dotimes (i len)
	 (push (object-to-val (c-galaxy::get-list-object obj i)) res))
       (reverse res)))
    ((gal-binary gal-int-16 gal-int-32 gal-float-32 gal-float-64)
     (let ((b-length (c-galaxy::object-byte-count obj))
	   (data (c-galaxy::array-value obj 0)))	     
       (binary-data-from-c-data
	(car (rassoc (c-galaxy::get-object-type obj) *TYPE-TABLE*))
	data b-length)))
    (gal-symbol
     ;; This can't be read-from-string, because the name
     ;; might contain all sorts of odd things.
     (intern (c-galaxy::keyword-value obj)))
    (gal-proxy
     ;; This will only be called on the inbound side.
     (make-instance 'galaxy-io::broker-proxy-in
       :proxy (c-galaxy::proxy-value obj)))
    (otherwise
     (error 'galaxy-object-conversion-error
	    :msg (format nil "Can't convert ~a (~d) to Lisp" obj (c-galaxy::get-object-type obj))))))
