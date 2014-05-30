;; This file (c) Copyright 1998 - 2002 The MITRE Corporation
;; 
;; This file is part of the Galaxy Communicator system. It is licensed
;; under the conditions described in the file LICENSE in the root 
;; directory of the Galaxy Communicator system.

(defpackage SLS-Util
  (:use "COMMON-LISP"))

(in-package "SLS-UTIL")

(defconstant *documentation* "
GalUtil_Fatal(format, ...):                      (fatal (ostream conn) msg)
GalUtil_Warn(format, ...):                       (gal-warn (ostream conn) msg)
GalUtil_Error(format, ...):                      (gal-error (ostream conn) msg)
GalUtil_Print(level, format, ...):               (gal-write-level (ostream conn) msg level)
GalUtil_Cprint(level, fore, back, format, ...):  [not implemented]
GalUtil_PInfo1(format, ...):                     (pinfo1 (ostream conn) msg)
GalUtil_PInfo2(format, ...):                     (pinfo2 (ostream conn) msg)
GalUtil_CPInfo1(fore, back, format, ...):        [not implemented]
GalUtil_CPInfo2(fore, back, format, ...):        [not implemented]
GalUtil_Debug1(format, ...):                     (debug1 (ostream conn) msg)
GalUtil_Debug2(format, ...):                     (debug2 (ostream conn) msg)
GalUtil_Assert(truth, format, ...):              (gal-assert (ostream conn) truth msg)
GalUtil_VerboseUseColor():                       [not implemented]
GalUtil_VerboseUseBW():                          [not implemented]
GalUtil_CPrint(level, fore, back, format, ...):  [not implemented]

GalUtil_OACheckUsage(argc, argv, oas, first_real_arg): [not implemented]
GalUtil_OAPrintUsage(argc, argv, oas):                 [not implemented]
GalUtil_OAExtract(argc, argv, oas, key, ...):          [not implemented]
GalUtil_OAExtractAsserting(argc, argv, oas, key, ...): [not implemented]
")

(defconstant *FATAL-LEVEL* 0)
(defconstant *WARNING-LEVEL* 1)
(defconstant *PINFO1-LEVEL* 2)
(defconstant *PINFO2-LEVEL* 3)
(defconstant *DEBUG1-LEVEL* 4)
(defconstant *DEBUG2-LEVEL* 5)
(defparameter *VERBOSE-LEVEL* 9999)

(let ((v-level (sys:getenv "GAL_VERBOSE")))
  (if (and v-level (integerp (read-from-string v-level)))
      (setf *VERBOSE-LEVEL* (read-from-string v-level))))

(defclass ostream () ())

(defmethod fatal ((o ostream) msg)
  (if (> *VERBOSE-LEVEL* *FATAL-LEVEL*)
      (progn 
	(gal-print-internal o (format nil "  (FATAL ERROR: ~a)" msg)
			    *FATAL-LEVEL* *error-output*)
	(sys::exit()))))

(defmethod gal-assert ((o ostream) truth msg)
  (if (not truth)
      (fatal o msg)))

(defmethod gal-error ((o ostream) msg)
  (if (> *VERBOSE-LEVEL* *FATAL-LEVEL*)
      (gal-print-internal o (format nil "  (ERROR: ~a)" msg)
			  *WARNING-LEVEL* *error-output*)))

(defmethod gal-warn ((o ostream) msg)
  (if (> *VERBOSE-LEVEL* *WARNING-LEVEL*)
      (gal-print-internal o (format nil "  (WARNING: ~a)" msg)
			  *WARNING-LEVEL* *error-output*)))

(defmethod gal-write-level ((o ostream) msg level)
  (if (> *VERBOSE-LEVEL* level)
      (gal-print-internal o msg level *standard-output*)))

(defmethod gal-write ((o ostream) msg)
  (gal-print-internal o msg -1 *standard-output*))

(defmethod pinfo1((o ostream) msg)
  (if (> *VERBOSE-LEVEL* *PINFO1-LEVEL*)
      (gal-print-internal o msg *PINFO1-LEVEL* *standard-output*)))

(defmethod pinfo2((o ostream) msg)
  (if (> *VERBOSE-LEVEL* *PINFO2-LEVEL*)
      (gal-print-internal o msg *PINFO2-LEVEL* *standard-output*)))

(defmethod debug1((o ostream) msg)
  (if (> *VERBOSE-LEVEL* *DEBUG1-LEVEL*)
      (gal-print-internal o msg *DEBUG1-LEVEL* *standard-output*)))

(defmethod debug2((o ostream) msg)
  (if (> *VERBOSE-LEVEL* *DEBUG2-LEVEL*)
      (gal-print-internal o msg *DEBUG2-LEVEL* *standard-output*)))

(defmethod gal-print-internal ((o ostream) msg level s)
  (format s "~a~%" msg)
  (force-output s))

