(cl:in-package #:cluster)

;;; Instances of this class are used as targets for branch
;;; instructions.  The identity of a label is not determined by a
;;; name, but by the Common Lisp definition of identity: EQ.
(defclass label (item) ())

(defun make-label ()
  (make-instance 'label))

(defmethod compute-encoding ((item label))
  '())
