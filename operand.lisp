(cl:in-package #:cluster)

(defclass operand () ())

(defclass sized-operand (operand)
  ((%size :initarg :size :reader size)))

(defclass register-operand (sized-operand)
  ((%code-number :initarg :code-number :reader code-number)))

(defclass gpr-operand (register-operand)
  ())

(defun make-gpr-operand (size code-number)
  (make-instance 'gpr-operand
    :size size
    :code-number code-number))

(defclass mmx-register-operand (register-operand)
  ())

(defclass memory-operand (sized-operand)
  (;; An integer or NIL.
   (%base-register
    :initform nil
    :initarg :base-register
    :reader base-register)
   ;; An integer or NIL.
   (%index-register
    :initform nil
    :initarg :index-register
    :reader index-register)
   ;; 1, 2, 4, 8, or NIL.
   (%scale
    :initform nil
    :initarg :scale
    :reader scale)
   ;; A signed integer or NIL.
   (%displacement
    :initform nil
    :initarg :displacement
    :reader displacement)))

(defun make-memory-operand
    (size &key base-register index-register scale displacement)
  (make-instance 'memory-operand
    :size size
    :base-register base-register
    :index-register index-register
    :scale scale
    :displacement displacement))

(defclass immediate-operand (operand)
  (;; A signed integer.
   (%value :initarg :value :reader value)))

(defun make-immediate-operand (value)
  (make-instance 'immediate-operand
    :value value))

(defun operand-matches-p (operand descriptor)
  (ecase (car descriptor)
    (gpr-a
     (and (typep operand 'gpr-operand)
	  (= (code-number operand) 0)
	  (= (size operand) (cadr descriptor))))
    (gpr
     (and (typep operand 'gpr-operand)
	  (= (size operand) (cadr descriptor))))
    (memory
     (and (typep operand 'memory-operand)
	  (= (size operand) (cadr descriptor))))
    (simm
     (and (typep operand 'immediate-operand)
	  (typep (value operand) `(signed-byte ,(cadr descriptor)))))
    (imm
     (and (typep operand 'immediate-operand)
	  (or (typep (value operand) `(signed-byte ,(cadr descriptor)))
	      (typep (value operand) `(unsigned-byte ,(cadr descriptor))))))
    (label
     ;; We don't take into account the size of the label at this
     ;; point, because we do not yet know what the final size of the
     ;; label is going to be.
     (typep operand 'label))))

(defun operands-match-p (operands descriptors)
  (and (= (length operands) (length descriptors))
       (every #'operand-matches-p operands descriptors)))

