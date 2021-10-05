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

;;; Always include the RXB bits of a potential REX byte.
(defun encode-memory-operand (memory-operand)
  (with-accessors ((base-register base-register)
                   (index-register index-register)
                   (scale scale)
                   (displacement displacement))
      memory-operand
    (cond ((and (null base-register)
                (null index-register))
           ;; We have only a displacement.
           `(#b000
             #b00000101 ; ModR/M byte.
             ;; SIB byte to encode only displacement using RIP-relative addressing.
             #b00100101
             ,@(encode-integer displacement 4)))
          ((and (null index-register)
                (null displacement))
           ;; We have only a base register.
           (multiple-value-bind (rex.b r/m)
               (floor base-register 8)
             (cond
               ((= r/m 4)
                `(,rex.b
                  #b00000100   ; ModR/M byte.
                  #b00100100)) ; SIB byte.
               ;; It is not possible to encode BP(5) or R13 as a base register
               ;; using the above method, as that encoding is used
               ;; for RIP relative addressing.
               ;; We must instead encode a displacement of 0.
               ((= r/m 5)
                `(,rex.b
                  #b01000101 ; ModR/M byte
                  ,@(encode-integer 0 1)))
               (t
                `(,rex.b
                  ,r/m)))))
          ((and (null index-register)
                (typep displacement '(signed-byte 8)))
           ;; We have a base register and an 8-bit displacement.
           (multiple-value-bind (rex.b r/m)
               (floor base-register 8)
             (if (= r/m 4)
                 `(,rex.b
                   #b01000100 ; ModR/M byte.
                   #b00100100 ; SIB byte.
                   ,@(encode-integer displacement 1))
                 `(,rex.b
                   ,(+ #b01000000 r/m)
                   ,@(encode-integer displacement 1)))))
          ((and (null index-register)
                (typep displacement '(signed-byte 32)))
           ;; We have a base register and a 32-bit displacement.
           (multiple-value-bind (rex.b r/m)
               (floor base-register 8)
             (if (= r/m 4)
                 `(,rex.b
                   #b10000100 ; ModR/M byte.
                   #b00100100 ; SIB byte.
                   ,@(encode-integer displacement 4))
                 `(,rex.b
                   #b10000100 ; ModR/M byte.
                   ,(+ #b00100000 r/m) ; SIB, only encode base register.
                   ,@(encode-integer displacement 4)))))
          ((null base-register)
           ;; The only encoding provided when there is no base
           ;; register has a 32-bit displacement, so even if the
           ;; displacement is small or even 0, we must use this
           ;; encoding.
           (multiple-value-bind (rex.x i)
               (floor index-register 8)
             `(,(ash rex.x 1)
               #b00000101 ; ModR/M byte.
               ,(+ (ash (round (log scale 2)) 6)
                   (ash i 3)
                   #b101)
               ,@(encode-integer (or displacement 0) 4))))
          ((null displacement)
           (multiple-value-bind (rex.b b)
               (floor base-register 8)
             (multiple-value-bind (rex.x i)
                 (floor index-register 8)
               (if (= b 5)
                   ;; If the base register is 5 (EBP) or 13, then we
                   ;; have a problem, because there is no encoding for
                   ;; that situation without a displacement.  So we
                   ;; use a displacement of 0.
                   `(,(+ (ash rex.x 1) rex.b)
                     #b01000100 ; ModR/M byte.
                     ,(+ (ash (round (log scale 2)) 6)
                         (ash i 3)
                         b)
                     0)
                   `(,(+ (ash rex.x 1) rex.b)
                     #b00000100 ; ModR/M byte.
                     ,(+ (ash (round (log scale 2)) 6)
                         (ash i 3)
                         b))))))
          (t
           (multiple-value-bind (rex.b b)
               (floor base-register 8)
             (multiple-value-bind (rex.x i)
                 (floor index-register 8)
               (when (= index-register 4)
                 (error "You can't use the stack pointer
as the index register with a scale."))
               (if (typep displacement '(signed-byte 8))
                   `(,(+ (ash rex.x 1) rex.b)
                     #b01000100 ; ModR/M byte.
                     ,(+ (ash (round (log scale 2)) 6)
                         (ash i 3)
                         b)
                     ,@(encode-integer displacement 1))
                   `(,(+ (ash rex.x 1) rex.b)
                     #b10000100 ; ModR/M byte.
                     ,(+ (ash (round (log scale 2)) 6)
                         (ash i 3)
                         b)
                     ,@(encode-integer displacement 4)))))))))

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
    (gpr-c
     (and (typep operand 'gpr-operand)
          (= (code-number operand) 2)
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
