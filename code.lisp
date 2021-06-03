(cl:in-package #:cluster)

(defclass code-command (command)
  ((%mnemonic :initarg :mnemonic :reader mnemonic)
   (%operands :initarg :operands :reader operands)))

(defun make-code-command (mnemonic operands)
  (make-instance 'code-command
    :mnemonic mnemonic
    :operands operands))

(defmethod compute-encoding ((item code-command))
  (let* ((operands (operands item))
         (candidates (candidates (mnemonic item) operands)))
    (flet ((best-candidate (c1 c2)
             (if (and (= (length operands) 1)
                      (typep (first operands) 'label))
                 (if (> (instruction-size c1 operands)
                        (instruction-size c2 operands))
                     c1
                     c2)
                 (if (< (instruction-size c1 operands)
                        (instruction-size c2 operands))
                     c1
                     c2))))
      (encode-instruction (reduce #'best-candidate candidates)
                          operands))))

;;; When the item is a CODE-COMMAND and it has a single operand of
;;; type LABEL, then the preliminary size is the MAXIMUM of the size
;;; of each candidate instruction.  When the item is a CODE-COMMAND
;;; and it has some other operands then the preliminary size is the
;;; MINIMUM of the size of each candidate instruction.
(defmethod preliminary-size ((item code-command))
  (let* ((operands (operands item))
         (candidates (candidates (mnemonic item) operands)))
    (reduce (if (and (= (length operands) 1)
                     (typep (first operands) 'label))
                #'max
                #'min)
            (mapcar (lambda (desc)
                      (instruction-size desc operands))
                    candidates))))

(defmethod encode-instruction-1 (desc (operand immediate-operand))
  (let ((type (first (encoding desc)))
        (length (/ (second (first (operands desc))) 8)))
    (ecase type
      (imm
       (let* ((rex-p (rex.w desc)))
         `(,@(if (operand-size-override desc) '(#x66) '())
           ,@(if rex-p '(#x48) '())
           ,@(opcodes desc)
           ,@(encode-integer (value operand) length)))))))

;;; A hash table mapping items to addresses relative to the
;;; beginning of the program.
(defparameter *addresses* nil)

;;; The address (relative to the beginning of the program) of the
;;; instruction immediately following the one being encoded.
(defparameter *instruction-pointer* nil)

(defmethod encode-instruction-1 (desc (operand label))
  (let ((type (first (encoding desc))))
    (ecase type
      (label
       (let* ((rex-p (rex.w desc)))
         `(,@(if (operand-size-override desc) '(#x66) '())
           ,@(if rex-p '(#x48) '())
           ,@(opcodes desc)
           ,@(encode-integer (- (gethash operand *addresses*)
                                *instruction-pointer*)
                             4)))))))

(defmethod encode-instruction-1 (desc (operand gpr-operand))
  (let ((type (first (encoding desc))))
    (ecase type
      (modrm
       `(,@(if (operand-size-override desc) '(#x66) '())
         ,@(if (rex.w desc)
               (if (>= (code-number operand) 7)
                   '(#b01001001)
                   '(#b01001000))
               (if (>= (code-number operand) 7)
                   '(#b01000001)
                   '()))
         ,@(opcodes desc)
         ,(+ #b11000000
             (ash (opcode-extension desc) 3)
             (mod (code-number operand) 8))))
      (+r
       `(,@(if (operand-size-override desc) '(#x66) '())
         ,@(if (rex.w desc)
               (if (>= (code-number operand) 7)
                   '(#b01001001)
                   '(#b01001000))
               (if (>= (code-number operand) 7)
                   '(#b01000001)
                   '()))
         ,(+ (first (opcodes desc))
             (mod (code-number operand) 8)))))))

(defmethod encode-instruction-1 (desc (operand memory-operand))
  (let ((type (first (encoding desc))))
    (ecase type
      (modrm
       (destructuring-bind (rex.xb modrm &rest rest)
           (encode-memory-operand operand)
         (let ((rex-low (+ (if (rex.w desc) #b1000 0) rex.xb)))
           `(,@(if (operand-size-override desc) '(#x66) '())
             ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
             ,@(opcodes desc)
             ,(logior modrm (ash (opcode-extension desc) 3))
             ,@rest)))))))

(defmethod encode-instruction-2
    (desc (operand1 gpr-operand) (operand2 immediate-operand))
  (multiple-value-bind (rex.b r/m)
      (floor (code-number operand1) 8)
    (let* ((rex-low (+ (if (rex.w desc) #b1000 0) rex.b)))
      (let ((type1 (first (encoding desc)))
            (type2 (second (encoding desc)))
            (length2 (/ (second (second (operands desc))) 8)))
        (ecase type1
          (-
           (ecase type2
             (imm
              `(,@(if (operand-size-override desc) '(#x66) '())
                ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
                ,@(opcodes desc)
                ,@(encode-integer (value operand2) length2)))))
          (modrm
           (ecase type2
             (imm
              `(,@(if (operand-size-override desc) '(#x66) '())
                ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
                ,@(opcodes desc)
                ,(+ (ash #b11 6)
                    (ash (opcode-extension desc) 3)
                    r/m)
                ,@(encode-integer (value operand2) length2)))))
          (+r
           (ecase type2
             (imm
              `(,@(if (operand-size-override desc) '(#x66) '())
                ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
                ,(+ (car (opcodes desc)) r/m)
                ,@(encode-integer (value operand2) length2))))))))))

(defmethod encode-instruction-2
  (desc (operand1 gpr-operand) (operand2 gpr-operand))
  (assert (or (equal (encoding desc) '(reg modrm))
              (equal (encoding desc) '(modrm reg))))
  (when (equal (encoding desc) '(modrm reg))
    (rotatef operand1 operand2))
  (multiple-value-bind (rex.b r/m)
      (floor (code-number operand2) 8)
    (multiple-value-bind (rex.r reg)
        (floor (code-number operand1) 8)
      (let ((rex-low (+ (if (rex.w desc) #b1000 0)
                        (ash rex.r 2)
                        rex.b)))
        `(,@(if (operand-size-override desc) '(#x66) '())
          ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
          ,@(opcodes desc)
          ,(+ #b11000000
              (ash reg 3)
              r/m))))))

(defmethod encode-instruction-2
  (desc (operand1 gpr-operand) (operand2 memory-operand))
  (assert (equal (encoding desc) '(reg modrm)))
  (destructuring-bind (rex.xb modrm &rest rest)
      (encode-memory-operand operand2)
    (multiple-value-bind (rex.r reg)
        (floor (code-number operand1) 8)
      (let ((rex-low (+ (if (rex.w desc) #b1000 0)
                        rex.xb
                        (ash rex.r 2))))
        `(,@(if (operand-size-override desc) '(#x66) '())
          ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
          ,@(opcodes desc)
          ,(logior modrm (ash reg 3))
          ,@rest)))))

(defmethod encode-instruction-2
  (desc (operand1 memory-operand) (operand2 immediate-operand))
  (assert (equal (encoding desc) '(modrm imm)))
  (destructuring-bind (rex.xb modrm &rest rest)
      (encode-memory-operand operand1)
    (let ((rex-low (+ (if (rex.w desc) #b1000 0) rex.xb)))
      `(,@(if (operand-size-override desc) '(#x66) '())
        ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
        ,@(opcodes desc)
        ,(logior modrm (ash (opcode-extension desc) 3))
        ,@rest
        ,@(encode-integer (value operand2)
                          (/ (second (second (operands desc))) 8))))))

(defmethod encode-instruction-2
  (desc (operand1 memory-operand) (operand2 gpr-operand))
  (assert (equal (encoding desc) '(modrm reg)))
  (destructuring-bind (rex.xb modrm &rest rest)
      (encode-memory-operand operand1)
    (multiple-value-bind (rex.r reg)
        (floor (code-number operand2) 8)
      (let ((rex-low (+ (if (rex.w desc) #b1000 0)
                        rex.xb
                        (ash rex.r 2))))
        `(,@(if (operand-size-override desc) '(#x66) '())
          ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
          ,@(opcodes desc)
          ,(logior modrm (ash reg 3))
          ,@rest)))))

(defun encode-instruction (desc operands)
  (ecase (length operands)
    (0 (opcodes desc))
    (1 (encode-instruction-1 desc (first operands)))
    (2 (encode-instruction-2 desc (first operands) (second operands)))))
