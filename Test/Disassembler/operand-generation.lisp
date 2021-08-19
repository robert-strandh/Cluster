(cl:in-package #:cluster-test.disassembler)

(defmethod generate-operand (generator (operand-descriptor (eql 'c:gpr))
                             encoding
                             operand-size)
  (declare (ignore generator operand-descriptor encoding))
  (let ((code-number (if (= operand-size 64)
                         (random 16)
                         (random 8))))
    (c:make-gpr-operand operand-size code-number)))

(defmethod generate-operand (generator (operand-descriptor (eql 'c:gpr-a))
                             encoding operand-size)
  (declare (ignore generator operand-descriptor encoding))
  (c:make-gpr-operand operand-size 0))

;;; https://wiki.osdev.org/X86-64_Instruction_Encoding#32.2F64-bit_addressing_2
;;; this is quite painful to exhaustively generate for
;;; and it doesn't do that yet
(defmethod generate-operand (generator (operand-descriptor (eql 'c:memory))
                             (encoding (eql 'c:modrm))
                             operand-size)
  (declare (ignore generator operand-descriptor encoding))
  (flet ((random-scale ()
           (case (random 4)
             (0 1)
             (1 2)
             (2 4)
             (3 8)))

         (random-displacement (&optional nullable-p)
           (let ((displacement-size-limit
                   (case (random (if nullable-p 3 2))
                     (0 8)
                     (1 32)
                     (2 0))))
             (if  (= 0 displacement-size-limit)
                  nil
                  (c.d::unsigned-to-signed
                   displacement-size-limit
                   (random (expt 2 displacement-size-limit))))))

         (random-gpr (gpr-limit &optional (exclude '()))
           (loop for register-code = (random gpr-limit)
                 while (member register-code exclude)
                 finally (return register-code))))

    (let* ((gpr-random-limit (if (= operand-size 64) 16 8)))
      (case (random 4)
        (0 (c:make-memory-operand
            operand-size
            :displacement (random-displacement)))
        (1 (c:make-memory-operand
            operand-size
            :base-register (random gpr-random-limit)
            :displacement (random-displacement t)))
        (2 (c:make-memory-operand
            operand-size
            :index-register (random-gpr (1- gpr-random-limit) '(4))
            :scale (random-scale)
            :displacement (random-displacement t)))
        (3 (c:make-memory-operand
            operand-size
            :base-register (random gpr-random-limit)
            :index-register (random-gpr (1- gpr-random-limit) '(4))
            :scale (random-scale)
            :displacement (random-displacement t)))))))

;;; Yes, there is a difference between imm and simm.
;;; Yes, the descriptor tells you which to use.
(defmethod generate-operand (generator (operand-descriptor (eql 'c:imm))
                             (encoding (eql 'c:imm))
                             operand-size)
  (declare (ignore generator operand-descriptor encoding))
  (c:make-immediate-operand (random (expt 2 operand-size))))

(defmethod generate-operand (generator (operand-descriptor (eql 'c:simm))
                             (encoding (eql 'c:imm))
                             operand-size)
  (declare (ignore generator operand-descriptor encoding))
  (c:make-immediate-operand
   (c.d::unsigned-to-signed operand-size (random (expt 2 operand-size)))))

(defmethod generate-operand ((generator code-command-generator)
                             (operand-descriptor (eql 'c:label))
                             (encoding (eql 'c:label))
                             operand-size)
  (declare (ignore operand-descriptor encoding operand-size))
  (make-label generator))
