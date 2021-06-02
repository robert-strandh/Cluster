(cl:in-package #:cluster-test.disassembler)

;;;; can we rely on cluster to select the smallest
;;;; encoding for immediates? we do here.
(defun add-r-immediate (register-size register-number immediate)
  (let ((encoding
          (c:compute-encoding
           (c:make-code-command
            "ADD"
            (list (c:make-gpr-operand register-size register-number)
                  (c:make-immediate-operand immediate))))))
    (make-array (length encoding) :initial-contents encoding)))

(defun decode-instruction (instruction)
  (let ((interpreter (c.d::make-interpreter c.d::*x86-table*)))
    (c.d::decode-instruction interpreter instruction 0)))

(defun assert-decoded-descriptor (instruction &key operand-size-override
                                                 opcode-extension
                                                 mnemonic
                                                 rex.w
                                                 operands)
  (assert (eql operand-size-override (c:operand-size-override
                                      (c.d::instruction-descriptor instruction))))
  (assert (eql rex.w (c:rex.w
                      (c.d::instruction-descriptor instruction))))
  (assert (string= mnemonic (c:mnemonic
                             (c.d::instruction-descriptor instruction))))
  (assert (eql opcode-extension (c:opcode-extension
                                 (c.d::instruction-descriptor instruction))))
  (assert (equal operands  (c:operands
                            (c.d::instruction-descriptor instruction)))))

(defun assert-immediate (immediate-value disassembled-command)
  (assert (= immediate-value
             (c:value (second
                       (c:operands disassembled-command))))))

(defun assert-memory-operand
    (memory-operand &key base-register displacement index-register scale size)
  (assert (eql base-register (c:base-register memory-operand)))
  (assert (eql displacement (c:displacement memory-operand)))
  (assert (eql index-register (c:index-register memory-operand)))
  (assert (eql scale (c:scale memory-operand)))
  (assert (eql size (c:size memory-operand))))

(defun test-add-05 ()
  (let* ((add-ax-im16-300 (add-r-immediate 16 0 300))
         (disassembled-instruction (decode-instruction add-ax-im16-300)))

    (assert-decoded-descriptor disassembled-instruction
                               :operand-size-override t
                               :mnemonic "ADD"
                               :operands '((c:gpr-a 16) (c:imm 16)))
    (assert-immediate 300 disassembled-instruction))

  (let* ((add-eax-im32-100000 (add-r-immediate 32 0 100000))
         (disassembled-instruction (decode-instruction add-eax-im32-100000)))
    (assert-decoded-descriptor disassembled-instruction
                               :mnemonic "ADD"
                               :operands '((c:gpr-a 32) (c:imm 32)))
    (assert-immediate 100000 disassembled-instruction))

  (let* ((add-rax-im32-100000 (add-r-immediate 64 0 100000))
         (disassembled-instruction (decode-instruction add-rax-im32-100000)))
    (assert-decoded-descriptor disassembled-instruction
                               :rex.w t
                               :mnemonic "ADD"
                               :operands '((c:gpr-a 64) (c:simm 32)))
    (assert-immediate 100000 disassembled-instruction))

  (let* ((add-rax-im32--100000 (add-r-immediate 64 0 -100000))
         (disassembled-instruction (decode-instruction add-rax-im32--100000)))
    (assert-decoded-descriptor disassembled-instruction
                               :rex.w t
                               :mnemonic "ADD"
                               :operands '((c:gpr-a 64) (c:simm 32)))
    (assert-immediate -100000 disassembled-instruction)))


(defun add-r/m8-immediate (r/m-operand immediate-value)
  (let ((encoding
          (c:compute-encoding
           (c:make-code-command
            "ADD"
            (list r/m-operand
                  (c:make-immediate-operand immediate-value))))))
    (make-array (length encoding) :initial-contents encoding)))

(defun test-add-80 ()
  (let* ((add-cl-127 (add-r/m8-immediate
                      (cluster:make-gpr-operand 8 1)
                      127))
         (disassembled-code-command (decode-instruction add-cl-127)))

    (assert-decoded-descriptor disassembled-code-command
                               :mnemonic "ADD"
                               :operands '((c:gpr 8) (c:imm 8)))
    (assert-immediate 127 disassembled-code-command))

  ;; We don't have rex.w for this instruction in the database yet...
  #+ (or)
  (let* ((add-R10L-127 (add-r/m8-immediate
                        (cluster:make-gpr-operand 8 10)
                        127))
         (disassembled-code-command (decode-instruction add-R10L-127)))

    (assert-decoded-descriptor disassembled-code-command
                               :rex.w t
                               :mnemonic "ADD"
                               :operands '((c:gpr 8) (c:imm 8)))
    (assert-immediate 127 disassembled-code-command))  )
(defun test-add-81 ())
(defun test-add-83 ())
(defun test-add-00 ())
(defun test-add-01 ())
(defun test-add-02 ())
(defun test-add-03 ())
