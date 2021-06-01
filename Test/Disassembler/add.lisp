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

(defun test-add-80 ())
(defun test-add-81 ())
(defun test-add-83 ())
(defun test-add-00 ())
(defun test-add-01 ())
(defun test-add-02 ())
(defun test-add-03 ())
