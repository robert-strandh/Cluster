(cl:in-package #:cluster-test.disassembler)

(defun make-jmp-r/m (r/m-operand)
  (let ((encoding
          (c:compute-encoding (c:make-code-command
                               "JMP"
                               (list r/m-operand)))))
    (make-array (length encoding) :initial-contents encoding)))

(defun test-jmp-r/m64 ()
  (let* ((jmp-rbx+rdi*4
           (make-jmp-r/m
            (c:make-memory-operand 64
                                   :base-register 3
                                   :index-register 7
                                   :scale 4)))
         (disassembled-code-command (decode-instruction jmp-rbx+rdi*4)))
    (assert-decoded-descriptor disassembled-code-command
                               :mnemonic "JMP"
                               :opcode-extension 4
                               :operands '((c:memory 64)))
    (assert-memory-operand (first (c:operands disassembled-code-command))
                           :base-register 3
                           :index-register 7
                           :scale 4
                           :size 64)))
