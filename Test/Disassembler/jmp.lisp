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

(defun test-jmp-rel32 ()
  (let* ((jmp-label (c:make-label))
         (add-bx-5 (c:make-code-command
                    "ADD"
                    (list (c:make-gpr-operand 16 3)
                          (c:make-immediate-operand 5))))
         (jmp-rip-32
           (cluster:make-code-command "JMP" (list jmp-label)))
         (assembled-instructions (cluster:assemble
                                     (list jmp-label add-bx-5 jmp-rip-32)))
         (disassembled-commands
           (c.d:decode-sequence (c.d:make-interpreter c.d:*x86-table*)
                                assembled-instructions)))
    ;; But why does cluster choose this and not JMP (opcode #xEB) (label 8) ?
    (assert-decoded-descriptor (aref disassembled-commands 2)
                               :mnemonic "JMP"
                               :operands '((c:label 32)))))
