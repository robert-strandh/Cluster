(cl:in-package #:cluster-test.disassembler)

;;; TODO Cluster doesn't encode this instruction
;;; using make-memory-operand properly
(defun test-jmp-r/m32 ()
  (let* ((jmp-ebx+edi*4 #(#xFF #b00100100 #b10111011))
         (disassembled-code-command (decode-instruction jmp-ebx+edi*4)))
    (assert-decoded-descriptor disassembled-code-command
                               :mnemonic "JMP"
                               :opcode-extension 4
                               :operands '((c:memory 32)))
    (assert-memory-operand (first (c:operands disassembled-code-command))
                           :base-register 3
                           :index-register 7
                           :scale 4
                           :size 32)))
