(cl:in-package #:cluster-test.disassembler)

(defun test-all-x86 ()
  (let* ((generator (make-code-command-generator))
         (test-program
           (generate-test-commands generator c::*instruction-descriptors*)))
    (assert-assembler-equal-encoding test-program)))
