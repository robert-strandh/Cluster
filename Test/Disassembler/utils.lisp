(cl:in-package #:cluster-test.disassembler)

(defun decode-instruction (instruction)
  (let ((interpreter (c.d:make-interpreter c.d:*x86-table*)))
    (c.d:decode-instruction interpreter instruction)))

(defun decode-command-as-sequence (command)
  (let ((interpreter (c.d:make-debug-interpreter c.d:*x86-table*
                                                 (list command))))
    (elt (c.d:decode-sequence interpreter (c:compute-encoding command)) 0)))

(defun disassemble-sequence (assembled-program)
  (let ((interpreter (c.d:make-interpreter c.d:*x86-table*)))
    (c.d::decode-sequence interpreter assembled-program)))

(defun disassemble-sequence-with-debug (code-command-program)
  (let ((interpreter (c.d:make-debug-interpreter c.d:*x86-table*
                                                 code-command-program)))
    (c.d:decode-sequence interpreter
                         (cluster:assemble code-command-program))))

(defun assert-assembler-equal-encoding (test-program)
  (setf test-program (coerce test-program 'list))
  (let ((assembled-program (c:assemble test-program))
        (disassembled-test-program
          (disassemble-sequence-with-debug test-program)))
    (assert (= (length test-program)
               (length disassembled-test-program)))
    (let ((re-assembled-program
            (cluster:assemble (coerce disassembled-test-program 'list))))
      ;; coerced so don't have to write anything for array equality
      (assert (equal (coerce assembled-program 'list)
                     (coerce re-assembled-program 'list))))))

(defun assert-decoded-descriptor (instruction &key operand-size-override
                                                opcode-extension
                                                mnemonic
                                                rex.w
                                                operands)
  (assert (eql operand-size-override (c:operand-size-override
                                      (c.d:instruction-descriptor instruction))))
  (assert (eql rex.w (c:rex.w
                      (c.d:instruction-descriptor instruction))))
  (assert (string= mnemonic (c:mnemonic
                             (c.d:instruction-descriptor instruction))))
  (assert (eql opcode-extension (c:opcode-extension
                                 (c.d:instruction-descriptor instruction))))
  (assert (equal operands  (c:operands
                            (c.d:instruction-descriptor instruction)))))

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

(defun assert-gpr-operand (operand size code)
  (assert (= size (c:size operand)))
  (assert (= code (c:code-number operand))))
