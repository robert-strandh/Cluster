(cl:in-package #:cluster-test.disassembler)

(defun disassemble-sequence (assembled-program)
  (let ((interpreter (c.d::make-interpreter c.d::*x86-table*)))
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

;;; we need something like this just with using labels
;;; and source tracking.
(defun simple-sequence-test ()
  (let* ((instructions
           (concatenate 'vector
                        (add-r-immediate 8 0 8)
                        (add-r-immediate 16 0 16)
                        (add-r-immediate 32 0 32)))
         (disassembled-code-commands
           (disassemble-sequence instructions)))
    disassembled-code-commands))

;;; this was used to huntdown the bug that ultimatley
;;; was the generator inserting 32 bit instructions
#+ (or)
(defun test-label-source-tracking ()
  (let ((code-command-program
          (list
           (c:make-label)
           (c:make-code-command
            "MUL"
            (list (c:make-gpr-operand 8 0)
                  (c:make-gpr-operand 8 2)))
           (c:make-code-command
            "JMP"
            (list (c:make-gpr-operand 64 1)))
           (c:make-code-command
            "XOR"
            (list (c:make-memory-operand
                   32
                   :index-register 4
                   :scale 1)
                  (c:make-gpr-operand 32 6)))
           (c:make-label)
           (c:make-code-command
            "JMP"
            (list (c:make-gpr-operand 32 1)))
           (c:make-code-command
            "SUB"
            (list (c:make-gpr-operand 16 5)
                  (c:make-gpr-operand 16 3))))))

    (assert-assembler-equal-encoding code-command-program)))

(defun test-problem ()
  (let ((code-command-program
          (list
           (c:make-label)
           (c:make-code-command
            "XOR"
            (list (c:make-memory-operand
                   16
                   :index-register 4
                   :scale 4)
                  (c:make-immediate-operand 120)))
           (c:make-code-command
            "SUB"
            (list (c:make-gpr-operand 8 0)
                  (c:make-immediate-operand 251)))
           (c:make-code-command
            "OR"
            (list (c:make-gpr-operand 64 0)
                  (c:make-immediate-operand 26383491308))))))

    (assert-assembler-equal-encoding code-command-program)))
