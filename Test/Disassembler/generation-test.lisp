(cl:in-package #:cluster-test.disassembler.generator)

;;; Don't really like this because it's leaking too much internals
(defclass position-tracking-interpreter (c.d::table-interpreter)
  ((%instruction-boundaries :initarg :instruction-boundaries
                            :reader instruction-boundaries)))

(defun instruction-boundaries (pre-assembled-program)
  (let ((previous-bound 0))
    (mapcar (lambda (boundary) (incf previous-bound boundary))
            (map 'list (lambda (command) (length (c:compute-encoding command)))
                    pre-assembled-program))))

(defun make-tracking-interpreter (code-command-program)
  (let ((old (c.d::make-interpreter c.d::*x86-table*)))
    (change-class old ' position-tracking-interpreter
                  :instruction-boundaries
                  (instruction-boundaries code-command-program))))

(defmethod c.d::reset :after ((interpreter position-tracking-interpreter))
  (assert (not (null (find (c.d::buffer-position interpreter)
                           (instruction-boundaries interpreter))))))

(defun assemble-test-program (program)
  (cluster:assemble (coerce program 'list)))

(defun assert-assembler-equal-encoding (test-program)
  (let* ((assembled-program (assemble-test-program test-program))
         (disassembled-test-program
           (let ((position-tracking-interpreter
                   (make-tracking-interpreter test-program)))
             (c.d:decode-sequence position-tracking-interpreter assembled-program))))
    (assert (= (length test-program)
               (length disassembled-test-program)))
    (let ((re-assembled-program (assemble-test-program disassembled-test-program)))
      (assert (equal assembled-program re-assembled-program)))))

(defun test-all-x86 ()
  (let* ((generator (make-code-command-generator))
         (test-program
           (generate-test-commands generator c::*instruction-descriptors*)))
    (assert-assembler-equal-encoding test-program)))
