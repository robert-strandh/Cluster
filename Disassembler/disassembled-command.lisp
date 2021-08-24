(cl:in-package #:cluster.disassembler)

(defclass disassembled-command (cluster:code-command)
  ((%instruction-descriptor :initarg :instruction-descriptor
                            :reader instruction-descriptor)
   ;; The position of the instruction in the sequence it was decoded
   ;; from if indeed it was present in one.
   (%start-position :initarg :start-position :accessor start-position
                    :initform nil)))

(defmethod print-object ((object disassembled-command) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a" (c:mnemonic object)
            (c:operands (instruction-descriptor object)))))

(defun make-disassembled-command (instruction-descriptor operands)
  (make-instance 'disassembled-command
                 :instruction-descriptor instruction-descriptor
                 :operands operands
                 :mnemonic (c:mnemonic instruction-descriptor)))
