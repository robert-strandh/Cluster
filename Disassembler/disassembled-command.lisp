(cl:in-package #:cluster.disassembler)

(defclass disassembled-command (cluster:command)
  ((%instruction-descriptor :initarg :instruction-descriptor
                            :reader instruction-descriptor)
   (%operands :initarg :operands :reader cluster:operands)
   ;; The position of the instruction in the sequence it was decoded
   ;; from if indeed it was present in one.
   (%start-position :initarg :start-position :accessor start-position
                    :initform nil)))

(defmethod cluster:mnemonic ((command disassembled-command))
  (cluster:mnemonic (instruction-descriptor command)))

(defun make-disassembled-command (instruction-descriptor operands)
  (make-instance 'disassembled-command
                 :instruction-descriptor instruction-descriptor
                 :operands operands))
