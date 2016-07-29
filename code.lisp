(cl:in-package #:cluster)

(defclass code-command (command)
  ((%mnemonic :initarg :mnemonic :reader mnemonic)
   (%operands :initarg :operands :reader operands)))

(defun make-code-command (mnemonic operands)
  (make-instance 'code-command
    :mnemonic mnemonic
    :operands operands))
