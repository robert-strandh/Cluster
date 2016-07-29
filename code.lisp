(cl:in-package #:cluster)

(defclass code-command (command)
  ((%mnemonic :initarg :mnemonic :reader mnemonic)
   (%operands :initarg :operands :reader operands)))

(defun make-code-command (mnemonic operands)
  (make-instance 'code-command
    :mnemonic mnemonic
    :operands operands))

(defmethod compute-encoding ((item code-command))
  (let* ((operands (operands item))
	 (candidates (candidates (mnemonic item) operands)))
    (flet ((best-candidate (c1 c2)
	     (if (and (= (length operands) 1)
		      (typep (first operands) 'label))
		 (if (> (instruction-size c1 operands)
			(instruction-size c2 operands))
		     c1
		     c2)
		 (if (< (instruction-size c1 operands)
			(instruction-size c2 operands))
		     c1
		     c2))))
      (encode-instruction (reduce #'best-candidate candidates)
			  operands))))
