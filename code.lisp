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

;;; When the item is a CODE-COMMAND and it has a single operand of
;;; type LABEL, then the preliminary size is the MAXIMUM of the size
;;; of each candidate instruction.  When the item is a CODE-COMMAND
;;; and it has some other operands then the preliminary size is the
;;; MINIMUM of the size of each candidate instruction.
(defmethod preliminary-size ((item code-command))
  (let* ((operands (operands item))
	 (candidates (candidates (mnemonic item) operands)))
    (reduce (if (and (= (length operands) 1)
		     (typep (first operands) 'label))
		#'max
		#'min)
	    (mapcar (lambda (desc)
		      (instruction-size desc operands))
		    candidates))))
