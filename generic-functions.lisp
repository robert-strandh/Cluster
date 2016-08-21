(cl:in-package #:cluster)

(defgeneric compute-encoding (item))

(defmethod compute-encoding (item)
  (error "Item of unknown type: ~s" item))

;;; Take an item and return the preliminary size of that item.
(defgeneric preliminary-size (item))

(defmethod preliminary-size (item)
  (error "Item of unknown type: ~s" item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Given an instruction descriptor and the operands to the command
;;; that the instruction descriptor matches, compute the encoding of
;;; the resulting instruction.

;;; Encode an instruction with a single operand.
(defgeneric encode-instruction-1 (desc operand))

;;; Encode an instruction with two operands.
(defgeneric encode-instruction-2 (desc operand1 operand2))
