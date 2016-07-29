(cl:in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Items of the list making up a source program.

;;; This is the base class of all items that can occur in the list
;;; that is submitted to the assembler.
(defclass item () ())
