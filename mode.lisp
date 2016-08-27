(cl:in-package :cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mode of operation.
;;; 
;;; Either 64-bit or 32-bit for now.
;;;
;;; At the moment, we do not handle address-size overrides.  As a
;;; consequence, when the mode is 64-bit, base and index registers
;;; must be 64-bit registers, and when the mode is 32-bit, base and
;;; index registers must be 32-bit registers.

(defparameter *mode* '64-bit)
