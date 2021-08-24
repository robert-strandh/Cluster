(cl:in-package #:cluster)

;;; LOCK is annotative, it doesn't change the instruction descriptor
;;; that is used. We also don't have a way to show LOCK
;;; in code-commands in Cluster either at the moment.
#+ (or)
(define-modifier-prefix lock lock #xF0)
(define-modifier-prefix operand-size-override
  operand-size-override #x66)

(define-range-prefix rex (#x40 #x4F)
  (#b1000 rex.w))

(define-instruction-set x86
    (:modifier-prefixes (operand-size-override)
     :range-prefixes (rex)))
