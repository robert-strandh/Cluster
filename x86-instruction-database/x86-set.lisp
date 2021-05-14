(cl:in-package #:cluster)

(define-modifier-prefix lock lock #xF0)
(define-modifier-prefix operand-size-override
  operand-size-override #x66)

(define-range-prefix rex rex.w
  #x40 #x4F)

(define-instruction-set x86
    (:modifier-prefixes (lock operand-size-override)
     :range-prefixes (rex)))
