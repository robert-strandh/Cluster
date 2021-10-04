(cl:in-package #:cluster)

;;; In a 64-bit GPR (destination), store the effective addres of a
;;; 64-bit memory location.
(define-instruction "LEA"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x8D)
  :encoding (reg modrm)
  :rex.w t)
