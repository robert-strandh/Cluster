(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 64-bit GPR (destination), move the contents of a 32-bit GPR
;;; or memory location, and sign extend it.

;;; To a 64-bit GPR (destination), move the contents of a 32-bit GPR,
;;; and sign extend it.
(define-instruction "MOVSXD"
  :modes (64)
  :operands ((gpr 64) (gpr 32))
  :opcodes (#x63)
  :encoding (reg modrm)
  :rex.w t)

;;; To a 64-bit GPR (destination), move the contents of a 32-bit
;;; memory location, and sign extend it.
(define-instruction "MOVSXD"
  :modes (64)
  :operands ((gpr 64) (memory 32))
  :opcodes (#x63)
  :encoding (reg modrm)
  :rex.w t)
