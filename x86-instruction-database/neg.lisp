(cl:in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic NEG

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Nigate a 64-bit GPR or memory location.

;;; Negate a 64-bit GPR.
(define-instruction "NEG"
  :modes (64)
  :operands ((gpr 64))
  :opcodes (#xF7)
  :opcode-extension 3
  :encoding (modrm)
  :rex.w t)

;;; Negate a 64-bit memory location
(define-instruction "NEG"
  :modes (64)
  :operands ((memory 64))
  :opcodes (#xF7)
  :opcode-extension 3
  :encoding (modrm)
  :lock t
  :rex.w t)
