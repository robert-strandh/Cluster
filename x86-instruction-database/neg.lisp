(cl:in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic NEG

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Negate a 32-bit GPR or memory location.

;;; Negate a 32-bit GPR.
(define-instruction "NEG"
  :modes (32 64)
  :operands ((gpr 32))
  :opcodes (#xF7)
  :opcode-extension 3
  :encoding (modrm)
  :rex.w t)

;;; Negate a 32-bit memory location
(define-instruction "NEG"
  :modes (32 )
  :operands ((memory 32))
  :opcodes (#xF7)
  :opcode-extension 3
  :encoding (modrm)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Negate a 64-bit GPR or memory location.

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
