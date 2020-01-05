(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic SHL

;;; Shift a 64-bit GPR (destination) left a number of positions
;;; indicated by an 8-bit immediate value (source).
(define-instruction "SHL"
  :modes (64)
  :operands ((gpr 64) (imm 8))
  :opcodes (#xC1)
  :opcode-extension 4
  :encoding (modrm imm)
  :rex.w t)
