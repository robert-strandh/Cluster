(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic SAR

;;; Arithmetically shift a 64-bit GPR (destination) right a number of
;;; positions indicated by an 8-bit immediate value (source).
(define-instruction "SAR"
  :modes (64)
  :operands ((gpr 64) (imm 8))
  :opcodes (#xC1)
  :opcode-extension 7
  :encoding (modrm imm)
  :rex.w t)
