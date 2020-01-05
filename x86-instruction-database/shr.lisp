(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic SHR

;;; Logically shift a 64-bit GPR (destination) right a number of
;;; positions indicated by an 8-bit immediate value (source).
(define-instruction "SHR"
  :modes (64)
  :operands ((gpr 64) (imm 8))
  :opcodes (#xC1)
  :opcode-extension 5
  :encoding (modrm imm)
  :rex.w t)
