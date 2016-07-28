(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opcodes: C3

(define-instruction "RET"
  :modes (32 64)
  :operands ()
  :opcodes (#xC3)
  :encoding ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opcodes: C2

(define-instruction "RET"
  :modes (32 64)
  :operands ((imm 16))
  :opcodes (#xC2)
  :encoding (imm))
