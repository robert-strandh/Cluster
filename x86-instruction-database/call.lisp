(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic CALL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near call with target specified by a relative displacement.
;;;
;;; Opcodes : E8

;;; Near call with target specified by a 16-bit relative displacement.
(define-instruction "CALL"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#xE8)
  :encoding (label)
  :operand-size-override t)

;;; Near call with target specified by a 32-bit relative displacement.
(define-instruction "CALL"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#xE8)
  :encoding (label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near call with target specified by a GPR or a value in
;;; memory.
;;;
;;; Opcodes : FF
;;; Opcode extension: 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near call with target specified by a 16-bit GPR or a 16-bit
;;; value in memory.

;;; Near call with target specified by a 16-bit GPR.
(define-instruction "CALL"
  :modes (32 64)
  :operands ((gpr 16))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm)
  :operand-size-override t)

;;; Near call with target specified by a 16-bit memory value.
(define-instruction "CALL"
  :modes (32 64)
  :operands ((memory 16))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near call with target specified by a 32-bit GPR or a 32-bit
;;; value in memory.

;;; Near call with target specified by a 32-bit GPR.
(define-instruction "CALL"
  :modes (32)
  :operands ((gpr 32))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm))

;;; Near call with target specified by a 32-bit memory value.
(define-instruction "CALL"
  :modes (32)
  :operands ((memory 32))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near call with target specified by a 64-bit GPR or a 64-bit
;;; value in memory.

;;; Near call with target specified by a 64-bit GPR.
(define-instruction "CALL"
  :modes (64)
  :operands ((gpr 64))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm))

;;; Near call with target specified by a 64-bit memory value.
(define-instruction "CALL"
  :modes (64)
  :operands ((memory 64))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm))
