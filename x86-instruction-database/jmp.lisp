(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic JMP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Short jump with the target specified by an 8-bit signed
;;; displacement.
;;;
;;; Opcodes: EB

(define-instruction "JMP"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#xEB)
  :encoding (label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near jump with the target specified by a 16-bit or 32-bit signed
;;; displacement.
;;;
;;; Opcodes: E9

(define-instruction "JMP"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#xE9)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JMP"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#xE9)
  :encoding (label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near jump with the target specified by a 16-bit, 32-bit, or 64-bit
;;; address in GPR or memory.
;;;
;;; Opcodes: FF

(define-instruction "JMP"
  :modes (32 64)
  :operands ((gpr 16))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm)
  :operand-size-override t)

(define-instruction "JMP"
  :modes (32 64)
  :operands ((memory 16))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm)
  :operand-size-override t)

(define-instruction "JMP"
  :modes (32)
  :operands ((gpr 32))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm))

(define-instruction "JMP"
  :modes (32)
  :operands ((memory 32))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm))

(define-instruction "JMP"
  :modes (64)
  :operands ((gpr 64))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm))

(define-instruction "JMP"
  :modes (64)
  :operands ((memory 64))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm))
