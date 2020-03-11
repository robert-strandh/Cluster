(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic POP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Into a GPR or memory location (16/32/64) (destination), pop
;;; the top of the stack.
;;;
;;; Opcodes: 8F
;;; Opcode extension: 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Into a 16-bit GPR or memory location (destination), pop the top of
;;; the stack.

;;; Into a 16-bit GPR (destination), pop the top of the stack.
(define-instruction "POP"
  :modes (32 64)
  :operands ((gpr 16))
  :opcodes (#x8F)
  :opcode-extension 0
  :encoding (modrm)
  :operand-size-override t)

;;; Into a 16-bit memory location (destination), pop the top of the
;;; stack.
(define-instruction "POP"
  :modes (32 64)
  :operands ((memory 16))
  :opcodes (#x8F)
  :opcode-extension 0
  :encoding (modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Into a 32-bit GPR or memory location (destination), pop the top of
;;; the stack.

;;; Into a 32-bit GPR (destination), pop the top of the stack.
(define-instruction "POP"
  :modes (32)
  :operands ((gpr 32))
  :opcodes (#x8F)
  :opcode-extension 0
  :encoding (modrm))

;;; Into a 32-bit memory location (destination), pop the top of the
;;; stack.
(define-instruction "POP"
  :modes (32)
  :operands ((memory 32))
  :opcodes (#x8F)
  :opcode-extension 0
  :encoding (modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Into a 64-bit GPR or memory location (destination), pop the top of
;;; the stack.

;;; Into a 64-bit GPR (destination), pop the top of the stack.
(define-instruction "POP"
  :modes (64)
  :operands ((gpr 64))
  :opcodes (#x8F)
  :opcode-extension 0
  :encoding (modrm))

;;; Into a 64-bit memory location (destination), pop the top of the
;;; stack.
(define-instruction "POP"
  :modes (64)
  :operands ((memory 64))
  :opcodes (#x8F)
  :opcode-extension 0
  :encoding (modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Into a GPR (16/32/64) (destination), pop the top of the stack.
;;;
;;; Opcodes: 58

;;; Into a 16-bit GPR (destination), pop the top of the stack.
(define-instruction "POP"
  :modes (32 64)
  :operands ((gpr 16))
  :opcodes (#x58)
  :encoding (+r)
  :operand-size-override t)

;;; Into a 32-bit GPR (destination), pop the top of the stack.
(define-instruction "POP"
  :modes (32)
  :operands ((gpr 32))
  :opcodes (#x58)
  :encoding (+r))

;;; Into a 64-bit GPR (destination), pop the top of the stack.
(define-instruction "POP"
  :modes (64)
  :operands ((gpr 64))
  :opcodes (#x58)
  :encoding (+r))
