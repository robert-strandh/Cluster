(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic PUSH

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Onto the stack, push the contents of a GPR or a memory location
;;; (16/32/64) (source).
;;;
;;; Opcodes: FF
;;; Opcode extension: 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Onto the stack, push the contents of a 16-bit GPR or a memory
;;; location (source).

;;; Onto the stack, push the contents of a 16-bit GPR (source).
(define-instruction "PUSH"
  :modes (32 64)
  :operands ((gpr 16))
  :opcodes (#xFF)
  :opcode-extension 6
  :encoding (modrm)
  :operand-size-override t)

;;; Onto the stack, push the contents of a 16-bit memory location
;;; (source).
(define-instruction "PUSH"
  :modes (32 64)
  :operands ((memory 16))
  :opcodes (#xFF)
  :opcode-extension 6
  :encoding (modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Onto the stack, push the contents of a 32-bit GPR or a memory
;;; location (source).

;;; Onto the stack, push the contents of a 32-bit GPR (source).
(define-instruction "PUSH"
  :modes (32)
  :operands ((gpr 32))
  :opcodes (#xFF)
  :opcode-extension 6
  :encoding (modrm))

;;; Onto the stack, push the contents of a 32-bit memory location
;;; (source).
(define-instruction "PUSH"
  :modes (32)
  :operands ((memory 32))
  :opcodes (#xFF)
  :opcode-extension 6
  :encoding (modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Onto the stack, push the contents of a 64-bit GPR or a memory
;;; location (source).

;;; Onto the stack, push the contents of a 64-bit GPR (source).
(define-instruction "PUSH"
  :modes (64)
  :operands ((gpr 64))
  :opcodes (#xFF)
  :opcode-extension 6
  :encoding (modrm))

;;; Onto the stack, push the contents of a 64-bit memory location
;;; (source).
(define-instruction "PUSH"
  :modes (64)
  :operands ((memory 64))
  :opcodes (#xFF)
  :opcode-extension 6
  :encoding (modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Onto the stack, push the contents of a GPR (16/32/64) (source).
;;;
;;; Opcodes: 50

;;; Onto the stack, push the contents of a 16-bit GPR (source).
(define-instruction "PUSH"
  :modes (32 64)
  :operands ((gpr 16))
  :opcodes (#x50)
  :encoding (+r)
  :operand-size-override t)

;;; Onto the stack, push the contents of a 32-bit GPR (source).
(define-instruction "PUSH"
  :modes (32)
  :operands ((gpr 32))
  :opcodes (#x50)
  :encoding (+r))

;;; Onto the stack, push the contents of a 64-bit GPR (source).
(define-instruction "PUSH"
  :modes (64)
  :operands ((gpr 64))
  :opcodes (#x50)
  :encoding (+r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Onto the stack, push a sign-extended 8-bit immediate value
;;; (source).
;;;
;;; Opcodes: 6A

;;; Onto the stack, push an 8-bit immediate value, sign extended to 16
;;; bits (source).
(define-instruction "PUSH"
  :modes (32 64)
  :operands ((imm 8))
  :opcodes (#x6A)
  :encoding (imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Onto the stack, push an immediate value (16/32/64) (source).
;;;
;;; Opcodes: 68

;;; Onto the stack, push a 16-bit immediate value (source).
(define-instruction "PUSH"
  :modes (32 64)
  :operands ((imm 16))
  :opcodes (#x68)
  :encoding (imm)
  :operand-size-override t)

;;; Onto the stack, push a 32-bit immediate value sign-extened to 64
;;; bits (source).
(define-instruction "PUSH"
  :modes (32 64)
  :operands ((imm 32))
  :opcodes (#x68)
  :encoding (imm))
