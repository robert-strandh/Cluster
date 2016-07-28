(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonics CMP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit immediate value (source) with the contents of the
;;; AL GPR (destination)

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr-a 8) (imm 8))
  :opcodes (#x3C)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 16-bit immediate value (source) with the contents of the
;;; AX GPR (destination)

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr-a 16) (imm 16))
  :opcodes (#x3D)
  :encoding (- imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit immediate value (source) with the contents of the
;;; EAX GPR (destination)

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr-a 32) (imm 32))
  :opcodes (#x3D)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit immediate value (source) with the contents of the
;;; RAX GPR (destination)

(define-instruction "CMP"
  :modes (64)
  :operands ((gpr-a 64) (imm 32))
  :opcodes (#x3D)
  :encoding (- imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit immediate value (source) with the contents of an
;;; 8-bit GPR (destination)

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit immediate value (source) with the contents of an
;;; 8-bit memory location (destination)

(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 16-bit immediate value (source) with the contents of a
;;; 16-bit GPR (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 7
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit immediate value (source) with the contents of a
;;; 32-bit GPR (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit immediate value (source) with the contents of a
;;; 32-bit memory location (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit signed immediate value (source) with the contents
;;; of a 64-bit GPR (destination).

(define-instruction "CMP"
  :modes (64)
  :operands ((gpr 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 7
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit signed immediate value (source) with the contents
;;; of a 64-bit memory location (destination).

(define-instruction "CMP"
  :modes (64)
  :operands ((memory 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 7
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 16-bit GPR (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 16-bit memory location (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 32-bit GPR (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 32-bit memory location (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 64-bit GPR (destination).

(define-instruction "CMP"
  :modes (64)
  :operands ((gpr 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 64-bit memory location (destination).

(define-instruction "CMP"
  :modes (64)
  :operands ((memory 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of an 8-bit GPR or memory location
;;; (destination) with the contents of an 8-bit GPR (source).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of an 8-bit GPR (destination) with the
;;; contents of an 8-bit GPR (source).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x38)
  :encoding (modrm reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of an 8-bit memory location (destination)
;;; with the contents of an 8-bit GPR (source).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x38)
  :encoding (modrm reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a GPR or memory location
;;; (destination) with the contents of a GPR (source).
;;;
;;; Opcodes: 39

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 16-bit GPR or memory location
;;; (destination) with the contents of a 16-bit GPR (source).

;;; Compare the contents of a 16-bit GPR (destination) with the
;;; contents of a 16-bit GPR (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x39)
  :encoding (modrm reg)
  :operand-size-override t)

;;; Compare the contents of a 16-bit memory location (destination)
;;; with the contents of a 16-bit GPR (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x39)
  :encoding (modrm reg)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 32-bit GPR or memory location
;;; (destination) with the contents of a 32-bit GPR (source).

;;; Compare the contents of a 32-bit GPR (destination) with the
;;; contents of a 32-bit GPR (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x39)
  :encoding (modrm reg))

;;; Compare the contents of a 32-bit memory location (destination)
;;; with the contents of a 32-bit GPR (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x39)
  :encoding (modrm reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 64-bit GPR or memory location
;;; (destination) with the contents of a 64-bit GPR (source).

;;; Compare the contents of a 64-bit GPR (destination) with the
;;; contents of a 64-bit GPR (source).
(define-instruction "CMP"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x39)
  :encoding (modrm reg)
  :rex.w t)

;;; Compare the contents of a 64-bit memory location (destination)
;;; with the contents of a 64-bit GPR (source).
(define-instruction "CMP"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x39)
  :encoding (modrm reg)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a GPR (destination) with the contents
;;; of a GPR or memory location (source).
;;;
;;; Opcodes: 3B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 16-bit GPR (destination) with the
;;; contents of a 16-bit GPR or memory location (source).

;;; Compare the contents of a 16-bit GPR (destination) with the
;;; contents of a 16-bit GPR (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x3B)
  :encoding (reg modrm)
  :operand-size-override t)

;;; Compare the contents of a 16-bit GPR (destination) with the
;;; contents of a 16-bit memory location (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x3B)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 32-bit GPR (destination) with the
;;; contents of a 32-bit GPR or memory location (source).

;;; Compare the contents of a 32-bit GPR (destination) with the
;;; contents of a 32-bit GPR (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x3B)
  :encoding (reg modrm))

;;; Compare the contents of a 32-bit GPR (destination) with the
;;; contents of a 32-bit memory location (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x3B)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 64-bit GPR (destination) with the
;;; contents of a 64-bit GPR or memory location (source).

;;; Compare the contents of a 64-bit GPR (destination) with the
;;; contents of a 64-bit GPR (source).
(define-instruction "CMP"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x3B)
  :encoding (reg modrm)
  :rex.w t)

;;; Compare the contents of a 64-bit GPR (destination) with the
;;; contents of a 64-bit memory location (source).
(define-instruction "CMP"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x3B)
  :encoding (reg modrm)
  :rex.w t)
