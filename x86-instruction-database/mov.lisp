(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic MOV

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To an 8-bit GPR or memory location (destination), move the
;;; contents of an 8-bit GPR (source).
;;;
;;; Opcodes: 88

;;; To an 8-bit GPR (destination), move the contents of an 8-bit
;;; GPR (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x88)
  :encoding (modrm reg))

;;; To an 8-bit memory location (destination), move the contents of an
;;; 8-bit GPR (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x88)
  :encoding (modrm reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a GPR or memory location (destination), move the
;;; contents of a GPR (source).
;;;
;;; Opcodes: 89

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 16-bit GPR or memory location (destination), move the
;;; contents of a 16-bit GPR (source).

;;; To a 16-bit GPR (destination), move the contents of a 16-bit
;;; GPR (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x89)
  :encoding (modrm reg)
  :operand-size-override t)

;;; To a 16-bit memory location (destination), move the contents of a
;;; 16-bit GPR (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x89)
  :encoding (modrm reg)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 32-bit GPR or memory location (destination), move the
;;; contents of a 32-bit GPR (source).

;;; To a 32-bit GPR (destination), move the contents of a 32-bit
;;; GPR (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x89)
  :encoding (modrm reg))

;;; To a 32-bit memory location (destination), move the contents of a
;;; 32-bit GPR (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x89)
  :encoding (modrm reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 64-bit GPR or memory location (destination), move the
;;; contents of a 64-bit GPR (source).

;;; To a 64-bit GPR (destination), move the contents of a 64-bit
;;; GPR (source).
(define-instruction "MOV"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x89)
  :encoding (modrm reg)
  :rex.w t)

;;; To a 64-bit memory location (destination), move the contents of a
;;; 64-bit GPR (source).
(define-instruction "MOV"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x89)
  :encoding (modrm reg)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; To an 8-bit GPR (destination), move the contents of an 8-bit
;;; GPR or memory location.
;;;
;;; Opcodes: 8A

;;; To an 8-bit GPR (destination), move the contents of an 8-bit
;;; GPR.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x8A)
  :encoding (reg modrm))

;;; To an 8-bit GPR (destination), move the contents of an 8-bit
;;; memory location.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 8) (memory 8))
  :opcodes (#x8A)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a GPR (destination), move the contents of a GPR or
;;; memory location.
;;;
;;; Opcodes: 8B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 16-bit GPR (destination), move the contents of a 16-bit
;;; GPR or memory location.

;;; To a 16-bit GPR (destination), move the contents of a 16-bit
;;; GPR.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x8B)
  :encoding (reg modrm)
  :operand-size-override t)

;;; To a 16-bit GPR (destination), move the contents of a 16-bit
;;; memory location.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x8B)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 32-bit GPR (destination), move the contents of a 32-bit
;;; GPR or memory location.

;;; To a 32-bit GPR (destination), move the contents of a 32-bit
;;; GPR.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x8B)
  :encoding (reg modrm))

;;; To a 32-bit GPR (destination), move the contents of a 32-bit
;;; memory location.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x8B)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 64-bit GPR (destination), move the contents of a 64-bit
;;; GPR or memory location.

;;; To a 64-bit GPR (destination), move the contents of a 64-bit
;;; GPR.
(define-instruction "MOV"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x8B)
  :encoding (reg modrm)
  :rex.w t)

;;; To a 64-bit GPR (destination), move the contents of a 64-bit
;;; memory location.
(define-instruction "MOV"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x8B)
  :encoding (reg modrm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To an 8-bit GPR (destination) move an 8-bit immediate value.
;;;
;;; Opcodes: B0, B1, ..., B7

(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#xB0)
  :encoding (+r imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a GPR (destination) move an immediate value.
;;;
;;; Opcodes: B8, B9, ..., BF

;;; To a 16-bit GPR (destination) move a 16-bit immediate value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#xB8)
  :encoding (+r imm)
  :operand-size-override t)

;;; To a 32-bit GPR (destination) move a 32-bit immediate value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#xB8)
  :encoding (+r imm))

;;; To a 64-bit GPR (destination) move a 64-bit immediate value.
(define-instruction "MOV"
  :modes (64)
  :operands ((gpr 64) (imm 64))
  :opcodes (#xB8)
  :encoding (+r imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To an 8-bit GPR or memory location, move an 8-bit immediate
;;; value.
;;;
;;; Opcodes: C6

;;; To an 8-bit GPR, move an 8-bit immediate value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#xC6)
  :opcode-extension 0
  :encoding (modrm imm))

;;; To an 8-bit memory location, move an 80bit immediate value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#xC6)
  :opcode-extension 0
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a GPR or memory location, move an immediate value.
;;;
;;; Opcodes: C7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 16-bit GPR or memory location, move a 16-bit immediate
;;; value.

;;; To a 16-bit GPR, move a 16-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm)
  :operand-size-override t)

;;; To a 16-bit memory location, move a 16-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 16) (imm 16))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 32-bit GPR or memory location, move a 32-bit immediate
;;; value.

;;; To a 32-bit GPR, move a 32-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm))

;;; To a 32-bit memory location, move a 32-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 64-bit GPR or memory location, move a 64-bit immediate
;;; value.

;;; To a 64-bit GPR, move a 64-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (64)
  :operands ((gpr 64) (imm 64))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm)
  :rex.w t)

;;; To a 64-bit memory location, move a 64-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (64)
  :operands ((memory 64) (imm 64))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm)
  :rex.w t)
