(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic OR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of AL (destination) with an immediate 8-bit value
;;; (source) and store the result in AL.
;;;
;;; Opcodes: OC

(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr-a 8) (imm 8))
  :opcodes (#x0C)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of GPR A (16/32/64) (destination) with an
;;; immediate value (16/32) (source), and store the result in the
;;; destination.
;;;
;;; Opcodes: 0D

;;; OR the contents of GPR AX (destination) with an immediate 16-bit
;;; value (source) and store the result in GPR AX.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr-a 16) (imm 16))
  :opcodes (#x0D)
  :encoding (- imm)
  :operand-size-override t)

;;; OR the contents of GPR EAX (destination) with an immediate 32-bit
;;; value (source), and store the result in GPR EAX.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr-a 32) (imm 32))
  :opcodes (#x0D)
  :encoding (- imm))

;;; OR the contents of GPR RAX (destination) with a sign-extended
;;; 32-bit immediate value (source) and store the result in GPR RAX.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr-a 64) (imm 32))
  :opcodes (#x0D)
  :encoding (- imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of an 8-bit GPR or memory location (destination)
;;; with an immediate 8-bit value (source), and store the result in
;;; the destination.
;;;
;;; Opcodes: 80
;;; Opcode extension: 1

;;; OR the contents of an 8-bit GPR (destination) with an immediate
;;; 8-bit value (source), and store the result in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 1
  :encoding (modrm imm))

;;; OR the contents of an 8-bit memory location (destination) with an
;;; immediate 8-bit value (source), and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 1
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a GPR or a memory location (16/32/64)
;;; (destination) with an immediate value (16/32) (source), and store
;;; the result in the destination.
;;;
;;; Opcodes: 81
;;; Opcode extension 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 16-bit GPR or a memory location
;;; (destination) with an immediate 16-bit value (source), and store
;;; the result in the destination.

;;; OR the contents of a 16-bit GPR (destination) with an immediate
;;; 16-bit value (source), and store the result in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm)
  :operand-size-override t)

;;; OR the contents of a 16-bit a memory location (destination) with
;;; an immediate 16-bit value (source), and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 32-bit GPR or a memory location
;;; (destination) with an immediate 32-bit value (source), and store
;;; the result in the destination.

;;; OR the contents of a 32-bit GPR (destination) with an immediate
;;; 32-bit value (source), and store the result in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm))

;;; OR the contents of a 32-bit a memory location (destination) with
;;; an immediate 32-bit value (source), and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 64-bit GPR or a memory location
;;; (destination) with an immediate 32-bit sign-extended value
;;; (source), and store the result in the destination.

;;; OR the contents of a 64-bit GPR (destination) with an immediate
;;; 32-bit sign-extended value (source), and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm)
  :rex.w t)

;;; OR the contents of a 64-bit a memory location (destination) with
;;; an immediate 32-bit sign-extended value (source), and store the
;;; result in the destination.
(define-instruction "OR"
  :modes (64)
  :operands ((memory 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a GPR or memory location (16/32/64)
;;; (destination), with a an immediate 8-bit sign-extended value
;;; (source), and store the result in the destination.
;;;
;;; Opcodes: 83
;;; Opcode extension: 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 16-bit GPR or memory location (destination),
;;; with a an immediate 8-bit sign-extended value (source), and store
;;; the result in the destination.

;;; OR the contents of a 16-bit GPR (destination), with a an
;;; immediate 8-bit sign-extended value (source), and store the result
;;; in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm)
  :operand-size-override t)

;;; OR the contents of a 16-bit memory location (destination), with a
;;; an immediate 8-bit sign-extended value (source), and store the
;;; result in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 32-bit GPR or memory location (destination),
;;; with a an immediate 8-bit sign-extended value (source), and store
;;; the result in the destination.

;;; OR the contents of a 32-bit GPR (destination), with a an
;;; immediate 8-bit sign-extended value (source), and store the result
;;; in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm))

;;; OR the contents of a 32-bit memory location (destination), with a
;;; an immediate 8-bit sign-extended value (source), and store the
;;; result in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 64-bit GPR or memory location (destination),
;;; with a an immediate 8-bit sign-extended value (source), and store
;;; the result in the destination.

;;; OR the contents of a 64-bit GPR (destination), with a an
;;; immediate 8-bit sign-extended value (source), and store the result
;;; in the destination.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm)
  :rex.w t)

;;; OR the contents of a 64-bit memory location (destination), with a
;;; an immediate 8-bit sign-extended value (source), and store the
;;; result in the destination.
(define-instruction "OR"
  :modes (64)
  :operands ((memory 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of an 8-bit GPR or memory location (destination)
;;; with the contents of an 8-bit GPR (source) and store the result in
;;; the destination.
;;;
;;; Opcodes: 08

;;; OR the contents of an 8-bit GPR (destination) with the contents
;;; of an 8-bit GPR (source) and store the result in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x08)
  :encoding (modrm reg))

;;; OR the contents of an 8-bit memory location (destination) with
;;; the contents of an 8-bit GPR (source) and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x08)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a GPR or memory location (16/32/64)
;;; (destination), with the contents of a GPR (16/32/64) (source), and
;;; store the result in the destination.
;;; 
;;; Opcodes: 09

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 16-bit GPR or memory location (destination),
;;; with the contents of a 16-bit GPR (source), and store the result
;;; in the destination.

;;; OR the contents of a 16-bit GPR (destination), with the contents
;;; of a 16-bit GPR (source), and store the result in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x09)
  :encoding (modrm reg)
  :operand-size-override t)

;;; OR the contents of a 16-bit memory location (destination), with
;;; the contents of a 16-bit GPR (source), and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x09)
  :encoding (modrm reg)
  :lock t
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 32-bit GPR or memory location (destination),
;;; with the contents of a 32-bit GPR (source), and store the result
;;; in the destination.

;;; OR the contents of a 32-bit GPR (destination), with the contents
;;; of a 32-bit GPR (source), and store the result in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x09)
  :encoding (modrm reg))

;;; OR the contents of a 32-bit memory location (destination), with
;;; the contents of a 32-bit GPR (source), and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x09)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 64-bit GPR or memory location (destination),
;;; with the contents of a 64-bit GPR (source), and store the result
;;; in the destination.

;;; OR the contents of a 64-bit GPR (destination), with the contents
;;; of a 64-bit GPR (source), and store the result in the destination.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x09)
  :encoding (modrm reg)
  :rex.w t)

;;; OR the contents of a 64-bit memory location (destination), with
;;; the contents of a 64-bit GPR (source), and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x09)
  :encoding (modrm reg)
  :lock t
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of an 8-bit GPR (destination) with the contents
;;; of an 8-bit GPR or memory location (source), and store the result
;;; in the destination.
;;;
;;; Opcodes: 0A

;;; OR the contents of an 8-bit GPR (destination) with the contents
;;; of an 8-bit GPR (source), and store the result in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x0A)
  :encoding (reg modrm))

;;; OR the contents of an 8-bit GPR (destination) with the contents
;;; of an 8-bit memory location (source), and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 8) (memory 8))
  :opcodes (#x0A)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a GPR (16/32/64) (destination), with the
;;; contents of a GPR or memory location (16/32/64) (source), and
;;; store the result in the destination.
;;;
;;; Opcodes: 0B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 16-bit GPR (destination), with the contents
;;; of a 16-bit GPR or memory location (source), and store the result
;;; in the destination.

;;; OR the contents of a 16-bit GPR (destination), with the contents
;;; of a 16-bit GPR (source), and store the result in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x0B)
  :encoding (reg modrm)
  :operand-size-override t)

;;; OR the contents of a 16-bit GPR (destination), with the contents
;;; of a 16-bit memory location (source), and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x0B)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 32-bit GPR (destination), with the contents
;;; of a 32-bit GPR or memory location (source), and store the result
;;; in the destination.

;;; OR the contents of a 32-bit GPR (destination), with the contents
;;; of a 32-bit GPR (source), and store the result in the destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x0B)
  :encoding (reg modrm))

;;; OR the contents of a 32-bit GPR (destination), with the contents
;;; of a 32-bit memory location (source), and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x0B)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 64-bit GPR (destination), with the contents
;;; of a 64-bit GPR or memory location (source), and store the result
;;; in the destination.

;;; OR the contents of a 64-bit GPR (destination), with the contents
;;; of a 64-bit GPR (source), and store the result in the destination.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x0B)
  :encoding (reg modrm)
  :rex.w t)

;;; OR the contents of a 64-bit GPR (destination), with the contents
;;; of a 64-bit memory location (source), and store the result in the
;;; destination.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x0B)
  :encoding (reg modrm)
  :rex.w t)
