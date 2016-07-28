(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic SUB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From the contents of GPR A (destination), subtract an 8-bit
;;; immediate value (source), and store the result in GPR A.
;;; 
;;; Opcodes: 2C

(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr-a 8) (imm 8))
  :opcodes (#x2C)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From GPR A (16/32/64) (destination), subtract an immediate value
;;; (16/32) (source), and store the result in GPR A.
;;; 
;;; Opcodes: 2D

;;; From GPR AX (destination), subtract a 16-bit immediate value
;;; (source), and store the result in GPR AX.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr-a 16) (imm 16))
  :opcodes (#x2D)
  :encoding (- imm)
  :operand-size-override t)

;;; From GPR EAX (destination), subtract a 16-bit immediate value
;;; (source), and store the result in GPR EAX.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr-a 32) (imm 32))
  :opcodes (#x2D)
  :encoding (- imm))

;;; From GPR RAX (destination), subtract a 32-bit sign-extended
;;; immediate value (source), and store the result in GPR RAX.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr-a 64) (simm 32))
  :opcodes (#x2D)
  :encoding (- imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From an 8-bit GPR or memory location (destination), subtract
;;; an 8-bit immediate value (source), and store the result in the
;;; destination.
;;; 
;;; Opcodes: 80
;;; Opcode extension: 5

;;; From an 8-bit GPR (destination), subtract an 8-bit immediate value
;;; (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 5
  :encoding (modrm imm))

;;; From an 8-bit memory location (destination), subtract an 8-bit
;;; immediate value (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 5
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a GPR or memory location (16/32/64) (destination),
;;; subtract an immediate value (16/32) (source) and store the result
;;; in the destination.
;;;
;;; Opcodes: 81
;;; Opcode extension 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 16-bit GPR or memory location (destination), subtract
;;; an immediate 16-bit value (source) and store the result in the
;;; destination.

;;; From a 16-bit GPR (destination), subtract an immediate 16-bit
;;; value (source) and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm)
  :operand-size-override t)

;;; From a 16-bit memory location (destination), subtract an immediate
;;; 16-bit value (source) and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 32-bit GPR or memory location (destination), subtract
;;; an immediate 32-bit value (source) and store the result in the
;;; destination.

;;; From a 32-bit GPR (destination), subtract an immediate 32-bit
;;; value (source) and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm))

;;; From a 32-bit memory location (destination), subtract an immediate
;;; 32-bit value (source) and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 64-bit GPR or memory location (destination), subtract
;;; an immediate sign-extended 32-bit value (source) and store the
;;; result in the destination.

;;; From a 64-bit GPR (destination), subtract an immediate
;;; sign-extended 32-bit value (source) and store the result in the
;;; destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm)
  :rex.w t)

;;; From a 64-bit memory location (destination), subtract an immediate
;;; sign-extended 32-bit value (source) and store the result in the
;;; destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((memory 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a GPR or memory location (16/32/64) (destination),
;;; subtract an 8-bit sign-extended immediate value (source), and
;;; store the result in the destination.
;;;
;;; Opcodes: 83
;;; Opcode extension: 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 16-bit GPR or memory location (destination), subtract
;;; an 8-bit sign-extended immediate value (source), and store the
;;; result in the destination.

;;; From a 16-bit GPR (destination), subtract an 8-bit sign-extended
;;; immediate value (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm)
  :operand-size-override t)

;;; From a 16-bit memory location (destination), subtract an 8-bit
;;; sign-extended immediate value (source), and store the result in
;;; the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 32-bit GPR or memory location (destination), subtract
;;; an 8-bit sign-extended immediate value (source), and store the
;;; result in the destination.

;;; From a 32-bit GPR (destination), subtract an 8-bit sign-extended
;;; immediate value (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm))

;;; From a 32-bit memory location (destination), subtract an 8-bit
;;; sign-extended immediate value (source), and store the result in
;;; the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 64-bit GPR or memory location (destination), subtract
;;; an 8-bit sign-extended immediate value (source), and store the
;;; result in the destination.

;;; From a 64-bit GPR (destination), subtract an 8-bit sign-extended
;;; immediate value (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm)
  :rex.w t)

;;; From a 64-bit memory location (destination), subtract an 8-bit
;;; sign-extended immediate value (source), and store the result in
;;; the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((memory 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From an 8-bit GPR or memory location (destination), subtract
;;; the contents of an 8-bit GPR (source) and store the result in
;;; the destination.
;;;
;;; Opcodes: 28

;;; From an 8-bit GPR (destination), subtract the contents of an 8-bit
;;; GPR (source) and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x28)
  :encoding (modrm reg))

;;; From an 8-bit memory location (destination), subtract the contents
;;; of an 8-bit GPR (source) and store the result in the
;;; destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x28)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a GPR or memory location (16/32/64) (destination), subtract
;;; the contents of a GPR (16/32/64) (source), and store the result in
;;; the destination.
;;;
;;; Opcodes: 29

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 16-bit GPR or memory location (destination), subtract the
;;; contents of a 16-bit GPR (source), and store the result in the
;;; destination.

;;; From a 16-bit GPR (destination), subtract the contents of a 16-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x29)
  :encoding (modrm reg)
  :operand-size-override t)

;;; From a 16-bit memory location (destination), subtract the contents
;;; of a 16-bit GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x29)
  :encoding (modrm reg)
  :lock t
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 32-bit GPR or memory location (destination), subtract the
;;; contents of a 32-bit GPR (source), and store the result in the
;;; destination.

;;; From a 32-bit GPR (destination), subtract the contents of a 32-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x29)
  :encoding (modrm reg))

;;; From a 32-bit memory location (destination), subtract the contents
;;; of a 32-bit GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x29)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 64-bit GPR or memory location (destination), subtract the
;;; contents of a 64-bit GPR (source), and store the result in the
;;; destination.

;;; From a 64-bit GPR (destination), subtract the contents of a 64-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x29)
  :encoding (modrm reg)
  :rex.w t)

;;; From a 64-bit memory location (destination), subtract the contents
;;; of a 64-bit GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x29)
  :encoding (modrm reg)
  :lock t
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From an 8-bit GPR (destination), subtract the contents of an 8-bit
;;; GPR or memory location (source), and store the result in the
;;; destination.
;;;
;;; Opcodes: 2A

;;; From an 8-bit GPR (destination), subtract the contents of an 8-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x2A)
  :encoding (reg modrm))

;;; From an 8-bit GPR (destination), subtract the contents of an 8-bit
;;; memory location (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 8) (memory 8))
  :opcodes (#x2A)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a GPR (16/32/64) (destination), subtract the contents of a
;;; GPR or memory location (16/32/64) (source), and store the result
;;; in the destination.
;;;
;;; Opcodes: 2B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 16-bit GPR (destination), subtract the contents of a 16-bit
;;; GPR or memory location (source), and store the result in the
;;; destination.

;;; From a 16-bit GPR (destination), subtract the contents of a 16-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x2B)
  :encoding (reg modrm)
  :operand-size-override t)

;;; From a 16-bit GPR (destination), subtract the contents of a 16-bit
;;; memory location (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x2B)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 32-bit GPR (destination), subtract the contents of a 32-bit
;;; GPR or memory location (source), and store the result in the
;;; destination.

;;; From a 32-bit GPR (destination), subtract the contents of a 32-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x2B)
  :encoding (reg modrm))

;;; From a 32-bit GPR (destination), subtract the contents of a 32-bit
;;; memory location (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x2B)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 64-bit GPR (destination), subtract the contents of a 64-bit
;;; GPR or memory location (source), and store the result in the
;;; destination.

;;; From a 64-bit GPR (destination), subtract the contents of a 64-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x2B)
  :encoding (reg modrm)
  :rex.w t)

;;; From a 64-bit GPR (destination), subtract the contents of a 64-bit
;;; memory location (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x2B)
  :encoding (reg modrm)
  :rex.w t)
