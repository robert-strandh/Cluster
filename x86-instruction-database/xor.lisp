(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic XOR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of AL (destination) with an immediate 8-bit value
;;; (source) and store the result in AL.
;;;
;;; Opcodes: 34

(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr-a 8) (imm 8))
  :opcodes (#x34)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of GPR A (16/32/64) (destination) with an
;;; immediate value (16/32) (source), and store the result in the
;;; destination.
;;;
;;; Opcodes: 35

;;; XOR the contents of GPR AX (destination) with an immediate 16-bit
;;; value (source) and store the result in GPR AX.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr-a 16) (imm 16))
  :opcodes (#x35)
  :encoding (- imm)
  :operand-size-override t)

;;; XOR the contents of GPR EAX (destination) with an immediate 32-bit
;;; value (source), and store the result in GPR EAX.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr-a 32) (imm 32))
  :opcodes (#x35)
  :encoding (- imm))

;;; XOR the contents of GPR RAX (destination) with a sign-extended
;;; 32-bit immediate value (source) and store the result in GPR RAX.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr-a 64) (imm 32))
  :opcodes (#x35)
  :encoding (- imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of an 8-bit GPR or memory location (destination)
;;; with an immediate 8-bit value (source), and store the result in
;;; the destination.
;;;
;;; Opcodes: 80
;;; Opcode extension: 6

;;; XOR the contents of an 8-bit GPR (destination) with an immediate
;;; 8-bit value (source), and store the result in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 6
  :encoding (modrm imm))

;;; XOR the contents of an 8-bit memory location (destination) with an
;;; immediate 8-bit value (source), and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 6
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a GPR or a memory location (16/32/64)
;;; (destination) with an immediate value (16/32) (source), and store
;;; the result in the destination.
;;;
;;; Opcodes: 81
;;; Opcode extension 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 16-bit GPR or a memory location
;;; (destination) with an immediate 16-bit value (source), and store
;;; the result in the destination.

;;; XOR the contents of a 16-bit GPR (destination) with an immediate
;;; 16-bit value (source), and store the result in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm)
  :operand-size-override t)

;;; XOR the contents of a 16-bit a memory location (destination) with
;;; an immediate 16-bit value (source), and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 32-bit GPR or a memory location
;;; (destination) with an immediate 32-bit value (source), and store
;;; the result in the destination.

;;; XOR the contents of a 32-bit GPR (destination) with an immediate
;;; 32-bit value (source), and store the result in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm))

;;; XOR the contents of a 32-bit a memory location (destination) with
;;; an immediate 32-bit value (source), and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 64-bit GPR or a memory location
;;; (destination) with an immediate 32-bit sign-extended value
;;; (source), and store the result in the destination.

;;; XOR the contents of a 64-bit GPR (destination) with an immediate
;;; 32-bit sign-extended value (source), and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm)
  :rex.w t)

;;; XOR the contents of a 64-bit a memory location (destination) with
;;; an immediate 32-bit sign-extended value (source), and store the
;;; result in the destination.
(define-instruction "XOR"
  :modes (64)
  :operands ((memory 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a GPR or memory location (16/32/64)
;;; (destination), with a an immediate 8-bit sign-extended value
;;; (source), and store the result in the destination.
;;;
;;; Opcodes: 83
;;; Opcode extension: 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 16-bit GPR or memory location (destination),
;;; with a an immediate 8-bit sign-extended value (source), and store
;;; the result in the destination.

;;; XOR the contents of a 16-bit GPR (destination), with a an
;;; immediate 8-bit sign-extended value (source), and store the result
;;; in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm)
  :operand-size-override t)

;;; XOR the contents of a 16-bit memory location (destination), with a
;;; an immediate 8-bit sign-extended value (source), and store the
;;; result in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 32-bit GPR or memory location (destination),
;;; with a an immediate 8-bit sign-extended value (source), and store
;;; the result in the destination.

;;; XOR the contents of a 32-bit GPR (destination), with a an
;;; immediate 8-bit sign-extended value (source), and store the result
;;; in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm))

;;; XOR the contents of a 32-bit memory location (destination), with a
;;; an immediate 8-bit sign-extended value (source), and store the
;;; result in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 64-bit GPR or memory location (destination),
;;; with a an immediate 8-bit sign-extended value (source), and store
;;; the result in the destination.

;;; XOR the contents of a 64-bit GPR (destination), with a an
;;; immediate 8-bit sign-extended value (source), and store the result
;;; in the destination.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm)
  :rex.w t)

;;; XOR the contents of a 64-bit memory location (destination), with a
;;; an immediate 8-bit sign-extended value (source), and store the
;;; result in the destination.
(define-instruction "XOR"
  :modes (64)
  :operands ((memory 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of an 8-bit GPR or memory location (destination)
;;; with the contents of an 8-bit GPR (source) and store the result in
;;; the destination.
;;;
;;; Opcodes: 30

;;; XOR the contents of an 8-bit GPR (destination) with the contents
;;; of an 8-bit GPR (source) and store the result in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x30)
  :encoding (modrm reg))

;;; XOR the contents of an 8-bit memory location (destination) with
;;; the contents of an 8-bit GPR (source) and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x30)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a GPR or memory location (16/32/64)
;;; (destination), with the contents of a GPR (16/32/64) (source), and
;;; store the result in the destination.
;;; 
;;; Opcodes: 31

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 16-bit GPR or memory location (destination),
;;; with the contents of a 16-bit GPR (source), and store the result
;;; in the destination.

;;; XOR the contents of a 16-bit GPR (destination), with the contents
;;; of a 16-bit GPR (source), and store the result in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x31)
  :encoding (modrm reg)
  :operand-size-override t)

;;; XOR the contents of a 16-bit memory location (destination), with
;;; the contents of a 16-bit GPR (source), and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x31)
  :encoding (modrm reg)
  :lock t
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 32-bit GPR or memory location (destination),
;;; with the contents of a 32-bit GPR (source), and store the result
;;; in the destination.

;;; XOR the contents of a 32-bit GPR (destination), with the contents
;;; of a 32-bit GPR (source), and store the result in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x31)
  :encoding (modrm reg))

;;; XOR the contents of a 32-bit memory location (destination), with
;;; the contents of a 32-bit GPR (source), and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x31)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 64-bit GPR or memory location (destination),
;;; with the contents of a 64-bit GPR (source), and store the result
;;; in the destination.

;;; XOR the contents of a 64-bit GPR (destination), with the contents
;;; of a 64-bit GPR (source), and store the result in the destination.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x31)
  :encoding (modrm reg)
  :rex.w t)

;;; XOR the contents of a 64-bit memory location (destination), with
;;; the contents of a 64-bit GPR (source), and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x31)
  :encoding (modrm reg)
  :lock t
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of an 8-bit GPR (destination) with the contents
;;; of an 8-bit GPR or memory location (source), and store the result
;;; in the destination.
;;;
;;; Opcodes: 32

;;; XOR the contents of an 8-bit GPR (destination) with the contents
;;; of an 8-bit GPR (source), and store the result in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x32)
  :encoding (reg modrm))

;;; XOR the contents of an 8-bit GPR (destination) with the contents
;;; of an 8-bit memory location (source), and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 8) (memory 8))
  :opcodes (#x32)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a GPR (16/32/64) (destination), with the
;;; contents of a GPR or memory location (16/32/64) (source), and
;;; store the result in the destination.
;;;
;;; Opcodes: 33

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 16-bit GPR (destination), with the contents
;;; of a 16-bit GPR or memory location (source), and store the result
;;; in the destination.

;;; XOR the contents of a 16-bit GPR (destination), with the contents
;;; of a 16-bit GPR (source), and store the result in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x33)
  :encoding (reg modrm)
  :operand-size-override t)

;;; XOR the contents of a 16-bit GPR (destination), with the contents
;;; of a 16-bit memory location (source), and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x33)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 32-bit GPR (destination), with the contents
;;; of a 32-bit GPR or memory location (source), and store the result
;;; in the destination.

;;; XOR the contents of a 32-bit GPR (destination), with the contents
;;; of a 32-bit GPR (source), and store the result in the destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x33)
  :encoding (reg modrm))

;;; XOR the contents of a 32-bit GPR (destination), with the contents
;;; of a 32-bit memory location (source), and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x33)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 64-bit GPR (destination), with the contents
;;; of a 64-bit GPR or memory location (source), and store the result
;;; in the destination.

;;; XOR the contents of a 64-bit GPR (destination), with the contents
;;; of a 64-bit GPR (source), and store the result in the destination.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x33)
  :encoding (reg modrm)
  :rex.w t)

;;; XOR the contents of a 64-bit GPR (destination), with the contents
;;; of a 64-bit memory location (source), and store the result in the
;;; destination.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x33)
  :encoding (reg modrm)
  :rex.w t)
