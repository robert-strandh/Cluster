(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic ADC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To the contents of GPR A (destination), add an 8-bit immediate
;;; value (source) and the contents of the CF flag, and store the
;;; result in the destination.
;;;
;;; Opcodes: 14

(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr-a 8) (imm 8))
  :opcodes (#x14)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To the contents of GPR A (16/32/64) (destination), add an
;;; immediate value (16/32) (source) and the contents of the CF flag,
;;; and store the result in the destination.
;;; 
;;; Opcodes: 15

;;; To the contents of GPR AX (destination), add an immediate 16-bit
;;; value (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr-a 16) (imm 16))
  :opcodes (#x15)
  :encoding (- imm)
  :operand-size-override t)

;;; To the contents of GPR EAX (destination), add an immediate 32-bit
;;; value (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr-a 32) (imm 32))
  :opcodes (#x15)
  :encoding (- imm))

;;; To the contents of GPR RAX (destination), add an immediate
;;; sign-extended 32-bit value (source) and the contents of the CF
;;; flag, and store the result in the destination.
(define-instruction "ADC"
  :modes (64)
  :operands ((gpr-a 64) (simm 32))
  :opcodes (#x15)
  :encoding (- imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To an 8-bit GPR or memory location (destination), add an immediate
;;; 8-bit value (source) and the contents of the CF flag, and store
;;; the result in the destination.
;;; 
;;; Opcodes: 80
;;; Opcode extension: 2

;;; To an 8-bit GPR (destination), add an immediate 8-bit value
;;; (source) and the contents of the CF flag, and store the result in
;;; the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 2
  :encoding (modrm imm))

;;; To an 8-bit memory location (destination), add an immediate 8-bit
;;; value (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 2
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a GPR or memory location (16/32/64) (destination), add an
;;; immediate value (16/32) (source) and the contents of the CF flag,
;;; and store the result in the destination.
;;;
;;; Opcodes: 81
;;; Opcode extension 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 16-bit GPR or memory location (destination), add an immediate
;;; 16-bit value (source) and the contents of the CF flag, and store
;;; the result in the destination.

;;; To a 16-bit GPR (destination), add an immediate 16-bit value
;;; (source) and the contents of the CF flag, and store the result in
;;; the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 2
  :encoding (modrm imm)
  :operand-size-override t)

;;; To a 16-bit memory location (destination), add an immediate 16-bit
;;; value (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((memory 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 2
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 32-bit GPR or memory location (destination), add an immediate
;;; 32-bit value (source) and the contents of the CF flag, and store
;;; the result in the destination.

;;; To a 32-bit GPR (destination), add an immediate 32-bit value
;;; (source) and the contents of the CF flag, and store the result in
;;; the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 2
  :encoding (modrm imm))

;;; To a 32-bit memory location (destination), add an immediate 32-bit
;;; value (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 2
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 32-bit GPR or memory location (destination), add an immediate
;;; 32-bit sign-extended value (source) and the contents of the CF
;;; flag, and store the result in the destination.

;;; To a 32-bit GPR (destination), add an immediate 32-bit
;;; sign-extended value (source) and the contents of the CF flag, and
;;; store the result in the destination.
(define-instruction "ADC"
  :modes (64)
  :operands ((gpr 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 2
  :encoding (modrm imm)
  :rex.w t)

;;; To a 32-bit memory location (destination), add an immediate 32-bit
;;; sign-extended value (source) and the contents of the CF flag, and
;;; store the result in the destination.
(define-instruction "ADC"
  :modes (64)
  :operands ((memory 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 2
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a GPR or memory location (16/32/64) (destination), add an
;;; immediate 8-bit sign-extended value (source) and the contents of
;;; the CF flag, and store the result in the destination.
;;;
;;; Opcodes: 83
;;; Opcode extension: 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 16-bit GPR or memory location (destination), add an immediate
;;; 8-bit sign-extended value (source) and the contents of the CF
;;; flag, and store the result in the destination.

;;; To a 16-bit GPR (destination), add an immediate 8-bit
;;; sign-extended value (source) and the contents of the CF flag, and
;;; store the result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 2
  :encoding (modrm imm)
  :operand-size-override t)

;;; To a 16-bit memory location (destination), add an immediate 8-bit
;;; sign-extended value (source) and the contents of the CF flag, and
;;; store the result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((memory 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 2
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 32-bit GPR or memory location (destination), add an immediate
;;; 8-bit sign-extended value (source) and the contents of the CF
;;; flag, and store the result in the destination.

;;; To a 32-bit GPR (destination), add an immediate 8-bit
;;; sign-extended value (source) and the contents of the CF flag, and
;;; store the result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 2
  :encoding (modrm imm))

;;; To a 32-bit memory location (destination), add an immediate 8-bit
;;; sign-extended value (source) and the contents of the CF flag, and
;;; store the result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((memory 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 2
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 64-bit GPR or memory location (destination), add an immediate
;;; 8-bit sign-extended value (source) and the contents of the CF
;;; flag, and store the result in the destination.

;;; To a 64-bit GPR (destination), add an immediate 8-bit
;;; sign-extended value (source) and the contents of the CF flag, and
;;; store the result in the destination.
(define-instruction "ADC"
  :modes (64)
  :operands ((gpr 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 2
  :encoding (modrm imm)
  :rex.w t)

;;; To a 64-bit memory location (destination), add an immediate 8-bit
;;; sign-extended value (source) and the contents of the CF flag, and
;;; store the result in the destination.
(define-instruction "ADC"
  :modes (64)
  :operands ((memory 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 2
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To an 8-bit GPR or memory location (destination), add an immediate
;;; 8-bit value (source) and the contents of the CF flag, and store
;;; the result in the destination.
;;;
;;; Opcodes: 10

;;; To an 8-bit GPR (destination), add an immediate 8-bit value
;;; (source) and the contents of the CF flag, and store the result in
;;; the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x10)
  :encoding (modrm reg))

;;; To an 8-bit memory location (destination), add an immediate 8-bit
;;; value (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x10)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a GPR or memory location (16/32/64) (destination), add the
;;; contents of a GPR (16/32/64) (source) and the contents of the CF
;;; flag, and store the result in the destination.
;;;
;;; Opcodes: 11

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 16-bit GPR or memory location (destination), add the contents
;;; of a 16-bit GPR (source) and the contents of the CF flag, and
;;; store the result in the destination.

;;; To a 16-bit GPR (destination), add the contents of a 16-bit GPR
;;; (source) and the contents of the CF flag, and store the result in
;;; the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x11)
  :encoding (modrm reg)
  :operand-size-override t)

;;; To a 16-bit memory location (destination), add the contents of a
;;; 16-bit GPR (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x11)
  :encoding (modrm reg)
  :lock t
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 32-bit GPR or memory location (destination), add the contents
;;; of a 32-bit GPR (source) and the contents of the CF flag, and
;;; store the result in the destination.

;;; To a 32-bit GPR (destination), add the contents of a 32-bit GPR
;;; (source) and the contents of the CF flag, and store the result in
;;; the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x11)
  :encoding (modrm reg))

;;; To a 32-bit memory location (destination), add the contents of a
;;; 32-bit GPR (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x11)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 64-bit GPR or memory location (destination), add the contents
;;; of a 64-bit GPR (source) and the contents of the CF flag, and
;;; store the result in the destination.

;;; To a 64-bit GPR (destination), add the contents of a 64-bit GPR
;;; (source) and the contents of the CF flag, and store the result in
;;; the destination.
(define-instruction "ADC"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x11)
  :encoding (modrm reg)
  :rex.w t)

;;; To a 64-bit memory location (destination), add the contents of a
;;; 64-bit GPR (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x11)
  :encoding (modrm reg)
  :lock t
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To an 8-bit GPR (destination), add the contents of an 8-bit GPR or
;;; memory location (source) and the contents of the CF flag, and
;;; store the result in the destination.
;;;
;;; Opcodes: 12

;;; To an 8-bit GPR (destination), add the contents of an 8-bit GPR
;;; (source) and the contents of the CF flag, and store the result in
;;; the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x12)
  :encoding (reg modrm))

;;; To an 8-bit GPR (destination), add the contents of an 8-bit memory
;;; location (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 8) (memory 8))
  :opcodes (#x12)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a GPR (16/32/64) (destination), add the contents of a GPR or
;;; memory location (16/32/64) (source) and the contents of the CF
;;; flag, and store the result in the destination.
;;;
;;; Opcodes: 13

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 16-bit GPR (destination), add the contents of a 16-bit GPR or
;;; memory location (source) and the contents of the CF flag, and
;;; store the result in the destination.

;;; To a 16-bit GPR (destination), add the contents of a 16-bit GPR
;;; (source) and the contents of the CF flag, and store the result in
;;; the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x13)
  :encoding (reg modrm)
  :operand-size-override t)

;;; To a 16-bit GPR (destination), add the contents of a 16-bit memory
;;; location (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x13)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 32-bit GPR (destination), add the contents of a 32-bit GPR or
;;; memory location (source) and the contents of the CF flag, and
;;; store the result in the destination.

;;; To a 32-bit GPR (destination), add the contents of a 32-bit GPR
;;; (source) and the contents of the CF flag, and store the result in
;;; the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x13)
  :encoding (reg modrm))

;;; To a 32-bit GPR (destination), add the contents of a 32-bit memory
;;; location (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x13)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 64-bit GPR (destination), add the contents of a 64-bit GPR or
;;; memory location (source) and the contents of the CF flag, and
;;; store the result in the destination.

;;; To a 64-bit GPR (destination), add the contents of a 64-bit GPR
;;; (source) and the contents of the CF flag, and store the result in
;;; the destination.
(define-instruction "ADC"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x13)
  :encoding (reg modrm)
  :rex.w t)

;;; To a 64-bit GPR (destination), add the contents of a 64-bit memory
;;; location (source) and the contents of the CF flag, and store the
;;; result in the destination.
(define-instruction "ADC"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x13)
  :encoding (reg modrm)
  :rex.w t)
