(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic MUL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multiply the AL GPR (destination) by the contents of an 8-bit GPR
;;; or memory location (source), and store the result in the AX GPR.
;;;
;;; Opcodes: F6

;;; Multiply the AL GPR (destination) by the contents of an 8-bit GPR
;;; (source), and store the result in the AX GPR.
(define-instruction "MUL"
  :modes (32 64)
  :operands ((gpr-a 8) (gpr 8))
  :opcodes (F6)
  :opcode-extension 4
  :encoding (- modrm))
  
;;; Multiply the AL GPR (destination) by the contents of an 8-bit
;;; memory location (source), and store the result in the AX GPR.
(define-instruction "MUL"
  :modes (32 64)
  :operands ((gpr-a 8) (memory 8))
  :opcodes (F6)
  :opcode-extension 4
  :encoding (- modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multiply the A GPR (16/32/64) (destination) by the contents of a
;;; GPR or memory location (16/32/64) (source), and store the result
;;; in the A GPR.
;;;
;;; Opcodes: F7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multiply the AX GPR (destination) by the contents of a 16-bit GPR
;;; or memory location (source), and store the result in the DX:AX
;;; GPR.

;;; Multiply the AX GPR (destination) by the contents of a 16-bit GPR
;;; (source), and store the result in the DX:AX GPR.
(define-instruction "MUL"
  :modes (32 64)
  :operands ((gpr-a 16) (gpr 16))
  :opcodes (F7)
  :opcode-extension 4
  :encoding (- modrm)
  :operand-size-override t)

;;; Multiply the AX GPR (destination) by the contents of a 16-bit
;;; memory location (source), and store the result in the DX:AX GPR.
(define-instruction "MUL"
  :modes (32 64)
  :operands ((gpr-a 16) (memory 16))
  :opcodes (F7)
  :opcode-extension 4
  :encoding (- modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multiply the EAX GPR (destination) by the contents of a 32-bit GPR
;;; or memory location (source), and store the result in the EDX:EAX
;;; GPR.

;;; Multiply the EAX GPR (destination) by the contents of a 32-bit GPR
;;; (source), and store the result in the EDX:EAX GPR.
(define-instruction "MUL"
  :modes (32 64)
  :operands ((gpr-a 32) (gpr 32))
  :opcodes (F7)
  :opcode-extension 4
  :encoding (- modrm))

;;; Multiply the AX GPR (destination) by the contents of a 32-bit
;;; memory location (source), and store the result in the DX:AX GPR.
(define-instruction "MUL"
  :modes (32 64)
  :operands ((gpr-a 32) (memory 32))
  :opcodes (F7)
  :opcode-extension 4
  :encoding (- modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multiply the RAX GPR (destination) by the contents of a 64-bit GPR
;;; or memory location (source), and store the result in the RDX:RAX
;;; GPR.

;;; Multiply the AX GPR (destination) by the contents of a 64-bit GPR
;;; (source), and store the result in the RDX:RAX GPR.
(define-instruction "MUL"
  :modes (64)
  :operands ((gpr-a 64) (gpr 64))
  :opcodes (F7)
  :opcode-extension 4
  :encoding (- modrm)
  :operand-size-override t
  :rex.w t)

;;; Multiply the AX GPR (destination) by the contents of a 64-bit
;;; memory location (source), and store the result in the RDX:RAX GPR.
(define-instruction "MUL"
  :modes (64)
  :operands ((gpr-a 64) (memory 64))
  :opcodes (F7)
  :opcode-extension 4
  :encoding (- modrm)
  :operand-size-override t
  :rex.w t)
