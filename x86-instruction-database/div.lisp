(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic DIV

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Divide the contents of GPR AX by an 8-bit register or memory
;;; location.  Store the quotient in AL and the remainder in AH.

;;; Divide the contents of GPR AX by an 8-bit register location.
;;; Store the quotient in AL and the remainder in AH.
(define-instruction "DIV"
  :modes (32 64)
  :operands ((gpr 8))
  :opcodes (#xf6)
  :opcode-extension 6
  :encoding (modrm))

;;; Divide the contents of GPR AX by an 8-bit memory location.  Store
;;; the quotient in AL and the remainder in AH.
(define-instruction "DIV"
  :modes (32 64)
  :operands ((memory 8))
  :opcodes (#xf6)
  :opcode-extension 6
  :encoding (modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Divide the contents of GPR DX:AX by an 16-bit register or memory
;;; location.  Store the quotient in AX and the remainder in DX.

;;; Divide the contents of GPR DX:AX by an 16-bit register location.
;;; Store the quotient in AX and the remainder in DX.
(define-instruction "DIV"
  :modes (32 64)
  :operands ((gpr 16))
  :opcodes (#xf7)
  :opcode-extension 6
  :encoding (modrm)
  :operand-size-override t)

;;; Divide the contents of GPR DX:AX by an 16-bit memory location.
;;; Store the quotient in AX and the remainder in DX.
(define-instruction "DIV"
  :modes (32 64)
  :operands ((memory 16))
  :opcodes (#xf7)
  :opcode-extension 6
  :encoding (modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Divide the contents of GPR EDX:EAX by an 32-bit register or memory
;;; location.  Store the quotient in EAX and the remainder in EDX.

;;; Divide the contents of GPR EDX:EAX by an 32-bit register location.
;;; Store the quotient in EAX and the remainder in EDX.
(define-instruction "DIV"
  :modes (32 64)
  :operands ((gpr 32))
  :opcodes (#xf7)
  :opcode-extension 6
  :encoding (modrm))

;;; Divide the contents of GPR EDX:EAX by an 32-bit memory location.
;;; Store the quotient in EAX and the remainder in EDX.
(define-instruction "DIV"
  :modes (32 64)
  :operands ((memory 32))
  :opcodes (#xf7)
  :opcode-extension 6
  :encoding (modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Divide the contents of GPR RDX:RAX by an 64-bit register or memory
;;; location.  Store the quotient in RAX and the remainder in RDX.

;;; Divide the contents of GPR RDX:RAX by an 64-bit register location.
;;; Store the quotient in RAX and the remainder in RDX.
(define-instruction "DIV"
  :modes (64)
  :operands ((gpr 64))
  :opcodes (#xf7)
  :opcode-extension 6
  :encoding (modrm)
  :rex.w t)

;;; Divide the contents of GPR RDX:RAX by an 64-bit memory location.
;;; Store the quotient in RAX and the remainder in RDX.
(define-instruction "DIV"
  :modes (64)
  :operands ((memory 64))
  :opcodes (#xf7)
  :opcode-extension 6
  :encoding (modrm)
  :rex.w t)
