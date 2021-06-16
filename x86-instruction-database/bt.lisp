(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic BT

(define-instruction "BT"
  :modes (64)
  :operands ((gpr 64) (imm 8))
  :opcodes (#x0F #xBA)
  :opcode-extension 4
  :encoding (modrm imm)
  :rex.w t)

(define-instruction "BT"
  :modes (64)
  :operands ((memory 64) (imm 8))
  :opcodes (#x0F #xBA)
  :opcode-extension 4
  :encoding (modrm imm)
  :rex.w t)

(define-instruction "BT"
  :modes (32 64)
  :operands ((gpr 32) (imm 8))
  :opcodes (#x0F #xBA)
  :opcode-extension 4
  :encoding (modrm imm))

(define-instruction "BT"
  :modes (32 64)
  :operands ((memory 32) (imm 8))
  :opcodes (#x0F #xBA)
  :opcode-extension 4
  :encoding (modrm imm))

(define-instruction "BT"
  :modes (32 64)
  :operands ((gpr 16) (imm 8))
  :opcodes (#x0F #xBA)
  :opcode-extension 4
  :encoding (modrm imm)
  :operand-size-override t)

(define-instruction "BT"
  :modes (32 64)
  :operands ((memory 16) (imm 8))
  :opcodes (#x0F #xBA)
  :opcode-extension 4
  :encoding (modrm imm)
  :operand-size-override t)

(define-instruction "BT"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x0F #xA3)
  :encoding (modrm reg)
  :rex.w t)

(define-instruction "BT"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x0F #xA3)
  :encoding (modrm reg)
  :rex.w t)

(define-instruction "BT"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x0F #xA3)
  :encoding (modrm reg))

(define-instruction "BT"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x0F #xA3)
  :encoding (modrm reg))

(define-instruction "BT"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x0F #xA3)
  :encoding (modrm reg)
  :operand-size-override t)

(define-instruction "BT"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x0F #xA3)
  :encoding (modrm reg)
  :operand-size-override t)
