(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonics Jcc

(define-instruction "JO"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x70)
  :encoding (label))

(define-instruction "JO"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x80)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JO"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x80)
  :encoding (label))

(define-instruction "JNO"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x71)
  :encoding (label))

(define-instruction "JNO"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x81)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNO"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x81)
  :encoding (label))

(define-instruction "JB"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x72)
  :encoding (label))

(define-instruction "JB"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x82)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JB"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x82)
  :encoding (label))

(define-instruction "JC"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x72)
  :encoding (label))

(define-instruction "JC"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x82)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JC"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x82)
  :encoding (label))

(define-instruction "JNAE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x72)
  :encoding (label))

(define-instruction "JNAE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x82)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNAE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x82)
  :encoding (label))

(define-instruction "JNB"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x73)
  :encoding (label))

(define-instruction "JNB"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x83)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNB"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x83)
  :encoding (label))

(define-instruction "JNC"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x73)
  :encoding (label))

(define-instruction "JNC"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x83)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNC"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x83)
  :encoding (label))

(define-instruction "JAE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x73)
  :encoding (label))

(define-instruction "JAE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x83)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JAE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x83)
  :encoding (label))

(define-instruction "JZ"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x74)
  :encoding (label))

(define-instruction "JZ"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x84)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JZ"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x84)
  :encoding (label))

(define-instruction "JE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x74)
  :encoding (label))

(define-instruction "JE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x84)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x84)
  :encoding (label))

(define-instruction "JNZ"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x75)
  :encoding (label))

(define-instruction "JNZ"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x85)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNZ"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x85)
  :encoding (label))

(define-instruction "JNE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x75)
  :encoding (label))

(define-instruction "JNE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x85)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x85)
  :encoding (label))

(define-instruction "JBE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x76)
  :encoding (label))

(define-instruction "JBE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x86)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JBE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x86)
  :encoding (label))

(define-instruction "JNA"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x76)
  :encoding (label))

(define-instruction "JNA"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x86)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNA"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x86)
  :encoding (label))

(define-instruction "JNBE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x77)
  :encoding (label))

(define-instruction "JNBE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x87)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNBE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x87)
  :encoding (label))

(define-instruction "JA"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x77)
  :encoding (label))

(define-instruction "JA"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x87)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JA"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x87)
  :encoding (label))

(define-instruction "JS"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x78)
  :encoding (label))

(define-instruction "JS"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x88)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JS"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x88)
  :encoding (label))

(define-instruction "JNS"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x79)
  :encoding (label))

(define-instruction "JNS"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x89)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNS"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x89)
  :encoding (label))

(define-instruction "JP"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7A)
  :encoding (label))

(define-instruction "JP"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8A)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JP"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8A)
  :encoding (label))

(define-instruction "JPE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7A)
  :encoding (label))

(define-instruction "JPE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8A)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JPE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8A)
  :encoding (label))

(define-instruction "JNP"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7B)
  :encoding (label))

(define-instruction "JNP"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8B)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNP"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8B)
  :encoding (label))

(define-instruction "JPO"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7B)
  :encoding (label))

(define-instruction "JPO"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8B)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JPO"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8B)
  :encoding (label))

(define-instruction "JL"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7C)
  :encoding (label))

(define-instruction "JL"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8C)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JL"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8C)
  :encoding (label))

(define-instruction "JNGE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7C)
  :encoding (label))

(define-instruction "JNGE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8C)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNGE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8C)
  :encoding (label))

(define-instruction "JNL"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7D)
  :encoding (label))

(define-instruction "JNL"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8D)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNL"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8D)
  :encoding (label))

(define-instruction "JGE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7D)
  :encoding (label))

(define-instruction "JGE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8D)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JGE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8D)
  :encoding (label))

(define-instruction "JLE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7E)
  :encoding (label))

(define-instruction "JLE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8E)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JLE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8E)
  :encoding (label))

(define-instruction "JNG"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7E)
  :encoding (label))

(define-instruction "JNG"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8E)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNG"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8E)
  :encoding (label))

(define-instruction "JNLE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7F)
  :encoding (label))

(define-instruction "JNLE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8F)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNLE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8F)
  :encoding (label))

(define-instruction "JG"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7F)
  :encoding (label))

(define-instruction "JG"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8F)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JG"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8F)
  :encoding (label))

