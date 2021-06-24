(cl:in-package #:common-lisp-user)

(defpackage #:cluster
  (:use #:common-lisp)
  (:export
   #:label #:make-label
   #:code-command #:make-code-command
   #:immediate-operand #:make-immediate-operand
   #:gpr-operand #:make-gpr-operand
   #:memory-operand #:make-memory-operand
   #:assemble
   #:unknown-item
   #:item

   ;; command
   #:command
   #:compute-encoding
   #:best-candidate-descriptor
   ;; instruction-descriptor
   #:mnemonic
   #:modes
   #:operands
   #:opcodes
   #:encoding
   #:opcode-extension
   #:operand-size-override
   #:rex.w
   #:instruction-descriptor-equal
   #:operand-matches-p
   ;; operands
   #:size
   #:gpr-a
   #:gpr
   #:memory
   #:imm
   #:simm
   #:reg
   #:modrm
   #:+r
   #:-
   #:value
   #:base-register
   #:displacement
   #:index-register
   #:scale
   ;; prefixes
   #:prefix
   #:modifier-prefix
   #:instruction-descriptor-predicate
   #:prefix-name
   #:prefix-opcode
   #:modifier-prefix
   #:define-modifier-prefix
   #:find-modifier-prefix
   ;; range prefix
   #:range-prefix
   #:opcode-range-start
   #:opcode-range-end
   #:bitflag-prefix
   #:bitflag-prefixes
   #:find-range-prefix
   #:define-range-prefix
   #:prefix-opcode-bitmask
   #:interpreter-state-writer
   ;; instruction set
   #:instruction-set
   #:modifier-prefixes
   #:range-prefixes
   #:instruction-set-name
   #:find-instruction-set
   #:define-instruction-set))
