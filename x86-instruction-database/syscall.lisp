(in-package #:cluster)

(define-instruction "SYSCALL"
  :modes (64)
  :operands ()
  :opcodes (#x0F #x05))
