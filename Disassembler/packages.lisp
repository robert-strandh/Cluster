(cl:in-package #:common-lisp-user)

(defpackage #:cluster.disassembler
  (:use #:common-lisp)
  (:local-nicknames
   (#:c #:cluster))
  (:export
   #:make-interpreter
   #:make-debug-interpreter
   #:*x86-table*
   #:decode-instruction
   #:decode-sequence
   #:instruction-descriptor
   #:start-position))
