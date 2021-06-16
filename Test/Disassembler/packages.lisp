(cl:in-package #:common-lisp-user)

(defpackage #:cluster-test.disassembler
  (:use #:common-lisp)
  (:local-nicknames
   (#:c #:cluster)
   (#:c.d #:cluster.disassembler)))
