(cl:in-package #:common-lisp-user)

(defpackage #:cluster-test.disassembler
  (:use #:common-lisp)
  (:export #:test-all-x86)
  (:local-nicknames
   (#:c #:cluster)
   (#:c.d #:cluster.disassembler)))
