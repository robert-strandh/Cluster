(cl:in-package #:asdf-user)

(defsystem :cluster-test.disassembler
  :depends-on (:cluster :cluster.disassembler.x86)
  :serial t
  :components
  ((:file "packages")
   (:file "utils")
   (:file "command-generation")
   (:file "operand-generation")
   (:file "generation-test")))
