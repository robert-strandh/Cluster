(cl:in-package #:asdf-user)

(defsystem :cluster.disassembler
  :description "Disassembler for the Cluster system."
  :depends-on (:cluster)
  :serial t
  :components
  ((:file "packages")
   (:file "decoder-generator")
   (:file "disassembled-command")
   (:file "modrm")
   (:file "table-interpreter")))
