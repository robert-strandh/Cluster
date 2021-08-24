(cl:in-package #:asdf-user)

(defsystem :cluster.disassembler.x86
  :description "x86 backend for the Cluster disassembler."
  :depends-on (:cluster :cluster-x86-instruction-database
                        :cluster.disassembler)
  :serial t
  :components
  ((:file "packages")
   (:file "state")))
