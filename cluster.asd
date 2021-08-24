(cl:in-package #:asdf-user)

(defsystem :cluster
  :description "Assembler without surface syntax"
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :license "FreeBSD, see file LICENSE.text"
  :depends-on (:split-sequence
               :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "mode")
   (:file "item")
   (:file "label")
   (:file "command")
   (:file "data")
   (:file "operand")
   (:file "instruction-descriptors")
   (:file "instruction-set")
   (:file "code")
   (:file "assembler")
   (:file "print")
   (:file "conditions")
   (:file "condition-reporters-english")))
