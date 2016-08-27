(cl:in-package #:asdf-user)

(defsystem :cluster
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
   (:file "code")
   (:file "assembler")
   (:file "print")
   (:file "conditions")
   (:file "condition-reporters-english")))
