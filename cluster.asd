(cl:in-package #:asdf-user)

(defsystem :cluster
  :depends-on (:split-sequence
	       :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "data")
   (:file "assembler")
   (:file "instruction-descriptors")
   (:file "instruction-database")
   (:file "print")
   (:file "conditions")
   (:file "condition-reporters-english")))
