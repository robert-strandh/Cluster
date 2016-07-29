(cl:in-package #:asdf-user)

(defsystem :cluster-test
  :depends-on (:cluster)
  :serial t
  :components
  ((:file "packages")))
