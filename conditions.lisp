(cl:in-package #:cluster)

(define-condition cluster-error (error acclimation:condition)
  ())

(define-condition unknown-item (cluster-error)
  ((%item :initarg :item :reader item)))
