(cl:in-package #:cluster)

(defgeneric compute-encoding (item))

(defmethod compute-encoding (item)
  (error "Item of unknown type: ~s" item))
