(cl:in-package #:cluster)

(defgeneric compute-encoding (item))

(defmethod compute-encoding (item)
  (error "Item of unknown type: ~s" item))

;;; Take an item and return the preliminary size of that item.
(defgeneric preliminary-size (item))

(defmethod preliminary-size (item)
  (error "Item of unknown type: ~s" item))
