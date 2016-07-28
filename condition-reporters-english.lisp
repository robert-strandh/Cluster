(cl:in-package #:cluster)

(defmethod acclimation:report-condition
    ((condition unknown-item)
     stream
     (language acclimation:english))
  (format stream
	  "Unknown item: ~s"
	  (item condition)))
