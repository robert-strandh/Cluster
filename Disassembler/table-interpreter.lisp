(cl:in-package #:cluster.disassembler)

(defgeneric make-interpreter (table))
(defgeneric decode-instruction (interpreter vector start-position))
(defclass table-interpreter ()
  ((%dispatch-table :initarg :dispatch-table
                    :reader dispatch-table)
   (%state-object :initarg :state-object
                  :reader state-object)))

(defmethod make-interpreter (array-table)
  (make-instance 'table-interpreter :dispatch-table array-table
                 :state-object (make-instance 'x86-state)))


(defun decode-from-descriptor (interpreter vector start-position position
                               instruction-descriptor)
  (declare (ignore interpreter vector start-position position instruction-descriptor))
  (error "Not implemented but wohoo."))

(defmethod decode-instruction (interpreter vector start-position)
  (flet ((state-writer<-prefix (prefix)
           (fdefinition `(setf ,(cluster:interpreter-state-writer prefix)))))
    (let ((position start-position)
          (table-number 0))
      (tagbody
       :start
         (let ((lookup-value (aref (dispatch-table interpreter) table-number
                                   (aref vector position))))
           (when (eql :next-table lookup-value)
             (incf position)
             (incf table-number)
             (go :start))
           (etypecase lookup-value
             (null
              (error "Invalid opcode."))
             (list
              ;; probably want to call something that returns the instruction
              ;; and wil call this as part of that process.
              (let ((instruction-descriptor
                      (select-from-candidates (state-object interpreter)
                                              lookup-value)))
                (return-from decode-instruction
                  (decode-from-descriptor
                   interpreter vector start-position position instruction-descriptor))))
             (cluster:range-prefix
              (funcall (state-writer<-prefix lookup-value)
                       (aref vector position)
                       (state-object interpreter)))
             (cluster:modifier-prefix
              (funcall (state-writer<-prefix lookup-value)
                       t (state-object interpreter)))))
         (incf position)
         (go :start)))))
