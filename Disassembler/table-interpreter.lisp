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

;;; Would using Trivia would make this much easier to work with?
(defun decode-single-descriptor-operands
    (interpreter descriptor vector position)
  (declare (ignore interpreter))
  (cond ((and (assoc 'c:gpr-a (c:operands descriptor))
              (or (assoc 'c:imm (c:operands descriptor))
                  (assoc 'c:simm (c:operands descriptor))))
         (let ((operands (decode-gpr-a-and-s/imm
                          (cadr (assoc 'c:gpr-a (c:operands descriptor)))
                          (cadr (or (assoc 'c:simm (c:operands descriptor))
                                   (assoc 'c:imm (c:operands descriptor))))
                          vector position)))
           (make-disassembled-command descriptor
                                      operands)))))


(defun decode-from-descriptor (interpreter vector start-position position
                               instruction-descriptor)
  (declare (ignore start-position))
  (decode-single-descriptor-operands interpreter instruction-descriptor vector position))


(defun narrow-down-candidates-from-operands (candidates operands
                                             &key opcode-extension)
  (remove-if-not
   (lambda (candidate)
     (and (= opcode-extension (cluster:opcode-extension candidate))
          (null
           (set-difference (cluster:operands candidate)
                           (mapcan #'operand-descriptor<-cluster-operand
                                   operands)))))
   candidates))

;;; This is here because of a dependence problem between the interpreter
;;; generator and the table-interpreter for rex only
(defgeneric rex-value (state))
(defun try-decode-from-opcode-extension
    (interpreter vector start-position position candidates)
  (declare (ignore start-position))
  (unless (every (lambda (instruction-descriptor)
                   (not (null (cluster:opcode-extension instruction-descriptor))))
                 candidates)
    (error "There are too manay candidates ~a for this instruction" candidates))
  (let ((modrm (aref vector position))
        (rex (rex-value (state-object interpreter))))
    (incf position)
    (let* ((opcode-extension (register-number<-rex+modrm rex modrm))
           (rm-operand (rm-operand<-modrm rex modrm vector position))
           (remaining-candidates
             (remove-if-not
              (lambda (candidate)
                (and (= opcode-extension (cluster:opcode-extension candidate))
                     (equal (cluster:operands candidate)
                            (list
                             (operand-descriptor<-cluster-operand
                              rm-operand)))))
              candidates)))
      (assert (= 1 (length remaining-candidates)))
      (make-disassembled-command (first remaining-candidates) (list rm-operand)))))

(defun decode-instruction-with-register-operand
    (interpreter vector position candidates)
  (let ((modrm (prog1 (aref vector position) (incf position)))
        (rex (rex-value (state-object interpreter))))
    (let* ((operands (list (register-operand<-rex+modrm rex modrm)
                           (rm-operand<-modrm rex modrm vector position)))
           (remaining-candidates
             (narrow-down-candidates-from-operands candidates operands)))
      (assert (= 1 (length remaining-candidates)))
      (make-disassembled-command (first remaining-candidates)
                                 operands))))

(defun decode-from-remaining-candidates
    (interpreter vector start-position position candidates))

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
              (incf position)
              ;; probably want to call something that returns the instruction
              ;; and wil call this as part of that process.
              (let ((remaining-candidates
                      (narrow-down-candidates (state-object interpreter)
                                              lookup-value)))
                (if (= 1 (length remaining-candidates))
                    (return-from decode-instruction
                      (decode-from-descriptor
                       interpreter vector start-position position
                       (first remaining-candidates)))
                    (return-from decode-instruction
                      (if (not (null (cluster:opcode-extension (first remaining-candidates))))
                          (try-decode-from-opcode-extension
                           interpreter vector start-position position remaining-candidates)
                          (decode-from-remaining-candidates
                           interpreter vector start-position position remaining-candidates))))))
             (cluster:range-prefix
              (funcall (state-writer<-prefix lookup-value)
                       (aref vector position)
                       (state-object interpreter)))
             (cluster:modifier-prefix
              (funcall (state-writer<-prefix lookup-value)
                       t (state-object interpreter)))))
         (incf position)
         (go :start)))))
