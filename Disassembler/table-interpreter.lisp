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

(defun decode-immediate-from-descritpor
    (descriptor vector position)
  (let ((immediate-size (cadr (or (assoc 'c:simm (c:operands descriptor))
                                  (assoc 'c:imm  (c:operands descriptor))))))
    (assert (not (null immediate-size)))
    (c:make-immediate-operand (decode-immediate immediate-size vector position))))

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


(defun narrow-down-candidates-from-operands (candidates operands)
  (let ((operand-descriptors
          (mapcar #'operand-descriptor<-cluster-operand operands)))
    (remove-if-not
     ;; Needs to test subset because there may be an immediate operand
     ;; that has yet to be decoded.
     (lambda (candidate)
       (subsetp operand-descriptors (cluster:operands candidate) :test #'equal))
     candidates)))

;;; This is here because of a dependence problem between the interpreter
;;; generator and the table-interpreter for rex only
(defgeneric rex-value (state))

;;; Note, the candidates will always have the same operand-size-override
;;; and rex.w values bc of the way the interpreter works
;;; it is simply a case of looking at their operand descriptors
;;; and asserting that they are all the same size for r/m.
(defun r/m-operand-size-from-candidates (candidates)
  (flet ((descriptor-operand-size (desc)
           (let ((size
                   (cadr
                    (or (assoc 'c:gpr    (c:operands desc))
                        (assoc 'c:memory (c:operands desc))))))
             (assert (not (null size)))
             size)))
    (let ((operand-size-of-first-descriptor
            (descriptor-operand-size (first candidates))))
      (assert (every (lambda (candidate)
                       (= operand-size-of-first-descriptor
                          (descriptor-operand-size candidate)))
                     candidates))
      operand-size-of-first-descriptor)))

;;; We always assume that r/m operands are always the same size.
;;; Which I think is a valid assumption to make since intel manual
;;; only lists them together.
(defun try-decode-from-opcode-extension
    (interpreter vector start-position position candidates)
  (declare (ignore start-position))
  (unless (every (lambda (instruction-descriptor)
                   (not (null (cluster:opcode-extension instruction-descriptor))))
                 candidates)
    (error "There are too manay candidates ~a for this instruction" candidates))
  (let ((modrm (prog1 (aref vector position) (incf position)))
        (rex (rex-value (state-object interpreter))))
    (let* ((opcode-extension (register-number<-rex+modrm rex modrm))
           (remaining-candidates
             (remove-if-not
              (lambda (candidate)
                (= opcode-extension (cluster:opcode-extension candidate)))
              candidates))
           (r/m-operand-size (r/m-operand-size-from-candidates remaining-candidates))
           (rm-operand
             (rm-operand<-size+rex+modrm r/m-operand-size rex modrm vector position))
           (remaining-candidates
             (narrow-down-candidates-from-operands
              remaining-candidates (list rm-operand))))

      (assert (= 1 (length remaining-candidates)))
      (let ((candidate (first remaining-candidates)))
        ;; now we have to check if there is an immediate operand to also decode.
        (if (equal (cluster:operands candidate)
                   (list (operand-descriptor<-cluster-operand rm-operand)))
            (make-disassembled-command candidate (list rm-operand))
            ;; decode immediate
            (let ((immediate
                    (decode-immediate-from-descritpor candidate vector position)))
              (make-disassembled-command candidate (list rm-operand immediate))))))))

(defun decode-instruction-with-register-operand
    (interpreter vector position candidates)
  (let ((modrm (prog1 (aref vector position) (incf position)))
        (rex (rex-value (state-object interpreter)))
        (r/m-size (r/m-operand-size-from-candidates candidates)))
    (let* ((operands (list (register-operand<-rex+modrm rex modrm)
                           (rm-operand<-size+rex+modrm r/m-size rex modrm
                                                       vector position)))
           (remaining-candidates
             (narrow-down-candidates-from-operands candidates operands)))
      (assert (= 1 (length remaining-candidates)))
      (make-disassembled-command (first remaining-candidates)
                                 operands))))

(defun decode-from-remaining-candidates
    (interpreter vector start-position position candidates)
  (declare (ignore interpreter vector start-position position candidates))
  (error "Not implemented"))

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
                (cond
                  ((= 1 (length remaining-candidates))
                   (return-from decode-instruction
                     (decode-from-descriptor
                      interpreter vector start-position position
                      (first remaining-candidates))))
                  ((< 1 (length remaining-candidates))
                   (return-from decode-instruction
                     (if (not (null (cluster:opcode-extension (first remaining-candidates))))
                         (try-decode-from-opcode-extension
                          interpreter vector start-position position remaining-candidates)
                         (decode-from-remaining-candidates
                          interpreter vector start-position position remaining-candidates))))
                  (t (error "There are no candidate instructions for this instruction")))))
             (cluster:range-prefix
              (funcall (state-writer<-prefix lookup-value)
                       (aref vector position)
                       (state-object interpreter)))
             (cluster:modifier-prefix
              (funcall (state-writer<-prefix lookup-value)
                       t (state-object interpreter)))))
         (incf position)
         (go :start)))))
