(cl:in-package #:cluster.disassembler)

;;; more of a helper rather than a buffer
;;; it uses the GF reset to reset the modrm byte that has been read.
(defgeneric read-next-byte (buffer))
(defgeneric modrm-byte (buffer))
(defgeneric read-unsigned-integer (buffer size-in-bits))
(defgeneric read-signed-integer (buffer size-in-bits))
(defclass decoder-buffer ()
  ((%source-sequence :initarg :source-sequence
                     :reader source-sequence)
   (%buffer-position :initarg :buffer-position
                     :accessor buffer-position)
   (%modrm-byte :reader modrm-byte :initform nil)))

;;; This only exists because at the moment we create an interpreter
;;; and hide the decoder buffer / only use it when we use GFs
;;; on the interpreter accepting sequences
(defgeneric use-sequence (buffer sequence &key start)
  (:method ((buffer decoder-buffer) sequence &key (start 0))
    (setf (slot-value buffer '%source-sequence) sequence)
    (setf (buffer-position buffer) start)
    nil))

(defmethod read-next-byte ((buffer decoder-buffer))
  (prog1 (elt (source-sequence buffer) (buffer-position buffer))
    (incf (buffer-position buffer))))

(defmethod modrm-byte :around ((buffer decoder-buffer))
  (or (call-next-method)
      (setf (slot-value buffer '%modrm-byte)
            (read-next-byte buffer))))

(defmethod read-unsigned-integer ((buffer decoder-buffer) size-in-bits)
  (let ((result 0))
    (loop for i from 0 upto (- size-in-bits 8) by 8
          do (let ((current-byte (elt (source-sequence buffer)
                                      (buffer-position buffer))))
               (incf (buffer-position buffer))
               (setf (ldb (byte 8 i) result)
                     current-byte)))
    result))

(defun unsigned-to-signed (size unsigned-integer)
  (let ((max+1 (expt 2 size)))
    (if (>= unsigned-integer (ash max+1 -1))
        (- unsigned-integer max+1)
        unsigned-integer)))

(defmethod read-signed-integer ((buffer decoder-buffer) size-in-bits)
  (unsigned-to-signed size-in-bits (read-unsigned-integer buffer size-in-bits)))

(defmethod reset ((buffer decoder-buffer))
  (setf (slot-value buffer '%modrm-byte) nil))

(defgeneric make-interpreter (table))
(defgeneric decode-instruction (interpreter sequence &key start))
(defgeneric decode-sequence (interpreter sequence &key start end))

(defclass table-interpreter (decoder-buffer)
  ((%dispatch-table :initarg :dispatch-table
                    :reader dispatch-table)
   (%state-object :initarg :state-object
                  :reader state-object)))

(defmethod make-interpreter (array-table)
  (make-instance 'table-interpreter :dispatch-table array-table
                                    :state-object (make-instance 'x86-state)))

(defmethod reset ((table-interpreter table-interpreter))
  (call-next-method)
  (reset (state-object table-interpreter)))

;;; This is here because of a dependence problem between the interpreter
;;; generator and the table-interpreter for rex only
(defgeneric rex-value (state))

;;; we don't want to check if it is null here
(defun operand-size<-operand-descriptor (desc)
  (and (listp desc)
       (cadr desc)))

(defun maybe-filter-candidates-via-opcode-extension (interpreter candidates)
  (flet ((opcode-extensions-consistent-p (candidates)
           (let ((opcode-extension-of-first-candidate
                   (c:opcode-extension (first candidates))))
             (every (lambda (candidate)
                      (or (and (integerp opcode-extension-of-first-candidate)
                               (integerp (c:opcode-extension candidate)))
                          (and (null opcode-extension-of-first-candidate)
                               (null (c:opcode-extension candidate)))))
                    candidates)))

         (opcode-extension (interpreter)
           (register-number<-rex+modrm (rex-value (state-object interpreter))
                                       (modrm-byte interpreter))))

    (let ((opcode-extension-of-first-candidate
            (c:opcode-extension (first candidates))))
      (unless (opcode-extensions-consistent-p candidates)
        (error "There are too many candidates with conflicting
opcode extensions to continue decoding."))
      (if (not (null opcode-extension-of-first-candidate))
          (let ((opcode-extension (opcode-extension interpreter)))
            (remove-if-not
             (lambda (candidate)
               (= opcode-extension (cluster:opcode-extension candidate)))
             candidates))
          candidates))))

(defun decode-operands (interpreter candidates)
  (let ((operands '()))
    (loop for operand-position from 0
          while (< operand-position (length (c:operands (first candidates))))
          do (let* ((first-candidate (first candidates))
                    (first-candidate-operand
                      (elt (c:operands first-candidate) operand-position))
                    (first-candidate-encoding (elt (c:encoding first-candidate)
                                                   operand-position))
                    (first-candidate-operand-size
                      (operand-size<-operand-descriptor first-candidate-operand)))
               (assert (every (lambda (candidate)
                                (and
                                 ;; The encodoing of the operand
                                 ;; must be the same for each candidate.
                                 (equal first-candidate-encoding
                                        (elt (c:encoding candidate)
                                             operand-position))
                                 ;; the operand size must also be the same
                                 (eql
                                  first-candidate-operand-size
                                  (operand-size<-operand-descriptor
                                   (elt (c:operands candidate)
                                        operand-position)))))
                              candidates))
               (let ((operand
                       (read-operand interpreter first-candidate-encoding
                                     first-candidate-operand-size
                                     candidates)))
                 (push operand operands)
                 (setf candidates
                       (remove-if-not
                        (lambda (candidate)
                          (c:operand-matches-p
                           operand
                           (elt (cluster:operands candidate) operand-position)))
                        candidates))
                 (assert (not (null candidates))))))
    (values (nreverse operands)
            candidates)))

(defun %decode-instruction-from-candidates (interpreter candidates)
  (setf candidates (maybe-filter-candidates-via-opcode-extension
                    interpreter candidates))
  (multiple-value-bind (operands candidates)
      (decode-operands interpreter candidates)
    (assert (= 1 (length candidates)))
    (make-disassembled-command (first candidates) operands)))

(defun %decode-instruction (interpreter)
  (flet ((state-writer<-prefix (prefix)
           (fdefinition `(setf ,(cluster:interpreter-state-writer prefix)))))
    (let ((start-position (buffer-position interpreter))
          (table-number 0))
      (declare (ignore start-position)) ; use it later to annotate instructions
      (tagbody
       :start
         (let* ((opcode (read-next-byte interpreter) )
                (lookup-value (aref (dispatch-table interpreter) table-number
                                    opcode)))
           (when (eql :next-table lookup-value)
             (incf table-number)
             (go :start))
           (etypecase lookup-value
             (null
              (error "Invalid opcode."))
             (list
              (let ((remaining-candidates
                      (narrow-down-candidates (state-object interpreter)
                                              lookup-value)))
                (when (= 0 (length remaining-candidates))
                  (error "There are no candidate instructions for this instruction"))
                (return-from %decode-instruction
                  (%decode-instruction-from-candidates interpreter remaining-candidates))))
             (cluster:range-prefix
              (funcall (state-writer<-prefix lookup-value)
                       opcode
                       (state-object interpreter))
              (go :start))
             (cluster:modifier-prefix
              (funcall (state-writer<-prefix lookup-value)
                       t (state-object interpreter))
              (go :start))))))))

(defmethod decode-instruction (interpreter sequence &key (start 0))
  (use-sequence interpreter sequence :start start)
  (%decode-instruction interpreter))

(defmethod decode-sequence ((interpreter table-interpreter) sequence
                            &key (start 0) end)
  (use-sequence interpreter sequence :start start)
  (let ((position start)
        (end (or end (length sequence)))
        (disassembled-commands (make-array 8 :adjustable t :fill-pointer 0)))
    (loop while (< position end)
          do (multiple-value-bind
                   (instruction next-position)
                 (%decode-instruction interpreter)
               (vector-push-extend instruction disassembled-commands)
               (setf position next-position)
               (reset interpreter)))
    disassembled-commands))
