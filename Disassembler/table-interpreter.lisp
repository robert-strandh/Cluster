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
                  :reader state-object)
   (%label-table :initarg :label-table
                 :accessor label-table
                 :initform (make-hash-table))
   ;; the last opcode-byte that was decoded
   (%last-opcode-byte :accessor last-opcode-byte
                      :initform nil)
   ;; the source commands used to produce the program being
   ;; disassembled
   (%command-tracking-debug :initarg :command-tracking-debug
                            :accessor command-tracking-debug
                            :initform nil)
   ;; integer of command position in command-tracking-debug
   (%current-command-debug-position
    :initarg :current-command-debug-position
    :accessor current-command-debug-position
    :initform 0)))

(defgeneric increment-command-debug (interpreter)
  (:method ((interpreter table-interpreter))
    (with-accessors ((command-tracking-debug command-tracking-debug)
                     (current-command-debug-position
                      current-command-debug-position))
        interpreter
      (incf current-command-debug-position)
      (loop while (< (current-command-debug-position interpreter)
                     (length (command-tracking-debug interpreter)))
            for command = (current-command-debug interpreter)
            while (typep command 'c:label)
            do (incf current-command-debug-position)))))

(defgeneric current-command-debug (interpreter)
  (:method ((interpreter table-interpreter))
    (elt (command-tracking-debug interpreter)
         (current-command-debug-position interpreter))))

(defmethod make-interpreter (array-table)
  (make-instance 'table-interpreter :dispatch-table array-table
                                    :state-object (make-instance 'x86-state)))

;;; TODO
;;; I would like for the debug source code commands to be annotated
;;; with their start positions in the assembled sequence that is to be
;;; decoded so that when the assertion in the decoder loop that checks
;;; that the code commands that are being created are equal to the ones
;;; that were assembled fails, the person debuging can see exactly where the
;;; discrepancy is.
(defun make-debug-interpreter (array-table debug-command-program)
  (let ((interpreter (make-interpreter array-table)))
    (setf (command-tracking-debug interpreter) debug-command-program)
    (when (typep (current-command-debug interpreter) 'c:label)
      (increment-command-debug interpreter))
    interpreter))

(defmethod reset ((table-interpreter table-interpreter))
  (call-next-method)
  (reset (state-object table-interpreter)))

;;; we might want to keep the instruction length
;;; in the disassembled command object
;;; from there we can insert lables after by
;;; copying them to a new sequence and then
;;; scan for any labels that aren't in the sequence or something
;;; not sure though.
(defgeneric intern-label (tracker displacement)
  (:method ((tracker table-interpreter) displacement)
    (let ((absolute-address
            (+ displacement
               ;; the buffer position will be pointing at the next
               ;; instruction once it has been read
               (buffer-position tracker))))
      (or (gethash absolute-address (label-table tracker))
          (setf (gethash absolute-address (label-table tracker))
                (c:make-label))))))

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
                                     first-candidate-operand
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
    (assert (or (= 1 (length candidates))
                (every (lambda (c)
                         (c:instruction-descriptor-equal
                          (first candidates) c))
                       candidates)))
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
                (lookup-value
                  (opcode-byte-candidates
                   (dispatch-table interpreter) table-number opcode)))
           (setf (last-opcode-byte interpreter) opcode)
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

(defun insert-labels (interpreter disassembled-commands)
  (let ((labeled-commands
          (make-array (+ (length disassembled-commands)
                         (hash-table-size (label-table interpreter)))
                      :fill-pointer 0)))
    (loop for command across disassembled-commands
          for label = (gethash (start-position command) (label-table interpreter))
          unless (null label) do (vector-push label labeled-commands)
            do (vector-push command labeled-commands))
    labeled-commands))

(defmethod decode-sequence ((interpreter table-interpreter) sequence
                            &key (start 0) end)
  (when (null end) (setf end (length sequence)))
  (use-sequence interpreter sequence :start start)
  (setf (label-table interpreter) (make-hash-table))
  (let ((end (or end (length sequence)))
        (disassembled-commands (make-array 8 :adjustable t :fill-pointer 0)))
    (loop while (< (buffer-position interpreter) end)
          do (let ((start-position (buffer-position interpreter))
                   (instruction (%decode-instruction interpreter)))
               (setf (start-position instruction) start-position)
               (vector-push-extend instruction disassembled-commands)
               (when (not (null (command-tracking-debug interpreter)))
                 (assert (c:instruction-descriptor-equal
                          (instruction-descriptor instruction)
                          (c:best-candidate-descriptor
                           (current-command-debug interpreter))))
                 (increment-command-debug interpreter))
               (reset interpreter)))
    (insert-labels interpreter disassembled-commands)))
