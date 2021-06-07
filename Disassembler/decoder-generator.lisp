(cl:in-package #:cluster.disassembler)

;;;;
;;;; table generator
;;;; This takes a table, in the same format as the mnemonic table
;;;; where instructions are interned by their first opcode.
;;;; From this table, we generate the lookup tables for the interpreter
;;;; to use.
(defun next-opcode-table-pass (opcode-byte-position existing-table)
  (let ((table (make-array 256 :initial-element nil))
        (old-copy (make-array 256 :initial-element nil)))
    (loop for candidates across existing-table
          for i from 0 below 256
          if (and (listp candidates)
                  (not (null candidates))
                  (every (lambda (x)
                           (>= (length (c:opcodes x))
                               (1+ opcode-byte-position)))
                         candidates))
            do (loop for candidate in candidates
                     do (push candidate
                              (aref table
                                    (nth opcode-byte-position
                                         (c:opcodes candidate)))))
            and do (setf (aref old-copy i) :next-table)
          else do (setf (aref old-copy i) (aref existing-table i)))
    (values table old-copy)))

(defun array-table<-list-of-tables (tables)
  (let ((array-table (make-array `(,(length tables) 256))))
    (loop for table in tables
          for i from 0
          do (loop for candidates across table
                   for j from 0
                   do (setf (aref array-table i j) candidates)))
    array-table))

(defun add-prefixes (array-table instruction-set)
  (loop for modifier-prefix in (c:modifier-prefixes instruction-set)
        do (setf (aref array-table 0 (c:prefix-opcode modifier-prefix))
                 modifier-prefix))
  (loop for range-prefix in (c:range-prefixes instruction-set)
        do (loop for i from (c:opcode-range-start range-prefix)
                 upto (c:opcode-range-end range-prefix)
                 do (setf (aref array-table 0 i)
                          range-prefix)))
  array-table)

(defun make-opcode-table (starting-table instruction-set)
  (let ((previous-table '(:next-table))
        (current-table starting-table)
        (tables '()))
    (loop while (not (null (find :next-table previous-table)))
          for i from 1
          do (multiple-value-bind (next-table next-previous)
                 (next-opcode-table-pass i current-table)
               (push next-previous tables)
               (setf previous-table next-previous)
               (setf current-table next-table)))
    (add-prefixes
     (array-table<-list-of-tables (nreverse tables))
     instruction-set)))

;;;;
;;;; decoder/interpreter
;;;; This generator produces as class for an interpreter to use
;;;; to show the state of any prefixes and opcodes that have been
;;;; collected so far from interpreting the table.
;;;; This state object can then select a matching instruction
;;;; descriptor from a list of candidates.

(defgeneric instruction-set (decoder-state))
(defgeneric reset (decoder-state))
(defgeneric narrow-down-candidates (decoder-state candidates))
(defgeneric supported-mode (decoder-state))
(defclass decoder-state ()
  ((%supported-mode :initarg :supported-mode
                    :reader supported-mode
                    :initform 64)))

(defun prefix-slot-name<-prefix (prefix)
  (intern (format nil "%~a" (c:prefix-name prefix))))

(defun prefix-accessor-name<-prefix (prefix)
  "This is not redundant, one is for the value of a prefix,
this is for whether it has been set or not and returns a boolean."
  (intern (format nil "~a-PRESENT-P" (c:prefix-name prefix))))

(defun decoder-register-slots<-modifier-prefixes (modifier-prefixes)
  (loop for prefix in modifier-prefixes
        collect `(,(prefix-slot-name<-prefix prefix)
                  :accessor ,(prefix-accessor-name<-prefix prefix)
                  :accessor ,(c:instruction-descriptor-predicate prefix)
                  :initform nil)))

(defun prefix-value-slot-name<-prefix (prefix)
  (intern (format nil "%~a-VALUE" (c:prefix-name prefix))))

(defun prefix-value-slot-accessor<-prefix (prefix)
  (intern (format nil "~a-VALUE" (c:prefix-name prefix))))

(defun decoder-register-slots<-range-prefixes (range-prefixes)
  (loop for prefix in range-prefixes
        collect `(,(prefix-value-slot-name<-prefix prefix)
                  :accessor ,(prefix-value-slot-accessor<-prefix prefix)
                  :initform 0)))

(defun bitflag-accessors<-range-prefixes (set range-prefixes)
  (flet ((method-definition (set range-prefix prefix)
           `(defmethod ,(c:instruction-descriptor-predicate prefix)
                ((state ,(interpreter-state-class-name<-instruction-set set)))
              (logtest ',(c:prefix-opcode-bitmask prefix)
                       (,(prefix-value-slot-accessor<-prefix range-prefix)
                        state)))))
    (loop for range-prefix in range-prefixes
          append (loop for prefix in (c:bitflag-prefixes range-prefix)
                       collect (method-definition set range-prefix prefix)))))

(defun decoder-state-slots (instruction-set)
  (list
   `(%instruction-set
     :reader instruction-set
     :initform (c:find-instruction-set ',(c:instruction-set-name instruction-set))
     :allocation :class)))

(defun interpreter-state-class-name<-instruction-set (instruction-set)
  (intern (format nil "~a-STATE"
                  (symbol-name (c:instruction-set-name instruction-set)))))

(defun interpreter-state-class<-instruction-set (instruction-set direct-superclasses)
  `(defclass ,(interpreter-state-class-name<-instruction-set instruction-set)
       ,`(decoder-state . ,direct-superclasses)
     ,(append
       (decoder-register-slots<-modifier-prefixes
        (c:modifier-prefixes instruction-set))
       (decoder-register-slots<-range-prefixes
        (c:range-prefixes instruction-set))
       (decoder-state-slots instruction-set))))

;;; TODO
;;; do we need a presentp for a range prefix?
(defun reset-state-method-definition<-instruction-set (instruction-set)
  `(defmethod reset ((state ,(interpreter-state-class-name<-instruction-set
                                    instruction-set)))
     ,@ (loop for prefix in (c:modifier-prefixes instruction-set)
              collect `(setf (,(prefix-accessor-name<-prefix prefix) state)
                             nil))
     ,@ (loop for prefix in (c:range-prefixes instruction-set)
              collect `(setf (,(prefix-value-slot-accessor<-prefix prefix) state)
                             0))))

(defun make-form-to-add-metadata-to-prefixes (instruction-set)
  `(let ((instruction-set
           (c:find-instruction-set ',(c:instruction-set-name instruction-set))))
     (loop for prefix in (c:modifier-prefixes instruction-set)
           do (setf (c:interpreter-state-writer prefix)
                    (prefix-accessor-name<-prefix prefix)))
     (loop for prefix in (c:range-prefixes instruction-set)
           do (setf (c:interpreter-state-writer prefix)
                    (prefix-value-slot-accessor<-prefix prefix)))))

(defun discriminator-test<-prefix (prefix state-var descriptor-var)
  (let ((predicate-symbol (c:instruction-descriptor-predicate prefix)))
    `(eq (,predicate-symbol ,state-var)
         (,predicate-symbol ,descriptor-var))))

(defun discriminator-method-definition<-instruction-set (instruction-set)
  `(defmethod narrow-down-candidates
       ((state ,(interpreter-state-class-name<-instruction-set instruction-set))
        candidates)
     (remove-if-not
      (lambda (candidate)
        (and
         (member (supported-mode state) (c:modes candidate))
         ,@ (loop for prefix
                    in (append (c:modifier-prefixes instruction-set)
                               (mapcan #'c:bitflag-prefixes
                                       (c:range-prefixes instruction-set)))
                  collect (discriminator-test<-prefix prefix 'state
                                                      'candidate))))
      candidates)))

(defgeneric make-decoder/interpreter-state-definition
    (instruction-set direct-superclasses)
  (:method ((instruction-set c:instruction-set) direct-superclasses)
    `(progn
       ,(interpreter-state-class<-instruction-set instruction-set direct-superclasses)
       ,(reset-state-method-definition<-instruction-set instruction-set)
       ,@(bitflag-accessors<-range-prefixes instruction-set
                                            (c:range-prefixes instruction-set))
       ,(make-form-to-add-metadata-to-prefixes instruction-set)
       ,(discriminator-method-definition<-instruction-set instruction-set))))

(defmacro define-decoder/interpreter (instruction-set-name (&rest direct-superclasses))
  (make-decoder/interpreter-state-definition
   (c:find-instruction-set instruction-set-name)
   direct-superclasses))
