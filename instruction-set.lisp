(cl:in-package #:cluster)

(defgeneric instruction-descriptor-predicate (modifier-prefix)
  (:documentation "Return the symbol-name naming a predicate
that can be called on an instruction-descriptor for the prescence
of the prefix."))
(defgeneric prefix-name (modifier-prefix))
(defclass prefix () ())

(defclass modifier-prefix (prefix) ())
(defgeneric prefix-opcode (modifier-prefix))
;;; TODO
;;; We may also need some way to validate the modifier prefix
;;; can be used with a given instruction.

(defparameter *modifier-prefixes* (make-hash-table))

(defclass standard-modifier-prefix (modifier-prefix)
  ((%prefix-name :initarg :prefix-name
                 :reader prefix-name)
   (%instruction-descriptor-predicate
    :initarg :instruction-descriptor-predicate
    :reader instruction-descriptor-predicate)
   (%prefix-opcode :initarg :prefix-opcode
                   :reader prefix-opcode)))

(defmacro define-modifier-prefix (name predicate opcode)
  `(setf (gethash ',name *modifier-prefixes*)
         (make-instance 'standard-modifier-prefix
                        :prefix-name ',name
                        :instruction-descriptor-predicate ',predicate
                        :prefix-opcode ',opcode)))

(defun find-modifier-prefix (name)
  (gethash name *modifier-prefixes*))

(defgeneric opcode-range-start (range-prefix))
(defgeneric opcode-range-end (range-prefix))
(defclass range-prefix (prefix)
  ((%opcode-range-start :initarg :opcode-range-start
                        :reader opcode-range-start)
   (%opcode-range-end :initarg :opcode-range-end
                      :reader opcode-range-end)
   (%prefix-name :initarg :prefix-name
                 :reader prefix-name)
   (%bitflag-prefixes :initarg :bitflag-prefixes
                       :reader bitflag-prefixes
                       :initform '())))

(defgeneric prefix-opcode-bitmask (bitflag-prefix))
(defclass bitflag-prefix (prefix)
  ((%prefix-opcode-bitmask :initarg :prefix-opcode-bitmask
                           :reader prefix-opcode-bitmask)
   (%instruction-descriptor-predicate
    :initarg :instruction-descriptor-predicate
    :reader instruction-descriptor-predicate)))

(defparameter *range-prefixes* (make-hash-table))
(defun find-range-prefix (name)
  (gethash name *range-prefixes*))

(defmacro define-range-prefix
    (prefix-name (start end) &body bitflag-prefix-pairs)
  (flet ((make-bitflag-prefix-forms (bitflag-prefix-pairs)
           (loop for pair in bitflag-prefix-pairs
                 collect `(make-instance
                           'bitflag-prefix
                           :prefix-opcode-bitmask ',(first pair)
                           :instruction-descriptor-predicate ',(second pair)))))

    `(setf (gethash ',prefix-name *range-prefixes*)
           (make-instance 'range-prefix
                          :opcode-range-start ',start
                          :opcode-range-end ',end
                          :prefix-name ',prefix-name
                          :bitflag-prefixes (list ,@(make-bitflag-prefix-forms
                                                     bitflag-prefix-pairs))))))

;;;


(defclass instruction-set () ())
(defgeneric modifier-prefixes (instruction-set)
  (:documentation "Returns the
prefixes that are modifier prefixes for this set as a list."))
(defgeneric (setf modifier-prefixes) (new-set instruction-set))
(defgeneric table-prefixes (instruction-set)
  (:documentation "Returns a list of table prefixes
associated with the instruction set."))
(defgeneric encodings (instruction-set)
  (:documentation "Returns a list of encoding
types that are associated with this instruction-set."))
(defgeneric range-prefixes (instruction-set))
(defgeneric set-name (instruction-set))

(defclass standard-instruction-set (instruction-set)
  ((%modifier-prefixes :initarg :modifier-prefixes
                       :accessor modifier-prefixes)
   (%encodings :initarg :encodings
               :accessor encodings)
   (%range-prefixes :initarg :range-prefixes
                    :reader range-prefixes)
   (%set-name :initarg :set-name
              :reader set-name)))

(defparameter *instruction-sets* (make-hash-table))
(defun find-instruction-set (name)
  (gethash name *instruction-sets*))

(defmacro define-instruction-set (name (&key modifier-prefixes
                                          encodings
                                          range-prefixes))
  `(setf (gethash ',name *instruction-sets*)
         (make-instance 'standard-instruction-set
                        :modifier-prefixes
                        (loop for prefix in ',modifier-prefixes
                              collect (find-modifier-prefix prefix))
                        :encodings ',encodings
                        :set-name ',name
                        :range-prefixes
                        (loop for prefix in ',range-prefixes
                              collect (find-range-prefix prefix)))))

(defun next-opcode-table-pass (opcode-byte-position existing-table)
  (let ((table (make-array 256 :initial-element nil))
        (old-copy (make-array 256 :initial-element nil)))
    (loop for candidates across existing-table
          for i from 0 below 256
          if (and (listp candidates)
                    (not (null candidates))
                    (every (lambda (x)
                             (>= (length (opcodes x))
                                 (1+ opcode-byte-position)))
                           candidates))
            do (loop for candidate in candidates
                     do (push candidate
                         (aref table
                               (nth opcode-byte-position (opcodes candidate)))))
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
  (loop for modifier-prefix in (modifier-prefixes instruction-set)
        do (setf (aref array-table 0 (prefix-opcode modifier-prefix))
                 modifier-prefix))
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
;;;; decoder interprete state mixin

(defgeneric instruction-set (decoder-state))
(defgeneric reset-state (decoder-state))
(defclass decoder-state () ())

(defun prefix-slot-name<-prefix (prefix)
  (intern (format nil "%~w" (prefix-name prefix))))

(defun prefix-accessor-name<-prefix (prefix)
  "Possibly redundant because of the prefix predicate."
  (intern (format nil "~w-PRESENT-P" (prefix-name prefix))))

(defun decoder-register-slots<-modifier-prefixes (modifier-prefixes)
  (loop for prefix in modifier-prefixes
        collect `(,(prefix-slot-name<-prefix prefix)
                  :accessor ,(prefix-accessor-name<-prefix prefix)
                  :accessor ,(instruction-descriptor-predicate prefix)
                  :initform nil)))

(defun prefix-value-slot-name<-prefix (prefix)
  (intern (format nil "%~w-VALUE" (prefix-name prefix))))

(defun prefix-value-slot-accessor<-prefix (prefix)
  (intern (format nil "~w-VALUE" (prefix-name prefix))))

(defun decoder-register-slots<-range-prefixes (range-prefixes)
  (loop for prefix in range-prefixes
        collect `(,(prefix-value-slot-name<-prefix prefix)
                  :accessor ,(prefix-value-slot-name<-prefix prefix)
                  :initform nil)))

(defun bitflag-accessors<-range-prefixes (set range-prefixes)
  (flet ((method-definition (set range-prefix prefix)
           `(defmethod ,(instruction-descriptor-predicate prefix)
                ((state ,(interpreter-state-class-name<-instruction-set set)))
              (logtest ',(prefix-opcode-bitmask prefix)
                       (,(prefix-value-slot-accessor<-prefix range-prefix)
                        state)))))
    (loop for range-prefix in range-prefixes
          append (loop for prefix in (bitflag-prefixes range-prefix)
                       collect (method-definition set range-prefix prefix)))))

(defun decoder-state-slots (instruction-set)
  (list
   `(%instruction-set
     :reader instruction-set
     :initform (find-instruction-set ',(set-name instruction-set))
     :allocation :class)))

(defun interpreter-state-class-name<-instruction-set (instruction-set)
  (intern (format nil "~w-STATE" (set-name instruction-set))))

(defun interpreter-state-class<-instruction-set (instruction-set direct-superclasses)
  `(defclass ,(interpreter-state-class-name<-instruction-set instruction-set)
       ,`(decoder-state . ,direct-superclasses)
     ,(append
       (decoder-register-slots<-modifier-prefixes
        (modifier-prefixes instruction-set))
       (decoder-register-slots<-range-prefixes
        (range-prefixes instruction-set))
       (decoder-state-slots instruction-set))))

;;; TODO
;;; do we need a presentp for a range prefix?
(defun reset-state-method-definition<-instruction-set (instruction-set)
  `(defmethod reset-state ((state ,(interpreter-state-class-name<-instruction-set
                                    instruction-set)))
     ,@ (loop for prefix in (modifier-prefixes instruction-set)
              collect `(setf (,(prefix-accessor-name<-prefix prefix) state)
                             nil))
     ,@ (loop for prefix in (range-prefixes instruction-set)
              collect `(setf (,(prefix-value-slot-accessor<-prefix prefix) state)
                             0))))

(defgeneric make-decoder/interpreter-state-definition
    (instruction-set direct-superclasses)
  (:method ((instruction-set instruction-set) direct-superclasses)
    `(progn
       ,(interpreter-state-class<-instruction-set instruction-set direct-superclasses)
       ,(reset-state-method-definition<-instruction-set instruction-set)
       ,@(bitflag-accessors<-range-prefixes instruction-set
                                           (range-prefixes instruction-set)))))

(defmacro define-deocder/interpreter (instruction-set-name (&rest direct-superclasses))
  (make-decoder/interpreter-state-definition
   (find-instruction-set instruction-set-name)
   direct-superclasses))
