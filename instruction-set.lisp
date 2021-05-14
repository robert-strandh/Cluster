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

;;; TODO
;;; the instruction set might generate a class
;;; from the prefixes that have slots for
;;; the prefix predicates
;;; and the prefix opcode values
;;; this way it's easy to make a unified predicate for
;;; a range prefix without making 16 classes or something
;;; and then having to make a predicate that responds to all of them
;;; it also makes inspecting the decoder/interpreter state much easier
(defgeneric opcode-range-start (range-prefix))
(defgeneric opcode-range-end (range-prefix))
(defclass range-prefix (prefix)
  ((%opcode-range-start :initarg :opcode-range-start
                        :reader opcode-range-start)
   (%opcode-range-end :initarg :opcode-range-end
                      :reader opcode-range-end)
   (%prefix-name :initarg :prefix-name
                 :reader prefix-name)
   (%instruction-descriptor-predicate
    :initarg :instruction-descriptor-predicate
    :reader instruction-descriptor-predicate)))

(defparameter *range-prefixes* (make-hash-table))
(defun find-range-prefix (name)
  (gethash name *range-prefixes*))

(defmacro define-range-prefix (prefix-name prefix-predicate
                               start end)
  `(setf (gethash ',prefix-name *range-prefixes*)
         (make-instance 'range-prefix

                        :opcode-range-start ',start
                        :opcode-range-end ',end
                        :prefix-name ',prefix-name
                        :instruction-descriptor-predicate
                        ',instruction-descriptor-predicate)))



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

(defclass standard-instruction-set (instruction-set)
  ((%modifier-prefixes :initarg :modifier-prefixes
                       :accessor modifier-prefixes)
   (%encodings :initarg :encodings
               :accessor encodings)
   (%range-prefixes :initarg :range-prefixes
                    :reader range-prefixes)))

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
