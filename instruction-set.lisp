(cl:in-package #:cluster)

(defgeneric instruction-descriptor-predicate (modifier-prefix)
  (:documentation "Return the symbol-name naming a predicate
that can be called on an instruction-descriptor for the prescence
of the prefix."))
(defgeneric prefix-name (modifier-prefix))
(defclass prefix () ())

(defclass interpreter-writable-prefix-mixin ()
  (;; Technically this is something the decoder/interpreter
   ;; uses and should annotate these instances with, not really sure
   ;; if it matters that they're here though.
   (%interpreter-state-writer
    :initarg :interpreter-state-writer
    :accessor interpreter-state-writer)))

;;; TODO
;;; We may also need some way to validate the modifier prefix
;;; can be used with a given instruction.
(defclass modifier-prefix (prefix interpreter-writable-prefix-mixin) ())
(defgeneric prefix-opcode (modifier-prefix))

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
(defclass range-prefix (prefix interpreter-writable-prefix-mixin)
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
(defgeneric encodings (instruction-set)
  (:documentation "Returns a list of encoding
types that are associated with this instruction-set."))
(defgeneric range-prefixes (instruction-set))
(defgeneric instruction-set-name (instruction-set))

(defclass standard-instruction-set (instruction-set)
  ((%modifier-prefixes :initarg :modifier-prefixes
                       :accessor modifier-prefixes)
   (%encodings :initarg :encodings
               :accessor encodings)
   (%range-prefixes :initarg :range-prefixes
                    :reader range-prefixes)
   (%set-name :initarg :instruction-set-name
              :reader instruction-set-name)))

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
                        :instruction-set-name ',name
                        :range-prefixes
                        (loop for prefix in ',range-prefixes
                              collect (find-range-prefix prefix)))))
