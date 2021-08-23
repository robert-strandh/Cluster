(cl:in-package #:cluster-test.disassembler)

(defclass code-command-generator ()
  ((%interned-labels :reader interned-labels
                     :initform (make-array 8 :fill-pointer 0 :adjustable t))))
(defun make-code-command-generator ()
  (make-instance 'code-command-generator))


;;; So the purpose of this is to generate an operand
;;; we don't need to provide every single boundary/edge case
;;; to one instruction descriptor test
;;; just choose one as there should be enough instruction descriptors
;;; requiring operands of the same type to test them all.
(defgeneric generate-operand (generator operand-descriptor encoding operand-size))

(defgeneric make-label (code-command-generator)
  (:method ((code-command-generator code-command-generator))
    (let ((label (c:make-label)))
      (vector-push-extend label (interned-labels code-command-generator))
      label)))

(defgeneric generate-code-command (code-command-generator instruction-descriptor))

(defmethod generate-code-command ((generator code-command-generator)
                                  instruction-descriptor)
  (c:make-code-command
   (c:mnemonic instruction-descriptor)
   (loop for (operand-descriptor operand-size) in (c:operands instruction-descriptor)
         for encoding in (c:encoding instruction-descriptor)
         collect (generate-operand generator
                                   operand-descriptor encoding
                                   operand-size))))

(defun shuffle-commands (commands)
  (loop for i from (length commands) downto 2
        do (rotatef (aref commands (random i))
                    (aref commands (1- i))))
  commands)

;;; honestly, this is a bit of a pain, so what this does at the moment
;;; is shuffles the labels and inserts them evenly across the
;;; sequence.
(defgeneric insert-labels (generator commands)
  (:method ((generator code-command-generator) commands)
    (let ((labels (shuffle-commands (interned-labels generator))))
      (assert (> (length commands) (length labels)))
      (let ((label-spacing (floor (length commands) (length labels)))
            (labeled-commands (make-array (+ (length commands)
                                             (length labels))))
            (commands-position 0))
        (loop for i from 0 below (length labeled-commands)
              if (and (< 0 (length labels)) (= 0 (mod i label-spacing)))
                do (setf (aref labeled-commands i)
                         (vector-pop labels))
              else do (setf (aref labeled-commands i)
                            (aref commands commands-position))
                   and do (incf commands-position))
        labeled-commands))))

;;; Accepts the mnemonic-candidates table from the Cluster database
;;; and produces a sequence of code-commands that
;;; should be tested, including their labels
(defgeneric generate-test-commands
    (generator instruction-database &key variants-per-descriptor))

(defmethod generate-test-commands
    ((generator code-command-generator) instruction-database
     &key (variants-per-descriptor 2))
  (let* ((commands (make-array 256 :fill-pointer 0 :adjustable t)))
    (maphash
     (lambda (mnemonic candidates)
       (declare (ignore mnemonic))
       (loop for candidate in candidates
             when (not (null (find 64 (c:modes candidate))))
             do (loop for i from 0 below variants-per-descriptor
                      for command = (generate-code-command generator candidate)
                      do (vector-push-extend command commands))))
     instruction-database)
    (let ((commands (shuffle-commands commands)))
      (insert-labels generator commands))))
