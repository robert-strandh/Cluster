(in-package :cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction descriptor.

(defclass instruction-descriptor ()
  ((%mnemonic
    :initarg :mnemonic
    :reader mnemonic)
   (%modes
    :initarg :modes
    :reader modes)
   (%operands
    :initarg :operands
    :reader operands)
   (%opcodes
    :initarg :opcodes
    :reader opcodes)
   (%opcode-extension
    :initform nil
    :initarg :opcode-extension
    :reader opcode-extension)
   (%encoding
    :initarg :encoding
    :reader encoding)
   (%lock
    :initform nil
    :initarg :lock
    :reader lock)
   (%operand-size-override
    :initform nil
    :initarg :operand-size-override
    :reader operand-size-override)
   (%rex.w
    :initform nil
    :initarg :rex.w
    :reader rex.w)))

;;; This is used to determine if the mnemonic
;;; is an alias and the instructions are encoded
;;; exactly the same way
(defun instruction-descriptor-equal (desc1 desc2)
  (and
   (equal (modes desc1)                 (modes desc2))
   (equal (operands desc1)              (operands desc2))
   (equal (opcodes desc1)               (opcodes desc2))
   (equal (opcode-extension desc1)      (opcode-extension desc2))
   (equal (encoding desc1)              (encoding desc2))
   (equal (lock desc1)                  (lock desc2))
   (equal (operand-size-override desc1) (operand-size-override desc2))
   (equal (rex.w desc1)                 (rex.w desc2))))

(defmethod print-object ((object instruction-descriptor) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a" (mnemonic object) (operands object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Database of instruction descriptors.

(defparameter *instruction-descriptors* (make-hash-table :test #'equal))

(defun add-instruction-descriptor (instruction-descriptor)
  (push instruction-descriptor
        (gethash (mnemonic instruction-descriptor)
                 *instruction-descriptors*)))

(defun candidates (mnemonic operands)
  (loop for descriptor in (gethash mnemonic *instruction-descriptors*)
        when (operands-match-p operands (operands descriptor))
          collect descriptor))

(defparameter *instruction-descriptors-by-first-opcode*
  (make-array 256 :initial-element '())
  "The DEFINE-INSTRUCTION macro is already x86 specific, and this
the purpose of this is to hijack that macro in order to support the disassembler.")

(defun first-opcode-candidates (first-opcode)
  (aref *instruction-descriptors-by-first-opcode* first-opcode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFINE-INSTRUCTION.

(defmacro define-instruction (mnemonic &key
                                         modes
                                         operands
                                         opcodes
                                         opcode-extension
                                         encoding
                                         lock
                                         operand-size-override
                                         rex.w)
  `(let ((instruction-descriptor
           (make-instance 'instruction-descriptor
                          :mnemonic ,mnemonic
                          :modes ',modes
                          :operands ',operands
                          :opcodes ',opcodes
                          :opcode-extension ,opcode-extension
                          :encoding ',encoding
                          :lock ,lock
                          :operand-size-override ,operand-size-override
                          :rex.w ,rex.w)))
     (push instruction-descriptor
           (gethash ,mnemonic *instruction-descriptors*))
     (push instruction-descriptor
           (aref *instruction-descriptors-by-first-opcode*
                 (first ',opcodes)))))
