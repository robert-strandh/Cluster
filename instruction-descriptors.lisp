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
