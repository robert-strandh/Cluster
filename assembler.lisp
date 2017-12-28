(cl:in-package :cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some terminology.
;;;
;;; An OPERATION is an abstract concept.  Examples of operations are
;;; ADDITION, UNCONDITIONAL JUMP, RETURN FROM FUNCTION CALL, etc.  By
;;; definition, to each operation corresponds a unique MNEMONIC, such
;;; as ADD, MOV, CALL, etc.
;;;
;;; Each elementary step of a computation involves executing some
;;; operation with a sequence of OPERANDS.  An operand can be a
;;; REGISTER (General purpose, floating point, flags, etc), an
;;; EFFECTIVE ADDRESS denoting a sequence of locations in memory, or
;;; an IMMEDIATE operand.  Each operand has a SIZE associated with it
;;; which determines the number of bits involved in the operation.
;;; The different operands involved in a compuational step do not
;;; necessarily have the same size.  For instance, an 8-bit immedate
;;; can be used as a source operand when the target operand is a
;;; 64-bit general-purpose register.
;;;
;;; An INSTRUCTION is a sequence of between 1 and 15 bytes in memory
;;; that, when executed, will accomplish the intended operation with
;;; the operands that are given.  This sequence of bytes consists of
;;; some PREFIX bytes, one or two bytes of OPERATION CODE (or OPCODE
;;; for short), a ModRM byte, a SIB byte, a sequence (containing 1, 2,
;;; or 4 bytes) of bytes of DISPLACEMENT, and a sequence (containing
;;; 1, 2, or 4 bytes) of bytes of IMMEDIATES.  Only the OPCODE is
;;; mandatory.  The ModRM byte may contain an OPCODE EXTENSION.
;;; Together, the opcode and the opcode extension (when present)
;;; determine the operation to be accomplished.
;;;
;;; For each operation, there are typically several different opcodes,
;;; depending on the operands involved in the elementatry computation
;;; step.
;;;
;;; The input to Cluster is a sequence ABSTRACT COMMAND.  When the
;;; command represents an instruction, the abstract command contains a
;;; complete description of the operation and operands involved,
;;; except that the exact magnitude of some numeric values may not be
;;; completely known at this point.  Step 1 of the assembly process
;;; consists of computing the SIZE (i.e. the number of bytes that will
;;; be emitted) of each abstract command.  Step 2 assigns numeric
;;; values to symbolic labels, which will determine the missing
;;; numeric magnitudes.  Step 3 converts the abstract command to an
;;; INSTRUCTION or a sequence of DATA BYTES.  Finally, the resulting
;;; sequence of bytes is emitted.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Given an instruction descriptor and the operands to the command
;;; that the instruction descriptor matches, compute the size of the
;;; resulting instruction.

(defgeneric instruction-size-1 (desc operand))

(defmethod instruction-size-1 (desc operand)
  (length (encode-instruction-1 desc operand)))

(defmethod instruction-size-1 (desc (operand label))
  (let ((type (first (encoding desc))))
    (ecase type
      (label
       (+ (if (operand-size-override desc) 1 0)
          (if (rex.w desc) 1 0)
          (length (opcodes desc))
          (/ (second (first (operands desc))) 8))))))

(defgeneric instruction-size-2 (desc operand1 operand2))

(defmethod instruction-size-2 (desc operand1 operand2)
  (length (encode-instruction-2 desc operand1 operand2)))

(defun instruction-size (desc operands)
  (ecase (length operands)
    (0 (length (opcodes desc)))
    (1 (instruction-size-1 desc (first operands)))
    (2 (instruction-size-2 desc (first operands) (second operands)))))

;;; From a list if items and a list of preliminary sizes, compute a
;;; dictionary (represented as a hash table) mapping items to
;;; preliminary addresses relative to the beginning of the program.
(defun compute-preliminary-addresses (items preliminary-sizes)
  (loop with table = (make-hash-table :test #'eq)
        for absolute-address = 0 then (+ absolute-address size)
        for size in preliminary-sizes
        for item in items
        do (when (typep item 'label)
             (setf (gethash item table) absolute-address))
        finally (return table)))

(defun assemble (items)
  (let* ((preliminary-sizes (mapcar #'preliminary-size items))
         (addresses (compute-preliminary-addresses items preliminary-sizes)))
    (let* ((*addresses* addresses)
           (encodings (loop for item in items
                            for address = 0 then (+ address size)
                            for size in preliminary-sizes
                            collect
                            (let ((*instruction-pointer* (+ address size)))
                              (compute-encoding item)))))
      (coerce (reduce #'append encodings :from-end t)
              '(simple-array (unsigned-byte 8) (*))))))
