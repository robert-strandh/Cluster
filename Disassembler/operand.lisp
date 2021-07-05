(cl:in-package #:cluster.disassembler)

(defgeneric read-operand (interpreter encoding operand-size candidates))

(defmethod read-operand (interpreter (encoding (eql 'c:modrm)) operand-size
                         candidates)
  (declare (ignore encoding candidates))
  (decode-r/m-with-32/64-addressing interpreter operand-size))

(defmethod read-operand (interpreter (encoding (eql 'c:reg)) operand-size
                         candidates)
  (declare (ignore encoding candidates))
  (cluster:make-gpr-operand
   operand-size
   (register-number<-rex+modrm (rex-value (state-object interpreter))
                               (modrm-byte interpreter))))

(defmethod read-operand (buffer (encoding (eql 'c:imm)) operand-size candidates)
  (declare (ignore encoding candidates))
  (c:make-immediate-operand (read-signed-integer buffer operand-size)))

(defmethod read-operand (buffer (encoding (eql 'c:-)) operand-size candidates)
  (declare (ignore encoding))
  ;; we only know of GPR-A and also we aren't sure of how to represent
  ;; the operand position at this point and candidates is only being
  ;; passed to this GF for this situtation.
  (let ((gpr-a (assoc 'c:gpr-a (c:operands (first candidates)))))
    (assert (not (null gpr-a)))
    (c:make-gpr-operand (cadr gpr-a) 0)))

(defmethod read-operand (buffer (encoding (eql 'c:label)) operand-size candidates)
  (declare (ignore encoding candidates))
  ;; Label does not mean RIP-relative addressing via modrm
  ;; it means displacement immediatley following the instruction opcodes
  ;; with no modrm/sib present.
  ;; in the intel manual this is described as 'rel' (8, 16 or 32) in 3.1.1.3
  (let ((displacement (read-signed-integer buffer 32)))
    (intern-label buffer displacement)))

(defmethod read-operand (interpreter (encoding (eql 'c:+r)) operand-size candidates)
  (declare (ignore encoding candidates))
  (let ((register-number
          (ldb (byte 3 0) (last-opcode-byte interpreter))))
    (setf (ldb (byte 4 0) register-number)
          (rex.b (rex-value (state-object interpreter))))
    (c:make-gpr-operand operand-size register-number)))
