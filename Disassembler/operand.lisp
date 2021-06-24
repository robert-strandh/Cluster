(cl:in-package #:cluster.disassembler)

(defgeneric read-operand (interpreter encoding operand-size candidates))

(defmethod read-operand (interpreter (encoding (eql 'c:modrm)) operand-size
                         candidates)
  (declare (ignore encoding candidates))
  (labels ((base-register-number<-modrm+rex (modrm-byte rex)
             (if (zerop rex)
                 (modrm.rm modrm-byte)
                 (+ (rex.b rex) (modrm.rm modrm-byte))))

           (read-indirect-with-displacement (rex modrm-byte)
             (let ((displacement-size (if (= (modrm.mod modrm-byte) 1) 8 32)))
               (cluster:make-memory-operand
                operand-size
                :base-register (base-register-number<-modrm+rex modrm-byte rex)
                :displacement
                (read-unsigned-integer interpreter displacement-size)))))
    (let ((modrm-byte (modrm-byte interpreter))
          (rex        (rex-value (state-object interpreter))))
      (cond
        ((= #b100 (modrm.rm modrm-byte))
         (let* ((sib-byte (read-next-byte interpreter))
                (index (+ (rex.x rex)
                          (sib.i sib-byte)))
                (base (+ (rex.b rex)
                         (sib.b sib-byte)))
                (displacement-size (if (= 1 (modrm.mod modrm-byte)) 8 32))
                (displacement
                  (if (= 0 (modrm.mod modrm-byte))
                      nil
                      (read-unsigned-integer interpreter displacement-size))))
           (cluster:make-memory-operand operand-size
                                        :base-register base
                                        :index-register index
                                        :scale (scale-factor<-sib sib-byte)
                                        :displacement displacement)))
        (t
         (case (modrm.mod modrm-byte)
           (0
            (cluster:make-memory-operand
             operand-size
             :base-register (base-register-number<-modrm+rex modrm-byte rex)))
           (1 (read-indirect-with-displacement rex modrm-byte))
           (2 (read-indirect-with-displacement rex modrm-byte))
           (3 (cluster:make-gpr-operand
               operand-size
               (base-register-number<-modrm+rex modrm-byte rex)))))))))

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
