(cl:in-package #:cluster.disassembler)
;;;; See http://www.c-jump.com/CIS77/CPU/x86/lecture.html#X77_0090_addressing_modes
;;;; See https://wiki.osdev.org/X86-64_Instruction_Encoding#ModR.2FM_and_SIB_bytes
(declaim (inline modrm.mod modrm.reg modrm.rm rex.b rex.x rex.r sib.b sib.i sib.s))

(defun modrm.mod (modrm)
  (ldb (byte 2 6) modrm))

(defun modrm.rm (modrm)
  (ldb (byte 3 0) modrm))

(defun modrm.reg (modrm)
  (ldb (byte 3 3) modrm))

(defun rex.b (rex)
  (ldb (byte 1 0) rex))

(defun rex.x (rex)
  (ldb (byte 1 1) rex))

(defun rex.r (rex)
  (ldb (byte 1 2) rex))

(defun sib.b (sib)
  (ldb (byte 3 0) sib))

(defun sib.i (sib)
  (ldb (byte 3 3) sib))

(defun sib.s (sib)
  (ldb (byte 2 6) sib))

(defun register-number<-rex+modrm (rex modrm)
  (let ((modrm.reg (modrm.reg modrm)))
    (setf (ldb (byte 1 3) modrm.reg) (rex.r rex))
    modrm.reg))

(defun scale-factor<-sib (sib-byte)
  (case (sib.s sib-byte)
    (#b00 1)
    (#b01 2)
    (#b10 4)
    (#b11 8)))

(defun decode-r/m-with-32/64-addressing (interpreter operand-size)
  (labels ((base-register-number<-modrm+rex (modrm-byte rex)
             (if (zerop rex)
                 (modrm.rm modrm-byte)
                 (+ (ash (rex.b rex) 3)
                    (modrm.rm modrm-byte))))

           (read-indirect-with-displacement (rex modrm-byte)
             (let ((displacement-size (if (= (modrm.mod modrm-byte) 1) 8 32)))
               (cluster:make-memory-operand
                operand-size
                :base-register (base-register-number<-modrm+rex modrm-byte rex)
                :displacement
                (read-signed-integer interpreter displacement-size)))))
    (let ((modrm-byte (modrm-byte interpreter))
          (rex        (rex-value (state-object interpreter))))
      (cond
        ((and (= #b100 (modrm.rm modrm-byte))
              (not (= #b11 (modrm.mod modrm-byte))))
         (let* ((sib-byte (read-next-byte interpreter))
                (index (+ (ash (rex.x rex) 3)
                          (sib.i sib-byte)))
                (base (+ (ash (rex.b rex) 3)
                         (sib.b sib-byte)))
                (displacement-size (if (= 1 (modrm.mod modrm-byte)) 8 32))
                (displacement
                  (if (= 0 (modrm.mod modrm-byte))
                      nil
                      (read-signed-integer interpreter displacement-size))))
           (if (= index #b0100)
               ;; when REX.X/ModRM 0.100 then this means only the base
               ;; register is encoded with a mandatory displacement
               ;; see SIB table.
               (c:make-memory-operand operand-size
                                      :base-register base
                                      :displacement displacement)
               (cluster:make-memory-operand
                operand-size
                :base-register base
                :index-register index
                :scale (scale-factor<-sib sib-byte)
                :displacement displacement))))

        ;; MOD 00, R/M 101
        ((and (= #b00 (modrm.mod modrm-byte))
              (= #b101 (modrm.rm modrm-byte)))
         (let* ((sib-byte (read-next-byte interpreter))
                (index (+ (ash (rex.x rex) 3)
                          (sib.i sib-byte)))
                (base  (+ (ash (rex.b rex) 3)
                          (sib.b sib-byte)))
                (scale (scale-factor<-sib sib-byte)))
           ;; do not encode the base register when BP or R13 is given
           ;; ie RIP/EIP addressing in long mode
           (if (= (sib.b sib-byte) #b101)
               ;; displacement is always given and 32bit in EIP
               (let ((displacement (read-signed-integer interpreter 32)))
                 (if (= index #b100)
                     ;; when the index register is 4 then it is not provided.
                     (c:make-memory-operand operand-size
                                            :displacement displacement)
                     (c:make-memory-operand
                      operand-size
                      :index-register index
                      :scale scale
                      :displacement displacement)))
               ;; everything else is [base + (index * scale)]
               (c:make-memory-operand
                operand-size
                :base-register base
                :index-register index
                :scale scale))))
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
