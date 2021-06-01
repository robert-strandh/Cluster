(cl:in-package #:cluster.disassembler)
;;;; When modrm.rm is 4, then you use SIB mode.
;;;; but but MOD still tells you the size of the displacement
;;;; every other time R/M is using a source register
;;;; and you use MOD to figure out if there is a displacement, 8bit displacement
;;;; or 32bit displacement
;;;; See http://www.c-jump.com/CIS77/CPU/x86/lecture.html#X77_0090_addressing_modes

;;;; This file has several problems at the minute
;;;; for instance we don't care at all for sign of displacment which is really bad.
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

(defun unsigned-to-signed (size unsigned-integer)
  (let ((max+1 (expt 2 size)))
    (if (>= unsigned-integer (ash max+1 -1))
        (- unsigned-integer max+1)
        unsigned-integer)))

(defun read-from-array (size array position)
  (let ((result 0))
    (loop for i from 0 upto (- size 8) by 8
          for current-byte-position from position
          do (let ((current-byte (aref array current-byte-position)))
               (setf (ldb (byte 8 i) result)
                     current-byte)))
    result))

(defun register-number<-rex+modrm (rex modrm)
  (let ((modrm.reg (modrm.reg modrm)))
    (setf (ldb (byte 1 3) modrm.reg) (rex.r rex))
    modrm.reg))

(defun rm-operand<-modrm (rex modrm vector position)
  (flet ((base-register-number<-modrm+rex (modrm rex)
           (if (zerop rex)
               (modrm.rm modrm)
               (+ (rex.b rex) (modrm.rm modrm))))
         (scale-factor<-sib (sib)
           (case (sib.s sib)
             (#b00 1)
             (#b01 2)
             (#b10 4)
             (#b11 8))))
    (cond
      ((= #b100 (modrm.rm modrm))
       (let* ((sib (aref vector position))
              (index (+ (rex.x rex)
                        (sib.i sib)))
              (base (+ (rex.b rex)
                       (sib.b sib)))
              (displacement
                (if (= 0 (modrm.mod modrm))
                    nil
                    (let ((size (if (= 1 (modrm.rm modrm)) 8 32)))
                      (read-from-array size vector (1+ position))))))
         (cluster:make-memory-operand (if (zerop rex) 32 64)
                                      :base-register base
                                      :index-register index
                                      :scale (scale-factor<-sib sib)
                                      :displacement displacement)))
      (t
       (flet ((read-indirect-with-displacement ()
                (let ((displacement-size (if (= (modrm.mod modrm) 1) 8 32)))
                  (cluster:make-memory-operand
                   (if (zerop rex) 32 64)
                   :base-register (base-register-number<-modrm+rex modrm rex)
                   :displacement (read-from-array displacement-size
                                                  vector
                                                  position)))))
         (case (modrm.mod modrm)
           (0
            (cluster:make-memory-operand
             (if (zerop rex) 32 64)
             :base-register (base-register-number<-modrm+rex modrm rex)))
           (1 (read-indirect-with-displacement))
           (2 (read-indirect-with-displacement))
           (3 (cluster:make-gpr-operand
               (if (zerop rex) 32 64)
               (base-register-number<-modrm+rex modrm rex)))))))))

(defun register-operand<-rex+modrm (rex modrm)
  (cluster:make-gpr-operand
   (if (zerop rex) 32 64)
   (register-number<-rex+modrm rex modrm)))

(defgeneric operand-descriptor<-cluster-operand (operand)
  (:method ((operand cluster::memory-operand))
    `(cluster::memory ,(cluster:size operand)))
  (:method ((operand cluster::gpr-operand))
    `(cluster::gpr ,(cluster::size operand))))
