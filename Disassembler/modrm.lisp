(cl:in-package #:cluster.disassembler)
;;;; When modrm.rm is 4, then you use SIB mode.
;;;; but but MOD still tells you the size of the displacement
;;;; every other time R/M is using a source register
;;;; and you use MOD to figure out if there is a displacement, 8bit displacement
;;;; or 32bit displacement
;;;; See http://www.c-jump.com/CIS77/CPU/x86/lecture.html#X77_0090_addressing_modes
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
