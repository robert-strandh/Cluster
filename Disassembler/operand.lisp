(cl:in-package #:cluster.disassembler)

(defun decode-immediate (size vector position)
  (unsigned-to-signed size (read-from-array size vector position)))

(defun decode-gpr-a-and-s/imm (a-size imm-size vector position)
  (list (c:make-gpr-operand a-size 0)
        (c:make-immediate-operand (decode-immediate imm-size vector position))))
