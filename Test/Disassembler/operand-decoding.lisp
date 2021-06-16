(cl:in-package #:cluster-test.disassembler)

(defun test-read-from-array ()
  (let ((array #(#b10000000 #b10000000)))
    (assert (= 32896 (cluster.disassembler::read-from-array 16 array 0)))))

(defun assert-memory-operand (operand &key base-register displacement
                                        index-register scale size)
  (assert (eql base-register (cluster::base-register operand)))
  (assert (eql displacement (cluster::displacement operand)))
  (assert (eql index-register (cluster::index-register operand)))
  (assert (eql scale (cluster::scale operand)))
  (assert (eql size (cluster::size operand))))

(defun test-operands-add+disp8 ()
  (let* ((|ADD EAX, [ESI + 6]| #(#b00000011 #b01000110 #b00000110))
         (memory-operand (cluster.disassembler::rm-operand<-modrm
                          0 (aref |ADD EAX, [ESI + 6]| 1) |ADD EAX, [ESI + 6]|
                          2)))
    (assert-memory-operand memory-operand
                           :displacement 6
                           :size 32
                           :base-register 6)))

(defun test-operands-add-with-scale ()
  (let* ((|ADD ECX, [ EBX + EDI*4 ]| #(#b00000011 #b00001100 #b10111011))
         (memory-operand
           (cluster.disassembler::rm-operand<-modrm
            0 (aref |ADD ECX, [ EBX + EDI*4 ]| 1) |ADD ECX, [ EBX + EDI*4 ]|
            2)))
    (assert-memory-operand memory-operand
                           :size 32
                           :scale 4
                           :base-register 3
                           :index-register 7)))

(defun test-operands-jmp-with-scale ()
  (let* ((|JMP [ EBX + EDI*4 ]| #(#xFF #b00100100 #b10111011))
         (memory-operand (cluster.disassembler::rm-operand<-modrm
                          0 (aref |JMP [ EBX + EDI*4 ]| 1) |JMP [ EBX + EDI*4 ]|
                          2))
         (opcode-extension
           (cluster.disassembler::opcode-extension<-rex+modrm
            0 (aref |JMP [ EBX + EDI*4 ]| 1))))
    (assert (= 4 opcode-extension))
    (assert-memory-operand memory-operand
                           :size 32
                           :scale 4
                           :base-register 3
                           :index-register 7)))

(defun test-gpr-gpr ()
  (let* ((and-rax-rbx
           (cluster:compute-encoding
            (cluster:make-code-command
             "AND"
             (list (c:make-gpr-operand 64 0)
                   (c:make-gpr-operand 64 1)))))
         (disassembled-code-command (decode-instruction and-rax-rbx)))
    (assert-decoded-descriptor disassembled-code-command
                               :mnemonic "AND"
                               :rex.w t
                               :operands '((c:gpr 64) (c:gpr 64)))))

(defun test-adc-r/m-gpr ()
  (let* ((adc-mem-gpr
           (c:compute-encoding
            (c:make-code-command
             "ADC"
             (list (c:make-memory-operand
                    16
                    :displacement 1969325217)
                   (c:make-gpr-operand 16 5)))))
         (disassembled-code-command (decode-instruction adc-mem-gpr)))
    (assert-decoded-descriptor disassembled-code-command
                               :mnemonic "ADC"
                               :operand-size-override t
                               :operands '((c:memory 16) (c:gpr 16)))))

(defun test-sub-imm32 ()
  ;; this wasn't working for some reason
  (let* ((sub-gpr-imm32
           (c:compute-encoding
            (c:make-code-command
             "SUB"
             (list (c:make-gpr-operand 32 3)
                   (c:make-immediate-operand (- (expt 2 32) 1))))))
         (disassembled-code-command (decode-instruction sub-gpr-imm32)))
    (assert-decoded-descriptor disassembled-code-command
                               :mnemonic "SUB"
                               :operands '((c:gpr 32) (c:imm 32))
                               :opcode-extension 5))
  (let* ((sub-memory-imm32
           (c:compute-encoding
            (c:make-code-command
             "SUB"
             (list (c:make-memory-operand 32 :base-register 3
                                             :index-register 1
                                             :scale 4
                                          :displacement 3)
                   (c:make-immediate-operand (- (expt 2 32) 1))))))
         (disassembled-code-command (decode-instruction sub-memory-imm32)))
    (assert-decoded-descriptor disassembled-code-command
                               :mnemonic "SUB"
                               :operands '((c:gpr 32) (c:imm 32))
                               :opcode-extension 5)))

(defun test-and-gpr/mem ()
  (let ((and-gpr-gpr
          (c:make-code-command
           "AND"
           (list (c:make-gpr-operand 16 3)
                 (c:make-gpr-operand 16 2)))))
    (assert-decoded-descriptor
     (decode-command-as-sequence and-gpr-gpr)
     :mnemonic "AND"
     :operands '((c:gpr 16) (c:gpr 16))
     :operand-size-override t)))

(defun test-bt-r/m32 ()
  (let ((bt-r/m32-r32
          (c:compute-encoding
           (c:make-code-command
            "BT"
            (list (c:make-gpr-operand 32 2)
                  (c:make-gpr-operand 32 4))))))
    (assert-decoded-descriptor (decode-instruction bt-r/m32-r32)
                               :mnemonic "BT"
                               :operands '((c:gpr 32) (c:gpr 32))))
  (let ((bt-mem32-r32
          (c:compute-encoding
           (c:make-code-command
            "BT"
            (list (c:make-memory-operand 32 :index-register 2 :scale 4)
                  (c:make-gpr-operand 32 2))))))
    (assert-decoded-descriptor (decode-instruction bt-mem32-r32)
                               :mnemonic "BT"
                               :operands '((c:memory 32) (c:gpr 32)))))

(defun test-xor-31 ()
  (let ((xor-gpr16-gpr16
          (c:compute-encoding
           (c:make-code-command
            "XOR"
            (list (c:make-gpr-operand 16 1)
                  (c:make-gpr-operand 16 2))))))
    (assert-decoded-descriptor (decode-instruction xor-gpr16-gpr16)
                               :mnemonic "XOR"
                               :operands '((c:gpr 16) (c:gpr 16))
                               :operand-size-override t)))

(test-operands-add-with-scale)
(test-operands-add+disp8)
