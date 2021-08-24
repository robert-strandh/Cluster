(cl:in-package #:cluster.disassembler)

(defun make-simple-add ()
  #(#b01001000 ;rex.w
    #x05))

(defparameter *x86-table*
  (make-opcode-table cluster::*instruction-descriptors-by-first-opcode*
                     (cluster:find-instruction-set 'cluster::x86)))

;;; TODO
;;; maybe we need to add a config/client
;;; to the macro-generator that has things like the class name in it
(define-decoder/interpreter cluster::x86 ())
