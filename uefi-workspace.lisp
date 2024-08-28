(uiop:define-package :uefi-workspace
  (:use :uiop/common-lisp)
  (:use-reexport :uefi-workspace/edk2 :borax-build/c-testing)
  (:export #:test))

(in-package :uefi-workspace)

(defun test ()
  (asdf:test-system :borax-virtual-machine))
