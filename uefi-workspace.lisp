(uiop:define-package :uefi-workspace
  (:use :uiop/common-lisp :asdf)
  (:use-reexport :borax-build/c-testing))

(in-package :uefi-workspace)

(defun test ()
  (asdf:test-system :borax-virtual-machine))
