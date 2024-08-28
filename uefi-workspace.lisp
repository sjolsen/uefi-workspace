(uiop:define-package :uefi-workspace
  (:use :uiop/common-lisp)
  (:use-reexport :uefi-workspace/edk2 :borax-build/c-testing)
  (:export #:reload #:test-c #:test-lisp #:test))

(in-package :uefi-workspace)

(defun reload ()
  (asdf:load-system :uefi-workspace))

(defun test-c ()
  (let ((arch-files (make-test-files)))
    (loop for (arch . test-file) in arch-files
          for test-bin = (uiop:merge-pathnames*
                           (format nil "Borax/DEBUG_GCC/~A/BoraxVirtualMachineTest"
                                   (string arch))
                           *build-dir*)
          do (build #P"BoraxPkg/BoraxPkg.dsc" :arch arch)
          do (uiop:run-program (list "valgrind" "--error-exitcode=1"
                                     (namestring test-bin)
                                     (namestring test-file))
                               :error-output t))))

(defun test-lisp ()
  (asdf:test-system :borax-virtual-machine))

(defun test ()
  (test-lisp)
  (test-c))
