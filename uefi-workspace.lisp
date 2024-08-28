(uiop:define-package :uefi-workspace
  (:mix :uiop/common-lisp :borax-virtual-machine/image)
  (:use :borax-build/c-testing)
  (:use-reexport :uefi-workspace/edk2)
  (:export #:reload #:test-c #:test-lisp #:test))

(in-package :uefi-workspace)

(defun reload ()
  (asdf:load-system :uefi-workspace))

(defun test-c (arch memory-model)
  (let* ((test-base (join *build-dir* (format nil "Borax/DEBUG_GCC/~A/" (string arch))))
         (test-bin (join test-base #P"BoraxVirtualMachineTest"))
         (test-file (join test-base #P"TestFile.bxo")))
    (make-test-file test-file memory-model)
    (build #P"BoraxPkg/BoraxPkg.dsc" :arch arch)
    (run-program (list "valgrind" "--error-exitcode=1" test-bin test-file)
                 :output t
                 :error-output t)))

(defun test-lisp ()
  (asdf:test-system :borax-virtual-machine))

(defun test ()
  (test-lisp)
  (fresh-line)
  (test-c :IA32 +32-bit+)
  (test-c :X64 +64-bit+)
  (values))
