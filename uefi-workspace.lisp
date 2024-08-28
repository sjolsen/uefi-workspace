(uiop:define-package :uefi-workspace
  (:mix :uiop/common-lisp :borax-virtual-machine/image)
  (:use :borax-build/c-testing)
  (:use-reexport :uefi-workspace/edk2)
  (:export #:reload #:test-c #:test-lisp #:test))

(in-package :uefi-workspace)

(defun reload ()
  (asdf:load-system :uefi-workspace))

(defun test-c (arch memory-model)
  (let* ((test-base (uiop:merge-pathnames*
                     (format nil "Borax/DEBUG_GCC/~A/" (string arch))
                     *build-dir*))
         (test-bin (uiop:merge-pathnames* #P"BoraxVirtualMachineTest" test-base))
         (test-file (uiop:merge-pathnames* #P"TestFile.bxo" test-base)))
    (make-test-file test-file memory-model)
    (build #P"BoraxPkg/BoraxPkg.dsc" :arch arch)
    (uiop:run-program (list "valgrind" "--error-exitcode=1"
                            (namestring test-bin)
                            (namestring test-file))
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
