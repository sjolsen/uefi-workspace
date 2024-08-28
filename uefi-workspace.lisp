(uiop:define-package :uefi-workspace
  (:mix :uiop/common-lisp :borax-virtual-machine/image)
  (:use :borax-build/c-testing)
  (:use-reexport :uefi-workspace/edk2)
  (:export #:reload
           #:uncrustify
           #:build-ovmf #:build-refinery
           #:test-c #:test-lisp #:test))

(in-package :uefi-workspace)

(defun reload ()
  (asdf:load-system :uefi-workspace))

(defun uncrustify ()
  (uncrustify-files (find-source-files (join *workspace* #P"refinery/"))))

(defun build-basetools ()
  (run-program (list "make" "-j" "-C" *edk-tools-path*)
               :error-output t)
  (values))

(defun build-ovmf ()
  (build #P"OvmfPkg/OvmfPkgX64.dsc"))

(defun build-refinery ()
  (build #P"RefineryPkg/RefineryPkg.dsc"))

(defun test-c (arch memory-model)
  (let* ((test-base (join *build-dir* (format nil "Borax/DEBUG_GCC/~A/" (string arch))))
         (test-bin (join test-base #P"BoraxVirtualMachineTest"))
         (test-file (join test-base #P"TestFile.bxo")))
    (make-test-file test-file memory-model)
    (build #P"BoraxPkg/BoraxPkg.dsc" :arch arch)
    (run-program (list "valgrind" "--error-exitcode=1" test-bin test-file)
                 :output t
                 :error-output t))
  (values))

(defun test-lisp ()
  (asdf:test-system :borax-virtual-machine))

(defun test ()
  (test-lisp)
  (fresh-line)
  (test-c :IA32 +32-bit+)
  (test-c :X64 +64-bit+))
