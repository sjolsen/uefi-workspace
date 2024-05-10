;; TODO: vendor dependencies
(ql:quickload '(:clunit :flexi-streams))

;; TODO: compute repository base path
(asdf:initialize-source-registry
 '(:source-registry
   :ignore-inherited-configuration
   (:tree "~/Code/uefi-workspace/refinery/BoraxPkg/Lisp")))

(defun regenerate-test-data ()
  (asdf:load-system :borax-runtime/object-file-test)
  (let* ((test-base "~/Code/uefi-workspace/refinery/BoraxPkg/Test/BoraxRuntimeTest/")
         (test-files `(("TestFileIA32.bxo" . ,(borax-runtime/memory:make-memory-model 32 :little-endian))
                       ("TestFileX64.bxo"  . ,(borax-runtime/memory:make-memory-model 64 :little-endian)))))
    (loop for (basename . memory-model) in test-files
          for path = (concatenate 'string test-base basename)
          do (borax-runtime/object-file-test:make-test-file path memory-model))))
