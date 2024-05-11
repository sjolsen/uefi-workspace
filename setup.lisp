;; TODO: vendor dependencies
(ql:quickload '(:clunit :flexi-streams))

(asdf:initialize-source-registry
 '(:source-registry
   :ignore-inherited-configuration
   (:tree (:here "refinery/BoraxPkg/Lisp"))))

(defun regenerate-test-data ()
  (asdf:load-system :borax-build/c-testing)
  (uiop:symbol-call :borax-build/c-testing :make-test-files))
