;; TODO: vendor dependencies
(ql:quickload '(:clunit :flexi-streams))

;; TODO: compute repository base path
(asdf:initialize-source-registry
 '(:source-registry
   :ignore-inherited-configuration
   (:tree "~/Code/uefi-workspace/refinery/BoraxPkg/Lisp")))
