;; TODO: vendor dependencies
(ql:quickload '(:cl-locatives :closer-mop :clunit :flexi-streams) :silent t)

(asdf:initialize-source-registry
 '(:source-registry
   :ignore-inherited-configuration
   (:directory (:here))
   (:tree (:here "refinery/BoraxPkg/Lisp"))))

(asdf:load-system :uefi-workspace)

(uefi-workspace:activate)
