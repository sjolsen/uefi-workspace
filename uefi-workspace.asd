(defsystem "uefi-workspace"
  :components ((:file "edk2")
               (:file "uefi-workspace" :depends-on ("edk2")))
  :depends-on ("borax-build/c-testing" "borax-virtual-machine/initial-image"))
