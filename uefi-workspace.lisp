(uiop:define-package :uefi-workspace
  (:mix :uiop/common-lisp :borax-virtual-machine/image)
  (:use :borax-build/c-testing)
  (:use-reexport :uefi-workspace/edk2)
  (:export #:reload
           #:uncrustify
           #:build-basetools #:build-ovmf #:build-refinery
           #:run-qemu
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

(defun run-qemu (&key debug)
  (let ((hda (join *build-dir* #P"hda/")))
    (flet ((symlink (old new)
             (unless (uiop:file-exists-p new)
               (ensure-directories-exist new)
               (sb-posix:symlink old new))))
      (symlink (join *build-dir* #P"Refinery/DEBUG_GCC/X64/Refinery.efi")
               (join hda #P"EFI/BOOT/BOOTx64.efi"))
      (symlink (join *build-dir* #P"Refinery/DEBUG_GCC/X64/UsbMouseDxe.efi")
               (join hda #P"EFI/Drivers/USBMouseDxe.efi")))
    (let* ((bios (join *build-dir* #P"OvmfX64/DEBUG_GCC/FV/OVMF.fd"))
           (args (list "qemu-system-x86_64"
                       "-net" "none"
                       "-device" "VGA,xres=640,yres=480"
                       "-display" "gtk,gl=es"
                       "-usb"
                       "-device" "usb-mouse"
                       "-drive" (format nil "if=pflash,unit=0,format=raw,file=~A" (namestring bios))
                       "-drive" (format nil "format=raw,file=fat:~A" (namestring hda))
                       "-snapshot"))
           (debug-file (join *build-dir* #P"debug.log"))
           (debug-args (list "-debugcon" (format nil "file:~A" (namestring debug-file))
                             "-global" "isa-debugcon.iobase=0x402")))
      (when debug
        (setf args (nconc args debug-args)))
      (run-program args :error-output t)))
  (values))

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
