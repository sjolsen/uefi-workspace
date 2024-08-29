(uiop:define-package :uefi-workspace
  (:mix :uiop/common-lisp :borax-virtual-machine/image)
  (:use :borax-build/c-testing :borax-virtual-machine/initial-image
        :borax-virtual-machine/object-file)
  (:use-reexport :uefi-workspace/edk2)
  (:export #:reload
           #:uncrustify
           #:build-basetools #:build-ovmf #:build-refinery #:build-all
           #:run-qemu
           #:test-c #:test-lisp #:test))

(in-package :uefi-workspace)

(defun reload ()
  "Reload the development environment from source"
  (asdf:load-system :uefi-workspace))

(defun uncrustify ()
  "Auto-format C/C++ sources in the refinery tree"
  (uncrustify-files (find-source-files (join *workspace* #P"refinery/"))))

(defun build-basetools ()
  "Build EDK II's internal build tools"
  (run-program (list "make" "-j" "-C" *edk-tools-path*)
               :error-output t)
  (values))

(defun ovmf-name (arch)
  (ecase arch
    (:IA32 "Ia32")
    (:X64  "X64")))

(defconstant +default-arch+ :X64)

(defun build-ovmf (&optional (arch +default-arch+))
  "Build a UEFI firmware image for QEMU"
  (build (format nil "OvmfPkg/OvmfPkg~A.dsc" (ovmf-name arch)) :arch arch))

(defun build-refinery (&optional (arch +default-arch+))
  "Build the Refinery application"
  (build #P"RefineryPkg/RefineryPkg.dsc" :arch arch))

(defun build-all ()
  "Build all the software needed to run the Refinery application"
  (build-basetools)
  (build-ovmf :IA32)
  (build-ovmf :X64)
  (build-refinery :IA32)
  (build-refinery :X64))

(defun memory-model-for-arch (arch)
  (ecase arch
    (:IA32 +32-bit+)
    (:X64  +64-bit+)))

(defun run-qemu (&key (arch +default-arch+) debug)
  "Launch QEMU and boot into the Refinery application"
  (let* ((arch-name (string arch))
         (ovmf-name (ovmf-name arch))
         (qemu-dir (join *build-dir* (format nil "Qemu~A/" arch-name)))
         (hda (join qemu-dir #P"hda/"))
         (arch-dir (join *build-dir* (format nil "Refinery/DEBUG_GCC/~A/" arch-name))))
    (flet ((symlink (old new)
             (unless (uiop:file-exists-p new)
               (ensure-directories-exist new)
               (sb-posix:symlink old new))))
      (symlink (join arch-dir #P"Refinery.efi")
               (join hda (format nil "EFI/BOOT/BOOT~A.efi" arch-name)))
      (symlink (join arch-dir #P"UsbMouseDxe.efi")
               (join hda #P"EFI/Refinery/Drivers/USBMouseDxe.efi")))
    (with-open-file (stream (join hda #P"EFI/Refinery/initial-image.bxo")
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (with-image (memory-model-for-arch arch)
        (let ((root (make-initial-image)))
          (write-object-file root stream))))
    (let* ((bios (join *build-dir* (format nil "Ovmf~A/DEBUG_GCC/FV/OVMF.fd" ovmf-name)))
           (args (list (ecase arch
                         (:IA32 "qemu-system-i386")
                         (:X64  "qemu-system-x86_64"))
                       "-net" "none"
                       "-device" "VGA,xres=640,yres=480"
                       "-display" "gtk,gl=es"
                       "-usb"
                       "-device" "usb-mouse"
                       "-drive" (format nil "if=pflash,unit=0,format=raw,file=~A" (namestring bios))
                       "-drive" (format nil "format=raw,file=fat:~A" (namestring hda))
                       "-snapshot"))
           (debug-file (join qemu-dir #P"debug.log"))
           (debug-args (list "-debugcon" (format nil "file:~A" (namestring debug-file))
                             "-global" "isa-debugcon.iobase=0x402")))
      (when debug
        (setf args (nconc args debug-args)))
      (run-program args :error-output t)))
  (values))

(defun test-c (&optional (arch +default-arch+))
  "Build and run the unit tests for the C implementation of Borax"
  (let* ((test-base (join *build-dir* (format nil "Borax/DEBUG_GCC/~A/" (string arch))))
         (test-bin (join test-base #P"BoraxVirtualMachineTest"))
         (test-file (join test-base #P"TestFile.bxo")))
    (ensure-directories-exist test-file)
    (make-test-file test-file (memory-model-for-arch arch))
    (build #P"BoraxPkg/BoraxPkg.dsc" :arch arch)
    (run-program (list "valgrind" "--error-exitcode=1" test-bin test-file)
                 :output t
                 :error-output t))
  (values))

(defun test-lisp ()
  "Run unit tests for the Common Lisp implementation of Borax"
  (asdf:test-system :borax-virtual-machine))

(defun test ()
  "Run all tests"
  (test-lisp)
  (fresh-line)
  (test-c :IA32)
  (test-c :X64))
