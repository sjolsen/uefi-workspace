(uiop:define-package :uefi-workspace/edk2
  (:use :uiop/common-lisp)
  (:export #:*workspace* #:*conf-path* #:*edk-tools-path* #:*packages-path*
           #:activate
           #:*build-dir* #:build))

(in-package :uefi-workspace/edk2)

(defvar *workspace*
  (asdf:system-source-directory :uefi-workspace))

(defmacro defenv (name key)
  `(define-symbol-macro ,name (uiop:getenv ,key)))

(defenv *conf-path* "CONF_PATH")
(defenv *edk-tools-path* "EDK_TOOLS_PATH")
(defenv *packages-path* "PACKAGES_PATH")

(defun get-raw-environment ()
  (uiop:chdir *workspace*)
  (flet ((provide-input (s)
           (print '(prin1 (sb-ext:posix-environ)) s)))
    (uiop:run-program ". ./activate >/dev/null && exec sbcl --script"
                      :force-shell t
                      :input #'provide-input
                      :output #'read
                      :error-output t)))

(defun get-environment ()
  (loop with hash-table = (make-hash-table :test 'equal)
        for s in (get-raw-environment)
        for i = (position #\= s)
        for key = (subseq s 0 i)
        for value = (subseq s (1+ i))
        do (setf (gethash key hash-table) value)
        finally (return hash-table)))

(defun activate ()
  (let ((env (get-environment)))
    (loop for key being each hash-key of env
          for value being each hash-value of env
          do (setf (uiop:getenv key) value))))

(defvar *build-dir*
  (uiop:merge-pathnames* #P"Build/" *workspace*))

(defun build (platform &key arch)
  (let* ((arch-args (when arch
                      (list "-a" (string arch))))
         (args (list* "build" "-p" (namestring platform) arch-args)))
    (uiop:run-program args :error-output t)))
