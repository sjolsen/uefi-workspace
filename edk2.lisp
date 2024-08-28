(uiop:define-package :uefi-workspace/edk2
  (:use :uiop/common-lisp)
  (:export #:*workspace* #:*conf-path* #:*edk-tools-path* #:*packages-path*
           #:activate))

(in-package :uefi-workspace/edk2)

(defvar *workspace*
  (asdf:system-source-directory :uefi-workspace))

(defmacro defenv (name key)
  `(define-symbol-macro ,name (uiop:getenv ,key)))

(defenv *conf-path* "CONF_PATH")
(defenv *edk-tools-path* "EDK_TOOLS_PATH")
(defenv *packages-path* "PACKAGES_PATH")

(defun get-raw-environment ()
  (let* ((activate (uiop:merge-pathnames* #P"activate" *workspace*))
         (script (with-output-to-string (s)
                   (format s ". ~S >/dev/null~%" (namestring activate))
                   (format s "exec sbcl --script~%"))))
    (flet ((provide-input (s)
             (print '(prin1 (sb-ext:posix-environ)) s)))
      (uiop:run-program script :force-shell t
                               :input #'provide-input
                               :output #'read
                               :error-output t))))

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
