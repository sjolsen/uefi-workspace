(uiop:define-package :uefi-workspace/edk2
  (:use :uiop/common-lisp)
  (:export #:*workspace* #:*conf-path* #:*edk-tools-path* #:*packages-path*
           #:join #:run-program
           #:activate
           #:*build-dir* #:build
           #:find-source-files #:uncrustify-files))

(in-package :uefi-workspace/edk2)

(defvar *workspace*
  (asdf:system-source-directory :uefi-workspace))

(defmacro defenv (name key)
  `(define-symbol-macro ,name (uiop:getenv ,key)))

(defenv *conf-path* "CONF_PATH")
(defenv *edk-tools-path* "EDK_TOOLS_PATH")
(defenv *packages-path* "PACKAGES_PATH")

(defun join (root &rest paths)
  (dolist (path paths)
    (setf root (uiop:merge-pathnames* path root)))
  root)

(defgeneric stringify-program-argument (arg)
  (:method ((arg string)) arg)
  (:method ((arg pathname)) (namestring arg))
  (:method ((arg list)) (mapcar #'stringify-program-argument arg)))

(defun run-program (command &rest keys)
  (apply #'uiop:run-program (stringify-program-argument command) keys))

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
  (join *workspace* #P"Build/"))

(defun build (platform &key arch)
  (let* ((arch-args (when arch
                      (list "-a" (string arch))))
         (args (list* "build" "-p" platform arch-args)))
    (run-program args :output t :error-output t))
  (values))

(defun find-source-files (root)
  (let ((wildcard (join root #P"**/*"))
        (extensions '("h" "c" "hpp" "cpp")))
    (loop for extension in extensions
          nconcing (uiop:directory* (join wildcard (make-pathname :type extension))))))

(defun uncrustify-files (files)
  (let* ((plugin-dir (join *workspace* #P"edk2/.pytool/Plugin/UncrustifyCheck/"))
         (uncrustify (join plugin-dir #P"mu-uncrustify-release_extdep/Linux-x86/uncrustify"))
         (config (join plugin-dir #P"uncrustify.cfg"))
         (args (list* uncrustify "-c" config "--replace" "--no-backup" files)))
    (run-program args))
  (values))
