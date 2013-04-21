;;;; slime-helper.lisp

(defpackage #:quicklisp-slime-helper
  (:use #:cl)
  (:export #:install)
  (:shadowing-import-from #:alexandria
                          #:copy-file)
  (:shadowing-import-from #:ql-dist
                          #:ensure-installed
                          #:find-system)
  (:shadowing-import-from #:ql-setup
                          #:qmerge))

(in-package #:quicklisp-slime-helper)

(defun install ()
  (let ((source (asdf:system-relative-pathname "quicklisp-slime-helper"
                                               "slime-helper-template"
                                               :type "el"))
        (target (qmerge "slime-helper.el")))
    (copy-file source target)
    (ensure-installed (find-system "swank"))
    (format t "~&slime-helper.el installed in ~S~%~%"
            (namestring target))
    (let ((enough (enough-namestring target (user-homedir-pathname))))
      (unless (equal (pathname enough) target)
        (setf enough (format nil "~~/~A" enough)))
      (format t "To use, add this to your ~~/.emacs:~%~%")
      (format t "  (load (expand-file-name ~S))~%" enough)
      (format t "  ;; Replace \"sbcl\" with the path to your implementation~%")
      (format t "  (setq inferior-lisp-program \"sbcl\")~%")
      (format t "~%"))))
