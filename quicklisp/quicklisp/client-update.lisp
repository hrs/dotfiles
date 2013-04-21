;;;; client-update.lisp

(in-package #:quicklisp-client)

(defun version-from-file (file)
  (with-open-file (stream file)
    (let ((version-string (read-line stream)))
      (when (every #'digit-char-p version-string)
        (values (parse-integer version-string))))))

(defun local-version ()
  (version-from-file (qmerge "quicklisp/version.txt")))

(defun upstream-version ()
  (let ((local-file (qmerge "tmp/client-update/version.txt")))
    (ensure-directories-exist local-file)
    (fetch "http://beta.quicklisp.org/quickstart/version.txt"
           local-file :quietly t)
    (prog1 (version-from-file local-file)
      (delete-file local-file))))

(defun update-available-p ()
  (< (local-version) (upstream-version)))

(defun upstream-archive-url (version)
  (format nil "http://beta.quicklisp.org/quickstart/quicklisp-~D.tgz"
          version))

(defvar *upstream-asdf-url*
  "http://beta.quicklisp.org/quickstart/asdf.lisp")

(defvar *upstream-setup-url*
  "http://beta.quicklisp.org/quickstart/setup.lisp")

(defun retirement-directory (base)
  (let ((suffix 0))
    (loop
      (incf suffix)
      (let* ((try (format nil "~A-~D" base suffix))
             (dir (qmerge (make-pathname :directory
                                         (list :relative "retired" try)))))
        (unless (probe-directory dir)
          (return dir))))))

(defun update-client (&key (prompt t))
  (let ((upstream-version (upstream-version))
        (local-version (local-version)))
    (when (<= upstream-version local-version)
      (format t "Installed version ~D is as new as upstream version ~D. No update.~%"
              local-version upstream-version)
      (return-from update-client t))
    (format t "Updating from version ~D to version ~D.~%"
            local-version upstream-version)
    (when prompt
      (unless (press-enter-to-continue)
        (return-from update-client nil)))
    (let* ((work-dir (qmerge (make-pathname
                              :directory
                              (list :relative
                                    "tmp"
                                    "client-update"
                                    (princ-to-string upstream-version)))))
           (upstream-archive (merge-pathnames "quicklisp.tgz" work-dir))
           (upstream-tar (merge-pathnames "quicklisp.tar" work-dir))
           (upstream-unpacked (merge-pathnames "quicklisp/" work-dir))
           (retired (retirement-directory (format nil "quicklisp-~D"
                                                  local-version)))
           (current-dir (qmerge "quicklisp/")))
      (ensure-directories-exist (qmerge "retired/"))
      (ensure-directories-exist upstream-archive)
      (fetch (upstream-archive-url upstream-version) upstream-archive)
      (gunzip upstream-archive upstream-tar)
      (unpack-tarball upstream-tar :directory work-dir)
      (rename-directory current-dir retired)
      (rename-directory upstream-unpacked current-dir)
      ;; A little crude; should version these, too
      (fetch *upstream-setup-url* (qmerge "setup.lisp"))
      (fetch *upstream-asdf-url* (qmerge "asdf.lisp"))
      (format t "~&New quicklisp client installed. ~
                   It will take effect on restart.~%")
      t)))
