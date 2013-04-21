(in-package #:quicklisp)

(defun show-wrapped-list (words &key (indent 4) (margin 60))
 (let ((*print-right-margin* margin)
       (*print-pretty* t)
       (*print-escape* nil)
       (prefix (make-string indent :initial-element #\Space)))
   (pprint-logical-block (nil words :per-line-prefix prefix)
     (pprint-fill *standard-output* (sort (copy-seq words) #'string<) nil))
   (fresh-line)
   (finish-output)))

(defun recursively-install (name)
  (labels ((recurse (name)
             (let ((system (find-system name)))
               (unless system
                 (error "Unknown system ~S" name))
               (ensure-installed system)
               (mapcar #'recurse (required-systems system))
               name)))
    (with-consistent-dists
      (recurse name))))

(defclass load-strategy ()
  ((name
    :initarg :name
    :accessor name)
   (asdf-systems
    :initarg :asdf-systems
    :accessor asdf-systems)
   (quicklisp-systems
    :initarg :quicklisp-systems
    :accessor quicklisp-systems)))

(defmethod print-object ((strategy load-strategy) stream)
  (print-unreadable-object (strategy stream :type t)
    (format stream "~S (~D asdf, ~D quicklisp)"
            (name strategy)
            (length (asdf-systems strategy))
            (length (quicklisp-systems strategy)))))

(defgeneric quicklisp-releases (strategy)
  (:method (strategy)
    (remove-duplicates (mapcar 'release (quicklisp-systems strategy)))))

(defgeneric quicklisp-release-table (strategy)
  (:method ((strategy load-strategy))
    (let ((table (make-hash-table)))
      (dolist (system (quicklisp-systems strategy))
        (push system (gethash (release system) table nil)))
      table)))

(define-condition system-not-found (error)
  ((name
    :initarg :name
    :reader system-not-found-name))
  (:report (lambda (condition stream)
             (format stream "System ~S not found"
                     (system-not-found-name condition))))
  (:documentation "This condition is signaled by QUICKLOAD when a
  system given to load is not available via ASDF or a Quicklisp
  dist."))

(defun compute-load-strategy (name)
  (setf name (string-downcase name))
  (let ((asdf-systems '())
        (quicklisp-systems '()))
    (labels ((recurse (name)
               (let ((asdf-system (asdf:find-system name nil))
                     (quicklisp-system (find-system name)))
                 (cond (asdf-system
                        (push asdf-system asdf-systems))
                       (quicklisp-system
                        (push quicklisp-system quicklisp-systems)
                        (dolist (subname (required-systems quicklisp-system))
                          (recurse subname)))
                       (t
                        (error 'system-not-found
                               :name name))))))
      (with-consistent-dists
        (recurse name)))
    (make-instance 'load-strategy
                   :name name
                   :asdf-systems (remove-duplicates asdf-systems)
                   :quicklisp-systems (remove-duplicates quicklisp-systems))))

(defun show-load-strategy (strategy)
  (format t "To load ~S:~%" (name strategy))
  (let ((asdf-systems (asdf-systems strategy))
        (releases (quicklisp-releases strategy)))
    (when asdf-systems
      (format t "  Load ~D ASDF system~:P:~%" (length asdf-systems))
      (show-wrapped-list (mapcar 'asdf:component-name asdf-systems)))
    (when releases
      (format t "  Install ~D Quicklisp release~:P:~%" (length releases))
      (show-wrapped-list (mapcar 'name releases)))))

(defvar *macroexpand-progress-in-progress* nil)

(defun macroexpand-progress-fun (old-hook &key (char #\.)
                                 (chars-per-line 50)
                                 (forms-per-char 250))
  (let ((output-so-far 0)
        (seen-so-far 0))
    (labels ((finish-line ()
               (when (plusp output-so-far)
                 (dotimes (i (- chars-per-line output-so-far))
                   (write-char char))
                 (terpri)
                 (setf output-so-far 0)))
             (show-string (string)
               (let* ((length (length string))
                      (new-output (+ length output-so-far)))
                 (cond ((< chars-per-line new-output)
                        (finish-line)
                        (write-string string)
                        (setf output-so-far length))
                       (t
                        (write-string string)
                        (setf output-so-far new-output))))
               (finish-output))
             (show-package (name)
               ;; Only show package markers when compiling. Showing
               ;; them when loading shows a bunch of ASDF system
               ;; package noise.
               (when *compile-file-pathname*
                 (finish-line)
                 (show-string (format nil "[package ~(~A~)]" name)))))
      (lambda (fun form env)
        (when (and (consp form)
                   (eq (first form) 'cl:defpackage)
                   (ignore-errors (string (second form))))
	  (show-package (second form)))
        (incf seen-so-far)
        (when (<= forms-per-char seen-so-far)
          (setf seen-so-far 0)
          (write-char char)
          (finish-output)
          (incf output-so-far)
          (when (<= chars-per-line output-so-far)
            (setf output-so-far 0)
            (terpri)
            (finish-output)))
        (funcall old-hook fun form env)))))

(defun call-with-macroexpand-progress (fun)
  (let ((*macroexpand-hook* (if *macroexpand-progress-in-progress*
                                *macroexpand-hook*
                                (macroexpand-progress-fun *macroexpand-hook*)))
        (*macroexpand-progress-in-progress* t))
    (funcall fun)
    (terpri)))

(defun apply-load-strategy (strategy)
  (map nil 'ensure-installed (quicklisp-releases strategy))
  (call-with-macroexpand-progress
   (lambda ()
     (format t "~&; Loading ~S~%" (name strategy))
     (asdf:oos 'asdf:load-op (name strategy) :verbose nil))))

(defun autoload-system-and-dependencies (name &key prompt)
  "Try to load the system named by NAME, automatically loading any
Quicklisp-provided systems first, and catching ASDF missing
dependencies too if possible."
  (setf name (string-downcase name))
  (with-simple-restart (abort "Give up on ~S" name)
    (let ((strategy (compute-load-strategy name))
          (tried-so-far (make-hash-table :test 'equalp)))
      (show-load-strategy strategy)
      (when (or (not prompt)
                (press-enter-to-continue))
        (tagbody
         retry
         (handler-bind
             ((asdf:missing-dependency-of-version
               (lambda (c)
                 ;; Nothing Quicklisp can do to recover from this, so
                 ;; just resignal
                 (error c)))
              (asdf:missing-dependency
               (lambda (c)
                 (let ((parent (asdf::missing-required-by c))
                       (missing (asdf::missing-requires c)))
                   (when (typep parent 'asdf:system)
                     (if (gethash missing tried-so-far)
                         (error "Dependency looping -- already tried to load ~
                                 ~A" missing)
                         (setf (gethash missing tried-so-far) missing))
                     (autoload-system-and-dependencies missing
                                                       :prompt prompt)
                     (go retry))))))
           (apply-load-strategy strategy)))))
    name))

(defvar *initial-dist-url*
  "http://beta.quicklisp.org/dist/quicklisp.txt")

(defun maybe-initial-setup ()
  ;; Is this running under the quicklisp bootstrap?
  (let ((bootstrap-package (find-package 'quicklisp-quickstart)))
    (when bootstrap-package
      (let* ((proxy (find-symbol (string '#:*proxy-url*) bootstrap-package))
             (proxy-value (and proxy (symbol-value proxy))))
        (when (and proxy-value (not *proxy-url*))
          (setf *proxy-url* proxy-value)
          (setf (config-value "proxy-url") proxy-value)))))
  (unless (ignore-errors (truename (qmerge "dists/")))
    (let ((target (qmerge "dists/quicklisp/distinfo.txt")))
      (ensure-directories-exist target)
      (fetch *initial-dist-url* target)
      (enable (find-dist "quicklisp")))))

(defun setup ()
  (unless (member 'system-definition-searcher
                  asdf:*system-definition-search-functions*)
    (setf asdf:*system-definition-search-functions*
          (append asdf:*system-definition-search-functions*
                  (list 'local-projects-searcher
                        'system-definition-searcher))))
  (let ((files (nconc (directory (qmerge "local-init/*.lisp"))
                      (directory (qmerge "local-init/*.cl")))))
    (with-simple-restart (abort "Stop loading local setup files")
      (dolist (file (sort files #'string< :key #'pathname-name))
        (with-simple-restart (skip "Skip local setup file ~S" file)
          ;; Don't try to load Emacs lock files, other hidden files
          (unless (char= (char (pathname-name file) 0)
                         #\.)
            (load file))))))
  (maybe-initial-setup)
  (ensure-directories-exist (qmerge "local-projects/"))
  (pushnew :quicklisp *features*)
  t)
