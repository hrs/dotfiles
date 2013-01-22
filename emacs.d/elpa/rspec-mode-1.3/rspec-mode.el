;;; rspec-mode.el --- Enhance ruby-mode for RSpec

;; Copyright (C) 2008-2011 Peter Williams <http://barelyenough.org>
;; Authors: Peter Williams, et al.
;; URL: http://github.com/pezra/rspec-mode
;; Created: 2011
;; Version: 1.3
;; Keywords: rspec ruby
;; Package-Requires: ((ruby-mode "1.1") (mode-compile "2.29"))

;;; Commentary:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Documentation:
;;
;; This minor mode provides some enhancements to ruby-mode in
;; the contexts of RSpec specifications.  Namely, it provides the
;; following capabilities:
;;
;;  * toggle back and forth between a spec and it's target (bound to
;;    `\C-c ,t`)
;;
;;  * verify the spec file associated with the current buffer (bound to `\C-c ,v`)
;;
;;  * verify the spec defined in the current buffer if it is a spec
;;    file (bound to `\C-c ,v`)
;;
;;  * verify the example defined at the point of the current buffer (bound to `\C-c ,s`)
;;
;;  * re-run the last verification process (bound to `\C-c ,r`)
;;
;;  * toggle the pendingness of the example at the point (bound to
;;    `\C-c ,d`)
;;
;;  * disable the example at the point by making it pending
;;
;;  * reenable the disabled example at the point
;;
;;  * run spec for entire project (bound to `\C-c ,a`)
;;
;; You can choose whether to run specs using 'rake spec' or the 'spec'
;; command. Use the customization interface (customize-group
;; rspec-mode) or override using (setq rspec-use-rake-flag TVAL).
;;
;; Options will be loaded from spec.opts or .rspec if it exists and
;; rspec-use-opts-file-when-available is not set to nil, otherwise it
;; will fallback to defaults.
;;
;; Dependencies
;; ------------
;;
;; This minor mode depends on `mode-compile`.  The expectations depend
;; `on el-expectataions.el`.  If `ansi-color` is available it will be
;; loaded so that rspec output is colorized properly.  If
;; `rspec-use-rvm` is set to true `rvm.el` is required.
;;

;;; Change Log:
;;
;; 1.5 - Allow key prefix to be customized (`rspec-key-command-prefix`)
;; 1.4 - Allow .rspec/spec.opts files to be ignored (`rspec-use-opts-file-when-available` customization)
;; 1.3 - Bundler support (JD Huntington)
;; 1.2 - Rspec2 compatibility  (Anantha Kumaran)
;; 1.1 - Run verification processes from project root directory (Joe Hirn)
;; 1.0 - Advance to end of compilation buffer even if it not the other window (byplayer)
;; 0.8 - RVM support (Peter Williams)
;; 0.7 - follow RoR conventions for file in lib directory (Tim Harper)
;; 0.6 - support for arbitrary spec and rake commands (David Yeu)
;; 0.5 - minor changes from Tim Harper
;; 0.4 - ansi colorization of compliation buffers (teaforthecat)
;; 0.3 - Dave Nolan implements respect for spec.opts config and
;;       custom option to use 'rake spec' task or 'spec' command
;; 0.2 - Tim Harper implemented support for imenu to generate a basic
;;       tag outline
;; 0.1 - Pezra's version in master

;;; Code:
(require 'ruby-mode)

(defconst rspec-mode-abbrev-table (make-abbrev-table))

(define-prefix-command 'rspec-mode-verifible-keymap)
(define-key rspec-mode-verifible-keymap (kbd "v") 'rspec-verify)
(define-key rspec-mode-verifible-keymap (kbd "a") 'rspec-verify-all)
(define-key rspec-mode-verifible-keymap (kbd "t") 'rspec-toggle-spec-and-target)

(define-prefix-command 'rspec-mode-keymap)
(define-key rspec-mode-keymap (kbd "v") 'rspec-verify)
(define-key rspec-mode-keymap (kbd "a") 'rspec-verify-all)
(define-key rspec-mode-keymap (kbd "t") 'rspec-toggle-spec-and-target)
(define-key rspec-mode-keymap (kbd "s") 'rspec-verify-single)
(define-key rspec-mode-keymap (kbd "d") 'rspec-toggle-example-pendingness)

(defgroup rspec-mode nil
  "Rspec minor mode.")

(defcustom rspec-use-rake-flag t
  "*Whether rspec runner is run using rake spec task or the spec command"
  :tag "Rspec runner command"
  :type '(radio (const :tag "Use 'rake spec' task" t)
                (const :tag "Use 'spec' command" nil))
  :group 'rspec-mode)

(defcustom rspec-rake-command "rake"
  "The command for rake"
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-spec-command "spec"
  "The command for spec"
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-use-rvm nil
  "t when RVM in is in use. (Requires rvm.el)"
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-bundler-when-possible t
  "t when rspec should be run with 'bundle exec' whenever possible. (Gemfile present)"
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-opts-file-when-available t
  "t if rspec should use .rspec/spec.opts"
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-compilation-buffer-name "*compilation*"
  "The compilation buffer name for spec"
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-key-command-prefix  (kbd "C-c ,")
  "The prefix for all rspec related key commands"
  :type 'string
  :group 'rspec-mode)




;;;###autoload
(define-minor-mode rspec-mode
  "Minor mode for rSpec files"
  :lighter " rSpec"
  (local-set-key rspec-key-command-prefix rspec-mode-keymap))



(defvar rspec-imenu-generic-expression
  '(("Examples"  "^\\( *\\(it\\|describe\\|context\\) +.+\\)"          1))
  "The imenu regex to parse an outline of the rspec file")



(defun rspec-set-imenu-generic-expression ()
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression rspec-imenu-generic-expression))

(add-hook 'rspec-mode-hook 'rspec-set-imenu-generic-expression)

;; Snippets
(if (require 'snippet nil t)
    (snippet-with-abbrev-table
     'rspec-mode-abbrev-table
     ("helper" . "require 'pathname'\nrequire Pathname(__FILE__).dirname + '../spec_helper'\n\n$.")
     ("desc"   . "describe $${ClassName} do\n  $.\nend ")
     ("descm"  . "describe $${ClassName}, \"$${modifier}\" do\n  $.\nend ")
     ("it"     . "it \"should $${what exactly?}\" do\n  $.\n  end ")
     ("bef"    . "before do\n  $.\n  end"))
  )


(defun rspec-beginning-of-example ()
  "Moves point to the beginning of the example in which the point current is."
  (interactive)
  (let ((start (point)))
    (goto-char
     (save-excursion
       (end-of-line)
       (unless (and (search-backward-regexp "^[[:space:]]*it[[:space:]]*(?[\"']" nil t)
                    (save-excursion (ruby-end-of-block) (< start (point))))
         (error "Unable to find an example"))
       (point)))))

(defun rspec-example-pending-p ()
  "True if the example under point is pending. Otherwise false"
  (interactive)
  (save-excursion
    (rspec-beginning-of-example)
    (re-search-forward "^[[:space:]]*pending\\([[:space:](]\\|$\\)" (save-excursion (ruby-end-of-block) (point)) t)))


(defun rspec-toggle-example-pendingness ()
  "Disables active examples and enables pending examples."
  (interactive)
  (if (rspec-example-pending-p)
      (rspec-enable-example)
    (rspec-disable-example)))

(defun rspec-disable-example ()
  "Disable the example in which the point is located"
  (interactive)
  (when (not (rspec-example-pending-p))
    (save-excursion
      (rspec-beginning-of-example)
      (end-of-line)
      (insert "\npending")
      (indent-for-tab-command))))

(defun rspec-enable-example ()
  "Enable the example in which the point is located"
  (interactive)
  (when (rspec-example-pending-p)
    (save-excursion
      (rspec-beginning-of-example)
      (search-forward-regexp "^[[:space:]]*pending\\([[:space:](]\\|$\\)" (save-excursion (ruby-end-of-block) (point)))
      (beginning-of-line)
      (delete-region (save-excursion (beginning-of-line) (point))
                     (save-excursion (forward-line 1) (point))))))

(defun rspec-verify ()
  "Runs the specified spec, or the spec file for the current buffer."
  (interactive)
  (rspec-run-single-file (rspec-spec-file-for (buffer-file-name)) (rspec-core-options ())))

(defun rspec-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (rspec-run-single-file (rspec-spec-file-for (buffer-file-name)) (rspec-core-options ()) (concat "--line " (number-to-string (line-number-at-pos)))))

(defun rspec-verify-all ()
  "Runs the 'spec' rake task for the project of the current file."
  (interactive)
  (rspec-run (rspec-core-options)))

(defun rspec-toggle-spec-and-target ()
  "Switches to the spec for the current buffer if it is a
   non-spec file, or switch to the target of the current buffer
   if the current is a spec"
  (interactive)
  (find-file
   (if (rspec-buffer-is-spec-p)
       (rspec-target-file-for (buffer-file-name))
     (rspec-spec-file-for (buffer-file-name)))))
(defun rspec-spec-directory-has-lib? (a-file-name)
  (file-directory-p (concat (rspec-spec-directory a-file-name) "/lib")))


(defun rspec-spec-file-for (a-file-name)
  "Find spec for the specified file"
  (if (rspec-spec-file-p a-file-name)
      a-file-name
    (let ((replace-regex (if (and (rspec-target-lib-file-p a-file-name) (rspec-spec-directory-has-lib? a-file-name))
                             "^\\.\\./"
                           "^\\.\\./[^/]+/"))
          (relative-file-name (file-relative-name a-file-name (rspec-spec-directory a-file-name))))
      (rspec-specize-file-name (expand-file-name (replace-regexp-in-string replace-regex "" relative-file-name)
                                                 (rspec-spec-directory a-file-name))))))

(defun rspec-spec-lib-file-p (a-spec-file-name)
  (string-match (concat "^" (expand-file-name (regexp-quote (concat (rspec-spec-directory a-spec-file-name) "/lib")))) a-spec-file-name))

(defun rspec-target-lib-file-p (a-file-name)
  (string-match (concat "^" (expand-file-name (regexp-quote (concat (rspec-project-root a-file-name) "/lib")))) a-file-name))

(defun rspec-target-file-for (a-spec-file-name)
  "Find the target for a-spec-file-name"
  (first
   (file-expand-wildcards
        (replace-regexp-in-string
         "/spec/"
         (if (rspec-spec-lib-file-p a-spec-file-name) "/" "/*/")
         (rspec-targetize-file-name a-spec-file-name)))))

(defun rspec-specize-file-name (a-file-name)
  "Returns a-file-name but converted in to a spec file name"
  (concat
   (file-name-directory a-file-name)
   (replace-regexp-in-string "\\(\\.rb\\)?$" "_spec.rb" (file-name-nondirectory a-file-name))))

(defun rspec-targetize-file-name (a-file-name)
  "Returns a-file-name but converted into a non-spec file name"
     (concat (file-name-directory a-file-name)
             (rspec-file-name-with-default-extension
              (replace-regexp-in-string "_spec\\.rb" "" (file-name-nondirectory a-file-name)))))

(defun rspec-file-name-with-default-extension (a-file-name)
  "Adds .rb file extension to a-file-name if it does not already have an extension"
  (if (file-name-extension a-file-name)
      a-file-name ;; file has a extension already so do nothing
    (concat a-file-name ".rb")))

(defun rspec-directory-subdirectories (directory)
  "Returns list of subdirectories"
  (remove-if
   (lambda (dir) (or (string-match "^\\.\\.?$" (file-name-nondirectory dir))
                     (not (file-directory-p dir))))
   (directory-files directory t)))

(defun rspec-parent-directory (a-directory)
  "Returns the directory of which a-directory is a child"
  (file-name-directory (directory-file-name a-directory)))

(defun rspec-root-directory-p (a-directory)
  "Returns t if a-directory is the root"
  (equal a-directory (rspec-parent-directory a-directory)))

(defun rspec-spec-directory (a-file)
  "Returns the nearest spec directory that could contain specs for a-file"
  (if (file-directory-p a-file)
      (or
       (first (directory-files a-file t "^spec$"))
       (if (rspec-root-directory-p a-file)
           nil
         (rspec-spec-directory (rspec-parent-directory a-file))))
    (rspec-spec-directory (rspec-parent-directory a-file))))

(defun rspec-spec-file-p (a-file-name)
  "Returns true if the specified file is a spec"
  (numberp (string-match "\\(_\\|-\\)spec\\.rb$" a-file-name)))

(defun rspec-core-options (&optional default-options)
  "Returns string of options that instructs spec to use options file if it exists, or sensible defaults otherwise"
  (cond ((and rspec-use-opts-file-when-available
              (file-readable-p (rspec-spec-opts-file)))
         (concat "--options " (rspec-spec-opts-file)))
        (t (or default-options
            (rspec-default-options)))))

(defun rspec-bundle-p ()
  (and rspec-use-bundler-when-possible
       (file-readable-p (concat (rspec-project-root) "Gemfile"))))

(defun rspec2-p ()
  (or (string-match "rspec" rspec-spec-command)
      (file-readable-p (concat (rspec-project-root) ".rspec"))))

(defun rspec-default-options ()
  (if (rspec2-p)
      "--format documentation"
    (concat "--format specdoc " "--reverse")))

(defun rspec-spec-opts-file ()
  "Returns filename of spec opts file"
  (if (rspec2-p)
      (expand-file-name ".rspec" (rspec-project-root))
    (expand-file-name "spec.opts" (rspec-spec-directory (rspec-project-root)))))

(defun rspec-runner ()
  "Returns command line to run rspec"
  (let ((bundle-command (if (rspec-bundle-p) "bundle exec " "")))
    (concat bundle-command (if rspec-use-rake-flag
                               (concat rspec-rake-command " spec")
                             rspec-spec-command))))

(defun rspec-runner-options (&optional opts)
  "Returns string of options for command line"
  (let ((opts (if (listp opts)
                  opts
                (list opts))))
    (concat (when rspec-use-rake-flag "SPEC_OPTS=\'")
            (mapconcat 'identity opts " ")
            (when rspec-use-rake-flag "\'"))))

(defun rspec-runner-target (target)
  "Returns target file/directory wrapped in SPEC if using rake"
  (concat (when rspec-use-rake-flag "SPEC=\'") target (when rspec-use-rake-flag "\'")))

;;;###autoload
(defun rspec-buffer-is-spec-p ()
  "Returns true if the current buffer is a spec"
  (and (buffer-file-name)
       (rspec-spec-file-p (buffer-file-name))))

(defun rspec-example-name-at-point ()
  "Returns the name of the example in which the point is currently positioned; or nil if it is outside of and example"
  (save-excursion
    (rspec-beginning-of-example)
    (re-search-forward "it[[:space:]]+['\"]\\(.*\\)['\"][[:space:]]*\\(do\\|DO\\|Do\\|{\\)")
    (match-string 1)))

(defun rspec-end-of-buffer-target-window (buf-name)
  "end of line target window"
  (let ((cur-window (selected-window))
        (com-buffer (get-buffer buf-name)))
    (if com-buffer
        (let ((com-window (get-buffer-window com-buffer)))
          (cond (com-window
                 (unwind-protect
                     (progn
                       (select-window com-window)
                       (with-no-warnings
                         (goto-char (point-max))
                         (recenter '(t))))
                   (select-window cur-window))))))))

(defun rspec-run (&optional opts)
  "Runs spec with the specified options"
  (rspec-compile (rspec-spec-directory (rspec-project-root)) opts))

(defun rspec-run-single-file (spec-file &rest opts)
  "Runs spec on a file with the specified options"
  (rspec-compile (rspec-runner-target spec-file) opts))

(defun rspec-compile (a-file-or-dir &optional opts)
  "Runs a compile for the specified file or diretory with the specified opts"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") (eval `(lambda () (interactive)
                                       (rspec-from-direcory ,default-directory
                                                            (rspec-compile ,a-file-or-dir (quote ,opts))))))
    (global-set-key rspec-key-command-prefix map))

  (if rspec-use-rvm
      (rvm-activate-corresponding-ruby))
  (rspec-from-project-root
   (compile (mapconcat 'identity `(,(rspec-runner) ,a-file-or-dir ,(rspec-runner-options opts)) " ")))
  (rspec-end-of-buffer-target-window rspec-compilation-buffer-name))


(defun rspec-project-root (&optional directory)
  "Finds the root directory of the project by walking the directory tree until it finds a rake file."
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (cond ((rspec-root-directory-p directory) nil)
          ((file-exists-p (concat directory "Rakefile")) directory)
          (t (rspec-project-root (file-name-directory (directory-file-name directory)))))))

(defmacro rspec-from-direcory (directory body-form)
  "Peform body-form from the specified directory"
  `(let ((default-directory ,directory))
     ,body-form))

(defmacro rspec-from-project-root (body-form)
  "Peform body-form from the project root directory"
  `(rspec-from-direcory ,(or (rspec-project-root) default-directory)
                        ,body-form))

;; Makes sure that Rspec buffers are given the rspec minor mode by default
;;;###autoload
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook
             (lambda ()
               (when (rspec-buffer-is-spec-p)
                 (rspec-mode)))))

;; Add verify related spec keybinding to ruby ruby modes
;;;###autoload
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook
             (lambda ()
               (local-set-key rspec-key-command-prefix rspec-mode-verifible-keymap))))

;; Add verify related spec keybinding to ruby ruby modes
;;;###autoload
(eval-after-load 'rails
  '(add-hook 'rails-minor-mode-hook
             (lambda ()
               (local-set-key rspec-key-command-prefix rspec-mode-verifible-keymap))))

;; abbrev
;; from http://www.opensource.apple.com/darwinsource/Current/emacs-59/emacs/lisp/derived.el
(defun merge-abbrev-tables (old new)
  "Merge an old abbrev table into a new one.
This function requires internal knowledge of how abbrev tables work,
presuming that they are obarrays with the abbrev as the symbol, the expansion
as the value of the symbol, and the hook as the function definition."
  (when old
    (mapatoms
     (lambda(it)
       (or (intern-soft (symbol-name it) new)
           (define-abbrev new
             (symbol-name it)
             (symbol-value it)
             (symbol-function it)
             nil
             t)))
     old)))

(add-hook 'compilation-mode-hook
          (lambda ()
            (add-to-list 'compilation-error-regexp-alist-alist
                         '(rspec "\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 1 2))
            (add-to-list 'compilation-error-regexp-alist 'rspec)))

(condition-case nil
    (progn
      (require 'ansi-color)
      (defun rspec-colorize-compilation-buffer ()
        (toggle-read-only)
        (ansi-color-apply-on-region (point-min) (point-max))
        (toggle-read-only))
      (add-hook 'compilation-filter-hook 'rspec-colorize-compilation-buffer))
    (error nil))

(provide 'rspec-mode)
;;; rspec-mode.el ends here
