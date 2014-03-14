(defun not-david-p (s)
  (not (string-match "david" s)))

(defun filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(setq load-path (filter #'not-david-p load-path))

(set-frame-parameter nil 'fullscreen 'fullboth)
(setq default-directory "~/")

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/modes/")

(load "ui-prefs.el")
(load "utils.el")
(load "password-management.el")

(require 'dired-x)
(require 'multi-term)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; allow 20MB of memory (instead of 0.76MB) before calling GC
(setq gc-cons-threshold 20000000)

;; extend exec-path
(setq exec-path (append exec-path '("/usr/local/bin")))

;; mode-specific configuration
(mapcar (lambda (mode-file-name) (load mode-file-name))
        (directory-files "~/.emacs.d/modes/" nil ".el"))

;; fussy, fussy
(setq make-backup-files nil)
(delete-selection-mode t)
(setq-default indent-tabs-mode nil)

;; set search list for hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; delete trailing whitespace
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

(setq require-final-newline t)

;; configuring yasnippets
(setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
(yas-global-mode 1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; use projectile everywhere
(projectile-global-mode)
(load "keybindings.el")
