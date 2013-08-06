(add-to-list 'load-path "~/.emacs.d")

(set-frame-parameter nil 'fullscreen 'fullboth)

(load "package-management.el")
(load "ui-prefs.el")
;; (load "mail-prefs.el")
;; (load "slime-prefs.el")
(load "utils.el")

;; mode-specific configuration
(load "modes/css.el")
(load "modes/groovy.el")
(load "modes/haskell.el")
(load "modes/haml.el")
(load "modes/html.el")
(load "modes/lisp.el")
(load "modes/markdown.el")
(load "modes/octave.el")
(load "modes/org-mode.el")
(load "modes/prolog.el")
(load "modes/python.el")
(load "modes/r.el")
(load "modes/ruby.el")
(load "modes/sass.el")
(load "modes/shell.el")
(load "modes/tex.el")

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

;; configuring yasnippets
(setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
(yas-global-mode 1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; use projectile everywhere
(projectile-global-mode)
(load "keybindings.el")
