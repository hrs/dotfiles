(add-to-list 'load-path "~/.emacs.d")

(load "package-management.el")
(load "ui-prefs.el")
;; (load "mail-prefs.el")
;; (load "slime-prefs.el")
(load "utils.el")

;; language-specific configuration
(load "languages/css.el")
(load "languages/groovy.el")
(load "languages/haskell.el")
(load "languages/html.el")
(load "languages/lisp.el")
(load "languages/markdown.el")
(load "languages/octave.el")
(load "languages/python.el")
(load "languages/r.el")
(load "languages/ruby.el")
(load "languages/sass.el")
(load "languages/tex.el")

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

(load "keybindings.el")

;; delete trailing whitespace
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

;; configuring yasnippets
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; use projectile everywhere
(projectile-global-mode)
