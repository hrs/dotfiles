(add-to-list 'load-path "~/.emacs.d")

(set-frame-parameter nil 'fullscreen 'fullboth)

(load "package-management.el")
(load "ui-prefs.el")
;; (load "mail-prefs.el")
;; (load "slime-prefs.el")
(load "utils.el")

(require 'dired-x)

;; extend exec-path
(setq exec-path (append exec-path '("/usr/local/bin")))

;; mode-specific configuration
(mapcar (lambda (mode-file-name) (load (concat "modes/" mode-file-name)))
        (directory-files "~/.emacs.d/modes" nil ".el"))

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
