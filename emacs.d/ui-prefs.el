;; UI preferences

(load "fonts.el")
(load "hide-modes.el")
(smex-initialize)

(defun use-solarized-theme ()
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")
  (load-theme 'solarized-dark t))

(defun use-zenburn-theme ()
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn")
  (load-theme 'zenburn t))

(if window-system (tool-bar-mode 0)
  (menu-bar-mode 0))

;; quiet startup
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; UI prefs
(global-font-lock-mode t)
(global-auto-revert-mode t)
(show-paren-mode t)
(setq show-paren-delay 0.0)
(transient-mark-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(display-time-mode 1)
(setq ido-enable-flex-matching t)

;; cursor
(setq-default cursor-type '(bar . 1))

;; multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; expand-region
(global-set-key (kbd "C-@") 'er/expand-region)

;; Add line numbers
(global-linum-mode 1)

;; treat CamelCase as separate words
(add-hook 'prog-mode-hook 'subword-mode)

;; highlight long lines in programming modes
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; default to splitting windows horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(load-theme 'tango-dark t)
