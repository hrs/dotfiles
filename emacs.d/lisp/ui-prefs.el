;; UI preferences

(load "fonts.el")
(load "hide-modes.el")
(smex-initialize)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

(when (eq system-type 'darwin)
  (set-default-font "-apple-inconsolata-medium-r-normal--12-120-72-72-m-120-iso10646-1"))

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
(setq ns-pop-up-frames nil)

;; dired
(setq-default dired-listing-switches "-alh") ; make file sizes human-readable

;; ido configuration
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(flx-ido-mode 1) ; better/faster matching
(ido-vertical-mode 1)
(setq ido-create-new-buffer 'always) ; don't confirm to create new buffers

;; cursor
(setq-default cursor-type '(bar . 2))
(global-hl-line-mode t)

;; multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; expand-region
(global-set-key (kbd "C-@") 'er/expand-region)

;; treat CamelCase as separate words
(add-hook 'prog-mode-hook 'subword-mode)

;; highlight long lines in programming modes
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(setq solarized-use-variable-pitch nil)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-light t)
