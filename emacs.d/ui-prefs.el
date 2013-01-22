;; UI preferences

;; better font
(when window-system
  ;; (speedbar 1)
  (set-face-attribute 'default nil :font "Inconsolata-dz" :height 90))

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
(menu-bar-mode 0)
(tool-bar-mode 0)
(display-time-mode 1)

;; Add line numbers
(line-number-mode "on")
(require 'linum)
(global-linum-mode 1)

(setq whitespace-style '(lines))
(setq whitespace-line-column 78)
(global-whitespace-mode 1)

;; Improve scrolling
(setq
 scroll-margin 5
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

(load-theme 'tango-dark t)

;; use Solarized theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")
;; (load-theme 'solarized-dark t)
;; use Zenburn theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn")
;; (load-theme 'zenburn t)
