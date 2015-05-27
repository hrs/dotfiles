;; UI preferences

(load "hide-modes.el")
(smex-initialize)

(defun hrs/fullscreen-on-macs ()
  (when (hrs/mac?)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun hrs/set-default-font ()
  (set-frame-font "Inconsolata-14"))

(defun hrs/disable-window-chrome ()
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (if window-system
    (scroll-bar-mode -1)))

(defun hrs/quiet-startup ()
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil))

(defun hrs/make-dired-file-sizes-human-readable ()
  (setq-default dired-listing-switches "-alh"))

(defun hrs/highlight-long-lines-in-programming-modes ()
  (require 'whitespace)
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(defun hrs/customize-solarized-appearance ()
  (setq solarized-use-variable-pitch nil)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hrs/fullscreen-on-macs)
(hrs/set-default-font)
(hrs/disable-window-chrome)
(hrs/quiet-startup)

;; UI prefs
(global-font-lock-mode t)
(global-auto-revert-mode t)
(show-paren-mode t)
(setq show-paren-delay 0.0)
(transient-mark-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ns-pop-up-frames nil)
(setq visible-bell t)

(display-time)

(setq fill-column 80)

(hrs/make-dired-file-sizes-human-readable)
(hrs/highlight-long-lines-in-programming-modes)
(hrs/customize-solarized-appearance)
