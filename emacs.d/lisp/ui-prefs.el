;; UI preferences

(load "hide-modes.el")
(smex-initialize)

(defun hrs/fullscreen-on-macs ()
  (when (hrs/mac?)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun hrs/set-default-font ()
  (if (hrs/mac?)
      (set-frame-font "Inconsolata-18")
      (set-frame-font "Inconsolata-14")))

(defun hrs/disable-window-chrome ()
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (when window-system
    (scroll-bar-mode -1)))

(defun hrs/indicate-long-lines-in-programming-modes ()
  (setq fci-rule-column fill-column)
  (setq fci-rule-use-dashes t)
  (setq fci-rule-color "white")
  (add-hook 'prog-mode-hook 'turn-on-fci-mode))

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

(when window-system
  (global-hl-line-mode)
  (hrs/customize-solarized-appearance))
(hrs/indicate-long-lines-in-programming-modes)
