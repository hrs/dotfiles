(load-file "~/code/personal/sensible-emacs-defaults/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

(defun hrs/configure-cask ()
  (require 'cask "~/.cask/cask.el")
  (cask-initialize)
  (require 'pallet))

(defun hrs/include-custom-code-paths ()
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (add-to-list 'load-path "~/.emacs.d/modes/"))

(defun hrs/extend-exec-path ()
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(defun hrs/configure-all-custom-modes ()
  (mapcar (lambda (mode-file-name) (load mode-file-name))
          (directory-files "~/.emacs.d/modes/" nil ".el")))

(defun hrs/configure-auto-complete ()
  (require 'auto-complete-config)
  (ac-config-default))

(defun hrs/configure-yasnippet ()
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
  (yas-global-mode 1))

(defun hrs/configure-ido ()
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  (ido-ubiquitous)
  (flx-ido-mode 1) ; better/faster matching
  (setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(defun hrs/enable-region-case-modification ()
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

(defun hrs/configure-wrap-region ()
  (wrap-region-global-mode t)
  (wrap-region-add-wrapper "/" "/" nil 'ruby-mode)
  (wrap-region-add-wrapper "`" "`" nil '(markdown-mode ruby-mode)))

(defun hrs/split-horizontally-for-temp-buffers ()
  (when (one-window-p t)
      (split-window-horizontally)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hrs/include-custom-code-paths)
(hrs/configure-cask)

(load "utils.el")
(load "ui-prefs.el")
(load "password-management.el")

(hrs/extend-exec-path)
(hrs/configure-all-custom-modes)

(setq-default indent-tabs-mode nil)

(hrs/configure-auto-complete)
(hrs/configure-yasnippet)
(hrs/configure-ido)
(hrs/enable-region-case-modification)
(hrs/configure-wrap-region)

(add-hook 'temp-buffer-window-setup-hook
          'hrs/split-horizontally-for-temp-buffers)

(projectile-global-mode)

(load "keybindings.el")
