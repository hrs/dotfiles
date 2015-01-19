(defun hrs/not-david-p (s)
  (not (string-match "david" s)))

(defun hrs/filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun hrs/filter-david-from-load-path ()
  (setq load-path (hrs/filter #'hrs/not-david-p load-path)))

(defun hrs/default-to-home-directory ()
  (setq default-directory "~/"))

(defun hrs/configure-cask ()
  (require 'cask "~/.cask/cask.el")
  (cask-initialize)
  (require 'pallet))

(defun hrs/include-custom-code-paths ()
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (add-to-list 'load-path "~/.emacs.d/modes/"))

(defun hrs/configure-load-path ()
  (hrs/filter-david-from-load-path)
  (hrs/default-to-home-directory)
  (hrs/configure-cask)
  (hrs/include-custom-code-paths))

(defun hrs/increase-gc-threshold ()
    "Allow 20MB of memory (instead of 0.76MB) before calling GC."
    (setq gc-cons-threshold 20000000))

(defun hrs/extend-exec-path ()
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(defun hrs/configure-all-custom-modes ()
  (mapcar (lambda (mode-file-name) (load mode-file-name))
          (directory-files "~/.emacs.d/modes/" nil ".el")))

(defun hrs/backup-to-temp-directory ()
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(defun hrs/configure-auto-complete ()
  (require 'auto-complete-config)
  (ac-config-default))

(defun hrs/delete-trailing-whitespace ()
  (add-hook 'before-save-hook
            'delete-trailing-whitespace))

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
  (ido-vertical-mode 1))

(defun hrs/enable-region-case-modification ()
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

(defun hrs/treat-camelcase-as-separate-words ()
  (add-hook 'prog-mode-hook 'subword-mode))

(defun hrs/configure-wrap-region ()
  (wrap-region-global-mode t)
  (wrap-region-add-wrapper "/" "/" nil 'ruby-mode)
  (wrap-region-add-wrapper "`" "`" nil '(markdown-mode ruby-mode)))

(defun hrs/offer-to-create-parent-directories-on-save ()
  (add-hook 'before-save-hook
            (lambda ()
              (when buffer-file-name
                (let ((dir (file-name-directory buffer-file-name)))
                  (when (and (not (file-exists-p dir))
                             (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                    (make-directory dir t)))))))

(defun hrs/automatically-follow-symlinks ()
  (setq vc-follow-symlinks t))

(defun hrs/make-scripts-executable ()
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hrs/configure-load-path)

(load "ui-prefs.el")
(load "utils.el")
(load "password-management.el")
(load "erc-config.el")
(load "emms-config.el")

(require 'dired-x)
;; (require 'multi-term)

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; (require 'mu4e)

(hrs/increase-gc-threshold)
(hrs/extend-exec-path)
(hrs/configure-all-custom-modes)

(delete-selection-mode t)
(setq-default indent-tabs-mode nil)

(hrs/backup-to-temp-directory)
(hrs/configure-auto-complete)
(hrs/delete-trailing-whitespace)
(hrs/configure-yasnippet)
(hrs/configure-ido)
(hrs/enable-region-case-modification)
(hrs/treat-camelcase-as-separate-words)
(hrs/configure-wrap-region)
(hrs/offer-to-create-parent-directories-on-save)
(hrs/automatically-follow-symlinks)
(hrs/make-scripts-executable)

(projectile-global-mode)

(load "keybindings.el")

(setq require-final-newline t)
(setq confirm-kill-emacs 'y-or-n-p)
