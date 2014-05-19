;; minor modes to hide altogether
(eval-after-load "auto-complete"
  '(diminish 'auto-complete-mode))
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))
(eval-after-load "flycheck"
  '(diminish 'flycheck-mode))
(eval-after-load "flyspell"
  '(diminish 'flyspell-mode))
(eval-after-load "global-whitespace"
  '(diminish 'global-whitespace-mode))
(eval-after-load "magit"
  '(diminish 'magit-auto-revert-mode))
(eval-after-load "projectile"
  '(diminish 'projectile-mode))
(eval-after-load "rinari-minor"
  '(diminish 'rinari-minor-mode))
(eval-after-load "smartparens"
  '(diminish 'smartparens-mode))
(eval-after-load "subword"
  '(diminish 'subword-mode))
(eval-after-load "yard"
  '(diminish 'yard-mode))
(eval-after-load "yasnippet"
  '(diminish 'yas-minor-mode))
(eval-after-load "whitespace"
  '(diminish 'whitespace-mode))
(eval-after-load "wrap-region"
  '(diminish 'wrap-region-mode))

;; minor modes to rename
(eval-after-load "paredit"
  '(diminish 'paredit-mode " π"))

;; major modes to rename
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq mode-name "el")))
(add-hook 'haskell-mode-hook
          (lambda () (setq mode-name "λ=")))
(add-hook 'lisp-interaction-mode-hook
          (lambda () (setq mode-name "λ")))
(add-hook 'python-mode-hook
          (lambda () (setq mode-name "Py")))
