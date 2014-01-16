;; minor modes to hide altogether
(eval-after-load "global-whitespace"
  '(diminish 'global-whitespace-mode))
(eval-after-load "projectile"
  '(diminish 'projectile-mode))
(eval-after-load "rinari-minor"
  '(diminish 'rinari-minor-mode))
(eval-after-load "smartparens"
  '(diminish 'smartparens-mode))
(eval-after-load "yard"
  '(diminish 'yard-mode))
(eval-after-load "yas-minor"
  '(diminish 'yas/minor-mode))

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
