(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")

(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (setq show-paren-style 'expression)
                   (paredit-mode)
                   (eldoc-mode)
                   (rainbow-delimiters-mode))))

(hrs/add-auto-mode 'scheme-mode "\\.blu$")
