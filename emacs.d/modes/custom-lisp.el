(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(defun pretty-lambdas ()
  "Replace lambdas with the Greek character."
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hooks))

;; use pretty lambdas and paredit in all lisp-like modes
(dolist (hook lispy-mode-hooks)
    (add-hook hook 'pretty-lambdas)
    (add-hook hook 'paredit-mode)
    (add-hook hook 'eldoc-mode))
