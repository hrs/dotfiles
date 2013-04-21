(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; make those lambdas pretty!
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(setq lispy-modes '(emacs-lisp-mode lisp-mode scheme-mode))

;; use pretty lambdas and paredit in all lisp-like modes
(dolist (mode lispy-modes)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook 'pretty-lambdas)
    (add-hook hook 'paredit-mode)))
