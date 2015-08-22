(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")

(defun hrs/pretty-lambdas ()
  "Replace lambdas with the Greek character."
  (custom-set-faces
   '(font-lock-keyword-face ((t (:weight normal)))))
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (hrs/pretty-lambdas)
                   (setq show-paren-style 'expression)
                   (paredit-mode)
                   (eldoc-mode)
                   (rainbow-delimiters-mode))))
