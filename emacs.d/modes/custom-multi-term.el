(setq multi-term-program-switches "--login")

(add-hook 'term-mode-hook
	  (lambda ()
            (define-key term-raw-map (kbd "M-o") 'other-window)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (setq yas-dont-activate t)))
