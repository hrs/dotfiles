(add-hook 'term-mode-hook
	  (lambda ()
            (setq multi-term-program-switches "--login")
            (define-key term-raw-map (kbd "M-o") 'other-window)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (setq yas-dont-activate t)))
