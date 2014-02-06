(add-hook 'coffee-mode-hook
	  (lambda ()
            (yas-minor-mode 1)
            (setq coffee-tab-width 2)))
