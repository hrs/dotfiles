(add-hook 'haml-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

(add-hook 'haml-mode-hook 'rainbow-mode)
