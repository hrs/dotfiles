(add-hook 'haml-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

(add-hook 'haml-mode-hook 'rainbow-mode)
(hrs/add-auto-mode 'haml-mode "\\.hamlc$")
