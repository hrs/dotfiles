(add-hook 'haml-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

(add-hook 'haml-mode-hook 'rainbow-mode)
(setq auto-mode-alist (cons '("\\.hamlc$" . haml-mode) auto-mode-alist))
