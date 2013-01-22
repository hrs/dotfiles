;; don't insert the encoding comment
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (setq ruby-insert-encoding-magic-comment nil)))

(setq auto-mode-alist (cons '("\\.rake$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\Gemfile$" . ruby-mode) auto-mode-alist))
