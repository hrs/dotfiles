(setq auto-mode-alist
      (cons '("\\.md$" . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook
	  (lambda ()
            (flyspell-mode)))
