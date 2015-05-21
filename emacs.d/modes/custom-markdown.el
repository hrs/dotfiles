(hrs/add-auto-mode 'markdown-mode "\\.md$")

(add-hook 'markdown-mode-hook
	  (lambda ()
            (flyspell-mode)))
