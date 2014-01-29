(setq auto-mode-alist
      (cons '("\\.rhtml$" . html-mode) auto-mode-alist))

(add-hook 'html-mode-hook 'rainbow-mode)
