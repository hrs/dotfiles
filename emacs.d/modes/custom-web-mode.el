(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$\\'" . web-mode))

(add-hook 'web-mode-hook 'rainbow-mode)
