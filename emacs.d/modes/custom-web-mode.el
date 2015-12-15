(hrs/add-auto-mode 'web-mode "\\.erb$" "\\.html$" "\\.php$" "\\.rhtml$")

(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rspec-mode)
(add-hook 'web-mode-hook (lambda () (setq web-mode-markup-indent-offset 2)))
