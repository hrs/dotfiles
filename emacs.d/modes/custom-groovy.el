(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)

(hrs/add-auto-mode 'groovy-mode "\.groovy$" "\\.gremlin$")
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(add-hook 'groovy-mode-hook
          '(lambda ()
             (setq c-basic-offset 4)))
