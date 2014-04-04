;; make node prompt readable
(setenv "NODE_NO_READLINE" "1")

(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))
