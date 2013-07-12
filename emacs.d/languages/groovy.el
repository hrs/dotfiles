;;; ELPA's busted for groovy-mode apparently, so load the files directly
(load "~/.emacs.d/languages/groovy/groovy-mode.el")
;; (load "~/.emacs.d/languages/groovy/groovy-electric.el")

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gremlin$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(add-hook 'groovy-mode-hook
          '(lambda ()
             (setq c-basic-offset 4)))

;; ;;; make Groovy mode electric by default.
;; (add-hook 'groovy-mode-hook
;;           '(lambda ()
;;              (require 'groovy-electric)
;;              (groovy-electric-mode)))
