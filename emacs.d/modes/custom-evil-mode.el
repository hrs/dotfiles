(evil-mode 1)

(require 'surround)
(global-surround-mode 1)

(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
