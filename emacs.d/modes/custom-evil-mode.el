(evil-mode 1)

(require 'surround)
(global-surround-mode 1)

(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(evil-define-key 'normal dired-mode-map (kbd "j") 'dired-next-line)
(evil-define-key 'normal dired-mode-map (kbd "k") 'dired-previous-line)
