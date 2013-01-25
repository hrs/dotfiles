(global-set-key (kbd "C-w")     'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-l")     'goto-line) ; overwriting vertical center on point
(global-set-key (kbd "C-c s")   'shell)
(global-set-key (kbd "C-x |")   'align-regexp)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-M-\\")  'tidy-region)
(global-set-key (kbd "C-c C-s") 'ispell-word)
(global-set-key (kbd "C-c d")   'date)
(global-set-key (kbd "C-c t")   'time)
(global-set-key (kbd "C-x g")   'magit-status)

;; semantic navigation
(global-set-key (kbd "C-@") 'er/expand-region)

;; rebind M-x more usefully
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old M-x

(when (not window-system)
  (define-key input-decode-map "\e[1;2A" [S-up]))
