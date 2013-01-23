(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-l"     'goto-line) ; overwriting vertical center on point
(global-set-key "\C-cs"    'shell)
(global-set-key "\C-x|"    'align-regexp)
(global-set-key "\M-/"     'hippie-expand)
(global-set-key "\C-\M-\\" 'tidy-region)
(global-set-key "\C-c\C-s" 'ispell-word)
(global-set-key "\C-cd"    'date)
(global-set-key "\C-ct"    'time)

;; rebind M-x more usefully
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old M-x

(when (not window-system)
  (define-key input-decode-map "\e[1;2A" [S-up]))
