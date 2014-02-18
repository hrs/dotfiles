(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c s") 'multi-term)
(global-set-key (kbd "C-x |") 'align-regexp)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-M-\\") 'tidy-region)
(global-set-key (kbd "C-c C-s") 'ispell-word)
(global-set-key (kbd "C-c d") 'date)
(global-set-key (kbd "C-c t") 'time)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x w") 'count-words)
(global-set-key (kbd "C-c g") 'search-engine)
(global-set-key (kbd "C-c C-x b") 'generate-scratch-buffer)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-#") 'sort-lines)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; switch to new window on split
(global-set-key (kbd "C-x 2") 'split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'split-window-right-and-switch)

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; rebind M-x more usefully
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old M-x

(define-key input-decode-map "\e[1;2A" [S-up])
