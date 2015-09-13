(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c s") 'multi-term)
(global-set-key (kbd "C-x |") 'align-regexp)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-s") 'ispell-word)
(global-set-key (kbd "C-c d") 'hrs/date)
(global-set-key (kbd "C-c t") 'hrs/time)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c C-x b") 'hrs/generate-scratch-buffer)
(global-set-key (kbd "M-;") 'hrs/comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-#") 'sort-lines)
(global-set-key (kbd "C-x k") 'hrs/kill-current-buffer)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; switch to new window on split
(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)

;; change font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C-_") 'text-scale-decrease)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; org-mode
(global-set-key (kbd "C-c i") 'open-index-file)
(global-set-key (kbd "M-n") 'org-capture-todo)

(global-set-key (kbd "M-<up>") 'hrs/drag-line-up)
(global-set-key (kbd "M-<down>") 'hrs/drag-line-down)

;; rebind M-x more usefully
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old M-x

(define-key input-decode-map "\e[1;2A" [S-up])
