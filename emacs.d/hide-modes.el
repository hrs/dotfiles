(defvar modes-to-hide
  '(git-gutter-mode
    global-whitespace-mode
    projectile-mode
    rinari-minor-mode
    yard-mode
    yas-minor-mode))

(defun empty-mode (mode)
  (cons mode '("")))

(defun hidden-mode-p (mode)
  (member (car mode) modes-to-hide))

(defun hide-modes ()
  (setq minor-mode-alist (append (mapcar 'empty-mode modes-to-hide)
                                 (remove-if 'hidden-mode-p minor-mode-alist))))
