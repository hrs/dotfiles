(add-hook 'ruby-mode-hook
	  (lambda ()
	    (setq ruby-insert-encoding-magic-comment nil)
            (yard-mode)
            (rinari-minor-mode)
            (global-set-key (kbd "C-c C-f") 'rinari-find-file-in-project)
            (setq rinari-tags-file-name "TAGS")
            (local-set-key "\r" 'newline-and-indent)))

(setq auto-mode-alist (cons '("\\.rake$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.gemspec$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\Gemfile$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\Guardfile$" . ruby-mode) auto-mode-alist))
