(chruby "ruby-2.1.5")
(require 'rcodetools)

(add-hook 'ruby-mode-hook
	  (lambda ()
	    (setq ruby-insert-encoding-magic-comment nil)
            (rinari-minor-mode)
            (yas-minor-mode)
            (flycheck-mode)
            (global-set-key (kbd "C-c C-f") 'rinari-find-file-in-project)
            (setq rinari-tags-file-name "TAGS")
            (projectile-rails-mode)
            (local-set-key "\r" 'newline-and-indent)
            (define-key ruby-mode-map (kbd "C-c C-c") 'xmp)))

(setq files-in-ruby-mode
      '("\\Gemfile$"
        "\\.rake$"
        "\\.gemspec$"
        "\\Gemfile$"
        "\\Guardfile$"
        "\\Rakefile$"
        "\\Vagrantfile$"
        "\\Vagrantfile.local$"))

(dolist (file-regexp files-in-ruby-mode)
  (add-to-list 'auto-mode-alist `(,file-regexp . ruby-mode)))
