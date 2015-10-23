(chruby "ruby-2.2.3")
(require 'rcodetools)

(add-hook 'ruby-mode-hook
	  (lambda ()
	    (setq ruby-insert-encoding-magic-comment nil)
            (yas-minor-mode)
            (rspec-mode)
            (flycheck-mode)
            (projectile-rails-mode)
            (local-set-key "\r" 'newline-and-indent)
            (define-key ruby-mode-map (kbd "C-c C-c") 'xmp)))

(hrs/add-auto-mode 'ruby-mode
                   "\\Gemfile$" "\\.rake$" "\\.gemspec$"
                   "\\Gemfile$" "\\Guardfile$" "\\Rakefile$"
                   "\\Vagrantfile$" "\\Vagrantfile.local$")
