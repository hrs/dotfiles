(chruby "ruby-2.2.3")
(require 'rcodetools)

(add-hook 'enh-ruby-mode-hook
	  (lambda ()
	    (setq ruby-insert-encoding-magic-comment nil)
            (yas-minor-mode)
            (rspec-mode)
            (projectile-rails-mode)
            (yard-mode)
            (flycheck-mode)
            (local-set-key "\r" 'newline-and-indent)
            (define-key enh-ruby-mode-map (kbd "C-c C-c") 'xmp)))

(hrs/add-auto-mode 'enh-ruby-mode
                   "\\.rb$" "\\Gemfile$" "\\.rake$"
                   "\\.gemspec$" "\\Gemfile$" "\\Guardfile$"
                   "\\Rakefile$" "\\Vagrantfile$"
                   "\\Vagrantfile.local$")
