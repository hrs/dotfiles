(add-to-list 'load-path "~/.emacs/languages/slime")
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup)
