(package-initialize)

;; I use =cask= for managing packages. These need to be included in here to
;; ensure that the correct version of =org= is used to render my
;; =configuration.org= file.
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(org-babel-load-file "~/.emacs.d/configuration.org")
