(package-initialize)

;; I use =cask= and =pallet= for managing packages. These need to be included in
;; here to ensure that the correct version of =org= is used to render my
;; =configuration.org= file.
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(org-babel-load-file "~/.emacs.d/configuration.org")
