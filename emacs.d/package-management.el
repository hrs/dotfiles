;; package management
(require 'package)
(require 'cl)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)

;; anything
;; auto-complete
;; ctags-update
;; expand-region & smart-forward
;; watch-buffer
;; wget

(defvar favored-packages
  '(auctex
    clojure-mode
    coffee-mode
    elixir-mode
    erlang
    ess ;; Emacs Speaks Statistics: R mode
    groovy-mode
    haml-mode
    haskell-mode
    htmlize
    magit
    magithub
    markdown-mode
    octave-mod
    paredit
    python
    rainbow-mode
;;  rinari
    ruby-end
    ruby-mode
    ruby-electric
    rust-mode
    scss-mode
    shen-mode
    smex
    sml-mode
    solarized-theme
    tuareg
    yaml-mode
    yasnippet
    zenburn-theme))

(defun favored-packages-installed-p ()
  (loop for pkg in favored-packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (favored-packages-installed-p)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (pkg favored-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))
