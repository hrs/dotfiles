;; package management
(require 'package)
(require 'cl)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)

;; helm
;; auto-complete (meh)
;; etags-update
;; watch-buffer (meh)
;; wget

(defvar favored-packages
  '(auctex
    clojure-mode
    coffee-mode
    diminish
    elixir-mode
    erlang
    ess ;; Emacs Speaks Statistics: R mode
    expand-region
    gitconfig-mode
    gitignore-mode
    graphviz-dot-mode
    groovy-mode
    hackernews
    haml-mode
    haskell-mode
    htmlize ;; convert region to html, with css for syntax highlighting
    less-css-mode
    magit
    magithub
    markdown-mode
    multiple-cursors
    nrepl
    paredit
    php-mode
    projectile
    python
    rainbow-mode
    rinari
    ruby-end
    ruby-mode
    rust-mode
    rvm
    scss-mode
    shen-mode
    smart-forward
    smex ;; improve M-x prompts
    sml-mode
    solarized-theme
    tuareg
    yaml-mode
    yard-mode
    yasnippet
    zenburn-theme))

(unless (every 'package-installed-p favored-packages)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (pkg favored-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))
