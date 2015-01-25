(require 'elfeed)

(add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)

(define-key elfeed-show-mode-map "j" 'scroll-up-line)
(define-key elfeed-show-mode-map "k" 'scroll-down-line)

(define-key elfeed-search-mode-map "j" 'next-line)
(define-key elfeed-search-mode-map "k" 'previous-line)

(setq elfeed-search-filter "@2-days-old +unread ")

(load "~/owncloud-sync/feeds.el")
