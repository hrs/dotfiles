(require 'notmuch)
(require 'org-notmuch)

(setq send-mail-function 'smtpmail-send-it)
(evil-set-initial-state 'message-mode 'insert)
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
(setq notmuch-crypto-process-mime t)

(setq user-full-name "Harry R. Schwartz")

(defun hrs/reply-from-personal-email ()
  (setq
   user-mail-address "hello@harryrschwartz.com"
   smtpmail-stream-type 'ssl
   smtpmail-smtp-server "mail.messagingengine.com"
   smtpmail-smtp-service 465))

(defun hrs/reply-from-work-email ()
  (setq
   user-mail-address "harry@thoughtbot.com"
   smtpmail-stream-type 'ssl
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 465))

(defun hrs/refresh-notmuch-database ()
  (shell-command "notmuch new"))

(defun hrs/fetch-new-mail ()
  (interactive)
  (save-excursion
    (shell-command "offlineimap -qf INBOX > /dev/null")
    (hrs/refresh-notmuch-database)))

(defun hrs/sync-all-mailboxes ()
  (interactive)
  (shell-command "offlineimap > /dev/null")
  (hrs/refresh-notmuch-database))

(define-key notmuch-hello-mode-map "o" 'hrs/fetch-new-mail)
(define-key notmuch-hello-mode-map "O" 'hrs/sync-all-mailboxes)
(define-key notmuch-hello-mode-map "g" 'notmuch-jump-search)

(define-key notmuch-search-mode-map "g" 'notmuch-jump-search)
(define-key notmuch-search-mode-map "j" 'notmuch-search-next-thread)
(define-key notmuch-search-mode-map "k" 'notmuch-search-previous-thread)

(define-key notmuch-show-mode-map "j" 'next-line)
(define-key notmuch-show-mode-map "k" 'previous-line)
(define-key notmuch-show-mode-map "h" 'left-char)
(define-key notmuch-show-mode-map "l" 'right-char)
(define-key notmuch-show-mode-map "J" 'notmuch-show-next-thread-show)
(define-key notmuch-show-mode-map "K" 'notmuch-show-previous-thread-show)
(define-key notmuch-show-mode-map (kbd "C-c C-o") 'org-open-at-point)

(fset 'hrs/use-default-reply-from 'hrs/reply-from-personal-email)
(hrs/use-default-reply-from)
