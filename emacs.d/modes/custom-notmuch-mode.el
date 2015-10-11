(setq send-mail-function 'smtpmail-send-it)
(evil-set-initial-state 'message-mode 'insert)
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

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

(fset 'hrs/use-default-reply-from 'hrs/reply-from-personal-email)
(hrs/use-default-reply-from)
