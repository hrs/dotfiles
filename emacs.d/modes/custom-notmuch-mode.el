(setq send-mail-function 'smtpmail-send-it)

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

(fset 'hrs/use-default-reply-from 'hrs/reply-from-personal-email)

(hrs/use-default-reply-from)
