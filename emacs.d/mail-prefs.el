(setq user-mail-address "hrs@ginkgobioworks.com")
(setq user-full-name "Harry R Schwartz")

(load-library "smtpmail")
(load-library "nnimap")
(load-library "starttls")

(setq gnus-select-method '(nnimap "imap.gmail.com"
				  (nnimap-address "imap.gmail.com")
				  (nnimap-server-port 993)
				  (nnimap-authinfo-file "~/.imap-authinfo")
				  (nnimap-stream ssl)))

(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com"
				   587
				   "hrs@ginkgobioworks.com"
				   nil)))

(add-hook 'gnus-topic-mode-hook
	  'gnus-topic-mode)
