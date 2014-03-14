(setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-maildir "~/mail")

(setq mu4e-sent-folder "/Personal/[Gmail].Sent Mail"
      mu4e-drafts-folder "/Personal/[Gmail].Drafts"
      mu4e-trash-folder "/Personal/[Gmail].Trash")

(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash" . ?t)
        ("/[Gmail].All Mail" . ?a)))

(setq mu4e-attachment-dir  "~/Desktop")
(setq mu4e-view-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq user-mail-address "harryrschwartz@gmail.com"
      user-full-name "Harry R. Schwartz")

(setq mu4e-compose-signature-auto-include nil
      mu4e-compose-signature "")

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
