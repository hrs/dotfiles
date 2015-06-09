;; standard org <-> remember stuff, RTFM
(require 'org-capture)
(require 'org-protocol)

(setq org-default-notes-file "~/org/index.org")

;; ensure that emacsclient will show just the note to be edited when invoked
;; from Mutt, and that it will shut down emacsclient once finished;
;; fallback to legacy behavior when not invoked via org-protocol.
(add-hook 'org-capture-mode-hook 'delete-other-windows)
(setq my-org-protocol-flag nil)
(defadvice org-capture-finalize (after delete-frame-at-end activate)
  "Delete frame at remember finalization"
  (progn (if my-org-protocol-flag (delete-frame))
         (setq my-org-protocol-flag nil)))
(defadvice org-capture-kill (after delete-frame-at-end activate)
  "Delete frame at remember abort"
  (progn (if my-org-protocol-flag (delete-frame))
         (setq my-org-protocol-flag nil)))
(defadvice org-protocol-capture (before set-org-protocol-flag activate)
  (setq my-org-protocol-flag t))

(defun open-mail-in-mutt (message)
  "Open a mail message in Mutt, using an external terminal.

Message can be specified either by a path pointing inside a
Maildir, or by Message-ID."
  (interactive "MPath or Message-ID: ")
  (shell-command
   (format "gnome-terminal -e \"%s %s\""
	   (substitute-in-file-name "$HOME/bin/mutt-open") message)))

;; add support for "mutt:ID" links
(org-add-link-type "mutt" 'open-mail-in-mutt)
