;; calendar preferences

(setq calendar-latitude 42.2)
(setq calendar-longitude -71.0)
(setq calendar-location-name "Boston, MA")

(setq org-hide-leading-stars t)

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-c\C-x\C-s" 'mark-done-and-archive)
(setq org-log-done t)

;; derive the agenda from every file in the org directory, minus the archive
(setq org-agenda-files (remove "~/Dropbox/org/archive.org"
                               (file-expand-wildcards "~/Dropbox/org/*.org")))

(setq org-archive-location "~/Dropbox/org/archive.org::* From %s")
