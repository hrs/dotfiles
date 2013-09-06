;; derive the agenda from every file in the org directory, minus the archive
(setq org-agenda-files (remove "~/Dropbox/org/archive.org"
                               (file-expand-wildcards "~/Dropbox/org/*.org")))

(setq org-archive-location "~/Dropbox/org/archive.org::* From %s")

;; calendar preferences
(setq calendar-latitude 42.2)
(setq calendar-longitude -71.1)
(setq calendar-location-name "Cambridge, MA")

;; display prefs
(setq org-hide-leading-stars t)

(setq org-capture-templates
      '(("t" "Todo" entry (file "~/org/index.org")
         "* TODO %?\n  :PROPERTIES:\n  :created: %t\n  :END:\n")
        ("T" "Tagged todo" entry (file "~/org/index.org")
         "* TODO %? %^g\n  :PROPERTIES:\n  :created: %t\n  :END:\n")
        ("j" "Journal item" entry (file "~/org/journal.org")
         "** %?\n   CLOSED: %t\n")))

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-c\C-x\C-s" 'mark-done-and-archive)

(setq org-log-done t)
