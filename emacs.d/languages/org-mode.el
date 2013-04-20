;; calendar preferences

(setq calendar-latitude 42.2)
(setq calendar-longitude -71.0)
(setq calendar-location-name "Boston, MA")

;; org-mode stuff

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-c\C-x\C-s" 'mark-done-and-archive)
(setq org-log-done t)
(setq org-agenda-files '("~/Documents/org/main.org"
                         "~/Documents/org/work.org"
                         "~/Documents/org/to-process.org"))

(setq org-archive-location "~/Documents/org/archive.org::* From %s")
