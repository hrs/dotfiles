;; calendar preferences

(setq calendar-latitude 42.2)
(setq calendar-longitude -71.0)
(setq calendar-location-name "Boston, MA")

;; org-mode stuff

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '("~/Documents/org/main.org"
                         "~/Documents/org/work.org"
                         "~/Documents/org/to-process.org"))

(setq org-archive-location "~/Documents/org/archive.org::* From %s")
