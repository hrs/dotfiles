(setq org-directory "~/org")
(setq notes-directory "~/Dropbox/notes")

(defun construct-filename (directory filename)
  (concat (file-name-as-directory directory) filename))

(add-hook 'org-mode-hook
	  (lambda ()
            (flyspell-mode)))

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (construct-filename org-directory filename))

;; derive the agenda from every file in the org directory, minus the archive
(setq org-agenda-files (remove (org-file-path "archive.org")
                               (file-expand-wildcards (org-file-path "*.org"))))

(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(defun open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (find-file (org-file-path "index.org"))
  (end-of-buffer))

;; calendar preferences
(setq calendar-latitude 42.2)
(setq calendar-longitude -71.1)
(setq calendar-location-name "Cambridge, MA")

;; display prefs
(setq org-hide-leading-stars t)

(setq org-capture-templates
      '(("a" "Article/video to read/watch"
         entry
         (file (org-file-path "articles.org"))
         "* %?\n")

        ("b" "Blog idea"
         entry
         (file (org-file-path "blog-ideas.org"))
         "* TODO %?\n")

        ("g" "Groceries"
         checkitem
         (file (org-file-path "groceries.org")))

        ("q" "Media queues")

        ("qm" "Movies"
         entry
         (file (construct-filename notes-directory "media-movies.org"))
         "* %?\n")

        ("qu" "Music"
         entry
         (file (construct-filename notes-directory "media-music.org"))
         "* %?\n")

        ("qr" "Reading"
         entry
         (file (construct-filename notes-directory "media-reading.org"))
         "* %?\n")

        ("qt" "Television"
         entry
         (file (construct-filename notes-directory "media-tv.org"))
         "* %?\n")

        ("s" "Memorable snippet, word, or fact"
         entry
         (file (construct-filename notes-directory "remember.org"))
         "* %?\n")

        ("t" "Todo"
         entry
         (file (org-file-path "index.org"))
         "* TODO %?\n  %u\n")

        ("T" "Todo with tags"
         entry
         (file (org-file-path "index.org"))
         "* TODO %? %^g\n  %u\n")))

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
