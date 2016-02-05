(require 'ox-md)
(require 'ox-beamer)

(setq org-directory "~/org")
(setq notes-directory "~/documents/notes")

(defun construct-filename (directory filename)
  (concat (file-name-as-directory directory) filename))

(add-hook 'org-mode-hook
	  (lambda ()
            (org-bullets-mode t)
            (local-set-key (kbd "C-x s")
                           'org-insert-src-block)
            (local-set-key (kbd "C-c v")
                           'org-show-todo-tree)))

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (construct-filename org-directory filename))

;; derive the agenda from every file in the org directory, minus the archive
(setq org-agenda-files (list (org-file-path "index.org")))

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

        ("m" "Mail"
         entry
         (file (org-file-path "index.org"))
         "* TODO %^{Title}\n  Source: %u, %c\n\n  %i"
         :empty-lines 1)

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
         "* TODO %?\n")))

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-c\C-x\C-s" 'mark-done-and-archive)

(setq org-log-done t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)))

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-html-head
      (with-temp-buffer
        (insert "<style type=\"text/css\">\n")
        (insert-file-contents "~/.emacs.d/modes/default-org-export-stylesheet.css")
        (end-of-buffer)
        (insert "</style>")
        (buffer-string)))
