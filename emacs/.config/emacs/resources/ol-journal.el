;;; ol-journal.el -- Define a custom `journal:' link type.

;;; Commentary:

;; Link journal files like [[journal:2020-05-08][what a mess]].

;;; Code:
(eval-when-compile (require 'ol))

(defcustom ol-journal-entries-directory (expand-file-name "~/documents/journal/entries")
  "The directory containing journal entries as Org files."
  :group 'ol-journal
  :type 'string)

(org-link-set-parameters "journal"
                         :follow #'ol-journal-open
                         :export #'ol-journal-export
                         :store #'ol-journal-store-link)

(defun ol-journal-open (date)
  "Visit the journal entry from DATE. DATE should be a YYYY-MM-DD formatted string."
  (find-file (ol-journal--path-by-date date)))

(defun ol-journal--entry-p (buffer)
  "Return t if BUFFER is a journal entry."
  (let ((path (or (buffer-file-name buffer) "")))
    (and (string-prefix-p ol-journal-entries-directory path)
         (= 0 (string-match
               (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) ".org" eol)
               (file-relative-name path ol-journal-entries-directory))))))

(defun ol-journal--entry-link (buffer)
  "Return the YYYY-MM-DD stamp of the entry at BUFFER."
  (file-name-sans-extension
   (file-relative-name (buffer-file-name buffer) ol-journal-entries-directory)))

(defun ol-journal--path-by-date (date)
  "Return the absolute path to the entry at DATE."
  (expand-file-name (concat date ".org")
                    ol-journal-entries-directory))

(defun ol-journal-store-link ()
  "Store a link to a journal entry."
  (when (ol-journal--entry-p (current-buffer))
    ;; This is a journal entry, we do make this link.
    (let* ((date (ol-journal--entry-link (current-buffer)))
           (link (concat "journal:" date))
           (description date))
      (org-link-store-props
       :type "journal"
       :link link
       :description description))))

(defun ol-journal-export (date description format)
  "Export a `journal:' link from Org files.
The DATE, DESCRIPTION, and FORMAT are handled by the export
backend."
  (let* ((path (file-relative-name (ol-journal--path-by-date date)))
         (p (file-name-sans-extension path))
         (desc (or description (concat "journal:" date))))
    (cond
     ((eq format 'html) (format "<a href=\"%s.html\">%s</a>" p desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "[%s] <journal:%s>" desc path)) ; Could do better, maybe.
     ((eq format 'md) (format "[%s](%s.md)" desc p))
     (t path))))

(provide 'ol-journal)
;;; ol-journal.el ends here
