(defun tidy-region (start end)
  "Indent, delete whitespace, and untabify the region."
  (interactive "r")
  (progn
    (delete-trailing-whitespace start end)
    (indent-region start end nil)
    (untabify start end)))

(defun tidy-buffer ()
  "Indent, delete whitespace, and untabify the buffer."
  (interactive)
  (save-excursion
    (tidy-region (point-min) (point-max))))

(defun date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun time ()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun view-buffer-name ()
  "Display the filename of the current buffer."
  (interactive)
  (message (buffer-file-name)))

(setq search-engine-url "http://www.google.com/search?ie=utf-8&oe=utf-8&q=")

(defun search-engine ()
  "Search the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat search-engine-url
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search: "))))))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun generate-scratch-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun split-window-below-and-switch ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun de-unicode ()
  "Tidy up a buffer by replacing all special Unicode characters
   (smart quotes, etc.) with their more sane cousins"
  (interactive)
  (let ((unicode-map '(("[\u2018\|\u2019\|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("\u2013" . "--")
                       ("\u2014" . "---")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))
    (save-excursion
      (loop for (key . value) in unicode-map
            do
            (goto-char (point-min))
            (replace-regexp key value)))))

(defun beautify-json ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "jsonpp" (buffer-name) t)))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
