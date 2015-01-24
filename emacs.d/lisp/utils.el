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
  "Insert today's date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun time ()
  "Insert the current time."
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
  "Swap the line at point with the line above it, moving point to the line below."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Swap the line at point with the line beneath it, moving point to the line below."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
   name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
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
  "Pretty-print the JSON in the marked region. Currently shells
   out to `jsonpp'--be sure that's installed!"
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

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun visit-last-dired-file ()
  "Open the last file in an open dired buffer."
  (interactive)
  (end-of-buffer)
  (previous-line)
  (dired-find-file))

(defun visit-last-migration ()
  "Open the last file in 'db/migrate/'."
  (interactive)
  (dired (expand-file-name "db/migrate" (projectile-project-root)))
  (visit-last-dired-file)
  (kill-buffer "migrate"))

(defun hrs/mac-p ()
  "Returns `t' if this is an Apple machine, nil otherwise."
  (eq system-type 'darwin))

(defun hrs/system-notify (title message)
  "Display an alert window with `title' and `message'.

Depends on `terminal-notifier', which can be installed with `brew install terminal-notifier')."
  (when (hrs/mac-p)
    (with-temp-buffer
      (shell-command (format "terminal-notifier -title \"%s\" -message \"%s\"" title message) t))))

(if (hrs/mac-p)
    (setq os-terminal-command "open -a iTerm")
  (setq os-terminal-command "xterm"))

(defun hrs/os-terminal-here ()
  "Open an OS-specific terminal in the directory of the current buffer."
  (interactive)
  (dired-smart-shell-command (concat os-terminal-command " $PWD") nil nil))
