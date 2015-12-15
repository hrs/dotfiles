(require 'dired-x)
(require 'dired+)
(require 'dired-open)

(setq dired-open-extensions
      '(("pdf" . "evince")
        ("mkv" . "vlc")
        ("mp4" . "vlc")
        ("avi" . "vlc")))

(setq-default dired-listing-switches "-lhvA")

(setq dired-clean-up-buffers-too t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)
