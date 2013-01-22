;;; rinari-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rinari-minor-mode rinari-launch) "rinari" "rinari.el"
;;;;;;  (20462 2452))
;;; Generated autoloads from rinari.el

(autoload 'rinari-launch "rinari" "\
Run `rinari-minor-mode' if inside of a rails projecct,
otherwise turn `rinari-minor-mode' off if it is on.

\(fn)" t nil)

(defvar rinari-major-modes (if (boundp 'rinari-major-modes) rinari-major-modes (list 'find-file-hook 'mumamo-after-change-major-mode-hook 'dired-mode-hook)) "\
Major Modes from which to launch Rinari.")

(dolist (hook rinari-major-modes) (add-hook hook 'rinari-launch))

(autoload 'rinari-minor-mode "rinari" "\
Enable Rinari minor mode providing Emacs support for working
with the Ruby on Rails framework.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("rinari-pkg.el") (20462 2452 97907))

;;;***

(provide 'rinari-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rinari-autoloads.el ends here
