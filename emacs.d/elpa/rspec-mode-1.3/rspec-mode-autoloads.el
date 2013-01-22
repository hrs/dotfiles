;;; rspec-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rspec-buffer-is-spec-p rspec-mode) "rspec-mode"
;;;;;;  "rspec-mode.el" (20498 39978 778288 200000))
;;; Generated autoloads from rspec-mode.el

(autoload 'rspec-mode "rspec-mode" "\
Minor mode for rSpec files

\(fn &optional ARG)" t nil)

(autoload 'rspec-buffer-is-spec-p "rspec-mode" "\
Returns true if the current buffer is a spec

\(fn)" nil nil)

(eval-after-load 'ruby-mode '(add-hook 'ruby-mode-hook (lambda nil (when (rspec-buffer-is-spec-p) (rspec-mode)))))

(eval-after-load 'ruby-mode '(add-hook 'ruby-mode-hook (lambda nil (local-set-key rspec-key-command-prefix rspec-mode-verifible-keymap))))

(eval-after-load 'rails '(add-hook 'rails-minor-mode-hook (lambda nil (local-set-key rspec-key-command-prefix rspec-mode-verifible-keymap))))

;;;***

;;;### (autoloads nil nil ("rspec-mode-pkg.el") (20498 39978 897400
;;;;;;  840000))

;;;***

(provide 'rspec-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rspec-mode-autoloads.el ends here
