;;; sml-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (sml-yacc-mode sml-lex-mode sml-cm-mode sml-mode
;;;;;;  sml-run) "sml-mode" "sml-mode.el" (20692 48574))
;;; Generated autoloads from sml-mode.el

(defalias 'run-sml 'sml-run)

(autoload 'sml-run "sml-mode" "\
Run the program CMD with given arguments ARG.
The command is run in buffer *CMD* using mode `inferior-sml-mode'.
If the buffer already exists and has a running process, then
just go to this buffer.

If a prefix argument is used, the user is also prompted for a HOST
on which to run CMD using `remote-shell-program'.

\(Type \\[describe-mode] in the process's buffer for a list of commands.)

\(fn CMD ARG &optional HOST)" t nil)

(add-to-list 'auto-mode-alist '("\\.s\\(ml\\|ig\\)\\'" . sml-mode))

(autoload 'sml-mode "sml-mode" "\
\\<sml-mode-map>Major mode for editing Standard ML code.
This mode runs `sml-mode-hook' just before exiting.
See also (info \"(sml-mode)Top\").
\\{sml-mode-map}

\(fn)" t nil)

(add-to-list 'completion-ignored-extensions ".cm/")

(add-to-list 'auto-mode-alist '("\\.cm\\'" . sml-cm-mode))

(autoload 'sml-cm-mode "sml-mode" "\
Major mode for SML/NJ's Compilation Manager configuration files.

\(fn)" t nil)

(autoload 'sml-lex-mode "sml-mode" "\
Major Mode for editing ML-Lex files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.grm\\'" . sml-yacc-mode))

(autoload 'sml-yacc-mode "sml-mode" "\
Major Mode for editing ML-Yacc files.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("sml-mode-pkg.el") (20692 48574 241931))

;;;***

(provide 'sml-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sml-mode-autoloads.el ends here
