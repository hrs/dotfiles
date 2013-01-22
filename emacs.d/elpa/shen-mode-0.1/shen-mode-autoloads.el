;;; shen-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (inferior-shen) "inf-shen" "inf-shen.el" (20716
;;;;;;  27697))
;;; Generated autoloads from inf-shen.el

(defvar inferior-shen-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'" "\
*What not to save on inferior Shen's input history.
Input matching this regexp is not saved on the input history in Inferior Shen
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)")

(defvar inferior-shen-program "shen" "\
*Program name for invoking an inferior Shen with for Inferior Shen mode.")

(defvar inferior-shen-load-command "(load \"%s\")\n" "\
*Format-string for building a Shen expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Shen expression that will command the inferior Shen
to load that file.  The default works acceptably on most Shens.
The string \"(progn (load \\\"%s\\\" :verbose nil :print t) (values))\\n\"
produces cosmetically superior output for this application,
but it works only in Common Shen.")

(defvar inferior-shen-prompt "^[^> \n]*>+:? *" "\
Regexp to recognise prompts in the Inferior Shen mode.
Defaults to \"^[^> \\n]*>+:? *\", which works pretty good for Lucid, kcl,
and franz.  This variable is used to initialize `comint-prompt-regexp' in the
Inferior Shen buffer.

This variable is only used if the variable
`comint-use-prompt-regexp-instead-of-fields' is non-nil.

More precise choices:
Lucid Common Shen: \"^\\\\(>\\\\|\\\\(->\\\\)+\\\\) *\"
franz: \"^\\\\(->\\\\|<[0-9]*>:\\\\) *\"
kcl: \"^>+ *\"

This is a fine thing to set in your .emacs file.")

(defvar inferior-shen-mode-hook 'nil "\
*Hook for customising Inferior Shen mode.")

(autoload 'inferior-shen "inf-shen" "\
Run an inferior Shen process, input and output via buffer `*inferior-shen*'.
If there is a process already running in `*inferior-shen*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-shen-program').  Runs the hooks from
`inferior-shen-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn CMD)" t nil)
 (add-hook 'same-window-buffer-names "*inferior-shen*")

(defalias 'run-shen 'inferior-shen)

;;;***

;;;### (autoloads nil nil ("shen-mode-pkg.el" "shen-mode.el") (20716
;;;;;;  27697 836044))

;;;***

(provide 'shen-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shen-mode-autoloads.el ends here
