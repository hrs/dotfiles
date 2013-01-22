;;; inferior-shen-mode --- an inferior-shen mode

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Authors: Michael Ilseman, Eric Schulte <schulte.eric@gmail.com>
;; Version: 0.1
;; Keywords: languages, shen, comint
;; Description: A major mode for editing shen source code

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines an inferior Shen mode.

;;; Code:
(require 'comint)
(require 'shen-mode)

;;;###autoload
(defvar inferior-shen-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "*What not to save on inferior Shen's input history.
Input matching this regexp is not saved on the input history in Inferior Shen
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)")

(defvar inferior-shen-mode-map nil)
(unless inferior-shen-mode-map
  (setq inferior-shen-mode-map (copy-keymap comint-mode-map))
;  (set-keymap-parent inferior-shen-mode-map shen-mode-shared-map)
  (define-key inferior-shen-mode-map "\C-x\C-e" 'shen-eval-last-sexp)
  (define-key inferior-shen-mode-map "\C-c\C-l" 'shen-load-file)
  (define-key inferior-shen-mode-map "\C-c\C-k" 'shen-compile-file)
  (define-key inferior-shen-mode-map "\C-c\C-a" 'shen-show-arglist)
  (define-key inferior-shen-mode-map "\C-c\C-d" 'shen-describe-sym)
  (define-key inferior-shen-mode-map "\C-c\C-f"
    'shen-show-function-documentation)
  (define-key inferior-shen-mode-map "\C-c\C-v"
    'shen-show-variable-documentation))

;;; These commands augment Shen mode, so you can process Shen code in
;;; the source files.
(define-key shen-mode-map "\M-\C-x"  'shen-eval-defun)     ; Gnu convention
(define-key shen-mode-map "\C-x\C-e" 'shen-eval-last-sexp) ; Gnu convention
(define-key shen-mode-map "\C-c\C-e" 'shen-eval-defun)
(define-key shen-mode-map "\C-c\C-r" 'shen-eval-region)
(define-key shen-mode-map "\C-c\C-c" 'shen-compile-defun)
(define-key shen-mode-map "\C-c\C-z" 'switch-to-shen)
(define-key shen-mode-map "\C-c\C-l" 'shen-load-file)
(define-key shen-mode-map "\C-c\C-k" 'shen-compile-file)  ; "kompile" file
(define-key shen-mode-map "\C-c\C-a" 'shen-show-arglist)
(define-key shen-mode-map "\C-c\C-d" 'shen-describe-sym)
(define-key shen-mode-map "\C-c\C-f" 'shen-show-function-documentation)
(define-key shen-mode-map "\C-c\C-v" 'shen-show-variable-documentation)


;;; This function exists for backwards compatibility.
;;; Previous versions of this package bound commands to C-c <letter>
;;; bindings, which is not allowed by the gnumacs standard.

;;;  "This function binds many inferior-shen commands to C-c <letter> bindings,
;;;where they are more accessible. C-c <letter> bindings are reserved for the
;;;user, so these bindings are non-standard. If you want them, you should
;;;have this function called by the inferior-shen-load-hook:
;;;    (setq inferior-shen-load-hook '(inferior-shen-install-letter-bindings))
;;;You can modify this function to install just the bindings you want."
(defun inferior-shen-install-letter-bindings ()
  (define-key shen-mode-map "\C-ce" 'shen-eval-defun-and-go)
  (define-key shen-mode-map "\C-cr" 'shen-eval-region-and-go)
  (define-key shen-mode-map "\C-cc" 'shen-compile-defun-and-go)
  (define-key shen-mode-map "\C-cz" 'switch-to-shen)
  (define-key shen-mode-map "\C-cl" 'shen-load-file)
  (define-key shen-mode-map "\C-ck" 'shen-compile-file)
  (define-key shen-mode-map "\C-ca" 'shen-show-arglist)
  (define-key shen-mode-map "\C-cd" 'shen-describe-sym)
  (define-key shen-mode-map "\C-cf" 'shen-show-function-documentation)
  (define-key shen-mode-map "\C-cv" 'shen-show-variable-documentation)

  (define-key inferior-shen-mode-map "\C-cl" 'shen-load-file)
  (define-key inferior-shen-mode-map "\C-ck" 'shen-compile-file)
  (define-key inferior-shen-mode-map "\C-ca" 'shen-show-arglist)
  (define-key inferior-shen-mode-map "\C-cd" 'shen-describe-sym)
  (define-key inferior-shen-mode-map "\C-cf" 'shen-show-function-documentation)
  (define-key inferior-shen-mode-map "\C-cv"
    'shen-show-variable-documentation))


;;;###autoload
(defvar inferior-shen-program "shen"
  "*Program name for invoking an inferior Shen with for Inferior Shen mode.")

;;;###autoload
(defvar inferior-shen-load-command "(load \"%s\")\n"
  "*Format-string for building a Shen expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Shen expression that will command the inferior Shen
to load that file.  The default works acceptably on most Shens.
The string \"(progn (load \\\"%s\\\" :verbose nil :print t) (values))\\n\"
produces cosmetically superior output for this application,
but it works only in Common Shen.")

;;;###autoload
(defvar inferior-shen-prompt "^[^> \n]*>+:? *"
  "Regexp to recognise prompts in the Inferior Shen mode.
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

(defvar inferior-shen-buffer nil "*The current inferior-shen process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Shen processes, you start the first up
with \\[inferior-shen].  It will be in a buffer named `*inferior-shen*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inferior-shen].  It will be in a new buffer,
named `*inferior-shen*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Shen processes --
like `shen-eval-defun' or `shen-show-arglist' -- have to choose a process
to send to, when you have more than one Shen process around.  This
is determined by the global variable `inferior-shen-buffer'.  Suppose you
have three inferior Shens running:
    Buffer              Process
    foo                 inferior-shen
    bar                 inferior-shen<2>
    *inferior-shen*     inferior-shen<3>
If you do a \\[shen-eval-defun] command on some Shen source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or *inferior-shen*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inferior-shen-buffer'.
This process selection is performed by function `inferior-shen-proc'.

Whenever \\[inferior-shen] fires up a new process, it resets
`inferior-shen-buffer' to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you can change `inferior-shen-buffer' to another process
buffer with \\[set-variable].")

;;;###autoload
(defvar inferior-shen-mode-hook '()
  "*Hook for customising Inferior Shen mode.")

(put 'inferior-shen-mode 'mode-class 'special)

(defun inferior-shen-mode ()
  "Major mode for interacting with an inferior Shen process.
Runs a Shen interpreter as a subprocess of Emacs, with Shen I/O through an
Emacs buffer.  Variable `inferior-shen-program' controls which Shen interpreter
is run.  Variables `inferior-shen-prompt', `inferior-shen-filter-regexp' and
`inferior-shen-load-command' can customize this mode for different Shen
interpreters.

For information on running multiple processes in multiple buffers, see
documentation for variable `inferior-shen-buffer'.

\\{inferior-shen-mode-map}

Customisation: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-shen-mode-hook' (in that order).

You can send text to the inferior Shen process from other buffers containing
Shen source.
    switch-to-shen switches the current buffer to the Shen process buffer.
    shen-eval-defun sends the current defun to the Shen process.
    shen-compile-defun compiles the current defun.
    shen-eval-region sends the current region to the Shen process.
    shen-compile-region compiles the current region.

    Prefixing the shen-eval/compile-defun/region commands with
    a \\[universal-argument] causes a switch to the Shen process buffer after sending
    the text.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Shen; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (set (make-local-variable 'font-lock-defaults) '(shen-font-lock-keywords))
  (setq comint-prompt-regexp inferior-shen-prompt)
  (setq major-mode 'inferior-shen-mode)
  (setq mode-name "Inferior Shen")
  (setq mode-line-process '(":%s"))

  (use-local-map inferior-shen-mode-map)    ;c-c c-k for "kompile" file
  (setq comint-get-old-input (function shen-get-old-input))
  (setq comint-input-filter (function shen-input-filter))
  (run-hooks 'inferior-shen-mode-hook))

(defun shen-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun shen-input-filter (str)
  "t if STR does not match `inferior-shen-filter-regexp'."
  (not (string-match inferior-shen-filter-regexp str)))

;;;###autoload
(defun inferior-shen (cmd)
  "Run an inferior Shen process, input and output via buffer `*inferior-shen*'.
If there is a process already running in `*inferior-shen*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-shen-program').  Runs the hooks from
`inferior-shen-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run shen: " inferior-shen-program)
		       inferior-shen-program)))
  (if (not (comint-check-proc "*inferior-shen*"))
      (let ((cmdlist (split-string cmd)))
	(set-buffer (apply (function make-comint)
			   "inferior-shen" (car cmdlist) nil (cdr cmdlist)))
	(inferior-shen-mode)))
  (setq inferior-shen-buffer "*inferior-shen*")
  (pop-to-buffer "*inferior-shen*"))
;;;###autoload (add-hook 'same-window-buffer-names "*inferior-shen*")

;;;###autoload
(defalias 'run-shen 'inferior-shen)

(defcustom shen-pre-eval-hook '()
  "Hook to run on code before sending it to the inferior-shen-process.
Functions on this hook will be called with an active region
containing the shen source code about to be evaluated.")

(defun shen-remember-functions (start end)
  "Add functions defined between START and END to `shen-functions'."
  (interactive "r")
  (flet ((clean (text)
                (when text
                  (set-text-properties 0 (length text) nil text) text)))
    (save-excursion
      (goto-char start)
      (let ((re (concat
                 "^(define[ \t]+\\(.+\\)[\n\r]" ; function name
                 "\\([ \t]*\\\\\\*[ \t]*\\([^\000]+?\\)[ \t]*\\*\\\\\\)?" ; doc
                 "[\n\r]?[ \t]*\\({\\(.+\\)}\\)?"))) ; type
        (while (re-search-forward re end t)
          (let ((name (intern (match-string 1)))
                (doc (clean (match-string 3)))
                (type (clean (match-string 5))))
            (add-to-list 'shen-functions (list name type doc))))))))

(add-hook 'shen-pre-eval-hook #'shen-remember-functions)

(defun check-balanced-parens (start end)
  "Check if parentheses in the region are balanced."
  (save-restriction (save-excursion
    (let ((deactivate-mark nil))
      (condition-case c
          (progn (narrow-to-region start end) (goto-char (point-min))
                 (while (/= 0 (- (point) (forward-list)))) t)
          (scan-error (signal 'scan-error '("Parentheses not balanced."))))))))

(add-hook 'shen-pre-eval-hook
          (lambda (start end)
            (condition-case err (check-balanced-parens start end)
              (error (unless (y-or-n-p (format "%s Eval anyway ?"
                                               (error-message-string err)))
                       (signal 'scan-error err))))))

(defun shen-eval-region (start end &optional and-go)
  "Send the current region to the inferior Shen process.
Prefix argument means switch to the Shen buffer afterwards."
  (interactive "r\nP")
  (let ((before-input (marker-position (process-mark (inferior-shen-proc))))
        result)
    
    (run-hook-with-args 'shen-pre-eval-hook start end)
    (comint-send-region (inferior-shen-proc) start end)
    (comint-send-string (inferior-shen-proc) "\n")
    (accept-process-output (inferior-shen-proc))
    (sit-for 0)
    (save-excursion
      (set-buffer inferior-shen-buffer)
      (goto-char before-input)
      (setq result (buffer-substring (point) (point-at-eol)))
      (message "%s" result)
      (goto-char (process-mark (inferior-shen-proc))))
    (if and-go (switch-to-shen t))
    result))

(defun shen-eval-defun (&optional and-go)
  "Send the current defun to the inferior Shen process.
Prefix argument means switch to the Shen buffer afterwards."
  (interactive "P")
  (let (result)
    (save-excursion
      (end-of-defun)
      (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
      (let ((end (point)))
        (beginning-of-defun)
        (setq result (shen-eval-region (point) end))))
    (if and-go (switch-to-shen t))
    result))

(defun shen-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Shen process.
Prefix argument means switch to the Shen buffer afterwards."
  (interactive "P")
  (shen-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

;;; Common Shen COMPILE sux.
(defun shen-compile-region (start end &optional and-go)
  "Compile the current region in the inferior Shen process.
Prefix argument means switch to the Shen buffer afterwards."
  (interactive "r\nP")
  (comint-send-string
   (inferior-shen-proc)
   (format "(funcall (compile nil `(lambda () (progn 'compile %s))))\n"
	   (buffer-substring start end)))
  (if and-go (switch-to-shen t)))

(defun shen-compile-defun (&optional and-go)
  "Compile the current defun in the inferior Shen process.
Prefix argument means switch to the Shen buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((e (point)))
      (beginning-of-defun)
      (shen-compile-region (point) e)))
  (if and-go (switch-to-shen t)))

(defun switch-to-shen (eob-p)
  "Switch to the inferior Shen process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inferior-shen-buffer)
      (let ((pop-up-frames
	     ;; Be willing to use another frame
	     ;; that already has the window in it.
	     (or pop-up-frames
		 (get-buffer-window inferior-shen-buffer t))))
	(pop-to-buffer inferior-shen-buffer))
      (run-shen inferior-shen-program))
  (when eob-p
	 (push-mark)
    (goto-char (point-max))))


;;; Now that shen-compile/eval-defun/region takes an optional prefix arg,
;;; these commands are redundant. But they are kept around for the user
;;; to bind if he wishes, for backwards functionality, and because it's
;;; easier to type C-c e than C-u C-c C-e.
(defun shen-eval-region-and-go (start end)
  "Send the current region to the inferior Shen, and switch to its buffer."
  (interactive "r")
  (shen-eval-region start end t))

(defun shen-eval-defun-and-go ()
  "Send the current defun to the inferior Shen, and switch to its buffer."
  (interactive)
  (shen-eval-defun t))

(defun shen-compile-region-and-go (start end)
  "Compile the current region in the inferior Shen, and switch to its buffer."
  (interactive "r")
  (shen-compile-region start end t))

(defun shen-compile-defun-and-go ()
  "Compile the current defun in the inferior Shen, and switch to its buffer."
  (interactive)
  (shen-compile-defun t))

;;; A version of the form in H. Shevis' soar-mode.el package. Less robust.
;;; (defun shen-compile-sexp (start end)
;;;   "Compile the s-expression bounded by START and END in the inferior shen.
;;; If the sexp isn't a DEFUN form, it is evaluated instead."
;;;   (cond ((looking-at "(defun\\s +")
;;; 	 (goto-char (match-end 0))
;;; 	 (let ((name-start (point)))
;;; 	   (forward-sexp 1)
;;; 	   (process-send-string "inferior-shen"
;;; 				(format "(compile '%s #'(lambda "
;;; 					(buffer-substring name-start
;;; 							  (point)))))
;;; 	 (let ((body-start (point)))
;;; 	   (goto-char start) (forward-sexp 1) ; Can't use end-of-defun.
;;; 	   (process-send-region "inferior-shen"
;;; 				(buffer-substring body-start (point))))
;;; 	 (process-send-string "inferior-shen" ")\n"))
;;; 	(t (shen-eval-region start end)))))
;;;
;;; (defun shen-compile-region (start end)
;;;   "Each s-expression in the current region is compiled (if a DEFUN)
;;; or evaluated (if not) in the inferior shen."
;;;   (interactive "r")
;;;   (save-excursion
;;;     (goto-char start) (end-of-defun) (beginning-of-defun) ; error check
;;;     (if (< (point) start) (error "region begins in middle of defun"))
;;;     (goto-char start)
;;;     (let ((s start))
;;;       (end-of-defun)
;;;       (while (<= (point) end) ; Zip through
;;; 	(shen-compile-sexp s (point)) ; compiling up defun-sized chunks.
;;; 	(setq s (point))
;;; 	(end-of-defun))
;;;       (if (< s end) (shen-compile-sexp s end)))))
;;;
;;; End of HS-style code


(defvar shen-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `shen-load-file' or `shen-compile-file' command.")

(defvar shen-source-modes '(shen-mode)
  "*Used to determine if a buffer contains Shen source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Shen source file by `shen-load-file' and `shen-compile-file'.
Used by these commands to determine defaults.")

(defun shen-load-file (file-name)
  "Load a Shen file into the inferior Shen process."
  (interactive (comint-get-source "Load Shen file: " shen-prev-l/c-dir/file
				  shen-source-modes nil)) ; NIL because LOAD
					; doesn't need an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq shen-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-shen-proc)
		      (format inferior-shen-load-command file-name))
  (switch-to-shen t))


(defun shen-compile-file (file-name)
  "Compile a Shen file in the inferior Shen process."
  (interactive (comint-get-source "Compile Shen file: " shen-prev-l/c-dir/file
				  shen-source-modes nil)) ; NIL = don't need
					; suffix .shen
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq shen-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-shen-proc) (concat "(compile-file \""
						   file-name
						   "\"\)\n"))
  (switch-to-shen t))



;;; Documentation functions: function doc, var doc, arglist, and
;;; describe symbol.
;;; ===========================================================================

;;; Command strings
;;; ===============

(defvar shen-function-doc-command
  "(let ((fn '%s))
     (format t \"Documentation for ~a:~&~a\"
	     fn (documentation fn 'function))
     (values))\n"
  "Command to query inferior Shen for a function's documentation.")

(defvar shen-var-doc-command
  "(let ((v '%s))
     (format t \"Documentation for ~a:~&~a\"
	     v (documentation v 'variable))
     (values))\n"
  "Command to query inferior Shen for a variable's documentation.")

(defvar shen-arglist-command
  "(let ((fn '%s))
     (format t \"Arglist for ~a: ~a\" fn (arglist fn))
     (values))\n"
  "Command to query inferior Shen for a function's arglist.")

(defvar shen-describe-sym-command
  "(describe '%s)\n"
  "Command to query inferior Shen for a variable's documentation.")


;;; Ancillary functions
;;; ===================

;;; Reads a string from the user.
(defun shen-symprompt (prompt default)
  (list (let* ((prompt (if default
			   (format "%s (default %s): " prompt default)
			 (concat prompt ": ")))
	       (ans (read-string prompt)))
	  (if (zerop (length ans)) default ans))))


;;; Adapted from function-called-at-point in help.el.
(defun shen-fn-called-at-pt ()
  "Returns the name of the function called in the current call.
The value is nil if it can't find one."
  (condition-case nil
      (save-excursion
	(save-restriction
	  (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	  (backward-up-list 1)
	  (forward-char 1)
	  (let ((obj (read (current-buffer))))
	    (and (symbolp obj) obj))))
    (error nil)))


;;; Adapted from variable-at-point in help.el.
(defun shen-var-at-pt ()
  (condition-case ()
      (save-excursion
	(forward-sexp -1)
	(skip-chars-forward "'")
	(let ((obj (read (current-buffer))))
	  (and (symbolp obj) obj)))
    (error nil)))


;;; Documentation functions: fn and var doc, arglist, and symbol describe.
;;; ======================================================================
(defun shen-show-function-documentation (fn)
  "Send a command to the inferior Shen to give documentation for function FN.
See variable `shen-function-doc-command'."
  (interactive (shen-symprompt "Function doc" (shen-fn-called-at-pt)))
  (comint-proc-query (inferior-shen-proc)
		     (format shen-function-doc-command fn)))

(defun shen-show-variable-documentation (var)
  "Send a command to the inferior Shen to give documentation for function FN.
See variable `shen-var-doc-command'."
  (interactive (shen-symprompt "Variable doc" (shen-var-at-pt)))
  (comint-proc-query (inferior-shen-proc) (format shen-var-doc-command var)))

(defun shen-show-arglist (fn)
  "Send a query to the inferior Shen for the arglist for function FN.
See variable `shen-arglist-command'."
  (interactive (shen-symprompt "Arglist" (shen-fn-called-at-pt)))
  (comint-proc-query (inferior-shen-proc) (format shen-arglist-command fn)))

(defun shen-describe-sym (sym)
  "Send a command to the inferior Shen to describe symbol SYM.
See variable `shen-describe-sym-command'."
  (interactive (shen-symprompt "Describe" (shen-var-at-pt)))
  (comint-proc-query (inferior-shen-proc)
		     (format shen-describe-sym-command sym)))


;;  "Returns the current inferior Shen process.
;; See variable `inferior-shen-buffer'."
(defun inferior-shen-proc ()
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-shen-mode)
				      (current-buffer)
				    inferior-shen-buffer))))
    (or proc
	(error "No Shen subprocess; see variable `inferior-shen-buffer'"))))


;;; Do the user's customisation...
;;;===============================
(defvar inferior-shen-load-hook nil
  "This hook is run when the library `inf-shen' is loaded.
This is a good place to put keybindings.")

(run-hooks 'inferior-shen-load-hook)

(provide 'inf-shen)
;;; inf-shen.el ends here
