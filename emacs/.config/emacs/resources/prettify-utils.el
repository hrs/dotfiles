;;; prettify-utils.el --- Helper functions for prettify-symbols-mode			-*- lexical-binding: t; -*-

;; Copyright © 2016 Ilazki

;; Author:              Ilazki
;; Created:             31 Oct 2016
;; URL:                 https://github.com/Ilazki/prettify-utils.el
;; Keywords:            lisp tools prettify utils prettify-symbols
;; Version:             1.0.2
;; Package-Version:     20161110.0430
;; Package-Requires:    ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prettify-utils provides helper functions to simplify configuration of emacs'
;; prettify-symbols-mode.  For the unfamiliar, prettify-symbols-mode detects
;; user-defined character combinations and replaces them with different ones
;; (usually ligatures) on-screen without changing the source file.  For example,
;; it can be used to display "≥" any time ">=" is found.
;;
;; Unfortunately, setup of prettify-symbols-mode is more complicated than it
;; needs to be, especially if you want the replacement string to be multiple
;; characters instead of just one.  For example, displaying "→ " for "->" in
;; order to improve appearance while still preserving the real  character length.
;;
;; To use prettify-symbols, you have to set a buffer-local variable,
;; prettify-symbols-alist, to an alist containing string/replacement pairs.  This
;; package provides a helper macro, prettify-utils-generate, to create the alist
;; in a simple, uniform manner, like so:
;;
;; 	(setq prettify-symbols-alist
;; 		  (prettify-utils-generate
;; 		   ("lambda" "λ")
;; 		   ("<="     "≤")
;; 		   (">="     "≥")
;; 		   ("->"     "→ ")))
;;
;;
;; Since prettify-symbols uses buffer-local settings, you have to add the alist
;; for each buffer.  The easiest way to do this is to add it to a mode's hooks.
;; Example:
;;
;; 	(add-hook 'prog-mode-hook (lambda ()
;; 								(setq prettify-symbols-alist
;; 								  (prettify-utils-generate
;; 								   ("lambda" "λ")))
;;                              (prettify-symbols-mode 1)))
;;
;; For convenience, a macro named prettify-utils-add-hook is also available
;; that generates the above automatically.
;; Example:
;;
;; (prettify-utils-add-hook prog-mode-hook
;;                          ("lambda" "λ")))
;;
;; Or, if you're using the same alist for multiple modes, you can create a named
;; function and pass that to the mode hooks:
;;
;;	(defun my-symbols-alist ()
;;		 (setq prettify-symbols-alist
;;			(prettify-utils-generate
;;			 ("lambda" "λ")
;;			 ("->" "→")
;;			 ("=>" "⇒"))))
;;	(add-hook 'prog-mode-hook 'my-symbols-alist)
;;
;; Generally, prettify-utils-generate and prettify-utils-add-hook should be the
;; only things needed, but some additional helpers are provided in case a need
;; arises:
;;
;; * prettify-utils-generate-f is the function equivalent of using
;;   prettify-utils-generate.  Should only be needed for creating higher-order
;;   functions.
;; * prettify-utils-create-pair is the function used by other functions to create
;;   alist pairs.
;; * prettify-utils-string converts a string into the list format required by
;;   prettify-symbols-alist.
;;
;; For more information about any functions or macros provided, as well as
;; additional example use, refer to any function's docstring with
;; `M-x describe-function`.

;;; Code:

(provide 'prettify-utils)

;;;###autoload
(defun prettify-utils--list (l &optional glue)
  "Takes two lists and interleaves the (optional) second between each element of 
the first.  Used to create multi-character sequences for use with the minor mode
`prettify-symbols-mode'.  If not supplied, GLUE defaults to '(Br . Bl).  For more 
information about GLUE, refer to the documentation for the `compose-region'
function and the `reference-point-alist' variable.

This function is used by `prettify-utils-string' to create the lists given to 
`prettify-symbols-alist'.  Calling prettify-utils--list directly is probably not 
what you want, check the documentation for `prettify-utils-string' and 
`prettify-utils-generate' instead.

Example use:
(prettify-utils--list (string-to-list \"hello\") '(Br . Bl))
"

  (let ((glue (or glue '(Br . Bl)))
		(head (car l))
		(tail (cdr l)))
	(cond
	 ((not (consp l))    '())
	 ((not (consp tail))  (list head))
	 (t (cons head
			  (cons glue
					(prettify-utils--list tail glue)))))))

;;;###autoload
(defun prettify-utils-string (s &optional glue)
  "Takes a string and an optional list, and returns a list of the string's 
characters with GLUE interleaved between each character, for use with 
`prettify-symbols-mode'.  If no GLUE is supplied, uses the 
`prettify-utils--list' default.  For more information about GLUE, refer to the 
documentation for the `compose-region' function and the `reference-point-alist' 
variable.

This function can be used to simplify multiple-character replacements when 
manually constructing a `prettify-symbols-alist'.  For something more high-level, 
consider using `prettify-utils-generate' to create the entire alist instead.

Example:

(prettify-utils-string \"example\" '(Br . Bl))
"
  (prettify-utils--list (append s nil) glue))

;; Was used during macro creation then removed
(defun prettify-utils-create-pair (old new &optional glue)
  "Takes two strings, OLD and NEW, and an optional GLUE list, and creates an 
alist pair for use when creating a `prettify-symbols-alist'.  For more information 
about GLUE, refer to the documentation for the `compose-region' function and the 
`reference-point-alist' variable.

This function is primarily for use by the user-friendly `prettify-utils-generate'
macro, but may be useful if manual alist creation is desired for some reason.

Example:

(setq prettify-symbols-alist `((\">=\" ?≥)
	  ,(prettify-utils-create-pair \"foo\" \"bar\" '(Br . Bl))))
"
  (cons old (prettify-utils-string new glue)))

;;;###autoload
(defmacro prettify-utils-generate (&rest lists)
  "Generates an alist for use when setting `prettify-symbols-alist'.  Takes one or 
more LISTS, each consisting of two strings and an optional GLUE list to be 
interleaved between characters in the replacement list.  If the optional GLUE
list is not supplied, uses the `prettify-utils--list' default of '(Br . Bl).  For more 
information about GLUE, refer to the documentation for the `compose-region'
function and the `reference-point-alist' variable.

Example #1:

(setq prettify-symbols-alist 
	  (prettify-utils-generate (\"foo\" \"bar\")
							   (\">=\" \"≥\" (Br . Bl))
							   (\"->\"     \"→ \")))

Example #2:

(setq prettify-symbols-alist
	  (prettify-utils-generate 
	   (\"lambda\"  \"λ\")
	   (\"|>\"      \"▷\")
	   (\"<|\"      \"◁\")
	   (\"->>\"     \"↠  \")
	   (\"->\"      \"→ \")
	   (\"<-\"      \"← \")
	   (\"=>\"      \"⇒\")
	   (\"<=\"      \"≤\")
	   (\">=\"      \"≥\")))
"
  (let* ((head       (car   lists))
         (tail       (cdr   lists))
         (old-string (car   head))
		 (new-string (cadr  head))
		 (glue-list  (caddr head)))
	(if (not (consp head))
		'()
       `(cons (quote ,(prettify-utils-create-pair old-string new-string glue-list))
			 (prettify-utils-generate ,@tail)))))


;;;###autoload
(defun prettify-utils-generate-f (&rest lists)
  "Generates an alist for use when setting `prettify-symbols-alist'.  Takes one or 
more LISTS, each consisting of two strings and an optional GLUE list to be 
interleaved between characters in the replacement list.  If the optional GLUE
list is not supplied, uses the `prettify-utils--list' default of '(Br . Bl).  For more 
information about GLUE, refer to the documentation for the `compose-region'
function and the `reference-point-alist' variable.

This is a function equivalent of the `prettify-utils-generate' macro.  Unless
you specifically need a function, such as for use with a higher-order function,
you should use the `prettify-utils-generate' macro instead.

Example:

(prettify-utils-generate-f '(\"foo\" \"bar\")
				           '(\">=\" \"≥\" (Br . Bl))
						   '(\"->\"     \"→ \"))
"
  (let* ((head       (car   lists))
         (tail       (cdr   lists))
         (old-string (car   head))
		 (new-string (cadr  head))
		 (glue-list  (caddr head)))
  (if (not (consp head))
	  '()
      (cons (prettify-utils-create-pair old-string new-string glue-list)
                   (apply 'prettify-utils-generate-f tail)))))

;; Macro based on a suggestion and example code by reddit user /u/Kungsgeten
;; Github @ https://github.com/Kungsgeten/
;;;###autoload
(defmacro prettify-utils-add-hook (mode &rest lists)
  "Convenience macro for the most likely use case of prettify-utils: using
`add-hook' to add LISTS to MODE. LISTS consists of one or more lists of
replacements, defined as expected by `prettify-utils-generate'.

Example:

;; Replace org-mode checkboxes with appropriate unicode boxes
(prettify-utils-add-hook org-mode
                         (\"[ ]\" \"☐\")
                         (\"[X]\" \"☑\")
                         (\"[-]\" \"❍\"))

"
  `(add-hook ',(intern (concat (symbol-name mode) "-hook"))
             (lambda ()
               (setq prettify-symbols-alist
                     (prettify-utils-generate ,@lists))
               (prettify-symbols-mode 1))))

;;; prettify-utils.el ends here
