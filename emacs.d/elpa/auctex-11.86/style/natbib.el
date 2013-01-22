;;; natbib.el --- Style hook for the NatBib package
;;;
;;; AUTHOR: Carsten Dominik <dominik@strw.leidenuniv.nl>
;;;         building on older code from Berwin Turlach

;;; Code:

(TeX-add-style-hook "natbib"
 (function
  (lambda ()
    ;; The number in the cdr of the following list indicates how many
    ;; optional note arguments we consider useful.  Prompting for those
    ;; arguments will still depend upon `TeX-arg-cite-note-p'.
    (let  ((citecmds 
	    '(("cite" . 0)
	      ("citet" . 1) ("citet*" . 1) ("citealt" . 1) ("citealt*" . 1)
	      ("citep" . 2) ("citep*" . 2) ("citealp" . 2) ("citealp*" . 2)
	      ("citeauthor" . 0) ("citeauthor*" . 0) ("citefullauthor" . 0)
	      ("citeyear" . 0) ("citeyearpar" . 0)
	      ("shortcites" . 0))))

      ;; Add these symbols
      (apply 
       'TeX-add-symbols
       (mapcar
	(lambda (cmd)
	  (cond 
	   ((= (cdr cmd) 0)
	    ;; No optional arguments
	    (list (car cmd) 'TeX-arg-cite))
	   ((= (cdr cmd) 1)
	    ;; Just one optional argument, the post note
	    (list
	     (car cmd)
	     '(TeX-arg-conditional TeX-arg-cite-note-p (["Post-note"]) nil)
	     'TeX-arg-cite))
	   ((= (cdr cmd) 2)
	    ;; Pre and post notes
	    (list
	     (car cmd)
	     '(TeX-arg-conditional TeX-arg-cite-note-p (natbib-note-args) nil)
	     'TeX-arg-cite))))
      citecmds))

      ;; Add the other symbols
      (TeX-add-symbols
       '("citetext" "Text")
       '("bibpunct" ["Post note separator"] 
		 "Opening bracket"
		 "Closing bracket"
		 "Punctuation between multiple citations"
		 "style [n]umeric [s]uperscript [a]uthor-year"
		 "Punctuation between author and year"
		 "Punctuation between years for common authors")
       '("citestyle" "Style")
       '("citeindextrue")
       '("citeindexfalse")
       '("citeindextype"))

      ;; Make an entry in TeX-complete-list
      (add-to-list
       'TeX-complete-list
       (list
	(concat "\\\\\\(" 
		(mapconcat (lambda (x) (regexp-quote (car x)))
			   citecmds "\\|")
		"\\)\\(\\[[^]\n\r\\%]*\\]\\)*{\\([^{}\n\r\\%,]*,\\)*\\([^{}\n\r\\%,]*\\)")
	4 'LaTeX-bibitem-list "}")))

    ;; Fontification
    (when (and (fboundp 'font-latex-add-keywords)
	       (eq TeX-install-font-lock 'font-latex-setup))
      (font-latex-add-keywords '(("cite" "*[[{")
				 ("citet" "*[[{")
				 ("citealt" "*[[{")
				 ("citep" "*[[{")
				 ("citealp" "*[[{")
				 ("citeauthor" "*[[{")
				 ("citefullauthor" "[[{")
				 ("citeyear" "[[{")
				 ("citeyearpar" "[[{")
				 ("shortcites" "{"))
			       'reference))

    ;; Tell RefTeX
    (if (fboundp 'reftex-set-cite-format)
	(reftex-set-cite-format 'natbib)))))

(defun natbib-note-args (optional &optional prompt definition)
  "Prompt for two note arguments a natbib citation command."
  (if TeX-arg-cite-note-p
      (let* ((pre (read-string 
		   (TeX-argument-prompt optional optional "Pre-note")))
	     (post (read-string
		    (TeX-argument-prompt optional optional "Post-note"))))
	(if (not (string= pre "")) (insert "[" pre "]"))
	(if (not (string= post ""))
	    (insert "[" post "]")
	  ;; Make sure that we have an empty post note if pre is not empty
	  (if (string= pre "") (insert "[]"))))))

(defvar LaTeX-natbib-package-options '("numbers" "super" "authoryear"
				       "round" "square" "angle" "curly"
				       "comma" "colon" "nobibstyle" 
				       "bibstyle" "openbib" "sectionbib"
				       "sort" "sort&compress"
				       "longnamesfirst" "nonamebreak")
  "Package options for the natbib package.")

;; natbib.el ends here
