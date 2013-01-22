;;; tex-info.el --- Support for editing Texinfo source.

;; Copyright (C) 1993, 1994, 1997, 2000, 2001,
;;               2004, 2005, 2006 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

(require 'tex)

(require 'texinfo)
;; Make sure the Texinfo mode of AUCTeX is still used after loading
;; texinfo.el.  (This is only an issue on Emacs 21.)
(when (and (boundp 'TeX-modes)
	   (memq 'texinfo-mode TeX-modes))
  (defalias 'texinfo-mode 'TeX-texinfo-mode))

;;; Environments:

(defvar Texinfo-environment-list
  '(("cartouche") ("command") ("copying") ("defcv") ("deffn") ("defivar")
    ("defmac") ("defmethod") ("defop") ("defopt") ("defspec")
    ("deftp") ("deftypefn") ("deftypefun") ("deftypevar") ("deftypevr")
    ("defun") ("defvar") ("defvr") ("description") ("detailmenu")
    ("direntry") ("display") ("documentdescription") ("enumerate")
    ("example") ("flushleft") ("flushright") ("format") ("ftable")
    ("group") ("ifclear") ("ifdocbook") ("ifhtml") ("ifinfo")
    ("ifnotdocbook") ("ifnothtml") ("ifnotinfo") ("ifnotplaintext")
    ("ifnottex") ("ifnotxml") ("ifplaintext") ("ifset") ("iftex")
    ("ifxml") ("ignore") ("itemize") ("lisp") ("macro") ("menu")
    ("multitable") ("quotation") ("smalldisplay") ("smallexample")
    ("smallformat") ("smalllisp") ("table") ("tex") ("titlepage")
    ("verbatim") ("vtable")) 
  "Alist of Texinfo environments.")

(defconst texinfo-environment-regexp
  ;; Overwrite version from `texinfo.el'.
  (concat "^@\\("
	  (mapconcat 'car Texinfo-environment-list "\\|")
	  "\\|end\\)\\>")
  "Regexp for environment-like Texinfo list commands.
Subexpression 1 is what goes into the corresponding `@end' statement.")

(defun Texinfo-environment (env &optional arg)
  "Make Texinfo environment ENV.
With optional ARG, modify current environment."
  ;; XXX: This could be enhanced to act like `LaTeX-environment',
  ;; i.e. suggest a default environment and have its own history.
  (interactive (list (completing-read "Environment: "
				      Texinfo-environment-list)
		     current-prefix-arg))
  (if arg
      (Texinfo-modify-environment env)
    (Texinfo-insert-environment env)))

(defun Texinfo-insert-environment (env)
  "Insert Texinfo environment ENV."
  (if (and (TeX-active-mark)
	   (not (eq (mark) (point))))
      (progn
	(when (< (mark) (point))
	  (exchange-point-and-mark))
	(unless (TeX-looking-at-backward "^[ \t]*")
	  (newline))
	(insert "@" env)
	(newline)
	(goto-char (mark))
	(unless (TeX-looking-at-backward "^[ \t]*")
	  (newline))
	(insert "@end " env)
	(save-excursion (newline))
	(end-of-line 0))
    (insert "@" env "\n\n@end " env "\n")
    (if (null (cdr-safe (assoc "defcv" Texinfo-environment-list)))
	(forward-line -2))))

(defun Texinfo-modify-environment (env)
  "Change current environment to environment ENV."
  (save-excursion
    (Texinfo-find-env-end)
    (re-search-backward (concat (regexp-quote TeX-esc) "end \\([a-zA-Z]*\\)")
			(line-beginning-position))
    (replace-match env t t nil 1)
    (beginning-of-line)
    (Texinfo-find-env-start)
    (re-search-forward (concat (regexp-quote TeX-esc) "\\([a-zA-Z]*\\)")
		       (line-end-position))
    (replace-match env t t nil 1)))

(defun Texinfo-find-env-end ()
  "Move point to the end of the current environment."
  (interactive)
  (let* ((envs (mapcar 'car Texinfo-environment-list))
	 (regexp (concat "^[ \t]*" (regexp-quote TeX-esc) "\\(end \\)*"
			 (regexp-opt envs t) "\\b"))
	 (level 1)
	 case-fold-search)
    (save-restriction
      (save-excursion
	(save-excursion
	  (beginning-of-line)
	  (when (and (looking-at regexp)
		     (match-string 1))
	    (setq level 0)))
	(while (and (> level 0) (re-search-forward regexp nil t))
	  (if (match-string 1)
	      (setq level (1- level))
	    (setq level (1+ level)))))
      (if (= level 0)
	  (goto-char (match-end 0))
	(error "Can't locate end of current environment")))))
      
(defun Texinfo-find-env-start ()
  "Move point to the start of the current environment."
  (interactive)
  (let* ((envs (mapcar 'car Texinfo-environment-list))
	 (regexp (concat "^[ \t]*" (regexp-quote TeX-esc) "\\(end \\)*"
			 (regexp-opt envs t) "\\b"))
	 (level 1)
	 case-fold-search)
    (save-restriction
      (save-excursion
	(save-excursion
	  (beginning-of-line)
	  (when (and (looking-at regexp)
		     (not (match-string 1)))
	    (setq level 0)))
	(while (and (> level 0) (re-search-backward regexp nil t))
	  (if (match-string 1)
	      (setq level (1+ level))
	    (setq level (1- level)))))
      (if (= level 0)
	  (goto-char (match-beginning 0))
	(error "Can't locate start of current environment")))))

(defun Texinfo-insert-node ()
  "Insert a Texinfo node in the current buffer.
That means, insert the string `@node' and prompt for current,
next, previous and upper node.  If there is an active region, use
this for the current node and inhibit the prompt for it.  Insert
a comment on the following line indicating the order of arguments
for @node."
  (interactive)
  (let ((active-mark (and (TeX-active-mark) (not (eq (mark) (point)))))
	nodes node-name next-node previous-node up-node)
    ;; Build list of nodes in current buffer.
    ;; (What about using `imenu--index-alist'?)
    ;; FIXME: Support multi-file documents.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^@node\\b" nil t)
	(skip-chars-forward " \t")
	(add-to-list 'nodes
		     (list (buffer-substring-no-properties
			    (point) (progn (skip-chars-forward "^,")
					   (point)))))))
    (unless active-mark
      (setq node-name (read-string "Node name: ")))
    ;; FIXME: What if key binding for `minibuffer-complete' was changed?
    ;; `substitute-command-keys' doesn't return the correct value.
    (setq next-node (completing-read "Next node (TAB completes): " nodes))
    (setq previous-node
	  (completing-read "Previous node (TAB completes): " nodes))
    (setq up-node (completing-read "Upper node (TAB completes): " nodes))
    (when (and active-mark
	       (< (mark) (point)))
      (exchange-point-and-mark))
    (insert "@node ")
    (if active-mark
	(goto-char (mark))
      (insert node-name))
    (insert ", " next-node ", " previous-node ", " up-node
	    "\n@comment  node-name,  next,  previous,  up\n")
    ;; Position point at first empty field.
    (unless (and (or (> (length node-name) 0) active-mark)
		 (> (length next-node) 0)
		 (> (length previous-node) 0)
		 (> (length  up-node) 0))
      (forward-line -2)
      (forward-char 6)
      (catch 'break
	(if (or (> (length node-name) 0) active-mark)
	    (progn (skip-chars-forward "^,") (forward-char 2))
	  (throw 'break nil))
	(dolist (node (list next-node previous-node up-node))
	  (if (> (length node) 0)
	      (progn (skip-chars-forward "^,") (forward-char 2))
	    (throw 'break nil)))))))


;;; Keymap:

(defvar Texinfo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map TeX-mode-map)

    ;; From texinfo.el
    ;; bindings for updating nodes and menus
    (define-key map "\C-c\C-um"      'texinfo-master-menu)
    (define-key map "\C-c\C-u\C-m"   'texinfo-make-menu)
    (define-key map "\C-c\C-u\C-n"   'texinfo-update-node)
    (define-key map "\C-c\C-u\C-e"   'texinfo-every-node-update)
    (define-key map "\C-c\C-u\C-a"   'texinfo-all-menus-update)

    ;; Simulating LaTeX-mode
    (define-key map "\C-c\C-e" 'Texinfo-environment)
    (define-key map "\C-c\n"   'texinfo-insert-@item)
    (or (key-binding "\e\r")
	(define-key map "\e\r" 'texinfo-insert-@item)) ;*** Alias
    (define-key map "\C-c\C-s" 'Texinfo-insert-node)
    (define-key map "\C-c]" 'texinfo-insert-@end)
    map)
  "Keymap for Texinfo mode.")

(easy-menu-define Texinfo-command-menu
  Texinfo-mode-map
  "Menu used in Texinfo mode for external commands."
  (TeX-mode-specific-command-menu 'texinfo-mode))

(easy-menu-define Texinfo-mode-menu
  Texinfo-mode-map
  "Menu used in Texinfo mode."
  (TeX-menu-with-help
   `("Texinfo"
     ["Node ..." texinfo-insert-@node
      :help "Insert a node"]
     ["Macro ..." TeX-insert-macro
      :help "Insert a macro and possibly arguments"]
     ["Complete Macro" TeX-complete-symbol
      :help "Complete the current macro"]
     ["Environment ..." Texinfo-insert-environment
      :help "Insert an environment"]
     ["Item" texinfo-insert-@item
      :help "Insert an @item"]
     "-"
     ("Insert Font"
      ["Emphasize"  (TeX-font nil ?\C-e) :keys "C-c C-f C-e"]
      ["Bold"       (TeX-font nil ?\C-b) :keys "C-c C-f C-b"]
      ["Typewriter" (TeX-font nil ?\C-t) :keys "C-c C-f C-t"]
      ["Small Caps" (TeX-font nil ?\C-c) :keys "C-c C-f C-c"]
      ["Italic"     (TeX-font nil ?\C-i) :keys "C-c C-f C-i"]
      ["Sample"    (TeX-font nil ?\C-s) :keys "C-c C-f C-s"]
      ["Roman"      (TeX-font nil ?\C-r) :keys "C-c C-f C-r"])
     ("Replace Font"
      ["Emphasize"  (TeX-font t ?\C-e) :keys "C-u C-c C-f C-e"]
      ["Bold"       (TeX-font t ?\C-b) :keys "C-u C-c C-f C-b"]
      ["Typewriter" (TeX-font t ?\C-t) :keys "C-u C-c C-f C-t"]
      ["Small Caps" (TeX-font t ?\C-c) :keys "C-u C-c C-f C-c"]
      ["Italic"     (TeX-font t ?\C-i) :keys "C-u C-c C-f C-i"]
      ["Sample"    (TeX-font t ?\C-s) :keys "C-u C-c C-f C-s"]
      ["Roman"      (TeX-font t ?\C-r) :keys "C-u C-c C-f C-r"])
     ["Delete Font" (TeX-font t ?\C-d) :keys "C-c C-f C-d"]
     "-"
     ["Create Master Menu" texinfo-master-menu
      :help "Make a master menu for the whole Texinfo file"]
     ["Create Menu" texinfo-make-menu
      :help "Make or update the menu for the current section"]
     ["Update Node" texinfo-update-node
      :help "Update the current node"]
     ["Update Every Node" texinfo-every-node-update
      :help "Update every node in the current file"]
     ["Update All Menus" texinfo-all-menus-update
      :help "Update every menu in the current file"]
     "-"
     ("Commenting"
      ["Comment or Uncomment Region"
       TeX-comment-or-uncomment-region
       :help "Comment or uncomment the currently selected region"]
      ["Comment or Uncomment Paragraph"
       TeX-comment-or-uncomment-paragraph
       :help "Comment or uncomment the current paragraph"])
     ,TeX-fold-menu
     "-"
     . ,TeX-common-menu-entries)))

(defvar Texinfo-font-list
  '((?\C-b "@b{" "}")
    (?\C-c "@sc{" "}")
    (?\C-e "@emph{" "}")
    (?\C-i "@i{" "}")
    (?\C-r "@r{" "}")
    (?\C-s "@samp{" "}")
    (?\C-t "@t{" "}")
    (?s    "@strong{" "}")
    (?\C-f "@file{" "}")
    (?d "@dfn{" "}")
    (?\C-v "@var{" "}")
    (?k    "@key{" "}")
    (?\C-k "@kbd{" "}")
    (?c    "@code{" "}")
    (?C    "@cite{" "}")
    (?\C-d "" "" t))
  "Font commands used in Texinfo mode.  See `TeX-font-list'.")
  
;;; Mode:

;;;###autoload
(defalias 'Texinfo-mode 'texinfo-mode)

;;;###autoload
(defun TeX-texinfo-mode ()
  "Major mode in AUCTeX for editing Texinfo files.

Special commands:
\\{Texinfo-mode-map}

Entering Texinfo mode calls the value of `text-mode-hook'  and then the
value of `Texinfo-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (setq TeX-mode-p t)
  ;; Mostly stolen from texinfo.el
  (setq TeX-base-mode-name "Texinfo")
  (setq major-mode 'texinfo-mode)
  (use-local-map Texinfo-mode-map)
  (set-syntax-table texinfo-mode-syntax-table)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter 
	(concat 
	 "^@node [ \t]*[Tt]op\\|^@\\(" 
	 texinfo-chapter-level-regexp 
	 "\\)"))
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	(concat "\b\\|^@[a-zA-Z]*[ \n]\\|" paragraph-separate))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start
	(concat "\b\\|^@[a-zA-Z]*[ \n]\\|" paragraph-start))
  (make-local-variable 'fill-column)
  (setq fill-column 72)
  (make-local-variable 'comment-start)
  (setq comment-start "@c ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "@c +\\|@comment +")
  (set (make-local-variable 'comment-use-syntax) nil)
  (make-local-variable 'words-include-escapes)
  (setq words-include-escapes t)
  (if (not (boundp 'texinfo-imenu-generic-expression))
      ;; This was introduced in 19.30.
      ()
    (make-local-variable 'imenu-generic-expression)
    (setq imenu-generic-expression texinfo-imenu-generic-expression))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	;; COMPATIBILITY for Emacs 20
	(if (boundp 'texinfo-font-lock-syntactic-keywords)
	    '(texinfo-font-lock-keywords
	      nil nil nil backward-paragraph
	      (font-lock-syntactic-keywords
	       . texinfo-font-lock-syntactic-keywords))
	  '(texinfo-font-lock-keywords t)))
  (if (not (boundp 'texinfo-section-list))
      ;; This was included in 19.31.
      ()
    (make-local-variable 'outline-regexp)
    (setq outline-regexp 
	  (concat "@\\("
		  (mapconcat 'car texinfo-section-list "\\>\\|")
		  "\\>\\)"))
    (make-local-variable 'outline-level)
    (setq outline-level 'texinfo-outline-level))
  
  ;; Mostly AUCTeX stuff
  (easy-menu-add Texinfo-mode-menu Texinfo-mode-map)
  (easy-menu-add Texinfo-command-menu Texinfo-mode-map)
  (make-local-variable 'TeX-command-current)
  (setq TeX-command-current 'TeX-command-master)

  (setq TeX-default-extension "texi")
  (make-local-variable 'TeX-esc)
  (setq TeX-esc "@")

  (make-local-variable 'TeX-auto-regexp-list)
  (setq TeX-auto-regexp-list 'TeX-auto-empty-regexp-list)
  (make-local-variable 'TeX-auto-update)
  (setq TeX-auto-update t)

  (setq TeX-command-default "TeX")
  (setq TeX-header-end "%*end")
  (setq TeX-trailer-start (regexp-quote (concat TeX-esc "bye")))
  
  (make-local-variable 'TeX-complete-list)
  (setq TeX-complete-list
	(list (list "@\\([a-zA-Z]*\\)" 1 'TeX-symbol-list nil)
	      (list "" TeX-complete-word)))

  (make-local-variable 'TeX-font-list)
  (setq TeX-font-list Texinfo-font-list)
  (make-local-variable 'TeX-font-replace-function)
  (setq TeX-font-replace-function 'TeX-font-replace-macro)
  
  (add-hook 'find-file-hooks (lambda ()
			       (unless (file-exists-p (buffer-file-name))
				 (TeX-master-file nil nil t))) nil t)

  (TeX-add-symbols
   '("appendix" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("appendixsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("appendixsection" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("appendixsubsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("appendixsubsubsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("asis")
   '("author" (TeX-arg-literal " ") (TeX-arg-free "Author"))
   '("b" "Text")
   '("bullet")
   '("bye")
   '("c" (TeX-arg-literal " ") (TeX-arg-free "Comment"))
   '("center" (TeX-arg-literal " ") (TeX-arg-free "Line of text"))
   '("chapheading" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("chapter" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("cindex" (TeX-arg-literal " ") (TeX-arg-free "Entry"))
   '("cite" "Reference")
   '("clear" (TeX-arg-literal " ") (TeX-arg-free "Flag"))
   '("code" "Sample code")
   '("command" "Command")
   '("comment" (TeX-arg-literal " ") (TeX-arg-free "Comment"))
   '("contents")
   '("copyright" nil)
   '("defcodeindex" (TeX-arg-literal " ") (TeX-arg-free "Index name"))
   '("defindex" (TeX-arg-literal " ") (TeX-arg-free "Index name"))
   '("dfn" "Term")
   '("dmn" "Dimension")
   '("dots" nil)
   '("emph" "Text")
   '("email" "Email address")
   '("equiv" nil)
   '("error")
   '("evenfooting" Texinfo-lrc-argument-hook)
   '("evenheading" Texinfo-lrc-argument-hook)
   '("everyfooting" Texinfo-lrc-argument-hook)
   '("everyheading" Texinfo-lrc-argument-hook)
   '("exdent" (TeX-arg-literal " ") (TeX-arg-free "Line of text"))
   '("expansion" nil)
   '("file" "Filename")
   '("finalout")
   '("findex" (TeX-arg-literal " ") (TeX-arg-free "Entry"))
   '("footnote" "Text of footnote")
   '("footnotestyle" (TeX-arg-literal " ") (TeX-arg-free "Style"))
   '("group")
   '("heading" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   ;; XXX: Would be nice with completion.
   '("headings" (TeX-arg-literal " ") (TeX-arg-free "On off single double"))
   '("i" "Text")
   '("ignore")
   '("include" (TeX-arg-literal " ") (TeX-arg-free "Filename"))
   '("inforef" "Node name" "Info file name")
   '("item")
   '("itemx")
   '("kbd" "Keyboard characters")
   '("key" "Key name")
   '("kindex" (TeX-arg-literal " ") (TeX-arg-free "Entry"))
   '("lowersections" 0)
   '("majorheading" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("menu")
   '("minus")
   '("need" "N")
   '("node" (TeX-arg-literal " ") (TeX-arg-free "Name")
     (TeX-arg-literal ", ") (TeX-arg-free "Next")
     (TeX-arg-literal ", ") (TeX-arg-free "Previous")
     (TeX-arg-literal ", ") (TeX-arg-free "Up"))
   '("noindent")
   '("oddfooting" Texinfo-lrc-argument-hook)
   '("oddheading" Texinfo-lrc-argument-hook)
   '("page")
   '("paragraphindent" (TeX-arg-literal " ") (TeX-arg-free "Indent"))
   '("pindex" "Entry")
   '("point" nil)
   '("print")
   '("printindex" (TeX-arg-literal " ") (TeX-arg-free "Index name"))
   '("pxref" "Node name")
   '("r" "Text")
   '("raisesections" 0)
   '("ref" "Node name")
   '("refill")
   '("result")
   '("samp" "Text")
   '("sc" "Text")
   '("section" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("set" (TeX-arg-literal " ") (TeX-arg-free "Flag"))
   ;; XXX: Would be nice with completion.
   '("setchapternewpage" (TeX-arg-literal " ") (TeX-arg-free "On off odd"))
   '("setfilename" (TeX-arg-literal " ") (TeX-arg-free "Info file name"))
   '("settitle" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("shortcontents")
   '("smallbook")
   '("sp" "N")
   '("strong" "Text")
   '("subheading" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("subsection" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("subsubheading" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("subsubsection" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("subtitle" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("summarycontents")
   '("syncodeindex" (TeX-arg-literal " ") (TeX-arg-free "From index")
     (TeX-arg-literal " ") (TeX-arg-free "Into index"))
   '("synindex" (TeX-arg-literal " ") (TeX-arg-free "From index")
     (TeX-arg-literal " ") (TeX-arg-free "Into index"))
   '("t" "Text")
   '("TeX" nil)
   '("thischapter")
   '("thischaptername")
   '("thisfile")
   '("thispage")
   '("tindex" (TeX-arg-literal " ") (TeX-arg-free "Entry"))
   '("title" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("titlefont" "Text")
   '("titlepage")
   '("today" nil)
   '("top" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("unnumbered" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("unnumberedsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("unnumberedsubsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("unnumberedsubsubsec" (TeX-arg-literal " ") (TeX-arg-free "Title"))
   '("value" "Flag")
   '("var" "Metasyntactic variable")
   '("vindex" (TeX-arg-literal " ") (TeX-arg-free "Entry"))
   '("vskip" (TeX-arg-literal " ") (TeX-arg-free "Amount"))
   '("w" "Text")
   '("xref" "Node name"))
  
  (TeX-run-mode-hooks 'text-mode-hook 'Texinfo-mode-hook)
  (TeX-set-mode-name))

(defcustom Texinfo-clean-intermediate-suffixes nil
  "List of regexps matching suffixes of files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)

(defcustom Texinfo-clean-output-suffixes
  ;; See `man texi2html' for the HTML stuff.
  '("\\.info\\(-[0-9]+\\)?" "\\.dvi" "\\.pdf" "\\.ps" "\\.html"
    "_toc\\.html" "_fot\\.html" "_abt\\.html" "_[0-9]+\\.html" "_l2h_img.+")
  "List of regexps matching suffixes of files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)
  
(provide 'tex-info)
  
;;; tex-info.el ends here
