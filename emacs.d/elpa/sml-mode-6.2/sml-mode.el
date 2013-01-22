;;; sml-mode.el --- Major mode for editing (Standard) ML  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 1989,1999,2000,2004,2007,2010-2012  Free Software Foundation, Inc.

;; Maintainer: (Stefan Monnier) <monnier@iro.umontreal.ca>
;; Version: 6.2
;; Keywords: SML
;; Author:	Lars Bo Nielsen
;;		Olin Shivers
;;		Fritz Knabe (?)
;;		Steven Gilmore (?)
;;		Matthew Morley <mjm@scs.leeds.ac.uk>
;;		Matthias Blume <blume@cs.princeton.edu>
;;		(Stefan Monnier) <monnier@iro.umontreal.ca>

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

;; A major mode to edit Standard ML (SML) code.
;; Provides the following features, among others:
;; - Indentation.
;; - Syntax highlighting.
;; - Prettified display of ->, =>, fn, ...
;; - Imenu.
;; - which-function-mode.
;; - Skeletons/templates.
;; - Electric pipe key.
;; - outline-minor-mode (with some known problems).
;; - Interaction with a read-eval-print loop.

;;;; Known bugs:

;; - Indentation after "functor toto() where type foo = bar ="
;;   Because the last is treated as an equality comparison.
;; - indentation of a declaration after a long `datatype' can be slow.

;;;; News:

;;;;; Changes since 5.0:

;; - sml-electric-pipe-mode to make the | key electric.
;; - Removal of a lot of compatibility code.  Requires Emacs-24.
;; - Integrate in GNU ELPA.

;;;;; Changes since 4.1:

;; - New indentation code using SMIE when available.
;; - `sml-back-to-outer-indent' is now on S-tab (aka `backtab') rather
;;   than M-tab.
;; - Support for electric-layout-mode and electric-indent-mode.
;; - `sml-mark-defun' tries to be more clever.
;; - A single file (sml-mode.el) is needed unless you want to use an
;;   interactive process like SML/NJ, or if your Emacs does not provide SMIE.

;;;;; Changes since 4.0:

;; - Switch to GPLv3+.
;; - When possible (i.e. running under Emacs>=23), be case-sensitive when
;;   expanding abbreviations, and don't expand them in comments and strings.
;; - When you `next-error' to a type error, highlight the actual parts of the
;;   types that differ.
;; - Flush the recorded errors not only upon sml-compile and friends, but also
;;   when typing commands directly at the prompt.
;; - New command sml-mlton-typecheck.
;; - Simple support to parse errors and warnings in MLton's output.
;; - Simple support for MLton's def-use files.

;;;;; Changes since 3.9.5:

;; - No need to add the dir to your load-path any more.
;;   The sml-mode-startup.el file does it for you.
;; - Symbols like -> can be displayed as real arrows.
;;   See sml-font-lock-symbols.
;; - Fix some incompatibilities with the upcoming Emacs-21.4.
;; - Indentation rules improved.  New customizable variable
;;   `sml-rightalign-and'.  Also `sml-symbol-indent' is now customizable.

;;;;; Changes since 3.9.3:

;; - New add-log support (try C-x 4 a from within an SML function).
;; - Imenu support
;; - sml-bindings has disappeared.
;; - The code skeletons are now abbrevs as well.
;; - A new *sml* process is sent the content of sml-config-file
;;   (~/.sml-proc.sml) if it exists.
;; - `sml-compile' works yet a bit differently.  The command can begin
;;   with `cd "path";' and it will be replaced by OS.FileSys.chDir.
;; - run-sml now pops up the new buffer.  It can also run the command on
;;   another machine.  And it always prompts for the command name.
;;   Use a prefix argument if you want to give args or to specify a host on
;;   which to run the command.
;; - mouse-2 to yank in *sml* should work again (but won't work for next-error
;;   any more).
;; - New major-modes sml-cm-mode, sml-lex-mode and sml-yacc-mode.
;; - sml-load-hook has disappeared as has inferior-sml-load-hook.
;; - sml-mode-startup.el is now automatically generated and you're supposed to
;;   `load' it from .emacs or site-start.el.
;; - Minor bug fixes.

;;; Code:

(eval-when-compile (require 'cl))
(require 'smie nil 'noerror)
(require 'electric)

(defgroup sml ()
  "Editing SML code."
  :group 'languages)

(defcustom sml-indent-level 4
  "Basic indentation step for SML code."
  :type 'integer)

(defcustom sml-indent-args sml-indent-level
  "Indentation of args placed on a separate line."
  :type 'integer)

(defcustom sml-rightalign-and t
  "If non-nil, right-align `and' with its leader.
If nil:					If t:
	datatype a = A				datatype a = A
	and b = B				     and b = B"
  :type 'boolean)

(defcustom sml-electric-pipe-mode t
  "If non-nil, automatically insert appropriate template when hitting |."
  :type 'boolean)

(defvar sml-mode-hook nil
  "Run upon entering `sml-mode'.
This is a good place to put your preferred key bindings.")

;; font-lock setup

(defvar sml-outline-regexp
  ;; `st' and `si' are to match structure and signature.
  "\\|s[ti]\\|[ \t]*\\(let[ \t]+\\)?\\(fun\\|and\\)\\_>"
  "Regexp matching a major heading.
This actually can't work without extending `outline-minor-mode' with the
notion of \"the end of an outline\".")

;;
;; Internal defines
;;

(defvar sml-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Text-formatting commands:
    (define-key map "\C-c\C-m" 'sml-insert-form)
    (define-key map "\M-|" 'sml-electric-pipe)
    (define-key map "\M-\ " 'sml-electric-space)
    (define-key map [backtab] 'sml-back-to-outer-indent)
    ;; The standard binding is C-c C-z, but we add this one for compatibility.
    (define-key map "\C-c\C-s" 'sml-prog-proc-switch-to)
    map)
  "The keymap used in `sml-mode'.")

(defvar sml-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\* ". 23n" st)
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\) ")(4" st)
    (mapc (lambda (c) (modify-syntax-entry c "_" st)) "._'")
    (mapc (lambda (c) (modify-syntax-entry c "." st)) ",;")
    ;; `!' is not really a prefix-char, oh well!
    (mapc (lambda (c) (modify-syntax-entry c "'"  st)) "~#!")
    (mapc (lambda (c) (modify-syntax-entry c "."  st)) "%&$+-/:<=>?@`^|")
    st)
  "The syntax table used in `sml-mode'.")


(easy-menu-define sml-mode-menu sml-mode-map "Menu used in `sml-mode'."
  '("SML"
    ("Process"
     ["Start SML repl"		sml-run		t]
     ["-" nil nil]
     ["Compile the project"	sml-prog-proc-compile	t]
     ["Send file"		sml-prog-proc-load-file	t]
     ["Switch to SML repl"	sml-prog-proc-switch-to	t]
     ["--" nil nil]
     ["Send buffer"		sml-prog-proc-send-buffer	t]
     ["Send region"		sml-prog-proc-send-region	t]
     ["Send function"		sml-send-function t]
     ["Goto next error"		next-error	t])
    ["Insert SML form"		sml-insert-form t]
    ("Forms" :filter sml-forms-menu)
    ["Indent region"		indent-region t]
    ["Outdent line"		sml-back-to-outer-indent t]
    ["-----" nil nil]
    ["Customize SML-mode"  (customize-group 'sml)	t]
    ["SML mode help"       describe-mode t]))

;;
;; Regexps
;;

(defun sml-syms-re (syms)
  (concat "\\_<" (regexp-opt syms t) "\\_>"))

;;

(defconst sml-module-head-syms
  '("signature" "structure" "functor" "abstraction"))


(defconst sml-=-starter-syms
  (list* "|" "val" "fun" "and" "datatype" "type" "abstype" "eqtype"
	 sml-module-head-syms)
  "Symbols that can be followed by a `='.")
(defconst sml-=-starter-re
  (concat "\\S.|\\S.\\|" (sml-syms-re (cdr sml-=-starter-syms)))
  "Symbols that can be followed by a `='.")

(defconst sml-non-nested-of-starter-re
  (sml-syms-re '("datatype" "abstype" "exception"))
  "Symbols that can introduce an `of' that shouldn't behave like a paren.")

(defconst sml-starters-syms
  (append sml-module-head-syms
	  '("abstype" "datatype" "exception" "fun"
	    "local" "infix" "infixr" "sharing" "nonfix"
	    "open" "type" "val" "and"
	    "withtype" "with"))
  "The starters of new expressions.")

(defconst sml-pipeheads
  '("|" "of" "fun" "fn" "and" "handle" "datatype" "abstype"
    "(" "{" "[")
   "A `|' corresponds to one of these.")

(defconst sml-keywords-regexp
  (sml-syms-re '("abstraction" "abstype" "and" "andalso" "as" "before" "case"
                 "datatype" "else" "end" "eqtype" "exception" "do" "fn"
                 "fun" "functor" "handle" "if" "in" "include" "infix"
                 "infixr" "let" "local" "nonfix" "o" "of" "op" "open" "orelse"
                 "overload" "raise" "rec" "sharing" "sig" "signature"
                 "struct" "structure" "then" "type" "val" "where" "while"
                 "with" "withtype"))
  "A regexp that matches any and all keywords of SML.")

(eval-and-compile
  (defconst sml-id-re "\\sw\\(?:\\sw\\|\\s_\\)*"))

(defconst sml-tyvarseq-re
  (concat "\\(?:\\(?:'+" sml-id-re "\\|(\\(?:[,']\\|" sml-id-re
          "\\|\\s-\\)+)\\)\\s-+\\)?"))

;;; Font-lock settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sml-font-lock-symbols nil
  "Display \\ and -> and such using symbols in fonts.
This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises w.r.t layout."
  :type 'boolean)

(defconst sml-font-lock-symbols-alist
  '(("fn" . ?λ)
    ("andalso" . ?∧) ;; ?⋀
    ("orelse"  . ?∨) ;; ?⋁
    ;; ("as" . ?≡)
    ("not" . ?¬)
    ("div" . ?÷)
    ("*"   . ?×)
    ("o"   . ?○)
    ("->"  . ?→)
    ("=>"  . ?⇒)
    ("<-"  . ?←)
    ("<>"  . ?≠)
    (">="  . ?≥)
    ("<="  . ?≤)
    ("..." . ?⋯)
    ;; ("::" . ?∷)
    ;; Some greek letters for type parameters.
    ("'a" . ?α)
    ("'b" . ?β)
    ("'c" . ?γ)
    ("'d" . ?δ)
    ))

(defun sml-font-lock-compose-symbol ()
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
	 (end (match-end 0))
	 (syntaxes (if (eq (char-syntax (char-after start)) ?w)
		       '(?w) '(?. ?\\))))
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
	    (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
	    (memq (get-text-property start 'face)
		  '(font-lock-doc-face font-lock-string-face
		    font-lock-comment-face)))
	;; No composition for you.  Let's actually remove any composition
	;; we may have added earlier and which is now incorrect.
	(remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end (cdr (assoc (match-string 0)
                                            sml-font-lock-symbols-alist)))))
  ;; Return nil because we're not adding any face property.
  nil)

(defun sml-font-lock-symbols-keywords ()
  (when sml-font-lock-symbols
    `((,(regexp-opt (mapcar 'car sml-font-lock-symbols-alist) t)
       (0 (sml-font-lock-compose-symbol))))))

;; The font lock regular expressions.

(defconst sml-font-lock-keywords
  `(;;(sml-font-comments-and-strings)
    (,(concat "\\_<\\(fun\\|and\\)\\s-+" sml-tyvarseq-re
              "\\(" sml-id-re "\\)\\s-+[^ \t\n=]")
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    (,(concat "\\_<\\(\\(?:data\\|abs\\|with\\|eq\\)?type\\)\\s-+"
              sml-tyvarseq-re "\\(" sml-id-re "\\)")
     (1 font-lock-keyword-face)
     (2 font-lock-type-def-face))
    (,(concat "\\_<\\(val\\)\\s-+\\(?:" sml-id-re "\\_>\\s-*\\)?\\("
              sml-id-re "\\)\\s-*[=:]")
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    (,(concat "\\_<\\(structure\\|functor\\|abstraction\\)\\s-+\\("
              sml-id-re "\\)")
     (1 font-lock-keyword-face)
     (2 font-lock-module-def-face))
    (,(concat "\\_<\\(signature\\)\\s-+\\(" sml-id-re "\\)")
     (1 font-lock-keyword-face)
     (2 font-lock-interface-def-face))
    
    (,sml-keywords-regexp . font-lock-keyword-face)
    ,@(sml-font-lock-symbols-keywords))
  "Regexps matching standard SML keywords.")

(defface font-lock-type-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight type definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-type-def-face 'font-lock-type-def-face
  "Face name to use for type definitions.")

(defface font-lock-module-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight module definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-module-def-face 'font-lock-module-def-face
  "Face name to use for module definitions.")

(defface font-lock-interface-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight interface definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-interface-def-face 'font-lock-interface-def-face
  "Face name to use for interface definitions.")

;;
;; Code to handle nested comments and unusual string escape sequences
;;

(defvar sml-syntax-prop-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?* "." st)
    st)
  "Syntax table for text-properties.")

(defconst sml-font-lock-syntactic-keywords
  `(("^\\s-*\\(\\\\\\)" (1 ',sml-syntax-prop-table))))

(defconst sml-font-lock-defaults
  '(sml-font-lock-keywords nil nil nil nil
    (font-lock-syntactic-keywords . sml-font-lock-syntactic-keywords)))


;;; Indentation with SMIE

(defconst sml-smie-grammar
  ;; We have several problem areas where SML's syntax can't be handled by an
  ;; operator precedence grammar:
  ;;
  ;; "= A before B" is "= A) before B" if this is the
  ;;   `boolean-=' but it is "= (A before B)" if it's the `definitional-='.
  ;;   We can work around the problem by tweaking the lexer to return two
  ;;   different tokens for the two different kinds of `='.
  ;; "of A | B" in a "case" we want "of (A | B, but in a `datatype'
  ;;   we want "of A) | B".
  ;; "= A | B" can be "= A ) | B" if the = is from a "fun" definition,
  ;;   but it is "= (A | B" if it is a `datatype' definition (of course, if
  ;;   the previous token introducing the = is `and', deciding whether
  ;;   it's a datatype or a function requires looking even further back).
  ;; "functor foo (...) where type a = b = ..." the first `=' looks very much
  ;;   like a `definitional-=' even tho it's just an equality constraint.
  ;;   Currently I don't even try to handle `where' at all.
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((exp ("if" exp "then" exp "else" exp)
            ("case" exp "of" branches)
            ("let" decls "in" cmds "end")
            ("struct" decls "end")
            ("sig" decls "end")
            (sexp)
            (sexp "handle" branches)
            ("fn" sexp "=>" exp))
       ;; "simple exp"s are the ones that can appear to the left of `handle'.
       (sexp (sexp ":" type) ("(" exps ")")
             (sexp "orelse" sexp)
             (marg ":>" type)
             (sexp "andalso" sexp))
       (cmds (cmds ";" cmds) (exp))
       (exps (exps "," exps) (exp))     ; (exps ";" exps)
       (branches (sexp "=>" exp) (branches "|" branches))
       ;; Operator precedence grammars handle separators much better then
       ;; starters/terminators, so let's pretend that let/fun are separators.
       (decls (sexp "d=" exp)
              (sexp "d=" databranches)
              (funbranches "|" funbranches)
              (sexp "=of" type)         ;After "exception".
              ;; FIXME: Just like PROCEDURE in Pascal and Modula-2, this
              ;; interacts poorly with the other constructs since I
              ;; can't make "local" a separator like fun/val/type/...
              ("local" decls "in" decls "end")
              ;; (decls "local" decls "in" decls "end")
              (decls "functor" decls)
              (decls "signature" decls)
              (decls "structure" decls)
              (decls "type" decls)
              (decls "open" decls)
              (decls "and" decls)
              (decls "infix" decls)
              (decls "infixr" decls)
              (decls "nonfix" decls)
              (decls "abstype" decls)
              (decls "datatype" decls)
              (decls "exception" decls)
              (decls "fun" decls)
              (decls "val" decls))
       (type (type "->" type)
             (type "*" type))
       (funbranches (sexp "d=" exp))
       (databranches (sexp "=of" type) (databranches "d|" databranches))
       ;; Module language.
       ;; (mexp ("functor" marg "d=" mexp)
       ;;       ("structure" marg "d=" mexp)
       ;;       ("signature" marg "d=" mexp))
       (marg (marg ":" type) (marg ":>" type))
       (toplevel (decls) (exp) (toplevel ";" toplevel)))
     ;; '(("local" . opener))
     ;; '((nonassoc "else") (right "handle"))
     '((nonassoc "of") (assoc "|"))     ; "case a of b => case c of d => e | f"
     '((nonassoc "handle") (assoc "|")) ; Idem for "handle".
     '((assoc "->") (assoc "*"))
     '((assoc "val" "fun" "type" "datatype" "abstype" "open" "infix" "infixr"
              "nonfix" "functor" "signature" "structure" "exception"
              ;; "local"
              )
       (assoc "and"))
     '((assoc "orelse") (assoc "andalso") (nonassoc ":"))
     '((assoc ";")) '((assoc ",")) '((assoc "d|")))

    (smie-precs->prec2
     '((nonassoc "andalso")                       ;To anchor the prec-table.
       (assoc "before")                           ;0
       (assoc ":=" "o")                           ;3
       (nonassoc ">" ">=" "<>" "<" "<=" "=")      ;4
       (assoc "::" "@")                           ;5
       (assoc "+" "-" "^")                        ;6
       (assoc "/" "*" "quot" "rem" "div" "mod")   ;7
       (nonassoc " -dummy- ")))                   ;Bogus anchor at the end.
    )))

(defvar sml-indent-separator-outdent 2)

(defun sml-smie-rules (kind token)
  ;; I much preferred the pcase version of the code, especially while
  ;; edebugging the code.  But that will have to wait until we get rid of
  ;; support for Emacs-23.
  (case kind
    (:elem (case token
             (basic sml-indent-level)
             (args  sml-indent-args)))
    (:list-intro (member token '("fn")))
    (:after
     (cond
      ((equal token "struct") 0)
      ((equal token "=>") (if (smie-rule-hanging-p) 0 2))
      ((equal token "in") (if (smie-rule-parent-p "local") 0))
      ((equal token "of") 3)
      ((member token '("(" "{" "[")) (if (not (smie-rule-hanging-p)) 2))
      ((equal token "else") (if (smie-rule-hanging-p) 0)) ;; (:next "if" 0)
      ((member token '("|" "d|" ";" ",")) (smie-rule-separator kind))
      ((equal token "d=")
       (if (and (smie-rule-parent-p "val") (smie-rule-next-p "fn")) -3))))
    (:before
     (cond
      ((equal token "=>") (if (smie-rule-parent-p "fn") 3))
      ((equal token "of") 1)
      ;; In case the language is extended to allow a | directly after of.
      ((and (equal token "|") (smie-rule-prev-p "of")) 1)
      ((member token '("|" "d|" ";" ",")) (smie-rule-separator kind))
      ;; Treat purely syntactic block-constructs as being part of their parent,
      ;; when the opening statement is hanging.
      ((member token '("let" "(" "[" "{"))
       (if (smie-rule-hanging-p) (smie-rule-parent)))
      ;; Treat if ... else if ... as a single long syntactic construct.
      ;; Similarly, treat fn a => fn b => ... as a single construct.
      ((member token '("if" "fn"))
       (and (not (smie-rule-bolp))
            (smie-rule-prev-p (if (equal token "if") "else" "=>"))
            (smie-rule-parent)))
      ((equal token "and")
       ;; FIXME: maybe "and" (c|sh)ould be handled as an smie-separator.
       (cond
        ((smie-rule-parent-p "datatype") (if sml-rightalign-and 5 0))
        ((smie-rule-parent-p "fun" "val") 0)))
      ((equal token "d=")
       (cond
        ((smie-rule-parent-p "datatype") (if (smie-rule-bolp) 2))
        ((smie-rule-parent-p "structure" "signature") 0)))
      ;; Indent an expression starting with "local" as if it were starting
      ;; with "fun".
      ((equal token "local") (smie-indent-keyword "fun"))
      ;; FIXME: type/val/fun/... are separators but "local" is not, even though
      ;; it appears in the same list.  Try to fix up the problem by hand.
      ;; ((or (equal token "local")
      ;;      (equal (cdr (assoc token smie-grammar))
      ;;             (cdr (assoc "fun" smie-grammar))))
      ;;  (let ((parent (save-excursion (smie-backward-sexp))))
      ;;    (when (or (and (equal (nth 2 parent) "local")
      ;;                   (null (car parent)))
      ;;              (progn
      ;;                (setq parent (save-excursion (smie-backward-sexp "fun")))
      ;;                (eq (car parent) (nth 1 (assoc "fun" smie-grammar)))))
      ;;      (goto-char (nth 1 parent))
      ;;      (cons 'column (smie-indent-virtual)))))
      ))))

(defun sml-smie-definitional-equal-p ()
  "Figure out which kind of \"=\" this is.
Assumes point is right before the = sign."
  ;; The idea is to look backward for the first occurrence of a token that
  ;; requires a definitional "=" and then see if there's such a definitional
  ;; equal between that token and ourselves (in which case we're not
  ;; a definitional = ourselves).
  ;; The "search for =" is naive and will match "=>" and "<=", but it turns
  ;; out to be OK in practice because such tokens very rarely (if ever) appear
  ;; between the =-starter and the corresponding definitional equal.
  ;; One known problem case is code like:
  ;; "functor foo (structure s : S) where type t = s.t ="
  ;; where the "type t = s.t" is mistaken for a type definition.
  (let ((re (concat "\\(" sml-=-starter-re "\\)\\|=")))
    (save-excursion
      (and (re-search-backward re nil t)
           (or (match-beginning 1)
               ;; If we first hit a "=", then that = is probably definitional
               ;; and  we're an equality, but not necessarily.  One known
               ;; problem case is code like:
               ;; "functor foo (structure s : S) where type t = s.t ="
               ;; where the first = is more like an equality (tho it doesn't
               ;; matter much) and the second is definitional.
               ;;
               ;; FIXME: The test below could be used to recognize that the
               ;; second = is not a mere equality, but that's not enough to
               ;; parse the construct properly: we'd need something
               ;; like a third kind of = token for structure definitions, in
               ;; order for the parser to be able to skip the "type t = s.t"
               ;; as a sub-expression.
               ;;
               ;; (and (not (looking-at "=>"))
               ;;      (not (eq ?< (char-before))) ;Not a <=
               ;;      (re-search-backward re nil t)
               ;;      (match-beginning 1)
               ;;      (equal "type" (buffer-substring (- (match-end 1) 4)
               ;;                                      (match-end 1))))
               )))))

(defun sml-smie-non-nested-of-p ()
  ;; FIXME: Maybe datatype-|-p makes this nested-of business unnecessary.
  "Figure out which kind of \"of\" this is.
Assumes point is right before the \"of\" symbol."
  (save-excursion
    (and (re-search-backward (concat "\\(" sml-non-nested-of-starter-re
                                     "\\)\\|\\_<case\\_>") nil t)
         (match-beginning 1))))

(defun sml-smie-datatype-|-p ()
  "Figure out which kind of \"|\" this is.
Assumes point is right before the | symbol."
  (save-excursion
    (forward-char 1)                    ;Skip the |.
    (let ((after-type-def
           '("|" "of" "in" "datatype" "and" "exception" "abstype" "infix"
             "infixr" "nonfix" "local" "val" "fun" "structure" "functor"
             "signature")))
      (or (member (sml-smie-forward-token-1) after-type-def) ;Skip the tag.
          (member (sml-smie-forward-token-1) after-type-def)))))

(defun sml-smie-forward-token-1 ()
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (progn
     (or (/= 0 (skip-syntax-forward "'w_"))
         (skip-syntax-forward ".'"))
     (point))))

(defun sml-smie-forward-token ()
  (let ((sym (sml-smie-forward-token-1)))
    (cond
     ((equal "op" sym)
      (concat "op " (sml-smie-forward-token-1)))
     ((member sym '("|" "of" "="))
      ;; The important lexer for indentation's performance is the backward
      ;; lexer, so for the forward lexer we delegate to the backward one.
      (save-excursion (sml-smie-backward-token)))
     (t sym))))

(defun sml-smie-backward-token-1 ()
  (forward-comment (- (point)))
  (buffer-substring-no-properties
   (point)
   (progn
     (or (/= 0 (skip-syntax-backward ".'"))
         (skip-syntax-backward "'w_"))
     (point))))

(defun sml-smie-backward-token ()
  (let ((sym (sml-smie-backward-token-1)))
    (unless (zerop (length sym))
      ;; FIXME: what should we do if `sym' = "op" ?
      (let ((point (point)))
	(if (equal "op" (sml-smie-backward-token-1))
	    (concat "op " sym)
	  (goto-char point)
	  (cond
	   ((string= sym "=") (if (sml-smie-definitional-equal-p) "d=" "="))
	   ((string= sym "of") (if (sml-smie-non-nested-of-p) "=of" "of"))
           ((string= sym "|") (if (sml-smie-datatype-|-p) "d|" "|"))
	   (t sym)))))))

;;;;
;;;; Imenu support
;;;;

(defvar sml-imenu-regexp
  (concat "^[ \t]*\\(let[ \t]+\\)?"
	  (regexp-opt (append sml-module-head-syms
			      '("and" "fun" "datatype" "abstype" "type")) t)
	  "\\_>"))

(defun sml-imenu-create-index ()
  (let (alist)
    (goto-char (point-max))
    (while (re-search-backward sml-imenu-regexp nil t)
      (save-excursion
	(let ((kind (match-string 2))
	      (column (progn (goto-char (match-beginning 2)) (current-column)))
	      (location
	       (progn (goto-char (match-end 0))
		      (forward-comment (point-max))
		      (when (looking-at sml-tyvarseq-re)
			(goto-char (match-end 0)))
		      (point)))
	      (name (sml-smie-forward-token)))
	  ;; Eliminate trivial renamings.
	  (when (or (not (member kind '("structure" "signature")))
		    (progn (search-forward "=")
			   (forward-comment (point-max))
			   (looking-at "sig\\|struct")))
	    (push (cons (concat (make-string (/ column 2) ?\ ) name) location)
		  alist)))))
    alist))

;;; Generic prog-proc interaction.

(require 'comint)
(require 'compile)

(defvar sml-prog-proc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-l] 'sml-prog-proc-load-file)
    (define-key map [?\C-c ?\C-c] 'sml-prog-proc-compile)
    (define-key map [?\C-c ?\C-z] 'sml-prog-proc-switch-to)
    (define-key map [?\C-c ?\C-r] 'sml-prog-proc-send-region)
    (define-key map [?\C-c ?\C-b] 'sml-prog-proc-send-buffer)
    ;; FIXME: Add
    ;; (define-key map [?\M-C-x] 'sml-prog-proc-send-defun)
    ;; (define-key map [?\C-x ?\C-e] 'sml-prog-proc-send-last-sexp)
    ;; FIXME: Add menu.  Now, that's trickier because keymap inheritance
    ;; doesn't play nicely with menus!
    map)
  "Keymap for `sml-prog-proc-mode'.")

(defvar sml-prog-proc--buffer nil
  "The inferior-process buffer to which to send code.")
(make-variable-buffer-local 'sml-prog-proc--buffer)

(defstruct (sml-prog-proc-descriptor
            (:constructor sml-prog-proc-make)
            (:predicate nil)
            (:copier nil))
  (name nil :read-only t)
  (run nil :read-only t)
  (load-cmd nil :read-only t)
  (chdir-cmd nil :read-only t)
  (command-eol "\n" :read-only t)
  (compile-commands-alist nil :read-only t))

(defvar sml-prog-proc-descriptor nil
  "Struct containing the various functions to create a new process, ...")

(defmacro sml-prog-proc--prop (prop)
  `(,(intern (format "sml-prog-proc-descriptor-%s" prop))
    (or sml-prog-proc-descriptor
        ;; FIXME: Look for available ones and pick one.
        (error "Not a `sml-prog-proc' buffer"))))
(defmacro sml-prog-proc--call (method &rest args)
  `(funcall (sml-prog-proc--prop ,method) ,@args))

;; The inferior process and his buffer are basically interchangeable.
;; Currently the code takes sml-prog-proc--buffer as the main reference,
;; but all users should either use sml-prog-proc-proc or sml-prog-proc-buffer
;; to find the info.

(defun sml-prog-proc-proc ()
  "Return the inferior process for the code in current buffer."
  (or (and (buffer-live-p sml-prog-proc--buffer)
           (get-buffer-process sml-prog-proc--buffer))
      (when (derived-mode-p 'sml-prog-proc-mode 'sml-prog-proc-comint-mode)
        (setq sml-prog-proc--buffer (current-buffer))
        (get-buffer-process sml-prog-proc--buffer))
      (let ((ppd sml-prog-proc-descriptor)
            (buf (sml-prog-proc--call run)))
        (with-current-buffer buf
          (if (and ppd (null sml-prog-proc-descriptor))
              (set (make-local-variable 'sml-prog-proc-descriptor) ppd)))
        (setq sml-prog-proc--buffer buf)
        (get-buffer-process sml-prog-proc--buffer))))

(defun sml-prog-proc-buffer ()
  "Return the buffer of the inferior process."
  (process-buffer (sml-prog-proc-proc)))

(defun sml-prog-proc-switch-to ()
  "Switch to the buffer running the read-eval-print process."
  (interactive)
  (pop-to-buffer (sml-prog-proc-buffer)))

(defun sml-prog-proc-send-string (proc str)
  "Send command STR to PROC, with an EOL terminator appended."
  (with-current-buffer (process-buffer proc)
    ;; FIXME: comint-send-string does not pass the string through
    ;; comint-input-filter-function, so we have to do it by hand.
    ;; Maybe we should insert the command into the buffer and then call
    ;; comint-send-input?
    (sml-prog-proc-comint-input-filter-function nil)
    (comint-send-string proc (concat str (sml-prog-proc--prop command-eol)))))

(defun sml-prog-proc-load-file (file &optional and-go)
  "Load FILE into the read-eval-print process.
FILE is the file visited by the current buffer.
If prefix argument AND-GO is used, then we additionally switch
to the buffer where the process is running."
  (interactive
   (list (or buffer-file-name
             (read-file-name "File to load: " nil nil t))
         current-prefix-arg))
  (comint-check-source file)
  (let ((proc (sml-prog-proc-proc)))
    (sml-prog-proc-send-string proc (sml-prog-proc--call load-cmd file))
    (when and-go (pop-to-buffer (process-buffer proc)))))

(defvar sml-prog-proc--tmp-file nil)

(defun sml-prog-proc-send-region (start end &optional and-go)
  "Send the content of the region to the read-eval-print process.
START..END delimit the region; AND-GO if non-nil indicate to additionally
switch to the process's buffer."
  (interactive "r\nP")
  (if (> start end) (let ((tmp end)) (setq end start) (setq start tmp))
    (if (= start end) (error "Nothing to send: the region is empty")))
  (let ((proc (sml-prog-proc-proc))
        (tmp (make-temp-file "emacs-region")))
    (write-region start end tmp nil 'silently)
    (when sml-prog-proc--tmp-file
      (ignore-errors (delete-file (car sml-prog-proc--tmp-file)))
      (set-marker (cdr sml-prog-proc--tmp-file) nil))
    (setq sml-prog-proc--tmp-file (cons tmp (copy-marker start)))
    (sml-prog-proc-send-string proc (sml-prog-proc--call load-cmd tmp))
    (when and-go (pop-to-buffer (process-buffer proc)))))

(defun sml-prog-proc-send-buffer (&optional and-go)
  "Send the content of the current buffer to the read-eval-print process.
AND-GO if non-nil indicate to additionally switch to the process's buffer."
  (interactive "P")
  (sml-prog-proc-send-region (point-min) (point-max) and-go))

(define-derived-mode sml-prog-proc-mode prog-mode "Sml-Prog-Proc"
  "Major mode for editing source code and interact with an interactive loop."
  )

;;; Extended comint-mode for Sml-Prog-Proc.

(defun sml-prog-proc-chdir (dir)
  "Change the working directory of the inferior process to DIR."
  (interactive "DChange to directory: ")
  (let ((dir (expand-file-name dir))
        (proc (sml-prog-proc-proc)))
    (with-current-buffer (process-buffer proc)
      (sml-prog-proc-send-string proc (sml-prog-proc--call chdir-cmd dir))
      (setq default-directory (file-name-as-directory dir)))))

(defun sml-prog-proc-comint-input-filter-function (str)
  ;; `compile.el' doesn't know that file location info from errors should be
  ;; recomputed afresh (without using stale info from earlier compilations).
  (compilation-forget-errors)       ;Has to run before compilation-fake-loc.
  (if (and sml-prog-proc--tmp-file (marker-buffer (cdr sml-prog-proc--tmp-file)))
      (compilation-fake-loc (cdr sml-prog-proc--tmp-file)
                            (car sml-prog-proc--tmp-file)))
  str)

(defvar sml-prog-proc-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-l" 'sml-prog-proc-load-file)
    map))

(define-derived-mode sml-prog-proc-comint-mode comint-mode "Sml-Prog-Proc-Comint"
  "Major mode for an inferior process used to run&compile source code."
  ;; Enable compilation-minor-mode, but only after the child mode is setup
  ;; since the child-mode might want to add rules to
  ;; compilation-error-regexp-alist.
  (add-hook 'after-change-major-mode-hook #'compilation-minor-mode nil t)
  ;; The keymap of compilation-minor-mode is too unbearable, so we
  ;; need to hide most of the bindings.
  (let ((map (make-sparse-keymap)))
    (dolist (keys '([menu-bar] [follow-link]))
      ;; Preserve some of the bindings.
      (define-key map keys (lookup-key compilation-minor-mode-map keys)))
    (add-to-list 'minor-mode-overriding-map-alist
                 (cons 'compilation-minor-mode map)))

  (add-hook 'comint-input-filter-functions
            #'sml-prog-proc-comint-input-filter-function nil t))

(defvar sml-prog-proc--compile-command nil
  "The command used by default by `sml-prog-proc-compile'.")

(defun sml-prog-proc-compile (command &optional and-go)
  "Pass COMMAND to the read-eval-loop process to compile the current file.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `compilation-read-command' is
non-nil.  With prefix arg, always prompts.

Prefix arg AND-GO also means to switch to the read-eval-loop buffer afterwards."
  (interactive
   (let* ((dir default-directory)
	  (cmd "cd \"."))
     ;; Look for files to determine the default command.
     (while (and (stringp dir)
                 (progn
                   (dolist (cf (sml-prog-proc--prop compile-commands-alist))
                     (when (file-exists-p (expand-file-name (cdr cf) dir))
                       (setq cmd (concat cmd "\"; " (car cf)))
                       (return nil)))
                   (not cmd)))
       (let ((newdir (file-name-directory (directory-file-name dir))))
	 (setq dir (unless (equal newdir dir) newdir))
	 (setq cmd (concat cmd "/.."))))
     (setq cmd
	   (cond
	    ((local-variable-p 'sml-prog-proc--compile-command)
             sml-prog-proc--compile-command)
	    ((string-match "^\\s-*cd\\s-+\"\\.\"\\s-*;\\s-*" cmd)
	     (substring cmd (match-end 0)))
	    ((string-match "^\\s-*cd\\s-+\"\\(\\./\\)" cmd)
	     (replace-match "" t t cmd 1))
	    ((string-match ";" cmd) cmd)
	    (t sml-prog-proc--compile-command)))
     ;; code taken from compile.el
     (list (if (or compilation-read-command current-prefix-arg)
               (read-from-minibuffer "Compile command: "
				     cmd nil nil '(compile-history . 1))
             cmd))))
     ;; ;; now look for command's file to determine the directory
     ;; (setq dir default-directory)
     ;; (while (and (stringp dir)
     ;; 	    (dolist (cf (sml-prog-proc--prop compile-commands-alist) t)
     ;; 	      (when (and (equal cmd (car cf))
     ;; 			 (file-exists-p (expand-file-name (cdr cf) dir)))
     ;; 		(return nil))))
     ;;   (let ((newdir (file-name-directory (directory-file-name dir))))
     ;;     (setq dir (unless (equal newdir dir) newdir))))
     ;; (setq dir (or dir default-directory))
     ;; (list cmd dir)))
  (set (make-local-variable 'sml-prog-proc--compile-command) command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((dir default-directory))
    (when (string-match "^\\s-*cd\\s-+\"\\([^\"]+\\)\"\\s-*;" command)
      (setq dir (match-string 1 command))
      (setq command (replace-match "" t t command)))
    (setq dir (expand-file-name dir))
    (let ((proc (sml-prog-proc-proc))
          (eol (sml-prog-proc--prop command-eol)))
      (with-current-buffer (process-buffer proc)
        (setq default-directory dir)
        (sml-prog-proc-send-string
         proc (concat (sml-prog-proc--call chdir-cmd dir)
                      ;; Strip the newline, to avoid adding a prompt.
                      (if (string-match "\n\\'" eol)
                          (replace-match " " t t eol) eol)
                      command))
        (when and-go (pop-to-buffer (process-buffer proc)))))))


;;; SML Sml-Prog-Proc support.

(defcustom sml-program-name "sml"
  "Program to run as Standard SML read-eval-print loop."
  :type 'string)

(defcustom sml-default-arg ""
  "Default command line option to pass to `sml-program-name', if any."
  :type 'string)

(defcustom sml-host-name ""
  "Host on which to run `sml-program-name'."
  :type 'string)

(defcustom sml-config-file "~/.smlproc.sml"
  "File that should be fed to the SML process when started."
  :type 'string)


(defcustom sml-prompt-regexp "^[-=>#] *"
  "Regexp used to recognise prompts in the inferior SML process."
  :type 'regexp)

(defcustom sml-compile-commands-alist
  '(("CMB.make()" . "all-files.cm")
    ("CMB.make()" . "pathconfig")
    ("CM.make()" . "sources.cm")
    ("use \"load-all\"" . "load-all"))
  "Commands used by default by `sml-sml-prog-proc-compile'.
Each command is associated with its \"main\" file.
It is perfectly OK to associate several files with a command or several
commands with the same file.")

;; FIXME: Try to auto-detect the process and set those vars accordingly.

(defvar sml-use-command "use \"%s\""
  "Template for loading a file into the inferior SML process.
Set to \"use \\\"%s\\\"\" for SML/NJ or Edinburgh ML; 
set to \"PolyML.use \\\"%s\\\"\" for Poly/ML, etc.")

(defvar sml-cd-command "OS.FileSys.chDir \"%s\""
  "Command template for changing working directories under SML.
Set this to nil if your compiler can't change directories.

The format specifier \"%s\" will be converted into the directory name
specified when running the command \\[sml-cd].")

(defvar sml-error-regexp-alist
  `( ;; Poly/ML messages
    ("^\\(Error\\|Warning:\\) in '\\(.+\\)', line \\([0-9]+\\)" 2 3)
    ;; Moscow ML
    ("^File \"\\([^\"]+\\)\", line \\([0-9]+\\)\\(-\\([0-9]+\\)\\)?, characters \\([0-9]+\\)-\\([0-9]+\\):" 1 2 5)
    ;; SML/NJ:  the file-pattern is anchored to avoid
    ;; pathological behavior with very long lines.
    ("^[-= ]*\\(.*[^\n)]\\)\\( (.*)\\)?:\\([0-9]+\\)\\.\\([0-9]+\\)\\(-\\([0-9]+\\)\\.\\([0-9]+\\)\\)? \\(Error\\|Warnin\\(g\\)\\): .*" 1
     (3 . 6) (4 . 7) (9))
    ;; SML/NJ's exceptions:  see above.
    ("^ +\\(raised at: \\)?\\(.+\\):\\([0-9]+\\)\\.\\([0-9]+\\)\\(-\\([0-9]+\\)\\.\\([0-9]+\\)\\)" 2
     (3 . 6) (4 . 7)))
  "Alist that specifies how to match errors in compiler output.
See `compilation-error-regexp-alist' for a description of the format.")

(defconst sml-pp-functions
  (sml-prog-proc-make :name "SML"
                  :run (lambda () (call-interactively #'sml-run))
                  :load-cmd (lambda (file) (format sml-use-command file))
                  :chdir-cmd (lambda (dir) (format sml-cd-command dir))
                  :compile-commands-alist sml-compile-commands-alist
                  :command-eol ";\n"
                  ))

;; font-lock support
(defconst inferior-sml-font-lock-keywords
  `(;; prompt and following interactive command
    ;; FIXME: Actually, this should already be taken care of by comint.
    (,(concat "\\(" sml-prompt-regexp "\\)\\(.*\\)")
     (1 font-lock-prompt-face)
     (2 font-lock-command-face keep))
    ;; CM's messages
    ("^\\[\\(.*GC #.*\n\\)*.*\\]" . font-lock-comment-face)
    ;; SML/NJ's irritating GC messages
    ("^GC #.*" . font-lock-comment-face))
  "Font-locking specification for inferior SML mode.")

(defface font-lock-prompt-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight prompts."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-prompt-face 'font-lock-prompt-face
  "Face name to use for prompts.")

(defface font-lock-command-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight interactive commands."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-command-face 'font-lock-command-face
  "Face name to use for interactive commands.")

(defconst inferior-sml-font-lock-defaults
  '(inferior-sml-font-lock-keywords nil nil nil nil))

(defun sml--read-run-cmd ()
  (list
   (read-string "SML command: " sml-program-name)
   (if (or current-prefix-arg (> (length sml-default-arg) 0))
       (read-string "Any args: " sml-default-arg)
     sml-default-arg)
   (if (or current-prefix-arg (> (length sml-host-name) 0))
       (read-string "On host: " sml-host-name)
     sml-host-name)))

;;;###autoload
(defalias 'run-sml 'sml-run)

;;;###autoload
(defun sml-run (cmd arg &optional host)
  "Run the program CMD with given arguments ARG.
The command is run in buffer *CMD* using mode `inferior-sml-mode'.
If the buffer already exists and has a running process, then
just go to this buffer.

If a prefix argument is used, the user is also prompted for a HOST
on which to run CMD using `remote-shell-program'.

\(Type \\[describe-mode] in the process's buffer for a list of commands.)"
  (interactive (sml--read-run-cmd))
  (let* ((pname (file-name-nondirectory cmd))
         (args (split-string arg))
	 (file (when (and sml-config-file (file-exists-p sml-config-file))
		 sml-config-file)))
    ;; And this -- to keep these as defaults even if
    ;; they're set in the mode hooks.
    (setq sml-program-name cmd)
    (setq sml-default-arg arg)
    (setq sml-host-name host)
    ;; For remote execution, use `remote-shell-program'
    (when (> (length host) 0)
      (setq args (list* host "cd" default-directory ";" cmd args))
      (setq cmd remote-shell-program))
    ;; Go for it.
    (save-current-buffer
      (let ((exec-path (if (and (file-name-directory cmd)
                                (not (file-name-absolute-p cmd)))
                           ;; If the command has slashes, make sure we
                           ;; first look relative to the current directory.
                           ;; Emacs-21 does it for us, but not Emacs-20.
                           (cons default-directory exec-path) exec-path)))
        (pop-to-buffer (apply 'make-comint pname cmd file args)))

      (inferior-sml-mode)
      (goto-char (point-max))
      (current-buffer))))

(defun sml-send-function (&optional and-go)
  "Send current paragraph to the inferior SML process. 
With a prefix argument AND-GO switch to the repl buffer as well."
  (interactive "P")
  (save-excursion
    (sml-mark-function)
    (sml-prog-proc-send-region (point) (mark)))
  (if and-go (sml-prog-proc-switch-to)))

(defvar inferior-sml-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-c\C-s" 'sml-run)
    (define-key map "\t" 'completion-at-point)
    map)
  "Keymap for inferior-sml mode.")


(declare-function smerge-refine-subst "smerge-mode"
                  (beg1 end1 beg2 end2 props-c))

(defun inferior-sml-next-error-hook ()
  ;; Try to recognize SML/NJ type error message and to highlight finely the
  ;; difference between the two types (in case they're large, it's not
  ;; always obvious to spot it).
  ;;
  ;; Sample messages:
  ;; 
  ;; Data.sml:31.9-33.33 Error: right-hand-side of clause doesn't agree with function result type [tycon mismatch]
  ;;   expression:  Hstring
  ;;   result type:  Hstring * int
  ;;   in declaration:
  ;;     des2hs = (fn SYM_ID hs => hs
  ;;                | SYM_OP hs => hs
  ;;                | SYM_CHR hs => hs)
  ;; Data.sml:35.44-35.63 Error: operator and operand don't agree [tycon mismatch]
  ;;   operator domain: Hstring * Hstring
  ;;   operand:         (Hstring * int) * (Hstring * int)
  ;;   in expression:
  ;;     HSTRING.ieq (h1,h2)
  ;; vparse.sml:1861.6-1922.14 Error: case object and rules don't agree [tycon mismatch]
  ;;   rule domain: STConstraints list list option
  ;;   object: STConstraints list option
  ;;   in expression:
  (save-current-buffer
    (when (and (derived-mode-p 'sml-mode 'inferior-sml-mode)
               (boundp 'next-error-last-buffer)
               (bufferp next-error-last-buffer)
               (set-buffer next-error-last-buffer)
               (derived-mode-p 'inferior-sml-mode)
               ;; The position of `point' is not guaranteed :-(
               (looking-at (concat ".*\\[tycon mismatch\\]\n"
                                   "  \\(operator domain\\|expression\\|rule domain\\): +")))
      (require 'smerge-mode)
      (save-excursion
        (let ((b1 (match-end 0))
              e1 b2 e2)
          (when (re-search-forward "\n  in \\(expression\\|declaration\\):\n"
                                   nil t)
            (setq e2 (match-beginning 0))
            (when (re-search-backward
                   "\n  \\(operand\\|result type\\|object\\): +"
                   b1 t)
              (setq e1 (match-beginning 0))
              (setq b2 (match-end 0))
              (smerge-refine-subst b1 e1 b2 e2
                                   '((face . smerge-refined-change))))))))))

(define-derived-mode inferior-sml-mode sml-prog-proc-comint-mode "Inferior-SML"
  "Major mode for interacting with an inferior SML process.

The following commands are available:
\\{inferior-sml-mode-map}

An SML process can be fired up (again) with \\[sml].

Customisation: Entry to this mode runs the hooks on `comint-mode-hook'
and `inferior-sml-mode-hook' (in that order).

Variables controlling behaviour of this mode are

`sml-program-name' (default \"sml\")
    Program to run as SML.

`sml-use-command' (default \"use \\\"%s\\\"\")
    Template for loading a file into the inferior SML process.

`sml-cd-command' (default \"System.Directory.cd \\\"%s\\\"\")
    SML command for changing directories in SML process (if possible).

`sml-prompt-regexp' (default \"^[\\-=] *\")
    Regexp used to recognise prompts in the inferior SML process.

You can send text to the inferior SML process from other buffers containing
SML source.
    `switch-to-sml' switches the current buffer to the SML process buffer.
    `sml-send-function' sends the current *paragraph* to the SML process.
    `sml-send-region' sends the current region to the SML process.

    Prefixing the sml-send-<whatever> commands with \\[universal-argument]
    causes a switch to the SML process buffer after sending the text.

For information on running multiple processes in multiple buffers, see
documentation for variable `sml-buffer'.

Commands:
RET after the end of the process' output sends the text from the
    end of process to point.
RET before the end of the process' output copies the current line
    to the end of the process' output, and sends it.
DEL converts tabs to spaces as it moves back.
TAB file name completion, as in shell-mode, etc.."
  (setq comint-prompt-regexp sml-prompt-regexp)
  (sml-mode-variables)

  ;; We have to install it globally, 'cause it's run in the *source* buffer :-(
  (add-hook 'next-error-hook 'inferior-sml-next-error-hook)

  ;; Make TAB add a " rather than a space at the end of a file name.
  (set (make-local-variable 'comint-completion-addsuffix) '(?/ . ?\"))

  (set (make-local-variable 'font-lock-defaults)
       inferior-sml-font-lock-defaults)

  ;; Compilation support (used for `next-error').
  (set (make-local-variable 'compilation-error-regexp-alist)
       sml-error-regexp-alist)
  ;; FIXME: move it to sml-mode?
  (set (make-local-variable 'compilation-error-screen-columns) nil)

  (setq mode-line-process '(": %s")))

;;; MORE CODE FOR SML-MODE

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.s\\(ml\\|ig\\)\\'" . sml-mode))

(defvar comment-quote-nested)

;;;###autoload
(define-derived-mode sml-mode sml-prog-proc-mode "SML"
  "\\<sml-mode-map>Major mode for editing Standard ML code.
This mode runs `sml-mode-hook' just before exiting.
See also (info \"(sml-mode)Top\").
\\{sml-mode-map}"
  (set (make-local-variable 'sml-prog-proc-descriptor) sml-pp-functions)
  (set (make-local-variable 'font-lock-defaults) sml-font-lock-defaults)
  (set (make-local-variable 'outline-regexp) sml-outline-regexp)
  (set (make-local-variable 'imenu-create-index-function)
       'sml-imenu-create-index)
  (set (make-local-variable 'add-log-current-defun-function)
       'sml-current-fun-name)
  ;; Treat paragraph-separators in comments as paragraph-separators.
  (set (make-local-variable 'paragraph-separate)
       (concat "\\([ \t]*\\*)?\\)?\\(" paragraph-separate "\\)"))
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'electric-indent-chars)
       (cons ?\; (if (boundp 'electric-indent-chars)
                     electric-indent-chars '(?\n))))
  (set (make-local-variable 'electric-layout-rules)
       `((?\; . ,(lambda ()
                   (save-excursion
                     (skip-chars-backward " \t;")
                     (unless (or (bolp)
                                 (progn (skip-chars-forward " \t;")
                                        (eolp)))
                       'after))))))
  (when sml-electric-pipe-mode
    (add-hook 'post-self-insert-hook #'sml-post-self-insert-pipe nil t))
  (sml-mode-variables))

(defun sml-mode-variables ()
  (set-syntax-table sml-mode-syntax-table)
  (setq local-abbrev-table sml-mode-abbrev-table)
  ;; Setup indentation and sexp-navigation.
  (smie-setup sml-smie-grammar #'sml-smie-rules
              :backward-token #'sml-smie-backward-token
              :forward-token #'sml-smie-forward-token)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-start-skip) "(\\*+\\s-*")
  (set (make-local-variable 'comment-end-skip) "\\s-*\\*+)")
  ;; No need to quote nested comments markers.
  (set (make-local-variable 'comment-quote-nested) nil))

(defun sml-funname-of-and ()
  "Name of the function this `and' defines, or nil if not a function.
Point has to be right after the `and' symbol and is not preserved."
  (forward-comment (point-max))
  (if (looking-at sml-tyvarseq-re) (goto-char (match-end 0)))
  (let ((sym (sml-smie-forward-token)))
    (forward-comment (point-max))
    (unless (or (member sym '(nil "d="))
		(member (sml-smie-forward-token) '("d=")))
      sym)))

(defun sml-find-forward (re)
  (while (progn (forward-comment (point-max))
                (not (looking-at re)))
    (or (ignore-errors (forward-sexp 1) t) (forward-char 1))))

(defun sml-electric-pipe ()
  "Insert a \"|\".
Depending on the context insert the name of function, a \"=>\" etc."
  (interactive)
  (unless (save-excursion (skip-chars-backward "\t ") (bolp)) (insert "\n"))
  (insert "| ")
  (unless (sml-post-self-insert-pipe (1- (point)))
    (indent-according-to-mode)))

(defun sml-post-self-insert-pipe (&optional acp)
  (when (or acp (and (eq ?| last-command-event)
                     (setq acp (electric--after-char-pos))))
    (let ((text
           (save-excursion
             (goto-char (1- acp))       ;Jump before the "|" we just inserted.
             (let ((sym (sml-find-matching-starter sml-pipeheads
                                                   ;; (sml-op-prec "|" 'back)
                                                   )))
               (sml-smie-forward-token)
               (forward-comment (point-max))
               (cond
                ((string= sym "|")
                 (let ((f (sml-smie-forward-token)))
                   (sml-find-forward "\\(=>\\|=\\||\\)\\S.")
                   (cond
                    ((looking-at "|") nil)     ; A datatype or an OR pattern?
                    ((looking-at "=>") " => ") ;`case', or `fn' or `handle'.
                    ((looking-at "=")          ;A function.
                     (cons (concat f " ")" = ")))))
                ((string= sym "and")
                 ;; Could be a datatype or a function.
                 (let ((funname (sml-funname-of-and)))
                   (if funname (cons (concat funname " ") " = ") nil)))
                ((string= sym "fun")
                 (while (and (setq sym (sml-smie-forward-token))
                             (string-match "^'" sym))
                   (forward-comment (point-max)))
                 (cons (concat sym " ") " = "))
                ((member sym '("case" "handle" "of")) " => ") ;; "fn"?
                ;;((member sym '("abstype" "datatype")) "")
                (t nil))))))
      (when text
        (save-excursion
          (goto-char (1- acp))
          (unless (save-excursion (skip-chars-backward "\t ") (bolp))
            (insert "\n")))
        (unless (memq (char-before) '(?\s ?\t)) (insert " "))
        (let ((use-region (and (use-region-p) (< (point) (mark)))))
          ;; (skeleton-proxy-new `(nil ,(if (consp text) (pop text)) _ ,text))
          (when (consp text) (insert (pop text)))
          (if (not use-region)
              (save-excursion (insert text))
            (goto-char (mark))
            (insert text)))
        (indent-according-to-mode)
        t))))


;;; Misc

(defun sml-mark-function ()
  "Mark the surrounding function.  Or try to at least."
  (interactive)
  ;; FIXME: Provide beginning-of-defun-function so mark-defun "just works".
  (let ((start (point)))
    (sml-beginning-of-defun)
    (let ((beg (point)))
      (smie-forward-sexp 'halfsexp)
      (if (or (< start beg) (> start (point)))
          (progn
            (goto-char start)
            (mark-paragraph))
        (push-mark nil t t)
        (goto-char beg)))))

(defun sml-back-to-outer-indent ()
  "Unindents to the next outer level of indentation."
  (interactive)
  (save-excursion
    (forward-line 0)
    (let ((start-column (current-indentation))
          indent)
      (when (> start-column 0)
        (save-excursion
          (while (>= (setq indent
                           (if (re-search-backward "^[ \t]*[^\n\t]" nil t)
                               (current-indentation)
                             0))
                     start-column))
          (skip-chars-forward " \t")
          (let ((pos (point)))
            (move-to-column start-column)
            (when (re-search-backward " \\([^ \t\n]\\)" pos t)
              (goto-char (match-beginning 1))
              (setq indent (current-column)))))
        (indent-line-to indent)))))

(defun sml-find-matching-starter (syms)
  (let ((halfsexp nil)
        tok)
    ;;(sml-smie-forward-token)
    (while (not (or (bobp)
                    (member (nth 2 (setq tok (smie-backward-sexp halfsexp)))
                            syms)))
      (cond
       ((null (car tok)) nil)
       ((numberp (car tok)) (setq halfsexp 'half))
       (t (goto-char (cadr tok)))))
    (if (nth 2 tok) (goto-char (cadr tok)))
    (nth 2 tok)))

(defun sml-skip-siblings ()
  (let (tok)
    (while (and (not (bobp))
                (progn (setq tok (smie-backward-sexp 'half))
                       (cond
                        ((null (car tok)) t)
                        ((numberp (car tok)) t)
                        (t nil)))))
    (if (nth 2 tok) (goto-char (cadr tok)))
    (nth 2 tok)))

(defun sml-beginning-of-defun ()
  (let ((sym (sml-find-matching-starter sml-starters-syms)))
    (if (member sym '("fun" "and" "functor" "signature" "structure"
		      "abstraction" "datatype" "abstype"))
	(save-excursion (sml-smie-forward-token) (forward-comment (point-max))
			(sml-smie-forward-token))
      ;; We're inside a "non function declaration": let's skip all other
      ;; declarations that we find at the same level and try again.
      (sml-skip-siblings)
      ;; Obviously, let's not try again if we're at bobp.
      (unless (bobp) (sml-beginning-of-defun)))))

(defcustom sml-max-name-components 3
  "Maximum number of components to use for the current function name."
  :type 'integer)

(defun sml-current-fun-name ()
  (save-excursion
    (let ((count sml-max-name-components)
	  fullname name)
      (end-of-line)
      (while (and (> count 0)
		  (setq name (sml-beginning-of-defun)))
	(decf count)
	(setq fullname (if fullname (concat name "." fullname) name))
	;; Skip all other declarations that we find at the same level.
	(sml-skip-siblings))
      fullname)))


;;; INSERTING PROFORMAS (COMMON SML-FORMS)

(defvar sml-forms-alist nil
  "Alist of code templates.
You can extend this alist to your heart's content.  For each additional
template NAME in the list, declare a keyboard macro or function (or
interactive command) called 'sml-form-NAME'.
If 'sml-form-NAME' is a function it takes no arguments and should
insert the template at point\; if this is a command it may accept any
sensible interactive call arguments\; keyboard macros can't take
arguments at all.
`sml-forms-alist' understands let, local, case, abstype, datatype,
signature, structure, and functor by default.")

(defmacro sml-def-skeleton (name interactor &rest elements)
  (let ((fsym (intern (concat "sml-form-" name))))
    `(progn
       (add-to-list 'sml-forms-alist ',(cons name fsym))
       (define-abbrev sml-mode-abbrev-table ,name "" ',fsym nil 'system)
       (let ((abbrev (abbrev-symbol ,name sml-mode-abbrev-table)))
         (abbrev-put abbrev :case-fixed t)
         (abbrev-put abbrev :enable-function
                     (lambda () (not (nth 8 (syntax-ppss))))))
       (define-skeleton ,fsym
         ,(format "SML-mode skeleton for `%s..' expressions" name)
         ,interactor
         ,(concat name " ") >
         ,@elements))))
(put 'sml-def-skeleton 'lisp-indent-function 2)

(sml-def-skeleton "let" nil
  @ "\nin " > _ "\nend" >)

(sml-def-skeleton "if" nil
  @ " then " > _ "\nelse " > _)

(sml-def-skeleton "local" nil
  @ "\nin" > _ "\nend" >)

(sml-def-skeleton "case" "Case expr: "
  str "\nof " > _ " => ")

(sml-def-skeleton "signature" "Signature name: "
  str " =\nsig" > "\n" > _ "\nend" >)

(sml-def-skeleton "structure" "Structure name: "
  str " =\nstruct" > "\n" > _ "\nend" >)

(sml-def-skeleton "functor" "Functor name: "
  str " () : =\nstruct" > "\n" > _ "\nend" >)

(sml-def-skeleton "datatype" "Datatype name and type params: "
  str " =" \n)

(sml-def-skeleton "abstype" "Abstype name and type params: "
  str " =" \n _ "\nwith" > "\nend" >)

;;

(sml-def-skeleton "struct" nil
  _ "\nend" >)

(sml-def-skeleton "sig" nil
  _ "\nend" >)

(sml-def-skeleton "val" nil
  @ " = " > _)

(sml-def-skeleton "fn" nil
  @ " =>" > _)

(sml-def-skeleton "fun" nil
  @ " =" > _)

;;

(defun sml-forms-menu (_menu)
  (mapcar (lambda (x) (vector (car x) (cdr x) t))
	  sml-forms-alist))

(defvar sml-last-form "let")

(defun sml-electric-space ()
  "Expand a symbol into an SML form, or just insert a space.
If the point directly precedes a symbol for which an SML form exists,
the corresponding form is inserted."
  (interactive)
  (let ((abbrev-mode (not abbrev-mode))
	(last-command-event ?\s)
	;; Bind `this-command' to fool skeleton's special abbrev handling.
	(this-command 'self-insert-command))
    (call-interactively 'self-insert-command)))

(defun sml-insert-form (name newline)
  "Interactive short-cut to insert the NAME common SML form.
If a prefix argument is given insert a NEWLINE and indent first, or
just move to the proper indentation if the line is blank\; otherwise
insert at point (which forces indentation to current column).

The default form to insert is 'whatever you inserted last time'
\(just hit return when prompted\)\; otherwise the command reads with
completion from `sml-forms-alist'."
  (interactive
   (list (completing-read
	  (format "Form to insert (default %s): " sml-last-form)
	  sml-forms-alist nil t nil nil sml-forms-alist)
	 current-prefix-arg))
  (setq sml-last-form name)
  (unless (or (not newline)
	      (save-excursion (beginning-of-line) (looking-at "\\s-*$")))
    (insert "\n"))
  (when (memq (char-syntax (preceding-char)) '(?_ ?w)) (insert " "))
  (let ((f (cdr (assoc name sml-forms-alist))))
    (cond
     ((commandp f) (command-execute f))
     (f (funcall f))
     (t (error "Undefined SML form: %s" name)))))

;;;
;;; MLton support
;;;

(defvar sml-mlton-command "mlton"
  "Command to run MLton.   Can include arguments.")

(defvar sml-mlton-mainfile nil)

(defconst sml-mlton-error-regexp-alist
  ;; I wish they just changed MLton to use one of the standard
  ;; error formats.
  `(("^\\(?:Error\\|\\(Warning\\)\\): \\(.+\\) \\([0-9]+\\)\\.\\([0-9]+\\)\\.$"
     2 3 4
     ;; If subgroup 1 matched, then it's a warning, otherwise it's an error.
     (1))))

(defvar compilation-error-regexp-alist)
(eval-after-load "compile"
  '(dolist (x sml-mlton-error-regexp-alist)
     (add-to-list 'compilation-error-regexp-alist x)))

(defun sml-mlton-typecheck (mainfile)
  "Typecheck using MLton.
MAINFILE is the top level file of the project."
  (interactive
   (list (if (and sml-mlton-mainfile (not current-prefix-arg))
             sml-mlton-mainfile
           (read-file-name "Main file: "))))
  (setq sml-mlton-mainfile mainfile)
  (save-some-buffers)
  (require 'compile)
  (dolist (x sml-mlton-error-regexp-alist)
    (add-to-list 'compilation-error-regexp-alist x))
  (with-current-buffer (find-file-noselect mainfile)
    (compile (concat sml-mlton-command
                     " -stop tc "       ;Stop right after type checking.
                     (shell-quote-argument
                      (file-relative-name buffer-file-name))))))

;;;
;;; MLton's def-use info.
;;;

(defvar sml-defuse-file nil)

(defun sml-defuse-file ()
  (or sml-defuse-file (sml-defuse-set-file)))

(defun sml-defuse-set-file ()
  "Specify the def-use file to use."
  (interactive)
  (setq sml-defuse-file (read-file-name "Def-use file: ")))

(defun sml-defuse-symdata-at-point ()
  (save-excursion
    (sml-smie-forward-token)
    (let ((symname (sml-smie-backward-token)))
      (if (equal symname "op")
          (save-excursion (setq symname (sml-smie-forward-token))))
      (when (string-match "op " symname)
        (setq symname (substring symname (match-end 0)))
        (forward-word)
        (forward-comment (point-max)))
      (list symname
            ;; Def-use files seem to count chars, not columns.
            ;; We hope here that they don't actually count bytes.
            ;; Also they seem to start counting at 1.
            (1+ (- (point) (progn (beginning-of-line) (point))))
            (save-restriction
              (widen) (1+ (count-lines (point-min) (point))))
            buffer-file-name))))

(defconst sml-defuse-def-regexp
  "^[[:alpha:]]+ \\([^ \n]+\\) \\(.+\\) \\([0-9]+\\)\\.\\([0-9]+\\)$")
(defconst sml-defuse-use-regexp-format "^    %s %d\\.%d $")

(defun sml-defuse-jump-to-def ()
  "Jump to the definition corresponding to the symbol at point."
  (interactive)
  (let ((symdata (sml-defuse-symdata-at-point)))
    (if (null (car symdata))
        (error "Not on a symbol")
      (with-current-buffer (find-file-noselect (sml-defuse-file))
        (goto-char (point-min))
        (unless (re-search-forward
                 (format sml-defuse-use-regexp-format
                         (concat "\\(?:"
                                 ;; May be an absolute file name.
                                 (regexp-quote (nth 3 symdata))
                                 "\\|"
                                 ;; Or a relative file name.
                                 (regexp-quote (file-relative-name
                                                (nth 3 symdata)))
                                 "\\)")
                         (nth 2 symdata)
                         (nth 1 symdata))
                 nil t)
          ;; FIXME: This is typically due to editing: any minor editing will
          ;; mess everything up.  We should try to fail more gracefully.
          (error "Def-use info not found"))
        (unless (re-search-backward sml-defuse-def-regexp nil t)
          ;; This indicates a bug in this code.
          (error "Internal failure while looking up def-use"))
        (unless (equal (match-string 1) (nth 0 symdata))
          ;; FIXME: This again is most likely due to editing.
          (error "Incoherence in the def-use info found"))
        (let ((line (string-to-number (match-string 3)))
              (char (string-to-number (match-string 4))))
          (pop-to-buffer (find-file-noselect (match-string 2)))
          (goto-char (point-min))
          (forward-line (1- line))
          (forward-char (1- char)))))))

;;;
;;; SML/NJ's Compilation Manager support
;;;

(defvar sml-cm-mode-syntax-table sml-mode-syntax-table)
(defvar sml-cm-font-lock-keywords
 `(,(concat "\\_<" (regexp-opt '("library" "group" "is" "structure"
				"functor" "signature" "funsig") t)
	    "\\_>")))
;;;###autoload
(add-to-list 'completion-ignored-extensions ".cm/")
;; This was used with the old compilation manager.
(add-to-list 'completion-ignored-extensions "CM/")
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cm\\'" . sml-cm-mode))
;;;###autoload
(define-derived-mode sml-cm-mode fundamental-mode "SML-CM"
  "Major mode for SML/NJ's Compilation Manager configuration files."
  (set (make-local-variable 'sml-prog-proc-descriptor) sml-pp-functions)
  (set (make-local-variable 'font-lock-defaults)
       '(sml-cm-font-lock-keywords nil t nil nil)))

;;;
;;; ML-Lex support
;;;

(defvar sml-lex-font-lock-keywords
  (append
   `((,(concat "^%" sml-id-re) . font-lock-builtin-face)
     ("^%%" . font-lock-module-def-face))
   sml-font-lock-keywords))
(defconst sml-lex-font-lock-defaults
  (cons 'sml-lex-font-lock-keywords (cdr sml-font-lock-defaults)))

;;;###autoload
(define-derived-mode sml-lex-mode sml-mode "SML-Lex"
  "Major Mode for editing ML-Lex files."
  (set (make-local-variable 'font-lock-defaults) sml-lex-font-lock-defaults))

;;;
;;; ML-Yacc support
;;;

(defface sml-yacc-bnf-face
  '((t (:foreground "darkgreen")))
  "Face used to highlight (non)terminals in `sml-yacc-mode'.")
(defvar sml-yacc-bnf-face 'sml-yacc-bnf-face)

(defcustom sml-yacc-indent-action 16
  "Indentation column of the opening paren of actions."
  :type 'integer)

(defcustom sml-yacc-indent-pipe nil
  "Indentation column of the pipe char in the BNF.
If nil, align it with `:' or with previous cases."
  :type 'integer)

(defcustom sml-yacc-indent-term nil
  "Indentation column of the (non)term part.
If nil, align it with previous cases."
  :type 'integer)

(defvar sml-yacc-font-lock-keywords
  (cons `((concat "^\\(" sml-id-re "\\s-*:\\|\\s-*|\\)\\(\\s-*" sml-id-re
                  "\\)*\\s-*\\(\\(%" sml-id-re "\\)\\s-+" sml-id-re "\\|\\)")
          (0 (save-excursion
               (save-match-data
                 (goto-char (match-beginning 0))
                 (unless (or (re-search-forward "\\_<of\\_>"
                                                (match-end 0) 'move)
                             (progn (forward-comment (point-max))
                                    (not (looking-at "("))))
                   sml-yacc-bnf-face))))
          (4 font-lock-builtin-face t t))
        sml-lex-font-lock-keywords))
(defconst sml-yacc-font-lock-defaults
  (cons 'sml-yacc-font-lock-keywords (cdr sml-font-lock-defaults)))

(defun sml-yacc-indent-line ()
  "Indent current line of ML-Yacc code."
  (let ((savep (> (current-column) (current-indentation)))
	(indent (max (or (ignore-errors (sml-yacc-indentation)) 0) 0)))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun sml-yacc-indentation ()
  (save-excursion
    (back-to-indentation)
    (or (and (looking-at (eval-when-compile
                           (concat "%\\|" sml-id-re "\\s-*:")))
             0)
	(when (save-excursion
		(condition-case nil (progn (up-list -1) nil) (scan-error t)))
	  ;; We're outside an action.
	  (cond
	   ;; Special handling of indentation inside %term and %nonterm
	   ((save-excursion
	      (and (re-search-backward "^%\\(\\sw+\\)" nil t)
		   (member (match-string 1) '("term" "nonterm"))))
	    (if (numberp sml-yacc-indent-term) sml-yacc-indent-term
	      (let ((offset (if (looking-at "|") -2 0)))
		(forward-line -1)
		(looking-at "\\s-*\\(%\\sw*\\||\\)?\\s-*")
		(goto-char (match-end 0))
		(+ offset (current-column)))))
	   ((looking-at "(") sml-yacc-indent-action)
	   ((looking-at "|")
	    (if (numberp sml-yacc-indent-pipe) sml-yacc-indent-pipe
	      (backward-sexp 1)
	      (while (progn (forward-comment (- (point)))
			    (/= 0 (skip-syntax-backward "w_"))))
	      (forward-comment (- (point)))
	      (if (not (looking-at "\\s-$"))
		  (1- (current-column))
		(skip-syntax-forward " ")
		(- (current-column) 2))))))
	;; default to SML rules
        (smie-indent-calculate))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.grm\\'" . sml-yacc-mode))
;;;###autoload
(define-derived-mode sml-yacc-mode sml-mode "SML-Yacc"
  "Major Mode for editing ML-Yacc files."
  (set (make-local-variable 'indent-line-function) 'sml-yacc-indent-line)
  (set (make-local-variable 'font-lock-defaults) sml-yacc-font-lock-defaults))



;;;; ChangeLog:

;; 2012-10-31  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sml-mode.el: Integrate BUGS&NEWS; re-add run-sml.
;; 
;; 2012-10-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Add SML-mode.
;; 
;; 2012-10-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Cleanup copyright; Merge prog-proc into sml-mode.el
;; 
;; 2012-10-19  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Move sml-compile to prog-proc.
;; 
;; 2012-10-19  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sml-mode.el (sml-electric-pipe-mode): New var.
;; 	(sml-pipeheads): Add (, {, and [ to more reliably detect cases where
;; 	the pipe is not part of a case/fun/...
;; 	(sml-tyvarseq-re): Use shy groups.
;; 	(sml-font-lock-keywords): Adjust accordingly.
;; 	(sml-compile): Avoid the 3rd part of dolist's spec.
;; 	(sml-post-self-insert-pipe): New fun, extracted from sml-electric-pipe.
;; 	(sml-mode): Use it to obey sml-electric-pipe-mode.
;; 	(sml-electric-pipe): Use sml-post-self-insert-pipe.
;; 	* makefile.pkg (ELFILES): Remove sml-proc.el.
;; 	* prog-proc.el: Rename from sml-prog-proc.el.
;; 
;; 2012-10-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Add sml-compile back into sml-mode
;; 
;; 2012-10-04  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Move sml-proc to either prog-proc or sml-mode.
;; 
;; 2012-10-04  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Fix compilation
;; 
;; 2012-10-03  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Start preparing for the move to ELPA.
;; 
;; 2012-04-11  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Merge from trunk
;; 
;; 2012-04-11  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Merge sml-defs.el into sml-mode.el.
;; 	* sml-mode.el: Merge code from sml-defs.el.
;; 	Remove ":group 'sml" since they're now redundant.
;; 	* makefile.pkg (ELFILES): Adjust.
;; 
;; 2012-04-11  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* sml-mode.el (sml-mark-function): New implementation using SMIE.
;; 	* sml-defs.el (sml-mode-map): Use backtab.
;; 	Remove leftover unused sml-drag-region binding.
;; 
;; 2012-04-11  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	-
;; 
;; 2012-04-11  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Merge from trunk
;; 
;; 2012-04-11  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Use SMIE by default and make sml-oldindent optional.
;; 	* sml-mode.el: Only load sml-oldindent if necessary.
;; 	(sml-use-smie): Default to t.
;; 	(sml-smie-datatype-|-p): Better handle incomplete datatype branch.
;; 	(sml-mode): Use prog-mode.  Setup electric-layout and electric-indent.
;; 	(sml-mode-variables): Always setup SMIE if possible.
;; 	(sml-imenu-create-index, sml-funname-of-and, sml-electric-pipe)
;; 	(sml-beginning-of-defun, sml-defuse-symdata-at-point)
;; 	(sml-yacc-font-lock-keywords, sml-yacc-indentation):
;; 	Avoid sml-oldindent functions.
;; 	(sml-find-forward): Move from sml-oldindent and re-implement.
;; 	(sml-electric-semi): Use self-insert-command so electric-layout and
;; 	electric-indent can do their job.
;; 	(sml-smie-find-matching-starter, sml-find-matching-starter)
;; 	(sml-smie-skip-siblings, sml-skip-siblings): New functions.
;; 	* sml-oldindent.el (sml-starters-indent-after, sml-exptrail-syms):
;; 	Remove, unused.
;; 	(sml-find-forward): Move back to sml-mode.el.
;; 	(sml-old-find-matching-starter): Rename from sml-find-matching-starter.
;; 	(sml-old-skip-siblings): Move&rename from sml-mode:sml-skip-siblings.
;; 
;; 2012-04-11  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Merge from trunk
;; 

(provide 'sml-mode)

;;; sml-mode.el ends here
