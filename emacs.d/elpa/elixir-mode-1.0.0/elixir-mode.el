;;; elixir-mode.el --- Major mode for editing Elixir files

;; Copyright (c) 2011 secondplanet
;; Author: Humza Yaqoob
;; URL: https://github.com/secondplanet/elixir-mode
;; Created: Mon Nov 7 2011
;; Keywords: languages elixir
;; Version: 1.0.0

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provides font-locking, indentation support, and navigation for Elixir programs.

;;; Code:

(require 'comint)   ; for interactive REPL
(require 'easymenu) ; for menubar features

(defconst elixir-mode-version "1.0.0"
	"Elixir mode version number.")
(defconst elixir-mode-date "2011-11-12"
	"Elixir mode version date.")

(defvar elixir-mode-hook nil)
(defvar elixir-mode-map (make-keymap)
  "Elixir mode keymap.")

(defgroup elixir nil
	"Elixir major mode."
	:group 'languages)

(defcustom elixir-compiler-command "elixirc"
	"Elixir mode command to compile code. Must be in your path."
	:type 'string
	:group 'elixir)

(defcustom elixir-iex-command "iex"
	"Elixir mode command for interactive REPL. Must be in your path."
	:type 'string
	:group 'elixir)

(defcustom elixir-mode-highlight-operators t
	"Elixir mode option for whether or not to highlight operators."
	:type 'boolean
	:group 'elixir)

(defcustom elixir-mode-cygwin-paths t
	"Elixir mode use Cygwin style paths on Windows operating systems."
	:type 'boolean
	:group 'elixir)

(defcustom elixir-mode-cygwin-prefix "/cygdrive/C"
	"Elixir mode Cygwin prefix."
	:type 'string
	:group 'elixir)

(defvar elixir-mode-keyword-names '(
  "->"
  "do"
  "after"
  "for"
  "module"
	"private"
  "def"
  "if"
  "when"
  "case"
  "match"
  "then"
  "else"
  "elsif"
  "try"
  "catch"
  "end")
"Elixir mode keywords.")
(defvar elixir-mode-module-names '(
  "Atom"
  "BitString"
  "Code"
  "Date"
  "DateTime"
  "EEx"
  "ETS"
  "ExUnit"
  "File"
  "Float"
  "Function"
  "GenServer"
  "GenTCP"
  "IEX"
  "Integer"
  "IO"
  "List"
  "Math"
  "Method"
  "Module"
  "Numeric"
  "OrderedDict"
  "OS"
  "Port"
  "Process"
  "Record"
  "Reference"
  "Regexp"
  "Set"
  "String"
  "Timer"
  "Tuple"
  "UnboundMethod")
"Elixir mode modules.")
(defvar elixir-mode-builtin-names '(
  "Erlang")
"Elixir mode builtins.")
(defvar elixir-mode-operator-names '(
  "+"
	"-"
	"/"
	"*"
	"div"
	"rem"
	"=="
	"!="
	"<="
	"<"
	">="
	">"
	"==="
	"!=="
	"and"
	"or"
	"andalso"
	"orelse"
	"not"
	"&&"
	"||"
	"!"
	"."
	"#"
	"="
	":="
	"<-")
"Elixir mode operators.")

(defvar font-lock-operator-face 'font-lock-operator-face)
(defface font-lock-operator-face
	'((((type tty) (class color)) nil)
		(((class color) (background light))
		 (:foreground "darkred"))
		(t nil))
	"For use with operators."
	:group 'font-lock-faces)

(defvar font-lock-atom-face 'font-lock-atom-face)
(defface font-lock-operator-face
	'((((type tty) (class color)) nil)
		(((class color) (background light))
		(:foreground "magenta"))
	(t nil))
	"For use with atoms."
	:group 'font-lock-faces)

(defconst elixir-mode-font-lock-defaults
  (list
    '("%.*$" . font-lock-comment-face)                                                                                                  ; comments
    '("^\\s *def\\s +\\([^( \t\n]+\\)" . font-lock-function-name-face)                                                                  ; methods
    `(,(concat "\\<" (regexp-opt elixir-mode-keyword-names t) "\\>") . font-lock-keyword-face)                                          ; keywords
    `(,(concat "\\<" (regexp-opt elixir-mode-builtin-names t) "\\>") . font-lock-builtin-face)                                          ; builtins
    `(,(concat "\\<" (regexp-opt elixir-mode-module-names t) "\\>") . font-lock-type-face)                                              ; core modules
		(when elixir-mode-highlight-operators `(,(concat "\\<" (regexp-opt elixir-mode-operator-names t) "\\>") . font-lock-operator-face)) ; operators
    '("\\(\\w*\\)\\s-*:?=" . font-lock-variable-name-face)                                                                              ; variables
		'("-[Rr].*[ \n\t]" . font-lock-constant-face)                                                                                       ; regexes
    '("\\<\\(true\\|false\\|nil\\)\\>" . font-lock-atom-face)                                                                           ; atoms, boolean
		'("'\\w*" . font-lock-atom-face))                                                                                                   ; atoms, generic
"Highlighting for Elixir mode.")

(defun elixir-mode-indent-line ()
  "Indent current line as Elixir code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
    (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*end$")
        (progn
          (save-excursion
            (forward-line -1)
            (setq cur-indent (- (current-indentation) default-tab-width)))
        (if (< cur-indent 0)
          (setq cur-indent 0)))
       (save-excursion
         (while not-indented
           (forward-line -1)
             (if (looking-at "^[ \t]*end$")
               (progn
                 (setq cur-indent (current-indentation))
                 (setq not-indented nil))
                 (if (looking-at "^[ \t]*\\(do\\|after\\|module\\|def\\|if\\|case\\|else\\|elsif\\|receive\\|after\\|try\\|catch\\)")
                   (progn
                     (setq cur-indent (+ (current-indentation) default-tab-width))
                     (setq not-idented nil))
                 (if (bobp)
                   (setq not-indented nil)))))))
      (if cur-indent
        (indent-line-to cur-indent)
        (indent-line-to 0)))))

(defun elixir-mode-cygwin-path (expanded-file-name)
	"Elixir mode get Cygwin absolute path name."
	(replace-regexp-in-string "^[a-zA-Z]:" elixir-mode-cygwin-prefix expanded-file-name t))

(defun elixir-mode-universal-path (file-name)
	"Elixir mode multi-OS path handler."
	(let ((full-file-name (expand-file-name file-name)))
		(if (and (equal system-type 'windows-nt)
						 elixir-mode-cygwin-paths)
				(elixir-mode-cygwin-path full-file-name)
				full-file-name)))

(defun elixir-mode-command-compile (file-name)
  "Elixir mode command to compile a file."
	(let ((full-file-name (elixir-mode-universal-path file-name)))
		(mapconcat 'identity (append (list elixir-compiler-command) (list full-file-name)) " ")))

(defun elixir-mode-compiled-file-name (&optional filename)
  "Elixir mode compiled filename."
	(concat (file-name-sans-extension (or filename (buffer-file-name))) ".beam"))

(defun elixir-mode-compile-file ()
  "Elixir mode compile and save current file."
  (interactive)
	(let ((compiler-output (shell-command-to-string (elixir-mode-command-compile (buffer-file-name)))))
		(when (string= compiler-output "")
			(message "Compiled and saved as %s" (elixir-mode-compiled-file-name)))))

(defun elixir-mode-iex ()
  "Elixir mode interactive REPL."
	(interactive)
	(unless (comint-check-proc "*IEX*")
		(set-buffer
		 (apply 'make-comint "IEX"
            elixir-iex-command nil '())))
	(pop-to-buffer "*IEX*"))

(defun elixir-mode-open-modegithub ()
  "Elixir mode open GitHub page."
  (interactive)
	(browse-url "https://github.com/secondplanet/elixir-mode"))

(defun elixir-mode-open-elixir-home ()
  "Elixir mode go to language home."
  (interactive)
	(browse-url "https://github.com/josevalim/elixir#README"))

(defun elixir-mode-show-version ()
  "Elixir mode print version."
	(interactive)
	(message (concat "elixir-mode v" elixir-mode-version " " elixir-mode-date " by Humza Yaqoob")))

(defvar elixir-mode-syntax-table
  (let ((elixir-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" elixir-mode-syntax-table)
    (modify-syntax-entry ?% "<" elixir-mode-syntax-table)
    (modify-syntax-entry ?\n ">" elixir-mode-syntax-table)
    (modify-syntax-entry ?\( "()" elixir-mode-syntax-table)
    (modify-syntax-entry ?\) ")(" elixir-mode-syntax-table)
    (modify-syntax-entry ?\{ "(}" elixir-mode-syntax-table)
    (modify-syntax-entry ?\} "){" elixir-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]" elixir-mode-syntax-table)
    (modify-syntax-entry ?\] ")[" elixir-mode-syntax-table)
    elixir-mode-syntax-table)
"Elixir mode syntax table.")

(easy-menu-define elixir-mode-menu elixir-mode-map
  "Elixir mode menu."
  '("Elixir"
    ["Indent line" elixir-mode-indent-line]
		["Compile file" elixir-mode-compile-file]
		["IEX" elixir-mode-iex]
		"---"
		["elixir-mode on GitHub" elixir-mode-open-modegithub]
		["Elixir homepage" elixir-mode-open-elixirhome]
    ["About" elixir-mode-show-version]
  ))

(defun elixir-mode ()
  "Major mode for editing Elixir files."
    (interactive)
    (kill-all-local-variables)
    (set-syntax-table elixir-mode-syntax-table)
    (set (make-local-variable 'indent-line-function) 'elixir-mode-indent-line)
    (set (make-local-variable 'font-lock-defaults) '(elixir-mode-font-lock-defaults))
    (setq major-mode 'elixir-mode)
    (setq mode-name "Elixir")
    (run-hooks 'elixir-mode-hook))

(define-minor-mode elixir-cos-mode
	"Elixir mode toggle compile on save."
	:group 'elixir-cos :lighter " CoS"
	(cond
	 (elixir-cos-mode
		(add-hook 'after-save-hook 'elixir-mode-compile-file nil t))
	 (t
		(remove-hook 'after-save-hook 'elixir-mode-compile-file t))))

(provide 'elixir-mode)

;; automatically opening ex and exs in elixir-mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))

;;; elixir-mode.el ends here
