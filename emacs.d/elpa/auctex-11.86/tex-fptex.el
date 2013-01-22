;;; tex-fptex.el --- fpTeX support for AUCTeX.

;; Copyright (C) 2000 Fabrice Popineau
;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Fabrice Popineau <Fabrice.Popineau@supelec.fr>
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

;;; Commentary:
;;
;; This file contains variables customized for fpTeX.

;;; Code:

(defmacro parent-directory (f)
  "Return safe parent directory of the directory given as argument."
  `(directory-file-name
    (file-name-directory
     (directory-file-name ,f))))

(unless (get 'TeX-lisp-directory 'saved-value)
  (setq-default TeX-lisp-directory
		(concat (parent-directory (invocation-directory))
			"/site-lisp/auctex")))

  ;; Remove the Queue entry from the default, and make a non-Unix
  ;; specific print entry, assuming that dvips will print by default.
(unless (get 'TeX-queue-command 'saved-value)
  (setq TeX-queue-command nil))

(unless (get 'TeX-printer-list 'saved-value)
  (setq TeX-printer-list nil))

(unless (get 'TeX-print-command 'saved-value)
  (setq TeX-print-command
	"dvips %d"))

(unless (get 'TeX-view-style 'saved-value)
  (setq TeX-view-style '(("^a5\\(?:comb\\|paper\\)?$" "windvi %d -qpaper a5")
			 ("^landscape$" "windvi %d -qpaper a4r -s 4")
			 ("^epsf$" "start \"\" %f")
			 ("." "windvi %d"))))

(unless (get 'TeX-output-view-style 'saved-value)
  (setq TeX-output-view-style
	'(("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start \"\" %f")
	  ("^dvi$" ("^a5\\(?:comb\\|paper\\)$" "^landscape$")
	   "windvi %d %dS -qpaper a5r -s 0")
	  ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "windvi %d %dS -qpaper a5")
	  ("^dvi$" "^b5paper$" "windvi %d %dS -qpaper b5")
	  ("^dvi$" ("^landscape$" "^pstricks$\\|^psfrag$")
	   "dvips -t landscape %d -o && start \"\" %f")
	  ("^dvi$" "^letterpaper$" "windvi %d %dS -qpaper us")
	  ("^dvi$" "^legalpaper$" "windvi %d %dS -qpaper legal")
	  ("^dvi$" "^executivepaper$" "windvi %d %dS -qpaper 7.25x10.5in")
	  ("^dvi$" "^landscape$" "windvi %d %dS -qpaper a4r")
	  ("^dvi$" "." "windvi %d %dS")
	  ("^pdf$" "." "start \"\" %o")
	  ("^html?$" "." "start \"\" %o"))))

;; WinDVI does not support source specials?
(unless (get 'TeX-source-specials-view-position-flags 'saved-value)
  (setq TeX-source-specials-view-position-flags ""))

(unless (get 'TeX-source-specials-view-editor-flags 'saved-value)
  (setq TeX-source-specials-view-editor-flags ""))

(unless (get 'TeX-kpathsea-path-delimiter 'saved-value)
  (setq TeX-kpathsea-path-delimiter ";"))

(provide 'tex-fptex)
(require 'tex-site)

;;; tex-fptex.el ends here
