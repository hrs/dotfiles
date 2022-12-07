;;; publish-mode.el --- A mode to facilitate production and previewing of PDFs.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Evaluating `publish-build-and-view-pdf' builds a PDF of the current buffer
;; (either Org or Markdown, using `pandoc') and opens an external viewer
;; (`zathura', by default, but configurable through `publish-viewer-pdf') if
;; it's not already open.

;; The key benefit here is that I can hit a key and see the PDF output of the
;; buffer in a side-by-side frame without having to think about whether the
;; viewer's already open or anything like that.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar publish-viewer-pdf "zathura"
  "The application to be used to view compiled PDF documents.")

(defun publish-build-pdf-markdown ()
  "Compile the current Markdown document into a PDF, returning the
resulting filename."
  (save-buffer)
  (let ((target-file (file-name-with-extension (buffer-file-name) "pdf")))
    (call-process-shell-command
     (format "pandoc -o \"%s\" \"%s\""
             target-file
             (buffer-file-name)))
    target-file))

(defun publish-build-pdf-org ()
  "Compile the current Org document into a PDF, returning the
resulting filename."
  (save-buffer)
  (org-latex-export-to-pdf))

(defun publish-build-pdf ()
  "Compile the current buffer into a PDF, returning the resulting
filename."
  (interactive)
  (cl-case major-mode
    (org-mode (publish-build-pdf-org))
    (gfm-mode (publish-build-pdf-markdown))
    (markdown-mode (publish-build-pdf-markdown))
    (otherwise (error "Don't know how to compile this buffer to PDF!"))))

(defun publish-viewer-open-for-file-p (viewer filename)
  "Is the document being generated already open in a viewer?"
  (eq 0 (call-process-shell-command
         (format "ps -aux | grep \"%s.*%s\" | grep --invert-match grep" viewer filename))))

(defun publish-ensure-viewer-for-pdf (viewer filename)
  "If `viewer' isn't already open for `filename', invoke it."
  (when (not (publish-viewer-open-for-file-p viewer filename))
    (start-process (format "%s \"%s\"" viewer filename) nil viewer filename)))

(defun publish-build-and-view-pdf ()
  "Build the document into a PDF and ensure the viewer is displaying it."
  (interactive)
  (publish-ensure-viewer-for-pdf publish-viewer-pdf
                                 (publish-build-pdf)))

;;;###autoload
(define-minor-mode publish-mode
  "Toggle writing mode on or off."
  :lighter " publish")

(provide 'publish-mode)
;;; publish-mode.el ends here
