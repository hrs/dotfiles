;;; writing-mode.el --- A mode to facilitate writing! Mostly graphical bits and
;;; bobs.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Toggle some graphical elements to make prose writing more pleasant.
;;
;; Specifically, when activated, `writing-mode' currently enables
;; `mixed-pitch-mode', `olivetti-mode', and `wc-mode', creating a
;; "distraction-free" writing environment with variable-pitch fonts and a word
;; count in the mode line. It also enables `visual-line-mode', so lines wrap
;; visually instead of using hard newlines.
;;
;; It also enables lots of Org features, including hiding emphasis markers
;; unless they're under point, using a pretty ellipsis, displaying mathematical
;; entities, and displaying images inline.
;;
;; When deactivated it restores all those modes and variables to their previous
;; states.

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
(eval-when-compile (require 'seq))

(defvar writing-org-ellipsis "…")
(defvar writing-org-image-actual-width '(600))

(defvar writing-enabled-modes
  '((text-mode . (visual-line-mode)))
  "An alist mapping a major mode to a list of minor modes. When
`writing-mode' is enabled, the minor modes associated with every
major mode that derives from the current major mode with also be
enabled.

For example, enabling `writing-mode' while in `org-mode' would
enable the minor modes associated with `org-mode' and `text-mode'
in this alist, but not e.g. `markdown-mode' or `prog-mode', since
`org-mode' doesn't derive from those.")

(defvar writing-preserved-variables
  '(org-ellipsis
    org-hide-emphasis-markers
    org-hide-leading-stars
    org-image-actual-width
    org-pretty-entities
    org-startup-with-inline-images
    org-superstar-special-todo-items)
  "Variables to be preserved and restored upon exiting `writing-mode'.")

(make-variable-buffer-local
 (defvar writing-buffer-variable-states nil
   "State of buffer variables before enabling `writing-mode'. Plist
mapping variables to those initial settings."))

(make-variable-buffer-local
 (defvar writing-buffer-mode-states nil
   "State of modes before enabling `writing-mode'. Plist mapping mode
names to those initial settings."))

(defun writing--save-variable (variable-name)
  "Store a variable's value as a pair in
`writing-buffer-variable-states'."
  (setq writing-buffer-variable-states
        (plist-put writing-buffer-variable-states
                   variable-name
                   (when (boundp variable-name)
                     (symbol-value variable-name)))))

(defun writing--restore-variable (variable-name)
  "Restore a variable's value from the value stored in
`writing-buffer-variable-states'."
  (set variable-name
       (plist-get writing-buffer-variable-states variable-name)))

(defun writing--save-mode (mode-name)
  "Store a mode's value as a pair in
`writing-buffer-mode-states'."
  (setq writing-buffer-mode-states
        (plist-put writing-buffer-mode-states
                   mode-name
                   (symbol-value mode-name))))

(defun writing--restore-mode (mode-name)
  "Restore a mode's state from the value stored in
`writing-buffer-mode-states'."
  (if (plist-get writing-buffer-mode-states mode-name)
      (funcall mode-name 1)
      (funcall mode-name -1)))

(defun writing--applicable-minor-modes ()
  "Return a list of minor modes (a subset of those defined in
`writing-enabled-modes') that ought to be enabled in the
current major mode."
  (seq-reduce (lambda (acc pair)
                (if (derived-mode-p (car pair))
                    (seq-union acc (cdr pair))
                  acc))
              writing-enabled-modes '()))

(defun writing--save-settings ()
  "Save all `writing-preserved-variables' in
`writing-buffer-variable-states'."
  (dolist (variable-name writing-preserved-variables)
    (writing--save-variable variable-name))

  (dolist (mode-name (writing--applicable-minor-modes))
    (writing--save-mode mode-name)))

(defun writing--restore-settings ()
  "Restore all `writing-preserved-variables' values from
`writing-buffer-variable-states'."
  (dolist (variable-name writing-preserved-variables)
    (writing--restore-variable variable-name))

  (dolist (mode-name (writing--applicable-minor-modes))
    (writing--restore-mode mode-name)))

(defun writing-enable ()
  "Enable minor writing-mode."

  (writing--save-settings)

  (when (eq major-mode 'org-mode)
    (setq org-ellipsis writing-org-ellipsis
          org-hide-emphasis-markers t
          org-hide-leading-stars t
          org-image-actual-width writing-org-image-actual-width
          org-pretty-entities t
          org-startup-with-inline-images t
          org-superstar-special-todo-items t)

    (org-redisplay-inline-images))

  (when (eq major-mode 'gfm-mode)
    (markdown-toggle-markup-hiding 1))

  (dolist (mode-name (writing--applicable-minor-modes))
    (funcall mode-name 1)))

(defun writing-disable ()
  "Disable minor writing-mode."

  (writing--restore-settings)

  (when (and (eq major-mode 'org-mode)
             (not org-startup-with-inline-images))
    (org-toggle-inline-images))

  (when (eq major-mode 'gfm-mode)
    (markdown-toggle-markup-hiding 0)))

;;;###autoload
(define-minor-mode writing-mode
  "Toggle writing mode on or off."
  :lighter " writing"
  (if writing-mode
      (writing-enable)
      (writing-disable)))

(provide 'writing-mode)
;;; writing-mode.el ends here
