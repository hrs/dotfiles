;;; writing-mode.el --- A mode to facilitate writing! Mostly graphical bits and
;;; bobs.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Toggle some graphical elements to make prose writing more pleasant.
;;
;; Specifically, when activated, `writing-mode' enables `mixed-pitch-mode',
;; `olivetti-mode', and `wc-mode', creating a "distraction-free" writing
;; environment with variable-pitch fonts and a word count in the mode line. It
;; also enables `visual-line-mode' and disables `auto-fill-mode', so lines wrap
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
(eval-when-compile (require 'mixed-pitch)
                   (require 'olivetti)
                   (require 'org-appear)
                   (require 'org-modern)
                   (require 'wc-mode))

(defvar writing-preserved-variables
  '(org-ellipsis
    org-hide-emphasis-markers
    org-image-actual-width
    org-pretty-entities
    org-startup-with-inline-images)
  "Variables to be preserved and restored upon exiting `writing-mode'.")

(make-variable-buffer-local
 (defvar writing-buffer-saved-settings nil
   "State of buffer variables before enabling `writing-mode'. Plist
mapping cariables to those initial settings."))

(make-variable-buffer-local
 (defvar writing-initial-state-auto-fill-mode nil
   "Was auto-fill-mode initially enabled?"))

(make-variable-buffer-local
 (defvar writing-initial-state-flycheck-mode nil
   "Was flycheck-mode initially enabled?"))

(make-variable-buffer-local
 (defvar writing-initial-state-mixed-pitch-mode nil
   "Was mixed-pitch-mode initially enabled?"))

(make-variable-buffer-local
 (defvar writing-initial-state-olivetti-mode nil
   "Was olivetti-mode initially enabled?"))

(make-variable-buffer-local
 (defvar writing-initial-state-org-appear-mode nil
   "Was org-appear-mode initially enabled?"))

(make-variable-buffer-local
 (defvar writing-initial-state-org-modern-mode nil
   "Was org-modern-mode initially enabled?"))

(make-variable-buffer-local
 (defvar writing-initial-state-visual-line-mode nil
   "Was visual-line-mode initially enabled?"))

(make-variable-buffer-local
 (defvar writing-initial-state-wc-mode nil
   "Was wc-mode initially enabled?"))

(defun writing--save-variable (variable-name)
  "Store a variable's value as a pair in
`writing-buffer-saved-settings'."
  (setq writing-buffer-saved-settings
        (plist-put writing-buffer-saved-settings
                   variable-name
                   (symbol-value variable-name))))

(defun writing--restore-variable (variable-name)
  "Restore a variable's value from the value stored in
`writing-buffer-saved-settings'."
  (set variable-name
       (plist-get writing-buffer-saved-settings variable-name)))

(defun writing--save-settings ()
  "Save all `writing-preserved-variables' in
`writing-buffer-saved-settings'."
  (dolist (var writing-preserved-variables)
    (writing--save-variable var)))

(defun writing--restore-settings ()
  "Restore all `writing-preserved-variables' values from
`writing-buffer-saved-settings'."
  (dolist (var writing-preserved-variables)
    (writing--restore-variable var)))

(defun writing-enable ()
  "Enable minor writing-mode."

  (writing--save-settings)

  (setq writing-initial-state-auto-fill-mode auto-fill-function)
  (auto-fill-mode -1)

  (setq writing-initial-state-flycheck-mode flycheck-mode)
  (flycheck-mode 1)

  (setq writing-initial-state-mixed-pitch-mode mixed-pitch-mode)
  (mixed-pitch-mode 1)

  (setq writing-initial-state-olivetti-mode olivetti-mode)
  (olivetti-mode 1)

  (when (eq major-mode 'org-mode)
    (setq org-ellipsis "…"
          org-image-actual-width '(600)
          org-pretty-entities t
          org-startup-with-inline-images t
          org-hide-emphasis-markers t)

    (org-redisplay-inline-images)

    (setq writing-initial-state-org-modern-mode org-appear-mode)
    (org-appear-mode 1)

    (setq writing-initial-state-org-modern-mode org-modern-mode)
    (org-modern-mode 1))

  (setq writing-initial-state-visual-line-mode visual-line-mode)
  (visual-line-mode 1)

  (setq writing-initial-state-wc-mode wc-mode)
  (wc-mode 1))

(defun writing-disable ()
  "Disable minor writing-mode."

  (writing--restore-settings)

  (when writing-initial-state-auto-fill-mode
    (auto-fill-mode 1))

  (when (not writing-initial-state-flycheck-mode)
    (flycheck-mode -1))

  (when (not writing-initial-state-mixed-pitch-mode)
    (mixed-pitch-mode -1))

  (when (not writing-initial-state-olivetti-mode)
    (olivetti-mode -1))

  (when (eq major-mode 'org-mode)
    (org-redisplay-inline-images)

    (when (not writing-initial-state-org-appear-mode)
      (org-appear-mode -1))

    (when (not org-startup-with-inline-images)
      (org-toggle-inline-images))

    (when (not writing-initial-state-org-modern-mode)
      (org-modern-mode -1)))

  (when (not writing-initial-state-visual-line-mode)
    (visual-line-mode -1))

  (when (not writing-initial-state-wc-mode)
    (wc-mode -1)))

;;;###autoload
(define-minor-mode writing-mode
  "Toggle writing mode on or off."
  :lighter " writing"
  (if writing-mode
      (writing-enable)
      (writing-disable)))

(provide 'writing-mode)
;;; writing-mode.el ends here
