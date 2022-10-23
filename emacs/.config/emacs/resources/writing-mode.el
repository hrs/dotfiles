;;; writing-mode.el --- A mode to facilitate writing! Mostly graphical bits and
;;; bobs.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Toggle some graphical elements to make prose writing more pleasant.

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
(eval-when-compile (require 'prose-assistant-mode)
                   (require 'mixed-pitch)
                   (require 'olivetti)
                   (require 'wc-mode))

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
 (defvar writing-initial-state-visual-line-mode nil
   "Was visual-line-mode initially enabled?"))

(make-variable-buffer-local
 (defvar writing-initial-state-wc-mode nil
   "Was wc-mode initially enabled?"))

(make-variable-buffer-local
 (defvar writing-initial-state-modus-themes-headings nil
   "Original settings for modus-themes-headings."))

(defun using-modus-operandi-theme-p ()
  (eq (car custom-enabled-themes)
      'modus-operandi))

(defun writing-enable ()
  "Enable minor writing-mode."
  (setq writing-initial-state-flycheck-mode flycheck-mode)
  (flycheck-mode 1)

  (setq writing-initial-state-mixed-pitch-mode mixed-pitch-mode)
  (mixed-pitch-mode 1)

  (setq writing-initial-state-olivetti-mode olivetti-mode)
  (olivetti-mode 1)

  (setq writing-initial-state-visual-line-mode visual-line-mode)
  (visual-line-mode 1)

  (setq writing-initial-state-wc-mode wc-mode)
  (wc-mode 1)

  (when (using-modus-operandi-theme-p)
    (setq writing-initial-state-modus-themes-headings
          modus-themes-headings)

    (setq modus-themes-headings
          '((0 . (1.3))
            (1 . (1.3))
            (2 . (1.2))
            (t . (1.1))))
    (modus-themes-load-operandi)))

(defun writing-disable ()
  "Disable minor writing-mode."
  (when (not writing-initial-state-flycheck-mode)
    (flycheck-mode -1))

  (when (not writing-initial-state-mixed-pitch-mode)
    (mixed-pitch-mode -1))

  (when (not writing-initial-state-olivetti-mode)
    (olivetti-mode -1))

  (when (not writing-initial-state-visual-line-mode)
    (visual-line-mode -1))

  (when (not writing-initial-state-wc-mode)
    (wc-mode -1))

  (when (using-modus-operandi-theme-p)
    (setq modus-themes-headings
          writing-initial-state-modus-themes-headings)
    (modus-themes-load-operandi)))

;;;###autoload
(define-minor-mode writing-mode
  "Toggle writing mode on or off."
  :lighter " writing"
  (if writing-mode
      (writing-enable)
      (writing-disable)))

(provide 'writing-mode)
;;; writing-mode.el ends here
