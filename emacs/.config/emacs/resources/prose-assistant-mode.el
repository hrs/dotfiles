;;; prose-assistant-mode.el --- Assorted utilities for writing arranged in
;;; graphical menus.

;; Package-Requires: ((engine-mode "2.2.0") (transient "0.3.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; `prose-assistant-mode' is a global minor mode that provides a Transient
;; interface to conveniently access a handful of common tools I reach for while
;; writing prose (dictionaries, word counting, etymologies, spellchecking,
;; translation, that sort of thing).

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
(eval-when-compile (require 'engine-mode)
                   (require 'transient))

(defun prose-assistant-region-or-word ()
  "Return the active region (if there is one) or the word at point."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word)))

(defun prose-assistant-term-prompt ()
  "Prompt for a term, defaulting to the currently active region or the word at point."
  (let ((selection (prose-assistant-region-or-word)))
    (if selection
        (read-string (format "Term (%s): " selection) nil nil selection)
        (read-string "Term: " nil nil nil))))

(defun prose-assistant-spellcheck-region-or-buffer ()
  "Run spellcheck on the active region (if there is one) or the whole buffer."
  (interactive)
  (if mark-active
      (ispell-region (region-beginning) (region-end))
    (ispell-buffer)))

(defengine prose-assistant-wiktionary
           "https://www.wikipedia.org/search-redirect.php?family=wiktionary&search=%s&language=en&go=Go")
(defun prose-assistant-wiktionary ()
  "Search Wiktionary for a prompted term."
  (interactive)
  (engine/search-prose-assistant-wiktionary (prose-assistant-term-prompt)))

(defengine prose-assistant-wikipedia
           "http://www.wikipedia.org/search-redirect.php?search=%s&language=en&go=Go")
(defun prose-assistant-wikipedia ()
  "Search Wikipedia for a prompted term."
  (interactive)
  (engine/search-prose-assistant-wikipedia (prose-assistant-term-prompt)))

(defengine prose-assistant-etymonline
           "http://etymonline.com/index.php?allowed_in_frame=0&search=%s")
(defun prose-assistant-etymonline ()
  "Search the Online Etymology Dictionary for a prompted term."
  (interactive)
  (engine/search-prose-assistant-etymonline (prose-assistant-term-prompt)))

(defengine prose-assistant-urban-dictionary
           "http://www.urbandictionary.com/define.php?term=%s")
(defun prose-assistant-urban-dictionary ()
  "Search Urban Dictionary for a prompted term."
  (interactive)
  (engine/search-prose-assistant-urban-dictionary (prose-assistant-term-prompt)))

(defengine prose-assistant-thesaurus-com
           "http://www.thesaurus.com/browse/%s")
(defun prose-assistant-thesaurus-com ()
  "Search Thesaurus.com for a prompted term."
  (interactive)
  (engine/search-prose-assistant-thesaurus-com (prose-assistant-term-prompt)))

(defengine prose-assistant-old-websters-dictionary
           "https://www.websters1913.com/words/%s")
(defun prose-assistant-old-websters-dictionary ()
  "Search Webster's 1913 for a prompted term."
  (interactive)
  (engine/search-prose-assistant-old-websters-dictionary (prose-assistant-term-prompt)))

(defengine prose-assistant-merriam-webster-dictionary
           "https://www.merriam-webster.com/dictionary/%s")
(defun prose-assistant-merriam-webster-dictionary ()
  "Search the Merriam-Webster dictionary for a prompted term."
  (interactive)
  (engine/search-prose-assistant-merriam-webster-dictionary (prose-assistant-term-prompt)))

(defengine prose-assistant-merriam-webster-thesaurus
           "https://www.merriam-webster.com/thesaurus/%s")
(defun prose-assistant-merriam-webster-thesaurus ()
  "Search the Merriam-Webster thesaurus for a prompted term."
  (interactive)
  (engine/search-prose-assistant-merriam-webster-thesaurus (prose-assistant-term-prompt)))

(defengine prose-assistant-google-translate
           "https://translate.google.com/?sl=auto&tl=en&text=%s&op=translate")
(defun prose-assistant-google-translate ()
  "Search Google Translate for a prompted term."
  (interactive)
  (engine/search-prose-assistant-google-translate (prose-assistant-term-prompt)))

(transient-define-prefix prose-assistant-menu-spellcheck ()
  "Prose assistant spellchecking menu."
  ["Spellcheck"
   ("b" "Spellcheck buffer/region" prose-assistant-spellcheck-region-or-buffer)
   ("s" "Spellcheck word" ispell-word)])

(transient-define-prefix prose-assistant-menu-thesaurus ()
  "Prose assistant thesaurus menu."
  ["Thesauruses"
   ("m" "Merriam-Webster" prose-assistant-merriam-webster-thesaurus)
   ("t" "Thesaurus.com" prose-assistant-thesaurus-com)])

(transient-define-prefix prose-assistant-menu-dictionary ()
  "Prose assistant dictionaries menu."
  ["Dictionaries"
   ("m" "Merriam-Webster" prose-assistant-merriam-webster-dictionary)
   ("o" "Webster's Dictionary, 1913" prose-assistant-old-websters-dictionary)
   ("u" "Urban Dictionary" prose-assistant-urban-dictionary)
   ("w" "Wiktionary" prose-assistant-wiktionary)])

(transient-define-prefix prose-assistant-menu ()
  "Prose assistant"
  ["Actions"
   ("c" "Word count" count-words)
   ("d" "Dictionary" prose-assistant-menu-dictionary)
   ("e" "Etymology" prose-assistant-etymonline)
   ("r" "Translate" prose-assistant-google-translate)
   ("s" "Spellcheck" prose-assistant-menu-spellcheck)
   ("t" "Thesaurus" prose-assistant-menu-thesaurus)])

(defvar prose-assistant-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x w") #'prose-assistant-menu)
    map))

;;;###autoload
(define-minor-mode prose-assistant-mode
  "Toggle prose assistant mode on or off.

Local bindings (`prose-assistant-mode-map'):
\\{prose-assistant-mode-map}"
  :global t
  :keymap prose-assistant-mode-map
  :lighter " prose")

(provide 'prose-assistant-mode)
;;; prose-assistant-mode.el ends here
