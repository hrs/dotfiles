;;; zen-and-art-theme.el --- zen and art color theme for GNU Emacs 24
;; Author: Nick Parker
;; Version: 1.0.1
;;
;; Ported theme to Emacs 24 color theme Nick Parker <nickp@developernotes.com>
;; original from https://github.com/irfn/zen-and-art
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(deftheme zen-and-art
  "zen-and-art color theme")

(custom-theme-set-faces
 'zen-and-art
 '(default ((t (:background "#191717" :foreground "#d2dec4"))))
 '(cursor ((t (:foreground "#a7a7a7"))))
 '(region ((t (:background "#999966"))))
 '(fringe ((t (:background "#252323"))))
 '(border-color ((t (:background "#000000"))))
 '(cursor-color ((t (:background "#A7A7A7"))))
 '(highlight-current-line-face ((t (:background "#252323"))))
 '(hl-line ((t (:background "#252323"))))
 '(font-lock-builtin-face ((t (:foreground "#86453A"))))
 '(font-lock-comment-face ((t (:italic t :foreground "#333B40"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#4C565D"))))
 '(font-lock-constant-face ((t (:foreground "#86453A"))))
 '(font-lock-function-name-face ((t (:foreground "#C6B032"))))
 '(font-lock-keyword-face ((t (:foreground "#AE5825"))))
 '(font-lock-preprocessor-face ((t (:foreground "#007575"))))
 '(font-lock-reference-face ((t (:foreground "#0055FF"))))
 '(font-lock-string-face ((t (:foreground "#5A7644"))))
 '(font-lock-doc-face ((t (:foreground "#DDFFD1"))))
 '(font-lock-type-face ((t (:italic t :foreground "#C6B032"))))
 '(font-lock-variable-name-face ((t (:foreground "#46657B"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Pink"))))
 '(paren-face-match-light ((t (:background "#252323"))))
 '(highlight ((t (:background "darkolivegreen"))))
 '(italic ((t (:italic t))))
 '(modeline ((t (:background "#3F3B3B" :foreground "white"))))
 '(modeline-buffer-id ((t (:background "#3F3B3B" :foreground "white"))))
 '(modeline-mousable ((t (:background "#a5baf1" :foreground "black"))))
 '(modeline-mousable-minor-mode ((t (:background "#a5baf1"
                                                 :foreground "#000000"))))
 '(primary-selection ((t (:background "#3B3B3F"))))
 '(isearch ((t (:background "#555555"))))
 '(zmacs-region ((t (:background "#555577"))))
 '(secondary-selection ((t (:background "#545459"))))
 '(flymake-errline ((t (:background "LightSalmon" :foreground "#000000"))))
 '(flymake-warnline ((t (:background "LightSteelBlue" :foreground "#000000"))))
 '(underline ((t (:underline t))))
 '(minibuffer-prompt ((t (:bold t :foreground "#ff6600")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'zen-and-art)
;;; zen-and-art-theme.el ends here
