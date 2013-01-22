;;; danish.el --- Setup AUCTeX for editing Danish text.

;;; Code:

(TeX-add-style-hook
 "danish"
 (lambda ()
   (setq TeX-quote-language `("danish" "\"`" "\"'" ,TeX-quote-after-quote))
   (setq LaTeX-babel-hyphen-language "danish")
   ;; Fontification of quotation marks.
   (when (fboundp 'font-latex-add-quotes)
     (font-latex-add-quotes '("\"`" "\"'"))
     (font-latex-add-quotes '("\"<" "\">" french)))
   (run-hooks 'TeX-language-dk-hook)))

;;; danish.el ends here
