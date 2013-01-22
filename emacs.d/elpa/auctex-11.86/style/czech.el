;;; czech.el --- Setup AUCTeX for editing Czech text.

(TeX-add-style-hook
 "czech"
 (lambda ()
   (setq TeX-quote-language `("czech" "\\uv{" "}" ,TeX-quote-after-quote))
   (run-hooks 'TeX-language-cz-hook)))
