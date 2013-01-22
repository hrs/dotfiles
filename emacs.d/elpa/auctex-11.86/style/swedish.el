;;; swedish.el --- Setup AUCTeX for editing Swedish text.

;;; Commentary:
;;
;; Apparently the Swedes use ''this style'' quotations.

(TeX-add-style-hook
 "swedish"
 (lambda ()
   (setq TeX-quote-language
	 `("swedish" "''" ,TeX-close-quote ,TeX-quote-after-quote))
   (setq LaTeX-babel-hyphen-language "swedish")
   (run-hooks 'TeX-language-sv-hook)))
