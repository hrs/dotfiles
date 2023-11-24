;;; witchhazel-theme.el --- A dark, feminine color theme.

;; Version: 1.0

(deftheme witchhazel "A dark, feminine color theme.")

(custom-theme-set-faces
 'witchhazel
 '(default ((t (:foreground "#f8f8f2" :background "#433e56"))))
 '(fringe ((t (:inherit default))))
 '(font-lock-comment-face ((t (:foreground "#b0bec5"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-string-face ((t (:foreground "#1bc5e0"))))
 '(font-lock-constant-face ((t (:foreground "#c5a3ff"))))
 '(font-lock-keyword-face ((t (:foreground "#c2ffdf" :slant italic))))
 '(font-lock-type-face ((t (:foreground "#c2ffdf"))))
 '(font-lock-function-name-face ((t (:foreground "#ceb1ff"))))
 '(font-lock-variable-name-face ((t (:inherit default))))
 '(font-lock-builtin-face ((t (:foreground "#ceb1ff"))))
 '(button ((t (:foreground "#2cb5cc" :underline t))))
 '(region ((t (:background "#8077a8"))))
 '(highlight ((t (:foreground "#f8f8f2" :background "#555166"))))
 '(mode-line ((t (:foreground "#f8f8f2" :background "#716799"))))
 '(mode-line-inactive ((t (:foreground "#f8f8f2" :background "#544b7a"))))
 '(match ((t (:background "#a37c8a"))))
 '(isearch ((t (:inherit match))))
 '(minibuffer-prompt ((t (:foreground "#ceb1ff"))))
 '(hl-line ((t (:background "#716799"))))
 '(whitespace-space ((t (:foreground "#a8757b"))))
 '(whitespace-trailing ((t (:foreground "#a8757b"))))
 ;; `company-mode'
 '(company-tooltip ((t (:background "#353144"))))
 '(company-tooltip-selection ((t (:background "#716799"))))
 '(company-tooltip-mouse ((t (:inherit highlight))))
 '(company-scrollbar-fg ((t (:background "#646464"))))
 '(company-scrollbar-bg ((t (:inherit company-tooltip))))
 ;; `highlight-numbers-mode'
 '(highlight-numbers-number ((t (:foreground "#c5a3ff"))))
 ;; `ivy-mode'
 '(ivy-current-match ((t (:background "#716799"))))
 '(ivy-highlight-face ((t (:underline t))))
 ;; `markdown-mode'
 '(markdown-header-face ((t (:foreground "#fff352" :weight bold))))
 '(markdown-header-delimiter-face ((t (:inherit markdown-header-face))))
 '(markdown-link-face ((t (:foreground "#1bc5e0" :weight bold))))
 '(markdown-link-title-face ((t (:foreground "#1bc5e0"))))
 '(markdown-url-face ((t (:foreground "#c2ffdf" :underline t :slant italic))))
 ;; `rust-mode'
 '(rust-string-interpolation ((t (:foreground "#c5a3ff"))))
 '(rust-question-mark ((t (:foreground "#c2ffdf"))))
 ;; `swiper'
 '(swiper-line-face ((t (:background "#555166")))))

;;;###autoload
(and load-file-name (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path (file-name-directory load-file-name)))

(provide-theme 'witchhazel)

(provide 'witchhazel-theme)
;;; witchhazel-theme.el ends here
