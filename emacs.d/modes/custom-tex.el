(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            'LaTeX-math-mode
            (setq TeX-master t)))

(setenv "PATH"
        (concat (getenv "PATH")
                ":" "/usr/local/texlive/2013basic/bin/universal-darwin"
                ":" "/usr/local/texlive/2013/bin/universal-darwin"
                ":" "/usr/local/bin"))
