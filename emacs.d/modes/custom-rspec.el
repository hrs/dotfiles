(defun hrs/rspec-compilation-hook ()
  (make-local-variable 'compilation-scroll-output)
  (setq compilation-scroll-output 'first-error)
  (define-key rspec-compilation-mode-map (kbd "C-x C-q") '(lambda ()
                                                            (interactive)
                                                            (inf-ruby-switch-from-compilation)
                                                            (comint-mode))))

(add-hook 'rspec-compilation-mode-hook 'hrs/rspec-compilation-hook)
