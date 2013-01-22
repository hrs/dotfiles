;; ==========================================
;;  Archived
;; ==========================================

;; Growing the load-path
;; (add-to-list 'load-path "~/.emacs.d/haskell")
;; (add-to-list 'load-path "~/.emacs.d/ocaml")
;; (add-to-list 'load-path "~/.emacs.d/utils")
;; (add-to-list 'load-path "/usr/local/Cellar/erlang/R15B01/lib/erlang/lib/tools-2.6.7/emacs")

;; setting up Erlang with flymake
;; (require 'flymake)
;; (setq flymake-log-level 3)

;; (defun flymake-erlang-init ()
;;  (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		     'flymake-create-temp-inplace))
;; 	 (local-file (file-relative-name
;; 		      temp-file
;; 		      (file-name-directory buffer-file-name))))
;;    (list "~/.emacs.d/flymake/erlang-flymake" (list local-file))))

;; (add-to-list 'flymake-allowed-file-name-masks
;; 	     '("\\.erl\\'" flymake-erlang-init))

;; (defun flymake-erlang-mode-hook ()
;;  (flymake-mode 1))

;; (add-hook 'erlang-mode-hook 'flymake-erlang-mode-hook)

;; setting up Erlang-mode
;; (setq erlang-root-dir "/usr/local/lib/erlang")
;; (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
;; (require 'erlang-start)

;; setting up OCaml (Tuareg mode)
;; (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
;; (autoload 'tuareg-mode "tuareg" "Major mode for editing OCaml code" t)
;; (autoload 'tuareg-run-ocaml "tuareg" "Run an inferior OCaml process." t)
;; (autoload 'ocamldebug "ocamldebug" "Run the OCaml debugger" t)

;; setting up Coq
;; (setq auto-mode-alist (cons '("\.v$" . coq-mode) auto-mode-alist))
;; (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;; setting up Haskell
;; (load "~/.emacs.d/haskell/haskell-site-file")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
