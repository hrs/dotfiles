quicklisp-slime-helper makes it easy to use SLIME from Quicklisp.

To use it, load quicklisp in your Common Lisp implementation, then
evaluate:

  (ql:quickload "quicklisp-slime-helper")

That command will create a file in the Quicklisp base directory called
"slime-helper.el". Loading that file will add the Quicklisp slime path
to your Emacs load-path.

In your ~/.emacs, you could have something like this:

  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")

quicklisp-slime-helper is available under the MIT license; see
LICENSE.txt for details.
