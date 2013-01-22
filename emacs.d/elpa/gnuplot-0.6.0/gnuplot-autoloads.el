;;; gnuplot-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (gnuplot-make-buffer gnuplot-mode) "gnuplot" "gnuplot.el"
;;;;;;  (20552 42782))
;;; Generated autoloads from gnuplot.el

(autoload 'gnuplot-mode "gnuplot" "\
Major mode for editing and executing GNUPLOT scripts.
This was written with version 3.7 of gnuplot in mind but it should
work fine with version 3.5 and the various 3.6 beta versions.

Report bugs in `gnuplot-mode' using \\[gnuplot-bug-report].

			    ------O------

The help functions, keyword completion, and several other features
depend upon having the info file properly installed.  The info file
can be made in the document directory of the gnuplot distribution or
is available at the `gnuplot-mode' web page:
    http://feff.phys.washington.edu/~ravel/software/gnuplot-mode/

If the help function does not work properly, you may have an older
version of the gnuplot info file.  Try the suggestion in the document
string for the variable `gnuplot-info-hook'.  See the `gnuplot-mode'
web page for more details.

			    ------O------

There are several known shortcomings of `gnuplot-mode', version 0.5g
and up.  Many of the shortcomings involve the graphical interface
\(refered to as the GUI) to setting arguments to plot options.  Here is
a list:

 1.  Currently there is no way for `gnuplot-mode' to know if information
     sent to gnuplot was correctly plotted.
 2.  Indentation is sometimes a bit flaky.
 3.  \"plot\", \"splot\", and \"fit\" are handled in the GUI, but are
     a bit flaky.  Their arguments may not be read correctly from
     existing text, and continuation lines (common for plot and splot)
     are not supported.
 4.  The GUI does not know how to read from continuation lines.
 5.  Comma separated position arguments to plot options are
     unsupported in the GUI.  Colon separated datafile modifiers (used
     for plot, splot, and fit) are not supported either.  Arguments
     not yet supported by the GUI generate messages printed in grey
     text.
 6.  The GUI handling of \"hidden3d\" is flaky and \"cntrparam\" is
     unsupported.

			    ------O------

 Key bindings:
 \\{gnuplot-mode-map}

\(fn)" t nil)

(autoload 'gnuplot-make-buffer "gnuplot" "\
Open a new buffer in `gnuplot-mode'.
When invoked, it switches to a new, empty buffer visiting no file
and then starts `gnuplot-mode'.

It is convenient to bind this function to a global key sequence.  For
example, to make the F10 key open a gnuplot script buffer, put the
following in your .emacs file:
     (autoload 'gnuplot-make-buffer \"gnuplot\"
               \"open a buffer in gnuplot mode\" t)
     (global-set-key [(f10)] 'gnuplot-make-buffer)

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("gnuplot-pkg.el") (20552 42782 557121))

;;;***

(provide 'gnuplot-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gnuplot-autoloads.el ends here
