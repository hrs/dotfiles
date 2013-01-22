;;; ess-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (Rd-mode) "ess-rd" "ess-rd.el" (20716 20435))
;;; Generated autoloads from ess-rd.el

(autoload 'Rd-mode "ess-rd" "\
Major mode for editing R documentation source files.

This mode makes it easier to write R documentation by helping with
indentation, doing some of the typing for you (with Abbrev mode) and by
showing keywords, strings, etc. in different faces (with Font Lock mode
on terminals that support it).

Type \\[list-abbrevs] to display the built-in abbrevs for Rd keywords.

Keybindings
===========

\\{Rd-mode-map}

Variables you can use to customize Rd mode
==========================================

`Rd-indent-level'
  Indentation of Rd code with respect to containing blocks.
  Default is 2.

Turning on Rd mode runs the hook `Rd-mode-hook'.

To automatically turn on the abbrev(iate) features, add the
following lines to your `.emacs' file:

  (add-hook 'Rd-mode-hook
	    (lambda ()
	      (abbrev-mode 1)))

\(fn)" t nil)

;;;***

;;;### (autoloads (mouse-me) "mouseme" "mouseme.el" (20716 20433))
;;; Generated autoloads from mouseme.el

(autoload 'mouse-me "mouseme" "\
Popup a menu of functions to run on selected string or region.

\(fn EVENT)" t nil)

;;;***

;;;### (autoloads nil nil ("ess-arc-d.el" "ess-bugs-d.el" "ess-bugs-l.el"
;;;;;;  "ess-comp.el" "ess-compat.el" "ess-custom.el" "ess-dde.el"
;;;;;;  "ess-debug.el" "ess-eldoc.el" "ess-font-lock.el" "ess-help.el"
;;;;;;  "ess-inf.el" "ess-install.el" "ess-jags-d.el" "ess-lsp-l.el"
;;;;;;  "ess-menu.el" "ess-mode.el" "ess-mous.el" "ess-mouse.el"
;;;;;;  "ess-noweb.el" "ess-omg-d.el" "ess-omg-l.el" "ess-pkg.el"
;;;;;;  "ess-r-a.el" "ess-r-args.el" "ess-r-d.el" "ess-r-gui.el"
;;;;;;  "ess-rdired.el" "ess-roxy.el" "ess-rutils.el" "ess-s-l.el"
;;;;;;  "ess-s3-d.el" "ess-s4-d.el" "ess-sas-a.el" "ess-sas-d.el"
;;;;;;  "ess-sas-l.el" "ess-send.el" "ess-send2.el" "ess-site.el"
;;;;;;  "ess-sp3-d.el" "ess-sp4-d.el" "ess-sp5-d.el" "ess-sp6-d.el"
;;;;;;  "ess-sp6w-d.el" "ess-sta-d.el" "ess-sta-l.el" "ess-swv.el"
;;;;;;  "ess-toolbar.el" "ess-trns.el" "ess-utils.el" "ess-vst-d.el"
;;;;;;  "ess-xls-d.el" "ess.el" "essd-els.el" "make-regexp.el" "msdos.el"
;;;;;;  "noweb-font-lock-mode.el" "noweb-mode.el") (20716 20436 59644))

;;;***

(provide 'ess-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ess-autoloads.el ends here
