(require 'erc-services)
(require 'erc-join)
(require 'erc-image)

(defun do-notify (nick message)
  "Pop up a desktop notification about this message. Called `do-notify' because ercn seems to require that. =/"
  (hrs/system-notify "erc" (format "%s: %s" nick message)))

(setq erc-prompt-for-nickserv-password nil)
(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-static-center 22)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "MODE"))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "MODE"))
(add-to-list 'erc-modules 'image)
(erc-update-modules)

(erc-services-enable)
(erc-autojoin-enable)

(setq erc-autojoin-channels-alist
      '(("thoughtbot" "#general")
        ("freenode.net" "#thoughtbot" "#emacs" )))

(setq erc-keywords '("\\NYC\\b"
                     "\\nyc\\b"
                     "\\BSD\\b"
                     "\\metis\\b"
                     "\\@all\\b"
                     "\\pr\\b"
                     "\\PR\\b"
                     "\\:statue_of_liberty:\\b"
                     "\\emacs\\b"
                     "\\lisp\\b"
                     "\\haskell\\b"
                     "\\caml\\b"
                     "\\scheme\\b"
                     "\\erlang\\b"))

(setq ercn-notify-rules
      '((current-nick . all)
        (keyword . all)))

(add-hook 'ercn-notify 'do-notify)

(defun hrs/erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (cond
   ((get-buffer "irc.freenode.net:6667")
    (erc-track-switch-buffer 1))
   (t
    (erc :server "irc.freenode.net" :port 6667 :nick "hrs")
    (erc-ssl :server "thoughtbot.irc.slack.com" :port 6667 :nick "harry"))))
