(require 'tls)

(defun hrs/erc-active-p ()
  "Return truthy if ERC is currently running."
  (get-buffer "irc.freenode.net:6667"))

(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net"
         "#emacs"
         "#clojure"
         "#lisp")
        ("thoughtbot.irc.slack.com"
         "#general")))

(erc-track-mode t)
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE"))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

(defun hrs/erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (hrs/erc-active-p)
    (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start erc? ")
      (let ((freenode-password (hrs/read-authinfo "password" "irc.freenode.net" "6667"))
            (thoughtbot-password (hrs/read-authinfo "password" "thoughtbot.irc.slack.com" "6667")))
        (erc :server "irc.freenode.net"
             :port 6667
             :nick "hrs"
             :password freenode-password)
        (erc-tls :server "thoughtbot.irc.slack.com"
                 :port 6667
                 :nick "harry"
                 :password thoughtbot-password
                 :full-name "Harry Schwartz")))))
