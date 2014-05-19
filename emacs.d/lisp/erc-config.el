(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net"
         "#emacs"
         "#clojure"
         "#lisp")))

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun hrs/erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
    (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start erc? ")
      (let ((nick-password (hrs/read-authinfo "password" "irc.freenode.net" "6667")))
        (erc :server "irc.freenode.net"
             :port 6667
             :nick "hrs"
             :password nick-password)))))
