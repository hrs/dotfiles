(defun start-irc ()
  (interactive)
  (erc :server "irc.freenode.net"
       :full-name "HRS"
       :nick "hrs"
       :port 6667
       :password (read-passwd "Enter IRC password: ")))
