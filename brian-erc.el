(require 'erc)
(require 'tls)

(setq erc-user-full-name "assem")
(setq erc-server-auto-reconnect nil)

(defun brian-irc ()
  (interactive)
  (let ((tls-program-copy tls-program))
    (setq tls-program 
	  '("gnutls-cli --priority secure256 --x509cafile /etc/ssl/certs/ca-certificates.crt --x509certfile /home/terranpro/.weechat/ssl/nick.pem -p %p %h"
	    "gnutls-cli --insecure -p %p %h" "gnutls-cli --insecure -p %p %h --protocols ssl3" 
	    "openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))
    (erc-tls :server "127.0.0.1" :port 7000
	     :nick "terranpro")

    ;; (erc-tls :server "roddenberry.freenode.net" :port 7000
    ;; 	     :nick "terranpro")
    (setq tls-program tls-program-copy)))
