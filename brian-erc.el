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

    ;; FreeNode SSL
    (erc-tls :server "127.0.0.1" :port 7000
     	     :nick "terranpro")

    ;; HanIRC Server
    ;;(erc :server "127.0.0.1" :port 7001
    ;;:nick "assem")

    ;; (erc-tls :server "roddenberry.freenode.net" :port 7000
    ;; 	     :nick "terranpro")
    (setq tls-program tls-program-copy)))

(defun brian-detect-http (string)
  "Detect the first occurrence of a HTTP URL in a string and return
it or return nil."

  (let ((http-regexp
	 "\\(https?:\\(//\\)\\|\\(\\\\\\)\\)+[\w\d:#@%/;$()~_?\+-=\\\\\.&]*"))
    
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (search-forward-regexp http-regexp (point-max) t)
      (thing-at-point-url-at-point))))

;;(brian-detect-http "asdfjkasdf
;;http://www.clown.com/klasdfjdsafl/sexintheface/retardedbackwards-style.htmls
")

