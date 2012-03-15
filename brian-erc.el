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
     	     :nick "assem")

    ;; HanIRC Server
    ;;(erc :server "127.0.0.1" :port 7001
    ;;:nick "assem")

    ;; (erc-tls :server "roddenberry.freenode.net" :port 7000
    ;; 	     :nick "terranpro")
    (setq tls-program tls-program-copy)))


(setq brian-detect-long-http-length 60)

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

(defun brian-detect-long-http ()
(let ((message (buffer-substring (point-min) (point-max)))
      (url))
  (setq url (brian-detect-http message))
  (when (and (not (string= url nil))
	     (> (length url) brian-detect-long-http-length))
    (brian-make-tinyurl url))))

(defun brian-make-tinyurl-cback (&rest args)
  "Asynchronous callback for brian-make-tinyurl.  Called when the
URL has been retrieved and is open in the current buffer.
Contains the headers and the retrieved data which is just one
line containing the new TinyURL.  Inserts this new URL into the
buffer."
  (let* ((new-url (brian-detect-http 
		   (buffer-substring (point-min) (point-max))))
	 (our-args (cdr args))
	 (old-url (first our-args))
	 (our-buffer (car (cdr our-args))))

    (when (string-match "korea" our-buffer)
      (save-window-excursion 
	(switch-to-buffer our-buffer)
	(erc-send-message new-url)))
    (with-temp-buffer
      (insert new-url)
      (kill-ring-save (point-min) (point-max)))
    (message "Buffer %s Old URL %s => New URL: %s" our-buffer old-url new-url)))

(defun brian-make-tinyurl (url)
  "Asynchronously sends a long url to tinyurl for generation.
Saves the new URL to the kill-ring for easy yank'ing."
  (let* ((tinyurl-stub "http://tinyurl.com/api-create.php?url=")
	 (tinyurl-full (concat tinyurl-stub url))
	 (tinyurl-buffer (buffer-name))
	 (tinyurl-result))
    (url-retrieve tinyurl-full 
		  'brian-make-tinyurl-cback 
		  (list url tinyurl-buffer))))

(add-to-list 'erc-insert-modify-hook 'brian-detect-long-http)
