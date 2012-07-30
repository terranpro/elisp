(require 'erc)
(require 'tls)
(require 'brian-config)
(require 'brian-ideone)

(setq erc-user-full-name "assem")
(setq erc-server-auto-reconnect t)

;; Limit buffer size (defaults to 30k chars)
(erc-truncate-mode)

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

;; Tiny URL converter for ERC 
(setq brian-detect-long-http-length 60)
(setq brian-make-tinyurl-enabled 0)
(defun brian-make-tinyurl-enable()
  (interactive)
  (if (= brian-make-tinyurl-enabled 0)
      (add-hook 'erc-insert-modify-hook 'brian-detect-long-http)
    (remove-hook 'erc-insert-modify-hook 'brian-detect-long-http))
  (setq brian-make-tinyurl-enabled (not brian-make-tinyurl-enabled)))

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
	 (old-url (car our-args))
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

;; Deniz Dogan erc scroll to bottom workaround
;; Modifications by Brian Fransioli
(setq erc-input-line-position -1)

(defun erc-display-line-1 (string buffer)
  "Display STRING in `erc-mode' BUFFER.
Auxiliary function used in `erc-display-line'.  The line gets filtered to
interpret the control characters.  Then, `erc-insert-pre-hook' gets called.
If `erc-insert-this' is still t, STRING gets inserted into the buffer.
Afterwards, `erc-insert-modify' and `erc-insert-post-hook' get called.
If STRING is nil, the function does nothing."
  (when string
    (with-current-buffer (or buffer (process-buffer erc-server-process))
      (let ((insert-position (or (marker-position erc-insert-marker)
				 (point-max))))
	(let ((string string) ;; FIXME! Can this be removed?
	      (buffer-undo-list t)
	      (inhibit-read-only t))
	  (unless (string-match "\n$" string)
	    (setq string (concat string "\n"))
	    (when (erc-string-invisible-p string)
	      (erc-put-text-properties 0 (length string)
				       '(invisible intangible) string)))
	  (erc-log (concat "erc-display-line: " string
			   (format "(%S)" string) " in buffer "
			   (format "%s" buffer)))
	  (setq erc-insert-this t)
	  (run-hook-with-args 'erc-insert-pre-hook string)
	  (if (null erc-insert-this)
	      ;; Leave erc-insert-this set to t as much as possible.  Fran
	      ;; Litterio <franl> has seen erc-insert-this set to nil while
	      ;; erc-send-pre-hook is running, which should never happen.  This
	      ;; may cure it.
	      (setq erc-insert-this t)
	    (save-excursion ;; to restore point in the new buffer
	      (save-restriction
		(widen)
		(goto-char insert-position)
		(insert-before-markers string)
		;; run insertion hook, with point at restored location
		(save-restriction
		  (narrow-to-region insert-position (point))
		  (run-hooks 'erc-insert-modify-hook)
		  (run-hooks 'erc-insert-post-hook)
		  (when erc-remove-parsed-property
		    (remove-text-properties (point-min) (point-max)
					    '(erc-parsed nil))))))))
	(erc-update-undo-list (- (or (marker-position erc-insert-marker)
				     (point-max))
				 insert-position)))
      (run-hooks 'erc-display-post-hook)))) ;;; only this line was added

(defvar erc-display-post-hook nil
  "New hook!")

(defun damd-erc-display-post-hook ()
  (let ((windows (get-buffer-window-list (current-buffer) nil 'visible)))
    (dolist (w windows)
      (when (>= (point) erc-input-marker)
        (with-selected-window w
          (recenter erc-input-line-position))))))
(add-hook 'erc-display-post-hook 'damd-erc-display-post-hook)

(defun damd-erc-send-post-hook ()
  (when (>= (point) erc-input-marker)
    (goto-char (point-max))
    (widen)
    (recenter erc-input-line-position)))
(add-hook 'erc-send-post-hook 'damd-erc-send-post-hook)

(defun damd-window-configuration-change-hook ()
  (when (and (eq major-mode 'erc-mode)
             (>= (point) erc-input-marker))
    (recenter erc-input-line-position)))
(add-hook 'window-configuration-change-hook 'damd-window-configuration-change-hook)

;; Brian window mods
;; Try to fix end of buffer to always go to the right place for input
(setq scroll-conservatively 2)
(define-key erc-mode-map (kbd "M->")
  '(lambda ()
     (interactive)
     (goto-char (point-max))
     (widen)
     (recenter erc-input-line-position)))
;; Fix C-l to recenter at the bottom, correctly.
(define-key erc-mode-map (kbd "C-l")
  '(lambda ()
     (interactive)
     (goto-char (point-max))
     (widen)
     (recenter erc-input-line-position)))

(provide 'brian-erc)
