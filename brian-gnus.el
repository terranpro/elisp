;; Gnus Mail and News Reader
(setq load-path (cons (expand-file-name "~/code/gnus/lisp") load-path))
(require 'gnus-load)

(require 'info)
(if (featurep 'xemacs)
    (add-to-list 'Info-directory-list "~/code/gnus/texi/")
  (add-to-list 'Info-default-directory-list "~/code/gnus/texi/"))


(setq gnus-select-method 
      '(nnimap "capp.snu.ac.kr") ; no special config
)

(setq gnus-secondary-select-methods
      '((nnimap "assem-gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl))

	(nnimap "terranpro-gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl))
	
	(nntp "nntp.aioe.org"))
)

(setq gnus-posting-styles
      '(((header "to" "assem@terranpro.org")
         (address "assem@terranpro.org"))
	((header "to" "terranpro@capp.snu.ac.kr")
         (address "terranpro@capp.snu.ac.kr"))
	((header "cc" "assem@terranpro.org")
         (address "assem@terranpro.org"))
	((header "cc" "terranpro@capp.snu.ac.kr")
         (address "terranpro@capp.snu.ac.kr"))
	((header "to" "members@capp.snu.ac.kr")
         (address "terranpro@capp.snu.ac.kr"))
	((header "cc" "members@capp.snu.ac.kr")
         (address "terranpro@capp.snu.ac.kr"))
	((header "to" "terranpro@gmail.com")
	 (address "terranpro@gmail.com"))
	((header "cc" "terranpro@gmail.com")
         (address "terranpro@gmail.com"))
	((header "to" "bfransioli3@gatech.edu")
         (address "bfransioli3@gatech.edu")))
)


;; SMTP Stuff for multi accounting
(setq message-send-mail-function 'message-send-mail-with-sendmail)
;; we substitute sendmail with msmtp
(setq sendmail-program "msmtp")

;; Choose account label to feed msmtp -a option based on From header in 
;; Message buffer
;; This function must be added to message-send-mail-hook for on-the-fly change 
;; of From address
;; before sending message since message-send-mail-hook is processed right 
;; before sending message.

(defun cg-feed-msmtp ()
  (if (message-mail-p)
      (save-excursion
	(let* ((from
		(save-restriction
		  (message-narrow-to-headers)
		  (message-fetch-field "from")))
	       (account
		(cond
		 ;; I use email address as account label in ~/.msmtprc
		 ((string-match "assem@terranpro.org" from)"assem@terranpro.org")
		 ((string-match "terranpro@gmail.com" from)"terranpro@gmail.com")

		 ((string-match "bfransioli3@gatech.edu" from)"terranpro@gmail.com")
		 ;; Add more string-match lines for your email accounts
		 ((string-match "terranpro@capp.snu.ac.kr" from)"capp"))))
	  (message (format "%s" account))
	  (setq message-sendmail-extra-arguments (list "-a" account))))))

(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'cg-feed-msmtp)


