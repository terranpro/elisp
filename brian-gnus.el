;; Gnus Mail and News Reader
(setq load-path (cons (expand-file-name "~/code/gnus/lisp") load-path))
(require 'gnus-load)
(require 'brian-minimal)
(require 'brian-config)
(require 'brian-themes)

(if (featurep 'xemacs)
    (add-to-list 'Info-directory-list "~/code/gnus/texi/")
  (add-to-list 'Info-default-directory-list "~/code/gnus/texi/"))


(setq gnus-select-method 
      '(nnimap "capp.snu.ac.kr"
	       (nnimap-address "capp.snu.ac.kr")
	       (nnimap-stream network)
	       (nnimap-streaming t))	; no special config
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

;;	(nntp "nntp.aioe.org")
	))

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


; more gnus customizations
(setq gnus-treat-display-smileys t)

(setq gnus-agent-max-fetch-size 10000000)

; w3m-el
(setq load-path (cons (expand-file-name "~/code/emacs-w3m") load-path))

(if (featurep 'xemacs)
    (add-to-list 'Info-directory-list (expand-file-name "~/code/emacs-w3m/doc"))
  (add-to-list 'Info-default-directory-list 
	       (expand-file-name "~/code/emacs-w3m/doc")))

;; (setq Info-directory-list (append '("/home/terranpro/code/emacs-w3m/doc") 
;; 				  Info-directory-list))
;; (setq Info-directory-list (append  Info-directory-list
;; 				   '("/home/terranpro/code/emacs-w3m/doc")))

(require 'w3m-load)
;(require 'mime-w3m)
(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)
(setq mm-w3m-safe-url-regexp nil)

;; BBDB
(require 'bbdb-loaddefs "~/code/bbdb/lisp/bbdb-loaddefs.el")
(if (featurep 'xemacs)
    (add-to-list 'Info-directory-list "~/bbdb/doc/")
  (add-to-list 'Info-default-directory-list "~/code/bbdb/doc/"))

 	
(require 'bbdb)
(bbdb-initialize)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;; TESTING
(setq-default
     gnus-summary-line-format "%U%R%z %(%&user-date;  %-20,20n  %B%s%)\n"
     gnus-user-date-format-alist '(((gnus-seconds-today)
				    . "  %H:%M")
				   ((gnus-seconds-year)
				    . "  %m-%d")
				   (t . "%Y-%m"))
     gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
     gnus-thread-sort-functions '((not gnus-thread-sort-by-number))
     gnus-article-sort-functions '((not gnus-article-sort-by-number))
     gnus-sum-thread-tree-false-root ""
     gnus-sum-thread-tree-indent " "
     gnus-sum-thread-tree-leaf-with-other "├► "
     gnus-sum-thread-tree-root ""
     gnus-sum-thread-tree-single-leaf "╰► "
     gnus-sum-thread-tree-vertical "│")


(setq gnus-parameters
      '(("*" 
	 (display . 250))
	("INBOX" 
	 (charset . euc-kr)
	 (display . 100)
	 (posting-style
	  (name "Brian Fransioli")
	  (address "terranpro@capp.snu.ac.kr")))
	("terranpro-gmail" 
	 (charset . euc-kr)
	 (display . 100)
	 (posting-style
	  (name "Brian Fransioli")
	  (address "terranpro@gmail.com")))
	("assem-gmail" 
	 (charset . euc-kr)
	 (display . 100)
	 (posting-style
	  (name "Brian Fransioli")
	  (address "assem@gmail.com")))))

(add-to-list 'default-frame-alist 
	     '(width . 150))
(add-to-list 'default-frame-alist 
	     '(height . 60))
	      

;; (set-face-font 'default "BatangChe")
;; (set-fontset-font "fontset-default" '(#x1100 . #xffdc)
;; 		  '("BatangChe" . "unicode-bmp"))
;; (set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
;; 		  '("BatangChe" . "unicode-bmp"))

(provide 'brian-gnus)
