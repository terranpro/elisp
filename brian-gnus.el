;; Gnus Mail and News Reader
;(setq load-path (cons (expand-file-name "~/elisp/foreign/gnus/lisp") load-path))
(push "~/elisp/foreign/gnus/lisp" load-path)
(pp load-path)
(require 'brian-minimal)
(pp load-path)
(require 'brian-config)
(require 'brian-themes)
(require 'gnus-load)

(if (featurep 'xemacs)
    (add-to-list 'Info-directory-list "~/code/gnus/texi/")
  (add-to-list 'Info-default-directory-list "~/code/gnus/texi/"))

;; (setq gnus-select-method '(nntp ""))

;; (setq gnus-select-method 
;;       '(nnimap "capp.snu.ac.kr"
;; 	       (nnimap-address "capp.snu.ac.kr")
;; 	       (nnimap-stream network)
;; 	       (nnimap-streaming t))	; no special config
;; )

;; (setq gnus-select-method 
;;       '(nnimap "pop3.samsung.com"
;; 	       (nnimap-address "pop3.samsung.com")
;; 	       (nnimap-stream network)
;; 	       (nnimap-streaming t))	; no special config
;; )

(setq user-full-name "Brian Fransioli")
(setq user-mail-address "br.fransioli@samsung.com")

(defun pw-from-authinfo (popserver)
  (require 'nntp)
  (require 'netrc)
  (let* ((x (netrc-parse nntp-authinfo-file))
         (item (netrc-machine x popserver))
         (pw (netrc-get item "password")))
    pw)) 

;; (setq gnus-secondary-select-methods '((nnml "")))
(setq gnus-secondary-select-methods nil)
(setq gnus-select-method '(nnml ""))
(setq gnus-verbose 10)
(setq mail-sources
      `((pop
	 :user "br.fransioli"
	 :password ,(pw-from-authinfo "pop3.samsung.com")
	 :server "pop3.samsung.com"
	 :port 995
	 :stream ssl
	 :leave t
	 )))
;; (setq gnus-select-method '(nntp "news.tweaknews.eu"
;; 				(nntp-port-number 563)
;; 				(nntp-open-connection-function nntp-open-ssl-stream)
;; 				(nntp-address "news.tweaknews.eu")))

;; (setq mail-sources

;;       '((pop
;; 	 :user "br.fransioli"
;; 	 :server "pop3.samsung.com"
;; 	 )))

(setq gnus-agent-max-fetch-size 10000000)

;; Ignore those mother fucking gnutls.c errors because #1 emacs thinks
;; i care and is wrong #2 work mailserver is suck garbage
(add-hook 'gnus-group-mode-hook 
	  #'(lambda ()
	      (define-key gnus-group-mode-map "g"
		#'(lambda () 
		    (interactive) 
		    (flet ((yes-or-no-p (&rest args) t)
			   (y-or-n-p (&rest args) t))
		      (ignore-errors
			(call-interactively 'gnus-group-get-new-news)))))))

;; required to use korean regex splits
(setq nnmail-mail-splitting-decodes t)

(let ((build-failed-rx (rx (zero-or-more any)
			   (or "build failed"
			       " Fails")))
      (offical-release-rx (rx "Official" (zero-or-more any) "Binary-Release"))
      (daily-release-rx (rx "Daily" (zero-or-more any) "Binary-Release"))
      (plm-ux (rx "PLM" (zero-or-more any) "UX" (zero-or-more any)))
      (forum-updates-rx (rx (or "게시물등록" "게시물수정" "Post"))))
  (setq nnmail-split-fancy 
	`(| ("from" "slpsystem.m@samsung.com"
		  (| ("subject" ,build-failed-rx "samsung.slp.build.fails")
		     ("subject" "Creating.*Daily.*image" 
		      "samsung.slp.build.daily")
		     ("subject" "Binary-Release"
		      (| ("subject" "Auto" "samsung.slp.build.misc")
			 "samsung.slp.build.release"))
		     "samsung.slp.build.misc"))

	    ("from" "download@nuance.com" "nuance.files")
	    ("from" "welstorymall.com" "samsung.ads.welstory")
	    ("from" "mailmaster@mail.sec.co.kr"
	     (| ("subject" "패밀리넷" "samsung.ads.familynet")))
	    ("from" "패밀리넷" "samsung.ads.familynet")

	    ("subject" "결재.*통보" "samsung.approval")

	    ("subject"
	     "Binary-Release" 
	     (| ("subject" "Official" "samsung.slp.build.release")
		("subject" "Auto" "samsung.slp.build.misc")
		("subject" "Daily" "samsung.slp.build.daily")
		"samsung.slp.build.misc"))

	    ("subject" "Change in magnolia.*" "samsung.slp.gerrit")
	    ("subject" ,forum-updates-rx "samsung.slp.forum-updates")
	    ("subject" "비업무" "samsung.notwork")
	    ("subject" ,plm-ux "samsung.plm.ux")
	    "samsung.misc"))
  (setq nnmail-split-methods 'nnmail-split-fancy))

;; Modified from default to put Date header first
;; it should at least be before all the possible giant fucking To and CC lists
(setq gnus-sorted-header-list 
      '("^Date:" "^From:" "^Subject:" "^Summary:" "^Keywords:"
	"^Newsgroups:" 	"^Followup-To:" "^To:" "^Cc:" "^Organization:"))

;(setq gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:")

(defvar brian-gnus-toggled-headers nil)
(defun brian-gnus-article-toggle-headers ()
  (interactive)
  (let ((gnus-visible-headers (if brian-gnus-toggled-headers 
				  gnus-visible-headers
				"^Date:\\|^Subject:\\|^From:")))
    (pp gnus-visible-headers)
    (gnus-article-hide-headers)
    (gnus-article-)
    (setq brian-gnus-toggled-headers (not brian-gnus-toggled-headers))))


;; this is the default
;(setq gnus-boring-article-headers '(empty followup-to reply-to))

(setq starttls-use-gnutls t)
(setq starttls-gnutls-program "gnutls-cli")
(setq starttls-extra-arguments '("-p" "995" "--x509certfile" "~/samsung.pem"))

;; (setq gnus-secondary-select-methods
;;       '((nntp "news.tweaknews.eu"
;; 				(nntp-port-number 563)
;; 				(nntp-open-connection-function nntp-open-ssl-stream)
;; 				(nntp-address "news.tweaknews.eu"))
;; 	(nnimap "assem-gmail"
;; 	       (nnimap-address "imap.gmail.com")
;; 	       (nnimap-server-port 993)
;; 	       (nnimap-stream ssl))

;; 	(nnimap "terranpro-gmail"
;; 	       (nnimap-address "imap.gmail.com")
;; 	       (nnimap-server-port 993)
;; 	       (nnimap-stream ssl))

;; ;;	(nntp "nntp.aioe.org")
;; 	))

;; window config
(gnus-add-configuration
 '(article
   (horizontal 1.0
	       (vertical 0.35
			 (group 1.0))
	       (vertical 1.0
			 (summary 0.45 point)
			 (article 1.0)))))
(gnus-add-configuration
 '(summary
   (horizontal 1.0
	       (vertical 0.35
			 (group 1.0))
	       (vertical 1.0
			 (summary 1.0 point)))))


(setq gnus-posting-styles
      '(((header "to" "assem@terranpro.org")
         (address "assem@terranpro.org"))

	((header "cc" "assem@terranpro.org")
         (address "assem@terranpro.org"))

	((header "to" "terranpro@capp.snu.ac.kr")
         (address "terranpro@capp.snu.ac.kr"))


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
         (address "bfransioli3@gatech.edu"))

	((header "to" "br.fransioli@samsung.com")
	 (address "br.fransioli@samsung.com"))
	((header "cc" "br.fransioli@samsung.com")
	 (address "br.fransioli@samsung.com"))
))


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
		 ((string-match "br.fransioli@samsung.com" from) "samsung")
		 ;; Add more string-match lines for your email accounts
		 ((string-match "terranpro@capp.snu.ac.kr" from)"capp"))))
	  (message (format "%s" account))
	  (setq message-sendmail-extra-arguments (list "-a" account))))))

(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'cg-feed-msmtp)


; more gnus customizations
(setq gnus-treat-display-smileys t)

(setq gnus-asynchronous t)
(setq gnus-agent-max-fetch-size 10000000)
(setq gnus-fetch-old-headers nil)

;; TEMP DISABLE~
;; ; w3m-el
;; (setq load-path (cons (expand-file-name "~/code/emacs-w3m") load-path))

;; (if (featurep 'xemacs)
;;     (add-to-list 'Info-directory-list (expand-file-name "~/code/emacs-w3m/doc"))
;;   (add-to-list 'Info-default-directory-list 
;; 	       (expand-file-name "~/code/emacs-w3m/doc")))

;; ;; (setq Info-directory-list (append '("/home/terranpro/code/emacs-w3m/doc") 
;; ;; 				  Info-directory-list))
;; ;; (setq Info-directory-list (append  Info-directory-list
;; ;; 				   '("/home/terranpro/code/emacs-w3m/doc")))

;; (require 'w3m-load)
;; ;(require 'mime-w3m)
;; (setq mm-text-html-renderer 'w3m)
;; (setq mm-inline-text-html-with-images t)
;; (setq mm-w3m-safe-url-regexp nil)

;; ;; BBDB
(require 'bbdb-loaddefs "~/elisp/foreign/bbdb/lisp/bbdb-loaddefs.el")
;; (if (featurep 'xemacs)
;;     (add-to-list 'Info-directory-list "~/bbdb/doc/")
;;   (add-to-list 'Info-default-directory-list "~/code/bbdb/doc/"))

 	
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
	  (address "assem@gmail.com")))
	("br.fransioli-samsung"
	 (charset . euc-kr)
	 (display . 100)
	 (posting-style 
	  (name "Brian Fransioli")
	  (address "br.fransioli@samsung.com")))))

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

