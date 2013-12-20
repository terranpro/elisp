;;; magit-gerrit.el --- 
;;
;; Copyright (C) 2013 Brian Fransioli
;;
;; Author: Brian Fransioli <terranpro@triforce>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Magit plugin for gerrit functionality!

;;; Code:

(require 'magit)
(require 'json)

(eval-when-compile
  (require 'cl-lib))

" gerrit query --format=JSON project:magnolia/framework/uifw/voice/libsttnuance status:open"

(defvar-local magit-gerrit-ssh-creds "br.fransioli@slp-info.sec.samsung.net"
  "Credentials used to execute gerrit commands via ssh of the form ID@Server")

(defun gerrit-command (cmd &rest args)
  (let ((gcmd (concat
	       "-x -p 29418 " 
	       (or magit-gerrit-ssh-creds
		   (error "`magit-gerrit-ssh-creds' must be set!"))
	       " "
	       "gerrit "
	       cmd
	       " "
	       (mapconcat 'identity args " "))))
    ;(message (format "Using cmd: %s" gcmd))
    gcmd))

(defun gerrit-query (prj &optional status)
  (gerrit-command "query" 
		  "--format=JSON"
		  "--all-approvals"
		  "--current-patch-set"
		  (concat "project:" prj)
		  (concat "status:" (or status "open"))))

(defun gerrit-review ())

(defun gerrit-ssh-cmd (cmd &rest args)
  (apply #'call-process
	 (executable-find "ssh") nil nil nil
	 (split-string (apply #'gerrit-command cmd args))))

(defun gerrit-review-abandon (prj rev)
  (gerrit-ssh-cmd "review" "--project" prj "--abandon" rev))

(defun gerrit-review-submit (prj rev)
  (gerrit-ssh-cmd "review" "--project" prj "--submit" rev))

(defun gerrit-code-review (prj rev score)
  (gerrit-ssh-cmd "review" "--project" prj "--code-review" score rev))

(defun gerrit-review-verify (prj rev score)
  (gerrit-ssh-cmd "review" "--project" prj "--verified" score rev))

(gerrit-query "magnolia/framework/uifw/voice/libsttnuance")

(magit-get-remote (magit-get-current-branch))
(magit-get-current-remote)

(rx (zero-or-one ?:) (zero-or-more (any digit)) ?/ 
    (group (not (any "/")))
    (group (one-or-more any)))

(magit-get "remote.origin.url")

(defun magit-gerrit-get-project ()
 (let* ((regx (rx (zero-or-one ?:) (zero-or-more (any digit)) ?/ 
		  (group (not (any "/")))
		  (group (one-or-more any))))     
	(str (magit-get "remote.origin.url"))
	(sstr (car (last (split-string str "//")))))
   (string-match regx sstr)
   (concat (match-string 1 sstr)
	   (match-string 2 sstr))))

(magit-gerrit-get-project)

(defun magit-gerrit-string-trunc (str maxlen)
  (if (> (length str) maxlen)
      (concat (substring str 0 maxlen)
	      "...")
    str))

(defun magit-gerrit-pretty-print-reviewer (name email crdone vrdone)
  (let* ((wid (1- (window-width)))
	 (crstr (propertize (if crdone "C" " ")
			    'face '(magit-log-head-label-bisect-bad bold)))
	 (vrstr (propertize (if vrdone "V" " ")
			    'face '(magit-log-head-label-bisect-good bold)))
	 (namestr (propertize name 'face' magit-diff-add))
	 (emailstr (propertize email 'face 'change-diff-del)))
    (format "%s %s\t%s (%s)" crstr vrstr namestr emailstr)))

(defun magit-gerrit-pretty-print-review (num subj owner-name)
  (let* ((wid (1- (window-width)))
	 (numstr (propertize num 'face 'magit-log-sha1))
	 (nlen (length numstr))
	 (olen (length owner-name))
	 (authmaxlen (/ wid 4))
	 (subjmaxlen (/ wid 2))

	 (author (propertize (magit-gerrit-string-trunc owner-name authmaxlen)
			     'face 'magit-log-author))
	 (subjstr (propertize (magit-gerrit-string-trunc subj subjmaxlen)
			     'face 'magit-log-reflog-label-cherry-pick))
	 (authsubjpadding (make-string 
			   (- wid (+ nlen 1 (length author) (length subjstr)))
			   ? )))
    
    (format "%s\t%s%s%s\n" 
	    numstr subjstr authsubjpadding author)))

(magit-gerrit-pretty-print-review "373589" "Update enGB pro ppen PIL file or die" "Brian Fransioli the fabulous and powerful")

(defun magit-gerrit-wash-approval (approval)
  (let* ((approver (cdr-safe (assoc 'by approval)))
	 (approvname (cdr-safe (assoc 'name approver)))
	 (approvemail (cdr-safe (assoc 'email approver)))
	 (type (cdr-safe (assoc 'type approval)))
	 (verified (string= type "Verified"))
	 (codereview (string= type "Code-Review"))
	 (score (cdr-safe (assoc 'value approval))))
    
    (magit-with-section "Approval" 'approval
      (magit-set-section-info approver)
	(insert (concat
	      (magit-gerrit-pretty-print-reviewer
	       approvname approvemail
	       (when codereview score) 
	       (when verified score))
	      "\n")))))

(defun magit-gerrit-wash-approvals (approvals)
  (mapc #'magit-gerrit-wash-approval approvals))

(defun magit-gerrit-wash-review ()
  (let* ((beg (point))
	 (jobj (json-read))
	 (end (point))
	 (num (cdr-safe (assoc 'number jobj)))
	 (subj (cdr-safe (assoc 'subject jobj)))
	 (owner (cdr-safe (assoc 'owner jobj)))
	 (owner-name (cdr-safe (assoc 'name owner)))
	 (owner-email (cdr-safe (assoc 'email owner)))
	 (patchsets (cdr-safe (assoc 'currentPatchSet jobj)))
	 (approvs (cdr-safe (if (listp patchsets)
				(assoc 'approvals patchsets)
			      (assoc 'approvals (aref patchsets 0))))))
    (if (and beg end)
	(delete-region beg end))
    (when (and num subj owner-name)
     (magit-with-section subj 'review
       (magit-set-section-info num)
       (insert 
	(propertize
	 (magit-gerrit-pretty-print-review num subj owner-name) 
	 'magit-gerrit-jobj
	 jobj))
       (unless (magit-section-hidden (magit-current-section))
	 (magit-gerrit-wash-approvals approvs))
       (add-text-properties beg (point) (list 'magit-gerrit-jobj jobj)))
     t)))

(defun magit-gerrit-wash-reviews ()
  (let ((magit-old-top-section nil))
    (magit-wash-sequence #'magit-gerrit-wash-review)))

(defun magit-gerrit-section (section title washer &rest args)
  (let ((magit-git-executable (executable-find "ssh"))
	(magit-git-standard-options nil))
   (apply #'magit-git-section section title washer 
	  (split-string (car args)))))

(defun magit-gerrit-remote-update (&optional remote)
  nil)

(defun magit-gerrit-review-at-point ()
  (get-text-property (point) 'magit-gerrit-jobj))

(magit-define-command gerrit-view-patchset-diff ()
  "View the Diff for a Patchset"
  (interactive)
  (let ((jobj (magit-gerrit-review-at-point)))
    (when jobj
      (let ((ref (cdr (assoc 'ref (assoc 'currentPatchSet jobj))))
	    (dir default-directory)
	    (buf (get-buffer-create magit-diff-buffer-name)))
	(let ((magit-custom-options (list ref)))
	  (magit-fetch-current)
	  )
	(message (format "Generating Gerrit Patchset for refs %s dir %s"
			 ref dir))
	

	(magit-diff "FETCH_HEAD..FETCH_HEAD~1")


	;; (display-buffer buf)
	;; (with-current-buffer buf
	  
	;;   (let ((default-directory dir))
	    
	;;     ;; (magit-mode-init default-directory
        ;;     ;;            'magit-diff-mode
        ;;     ;;            #'magit-refresh-diff-buffer
        ;;     ;;            "FETCH_HEAD..FETCH_HEAD~1" nil)
	;;     ))

	(magit-refresh-all)
	))))

(magit-define-command gerrit-download-patchset ()
  "Download a Gerrit Review Patchset"
  (interactive)
  (error "Not Yet Implemented!"))

(magit-define-inserter gerrit-reviews ()
  (magit-gerrit-section 'gerrit-reviews
			"Reviews:" 'magit-gerrit-wash-reviews
			(gerrit-query (magit-gerrit-get-project))))

(magit-define-command gerrit-add-reviewer ()
  (interactive)
  "ssh -x -p 29418 br.fransioli@tizen-rsa gerrit set-reviewers --project magnolia/framework/uifw/voice/libttssvox --add email@blah.com"
  
  (apply #'call-process
   (executable-find "ssh") nil nil nil
   (split-string (gerrit-command "set-reviewers"
		    "--project"
		    (magit-gerrit-get-project)
		    "--add"
		    (read-string "Reviewer Name/Email: ")
		    (cdr-safe (assoc 'id (magit-gerrit-review-at-point)))))))

(magit-define-command gerrit-verify-review ()
  "Verify a Gerrit Review"
  (interactive)
  ;(error "Not Yet Implemented!")
  (let ((score (completing-read "Score: "
				    '("-2" "-1" "0" "+1" "+2")
				    nil t
				    "+1"))
	(rev (cdr-safe (assoc
		      'revision
		      (cdr-safe (assoc 'currentPatchSet
				       (magit-gerrit-review-at-point))))))
	(prj (magit-gerrit-get-project)))
    (gerrit-review-verify prj rev score)
    (magit-refresh)))

(magit-define-command gerrit-code-review ()
  "Perform a Gerrit Code Review"
  (interactive)
  ;(error "Not Yet Implemented!")
  (let ((score (completing-read "Score: "
				    '("-2" "-1" "0" "+1" "+2")
				    nil t
				    "+1"))
	(rev (cdr-safe (assoc
		      'revision
		      (cdr-safe (assoc 'currentPatchSet
				       (magit-gerrit-review-at-point))))))
	(prj (magit-gerrit-get-project)))
    (gerrit-code-review prj rev score)
    (magit-refresh)))

(magit-define-command gerrit-submit-review ()
  (interactive)
  "ssh -x -p 29418 br.fransioli@tizen-rsa gerrit review REVISION  -- --project PRJ --submit "
  (apply #'call-process
	 (executable-find "ssh") nil nil nil
	 (split-string 
	  (gerrit-command 
	   "review"
	   (cdr-safe (assoc
		      'revision
		      (cdr-safe (assoc 'currentPatchSet
				       (magit-gerrit-review-at-point)))))
	   "--project"
	   (magit-gerrit-get-project)
	   "--submit")))
  (magit-fetch-current))

(magit-define-command  gerrit-create-review ()
  (interactive)
  (let* ((branch (or (magit-get-current-branch)
                     (error "Don't push a detached head.  That's gross")))
	 
	 (rev (magit-rev-parse (or (magit-commit-at-point)
				   (error "Select a commit for review"))))
	 
	 (branch-merge (and branch (magit-get "branch" branch "merge")))
	 (branch-pub (progn 
		       (string-match (rx "refs/heads" (group (one-or-more any)))
				    branch-merge)
		       (concat "refs/publish" (match-string 1 branch-merge))))
	 (branch-remote (and branch (magit-get "branch" branch "remote")))
	 (origin-remote (and (magit-get "remote" "origin" "url") "origin")))

    (message "Args: %s "
	     (concat rev ":" branch-pub))

    (magit-run-git-async "push" "-v" branch-remote
			 (concat rev ":" branch-pub))))

(magit-define-command  gerrit-abandon-review ()
  (interactive)
  (let ((prj (magit-gerrit-get-project))
	(id (cdr-safe (assoc 'id
		     (magit-gerrit-review-at-point))))
	(rev (cdr-safe (assoc
			'revision
			(cdr-safe (assoc 'currentPatchSet
					 (magit-gerrit-review-at-point)))))))
    (message "Prj: %s Rev: %s Id: %s" prj rev id)
    (gerrit-review-abandon prj rev)
    (magit-refresh)))

(progn
  (magit-key-mode-add-group 'gerrit)
  (magit-key-mode-insert-action 'gerrit "P" "Push Commit For Review"
				'magit-gerrit-create-review)
  (magit-key-mode-insert-action 'gerrit "A" "Add Reviewer"
				'magit-gerrit-add-reviewer)
  (magit-key-mode-insert-action 'gerrit "V" "Verify"
				'magit-gerrit-verify-review)
  (magit-key-mode-insert-action 'gerrit "C" "Code Review"
				'magit-gerrit-code-review)
  (magit-key-mode-insert-action 'gerrit "d" "View Patchset Diff"
				'magit-gerrit-view-patchset-diff)
  (magit-key-mode-insert-action 'gerrit "D" "Download Patchset"
				'magit-gerrit-download-patchset)
  (magit-key-mode-insert-action 'gerrit "S" "Submit Review"
				'magit-gerrit-submit-review)
  (magit-key-mode-insert-action 'gerrit "B" "Abandon Review"
				'magit-gerrit-abandon-review)

  (magit-key-mode-generate 'gerrit))

(defvar magit-gerrit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "T") 'magit-key-mode-popup-gerrit)
    map))

(define-minor-mode magit-gerrit-mode "Gerrit support for Magit"
  :lighter " Gerrit" :require 'magit-topgit :keymap 'magit-gerrit-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (cond
   (magit-gerrit-mode
    (add-hook 'magit-after-insert-stashes-hook 
	      'magit-insert-gerrit-reviews nil t)
    (add-hook 'magit-create-branch-command-hook 
	      'magit-gerrit-create-branch nil t)
    ;(add-hook 'magit-pull-command-hook 'magit-gerrit-pull nil t)
    (add-hook 'magit-remote-update-command-hook 
	      'magit-gerrit-remote-update nil t)
    (add-hook 'magit-push-command-hook 
	      'magit-gerrit-push nil t)
    ;; hide refs for top-bases namespace in any remote
    ;(add-hook 'magit-log-remotes-color-hook 'magit-topgit-get-remote-top-bases-color)

    ;; hide refs in the top-bases namespace, as they're not meant for the user
    ;(add-to-list 'magit-refs-namespaces magit-topgit-ignored-namespace)
    )

   (t
    (remove-hook 'magit-after-insert-stashes-hook 
		 'magit-insert-gerrit-reviews t)
    (remove-hook 'magit-create-branch-command-hook 
		 'magit-gerrit-create-branch t)
    ;(remove-hook 'magit-pull-command-hook 'magit-gerrit-pull t)
    (remove-hook 'magit-remote-update-command-hook 
		 'magit-gerrit-remote-update t)
    (remove-hook 'magit-push-command-hook 
		 'magit-gerrit-push t)
    ;(remove-hook 'magit-log-remotes-color-hook 'magit-gerrit-get-remote-top-bases-color)
    ;(delete magit-topgit-ignored-namespace magit-refs-namespaces)
    ))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(provide 'magit-gerrit)

;;; magit-gerrit.el ends here
