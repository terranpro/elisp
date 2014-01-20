;;; dlog.el --- 
;;
;; Copyright (C) 2013 Brian Fransioli
;;
;; Author: Brian Fransioli <assem@terranpro.org>
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
;; Tool for viewing and manipulating dlog files (Tizen logging style)

;;; Code:

(defvar dlog-line-format)
;; "format: threadtime"
(setq dlog-line-format 
  (vector '("date" 7 nil)
	  '("time" 16 dlog-sort-by-time :right-align t)
	  '("pid" 5 nil :right-align t)
	  '("gid" 5 nil :right-align t)
	  '("type" 5 nil)
	  '("tag" 16 nil)
	  '("file" 24 nil :right-align t)
	  '("msg" 0 nil)))

(defvar dlog-line-format-alist nil)
(setq dlog-line-format-alist
      '((date 0 ("date" 12 nil))
	(time 1 ("time" 14 dlog-sort-by-time :right-align t))
	(pid  2 ("pid" 6 nil :right-align t))
	(gid  3 ("gid" 6 nil :right-align t))
	(type 4 ("type" 3 nil))
	(tag  5 ("tag" 12 nil))
	(file 6 ("file" 28 nil :right-align t))
	(msg  7 ("msg" 0 nil))))

(assoc 'date  dlog-line-format-alist)
(loop for elem in dlog-line-format-alist
      for sym = (car elem)
      for fmt = (caddr elem)
      collect (vector fmt))

(last (caddr (nth (second (assoc 'date  dlog-line-format-alist))
		  dlog-line-format-alist)))

(defvar-local dlog-hidden-tags nil)

(defvar-local dlog-highlighted-tags nil)

(defvar dlog-tag-faces '(hi-blue hi-green hi-pink hi-yellow))
(defvar-local dlog-tag-face-idx 0)

(defvar-local dlog-filter-time nil)

(defun dlog-sort-by-time (entry1 entry2)
  (let* ((id1 (first entry1))
	 (tab-list-entry1 (second entry1))
	 (id2 (first entry2))
	 (tab-list-entry2 (second entry2))
	 (time1 (aref tab-list-entry1 0))
	 (time2 (aref tab-list-entry2 0)))
    (if (string= time1 time2)
	(< id1 id2)
      (string< time1 time2))))

(define-derived-mode dlog-mode tabulated-list-mode "DLog"
  (setq tabulated-list-format dlog-line-format)
  (setq tabulated-list-padding 0)
  (setq tabulated-list-printer 'dlog-tabulated-list-print-entry)
  (setq tabulated-list-revert-hook 'dlog-show-all)
  (tabulated-list-init-header)
  (setq tabulated-list-entries (dlog-create-tabulated-list))
  (tabulated-list-print t))

(defun dlog-create-tabulated-list ()
  (let ((id 0)
	(lines (split-string 
		(buffer-substring (point-min)
				  (point-max))
		"\n"
		t)))
    (mapcar #'(lambda (line)
		(setq id (1+ id))
		(dlog-create-tabulated-list-entry id line)) 
	    lines)))

(defun dlog-create-tabulated-list-entry (id txt)
  (let* ((tokens (dlog-tokenize-line txt)))
    (list id tokens)))

(defun dlog-tokenize-line (txt)
  (let* ((tokens (split-string txt (rx (any " ")) t "[:]"))
	 (msgidx (second (assoc 'msg dlog-line-format-alist)))
	 (msg (or (mapconcat 'identity (nthcdr msgidx tokens) " ")
		  "")))
    (concatenate 'vector 
		 (loop for i from 0 to (1- msgidx)
		       collect (or (nth i tokens)
				   ""))
		 (list msg))))

(dlog-tokenize-line "12-05 09:21:32.509   870   870 D RESOURCED: proc-noti.c: safe_write_int(178) > [safe_write_int,178] Response is not needed")

(dlog-tokenize-line "12-05 09:21:32.739 18109 18109 E NOTIFICATION: ")

(dlog-create-tabulated-list-entry 1 "12-05 09:21:32.509   870   870 D RESOURCED: proc-noti.c: safe_write_int(178) > [safe_write_int,178] Response is not needed")
(boundp 'dlog-hidden-tags)

(defun dlog-tabulated-list-print-entry (id entry)
  (let* ((time (dlog-list-entry-get-time entry))
	 (tag (dlog-list-entry-get-tag entry))
	 (sanitized-tag (dlog-list-entry-sanitize-tag tag)))
    (unless (or (member sanitized-tag dlog-hidden-tags)
		(dlog-time-check-filtered time))
      (tabulated-list-print-entry id entry))))

(defun dlog-time-check-filtered (time)
  (when dlog-filter-time
    (let ((start (first dlog-filter-time))
	  (end (second dlog-filter-time)))
      (or (string< time start)
	  (string< end time)))))

(defun dlog-list-entry-get-time (entry)
  (dlog-list-entry-get-sym entry 'time))

(defun dlog-list-entry-get-date (entry)
  (dlog-list-entry-get-sym entry 'date))

(defun dlog-list-entry-get-tag (entry)
  (dlog-list-entry-get-sym entry 'tag))

(defun dlog-list-entry-get-msg (entry)
  (mapconcat 'identity (cdr (cdr (cdr entry))) " "))

(defun dlog-list-entry-get-sym (entry sym)
  (let ((n (second (assoc sym dlog-line-format-alist))))
    (dlog-list-entry-get-n entry n)))

(defun dlog-list-entry-get-n (entry n)
  (cond ((vectorp entry)
	 (aref entry n))
	((listp entry)
	 (nth n entry))
	(t nil)))

(defun dlog-list-entry-sanitize-tag (rawtag)
  (car (split-string rawtag (rx (any "(" ":")) t)))

(defun dlog-hide-tag (&optional tag)
  (interactive)
  (let ((tags (dlog-get-unhidden-tags)))
    (unless tag
      (setq tag
	    (ido-completing-read 
	     "Tag: "
	     tags))))
  (setq dlog-hidden-tags (append (list tag) dlog-hidden-tags))
  (dlog-refresh))

(defun dlog-unhide-tag (&optional tag)
  (interactive)
  (unless tag
    (setq tag (ido-completing-read 
	       "Tag: " 
	       (dlog-get-hidden-tags))))
  (setq dlog-hidden-tags (remove tag dlog-hidden-tags))
  (dlog-refresh))

(defalias 'dlog-show-tag 'dlog-unhide-tag)

(defun dlog-get-all-tags ()
  (remove-if 'null
   (delete-dups 
    (mapcar
     #'(lambda (entry)
	 (dlog-list-entry-sanitize-tag 
	  (dlog-list-entry-get-tag (second entry))))
     tabulated-list-entries))))

(defun dlog-get-hidden-tags ()
  dlog-hidden-tags)

(defun dlog-get-unhidden-tags ()
  (remove-if #'(lambda (tg)
		 (member tg dlog-hidden-tags))
	     (dlog-get-all-tags)))

(defun dlog-get-unhighlighted-tags ()
  (remove-if #'(lambda (tg)
		 (member tg dlog-highlighted-tags))
	     (dlog-get-all-tags)))

(defun dlog-show-all ()
  (interactive)
  (setq dlog-hidden-tags nil)
  (dlog-refresh))

(defun dlog-hide-all ()
  (interactive)
  (setq dlog-hidden-tags (dlog-get-all-tags))
  (dlog-refresh))

(defun dlog-refresh ()
  (interactive)
  (tabulated-list-print t))

(defun dlog-highlight-pid (&optional pid)
)

(defun dlog-highlight-tag (&optional tag)
  (interactive)
  (let ((tags (dlog-get-unhighlighted-tags))
	(curface (nth dlog-tag-face-idx dlog-tag-faces)))
    (unless tag
      (setq tag (ido-completing-read 
		 "Tag: "
		 tags)))

    (when (not (member tag dlog-highlighted-tags))
      (setq dlog-tag-face-idx (1+ dlog-tag-face-idx))
      (if (= dlog-tag-face-idx (length dlog-tag-faces))
	  (setq dlog-tag-face-idx 0))
      (highlight-lines-matching-regexp 
       (dlog-hilight-tag-regexp tag)
       curface)
      (setq dlog-highlighted-tags 
	    (append (list tag)
		    dlog-highlighted-tags))
      t)))

(defun dlog-hilight-tag-regexp (tag)
  ;; (mapconcat 'identity
  ;; 	     (list (mapconcat 'identity 
  ;; 			      (loop for i from 0 to 
  ;; 				    (1- (second (assoc 'tag dlog-line-format-alist)))
  ;; 				    collect ".+[ ]+")
  ;; 			      "")
  ;; 		   tag)
  ;; 	     "")
  (concat "[0-9]+[ ]+[0-9]+[ ]+[A-Za-z][ ]+" tag "[ ]+"))

(defun dlog-unhilight-tag (&optional tag)
  (interactive)
  (let ((tags dlog-highlighted-tags))
    (unless tag
      (setq tag (ido-completing-read 
		 "Tag: "
		 tags)))
    (when (member tag dlog-highlighted-tags)
      (hi-lock-unface-buffer tag)
      (setq dlog-tag-face-idx
	    (max (1- dlog-tag-face-idx) 0))
      (setq dlog-highlighted-tags
	    (remove tag dlog-highlighted-tags)))))

(defun dlog-filter-time ()
  (interactive)
  (let ((start (ido-completing-read 
		"Beg Time: "
		(dlog-get-times)))
	(end (ido-completing-read
	      "End Time: "
	      (dlog-get-times))))
    (setq dlog-filter-time (if (string< start end)
			       (list start end)
			     (list end start)))
    (dlog-refresh)))

(defun dlog-clear-time-filter ()
  (interactive)
  (setq dlog-filter-time nil)
  (dlog-refresh))

(defun dlog-get-times ()
  (delete-dups 
   (mapcar
    #'(lambda (entry)
	(dlog-list-entry-get-time (second entry)))
    tabulated-list-entries)))

(defun dlog-save-to-file (&optional file)
  "Save the currently displayed dlog region to a separate file."
  (interactive "FFile: ")
  (write-file file t))

(dlog-list-entry-get-time (second (dlog-create-tabulated-list-entry 1 "10:44:36.075 12-14-2013 CLOWN(D): START!!!! AGAIN!!")))

(provide 'dlog)

;;; dlog.el ends here
