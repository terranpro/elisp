;;; Modified version due to bug when invoking mo-git-blame-file in
;;; Emacs 24.2.50.2
;; file-name was "DaliPTEWidget.cpp" but it looks like it expected it
;; to be /full/path/to/DaliPTEWidget.cpp aka root + DaliPTEWidget.cpp
 
(defun dir-locals-collect-variables (class-variables root variables)
  "Collect entries from CLASS-VARIABLES into VARIABLES.
ROOT is the root directory of the project.
Return the new variables list."
  (let* ((file-name (file-truename (buffer-file-name)))
	 (sub-file-name (if file-name
                            ;; FIXME: Why not use file-relative-name?
			    (substring file-name (length root)))))
    (condition-case err
        (dolist (entry class-variables variables)
          (let ((key (car entry)))
            (cond
             ((stringp key)
              ;; Don't include this in the previous condition, because we
              ;; want to filter all strings before the next condition.
              (when (and sub-file-name
                         (>= (length sub-file-name) (length key))
                         (string-prefix-p key sub-file-name))
                (setq variables (dir-locals-collect-variables
                                 (cdr entry) root variables))))
             ((or (not key)
                  (derived-mode-p key))
              (let* ((alist (cdr entry))
                     (subdirs (assq 'subdirs alist)))
                (if (or (not subdirs)
                        (progn
                          (setq alist (delq subdirs alist))
                          (cdr-safe subdirs))
                        ;; TODO someone might want to extend this to allow
                        ;; integer values for subdir, where N means
                        ;; variables apply to this directory and N levels
                        ;; below it (0 == nil).
                        (equal root default-directory))
                    (setq variables (dir-locals-collect-mode-variables
                                     alist variables))))))))
      (error
       ;; The file's content might be invalid (e.g. have a merge conflict), but
       ;; that shouldn't prevent the user from opening the file.
       (message ".dir-locals error: %s" (error-message-string err))
       nil))))

;; stackoverflow get isearch-face highlighting w/dired-do-search
(defvar dired-do-search-overlay nil)
(defvar dired-do-search-region nil)
(defun dired-do-search (regexp)
  "Search through all marked files for a match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue]."
  (interactive "sSearch marked files (regexp): ")
  (setq 
   tags-loop-operate `(progn 
			(if dired-do-search-overlay
			    (delete-overlay dired-do-search-overlay))
			(setq dired-do-search-overlay 
			      (make-overlay (car dired-do-search-region)
					    (cadr dired-do-search-region)))
			(overlay-put dired-do-search-overlay
				     'face isearch-face)
			(overlay-put dired-do-search-overlay
				     'priority 1001)	
			nil)
   tags-loop-scan `(progn
		     (if (re-search-forward ',regexp nil t)
			 (setq dired-do-search-region 
			       (list (match-beginning 0)
				     (match-end 0)))
		       (if dired-do-search-overlay 
			   (delete-overlay dired-do-search-overlay))
		       nil)))
  (tags-loop-continue 
   (or '(dired-get-marked-files nil nil 'dired-nondirectory-p) t)))


;; stackoverflow, remove org features + reload dev version 
(mapc
 #'(lambda (f) (and (featurep f) (unload-feature f t)))
 (loop for file-syms in load-history
       for prov = (assoc 'provide file-syms)
       with features
       if (and prov (string-match "^brian-t" (symbol-name (cdr prov)))) 
       collect (cdr prov) into features
       finally return features))


;; increment number at point
(defun brian-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))


;; ace-jump within N lines!
(defun brian-ace-jump-to-char-within-N-lines (&optional n)
  (interactive "p")
  (let* ((N (or n 1))
	 (query-char (read-char "Query Char:"))
	 (start (save-excursion
		  (if (= N 1)
		      (point-at-bol)
		    (forward-line (- N))
		    (point))))
	 (stop (save-excursion 
		 (if (= N 1)
		     (point-at-eol)
		   (forward-line (1+ N))
		   (point)))))
    (unwind-protect
	(condition-case err 
	    (progn
	      (narrow-to-region start stop)
	      (ace-jump-char-mode query-char))
	  (error 
	   (message (error-message-string err))))
      (widen))))



