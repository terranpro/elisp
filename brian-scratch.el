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



;; Stolen from http://www.emacswiki.org/emacs/MoveRegion
;; Easily move region/lines
(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun move-line-region-up (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-up start end n) (move-line-up n)))

(defun move-line-region-down (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-down start end n) (move-line-down n)))

(global-set-key (kbd "M-p") 'move-line-region-up)
(global-set-key (kbd "M-n") 'move-line-region-down)

;; hacking on c-electric-paren
(defun c-electric-paren (arg)
  "Insert a parenthesis.

If `c-syntactic-indentation' and `c-electric-flag' are both non-nil, the
line is reindented unless a numeric ARG is supplied, or the parenthesis
is inserted inside a literal.

Whitespace between a function name and the parenthesis may get added or
removed; see the variable `c-cleanup-list'.

Also, if `c-electric-flag' and `c-auto-newline' are both non-nil, some
newline cleanups are done if appropriate; see the variable `c-cleanup-list'."
  (interactive "*P")
  (let ((literal (c-save-buffer-state () (c-in-literal)))
	;; shut this up
	(c-echo-syntactic-information-p nil)
	case-fold-search)
    (self-insert-command (prefix-numeric-value arg))

    (if (and (not arg) (not literal))
	(let* (	;; We want to inhibit blinking the paren since this will
	       ;; be most disruptive.  We'll blink it ourselves
	       ;; afterwards.
	       (old-blink-paren blink-paren-function)
	       blink-paren-function)
	  (if (and c-syntactic-indentation c-electric-flag)
	      (indent-according-to-mode))

	  ;; If we're at EOL, check for new-line clean-ups.
	  (when (and c-electric-flag c-auto-newline
		     (looking-at "[ \t]*\\\\?$"))

	    ;; clean up brace-elseif-brace
	    (when
		(and (memq 'brace-elseif-brace c-cleanup-list)
		     (eq last-command-event ?\()
		     (re-search-backward
		      (concat "}"
			      "\\([ \t\n]\\|\\\\\n\\)*"
			      "else"
			      "\\([ \t\n]\\|\\\\\n\\)+"
			      "if"
			      "\\([ \t\n]\\|\\\\\n\\)*"
			      "("
			      "\\=")
		      nil t)
		     (not  (c-save-buffer-state () (c-in-literal))))
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert-and-inherit "} else if ("))

	    ;; clean up brace-catch-brace
	    (when
		(and (memq 'brace-catch-brace c-cleanup-list)
		     (eq last-command-event ?\()
		     (re-search-backward
		      (concat "}"
			      "\\([ \t\n]\\|\\\\\n\\)*"
			      "catch"
			      "\\([ \t\n]\\|\\\\\n\\)*"
			      "("
			      "\\=")
		      nil t)
		     (not  (c-save-buffer-state () (c-in-literal))))
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert-and-inherit "} catch (")))

	  ;; Check for clean-ups at function calls.  These two DON'T need
	  ;; `c-electric-flag' or `c-syntactic-indentation' set.
	  ;; Point is currently just after the inserted paren.
	  (let (beg (end (1- (point))))
	    (cond

	     ;; space-before-funcall clean-up?
	     ((and (memq 'space-before-funcall c-cleanup-list)
		   (eq last-command-event ?\()
		   (save-excursion
		     (backward-char)
		     (skip-chars-backward " \t")
		     (setq beg (point))
		     (and (c-save-buffer-state () (c-on-identifier))
			  (c-)
                          ;; Don't add a space into #define FOO()....
                          (not (and (c-beginning-of-macro)
                                    (c-forward-over-cpp-define-id)
                                    (eq (point) beg))))))
	      (save-excursion
		(delete-region beg end)
		(goto-char beg)
		(insert ?\ )))

	     ((and (memq 'space-after-funcall c-cleanup-list)
		   (eq last-command-event ?\()
		   (save-excursion
		     (backward-char)
		     (skip-chars-backward " \t")
		     (setq beg (point))
		     (pp beg)
		     (and (c-save-buffer-state () 
			    (or
			     (save-excursion (c-backward-token-2)
					     (looking-at c-keywords-regexp))
			     (c-on-identifier)))
                          ;; Don't add a space into #define FOO()....
                          (not (and (c-beginning-of-macro)
                                    (c-forward-over-cpp-define-id)
                                    (eq (point) beg))))))
	      
	      (insert ?\ ))

	     ((eq last-command-event ?\))
	      (progn
	       (when 
		   (save-excursion
		     (and (memq 'space-after-funcall c-cleanup-list)
			 
			  ;(eq (c-beginning-of-statement-1) 'same)
			  (and
			   (c-save-buffer-state () 
			     (search-backward-regexp "(" (point-min) t)
			     (or
			      (save-excursion (c-backward-token-2)
					      (looking-at c-keywords-regexp))
			      (c-on-identifier)))
			       ;; Don't add a space into #define FOO()....
			       (not (and (c-beginning-of-macro)
					 (c-forward-over-cpp-define-id)
					 (eq (point) beg))))))

		 (save-excursion
		   (backward-char)
		   (skip-chars-backward " \t")
		   (delete-region (point) end)
		   (insert ?\ )))

	       (when 
		   (c-save-buffer-state ()
		     (and (memq 'compact-empty-funcall c-cleanup-list)
			  (save-excursion
					;(c-safe (backward-char 2))
			    (pp (point))
			    (c-safe (backward-char 2))
			    (c-beginning-of-statement-1)
			    (c-safe (backward-char))
			    (when (looking-at "([ ]*)")
			      (delete-region
			       (match-beginning 0)
			       (match-end 0))
			      (insert "()")
			      (backward-char 2)
			      (setq end (point))
			      (skip-chars-backward " \t")
			      (setq beg (point))
			      (c-on-identifier)))))
		 (message (format "BOMB! %d %d" beg end))
		 (delete-region beg end)
		 (forward-char 2))))))
	  (and (eq last-input-event ?\))
	       (not executing-kbd-macro)
	       old-blink-paren
	       (funcall old-blink-paren))))))


(defun c-electric-brace (arg)
  "Insert a brace.

If `c-electric-flag' is non-nil, the brace is not inside a literal and a
numeric ARG hasn't been supplied, the command performs several electric
actions:

\(a) If the auto-newline feature is turned on (indicated by \"/la\" on
the mode line) newlines are inserted before and after the brace as
directed by the settings in `c-hanging-braces-alist'.

\(b) Any auto-newlines are indented.  The original line is also
reindented unless `c-syntactic-indentation' is nil.

\(c) If auto-newline is turned on, various newline cleanups based on the
settings of `c-cleanup-list' are done."

  (interactive "*P")
  (let (safepos literal
	;; We want to inhibit blinking the paren since this would be
	;; most disruptive.  We'll blink it ourselves later on.
	(old-blink-paren blink-paren-function)
	blink-paren-function case-fold-search)

    (c-save-buffer-state ()
      (setq safepos (c-safe-position (point) (c-parse-state))
	    literal (c-in-literal safepos)))

    ;; Insert the brace.  Note that expand-abbrev might reindent
    ;; the line here if there's a preceding "else" or something.
    (self-insert-command (prefix-numeric-value arg))

    (when (and c-electric-flag (not literal) (not arg))
      (if (not (looking-at "[ \t]*\\\\?$"))
	  (if c-syntactic-indentation
	      (indent-according-to-mode))

	(let ( ;; shut this up too
	      (c-echo-syntactic-information-p nil)
	      newlines
	      ln-syntax br-syntax syntax) ; Syntactic context of the original line,
			; of the brace itself, of the line the brace ends up on.
	  (c-save-buffer-state ((c-syntactic-indentation-in-macros t)
				(c-auto-newline-analysis t))
	    (setq ln-syntax (c-guess-basic-syntax)))
	  (if c-syntactic-indentation
	      (c-indent-line ln-syntax))

	  (when c-auto-newline
	    (backward-char)
	    (setq br-syntax (c-point-syntax)
		  newlines (c-brace-newlines br-syntax))

	    ;; Insert the BEFORE newline, if wanted, and reindent the newline.
	    (if (and (memq 'before newlines)
		     (> (current-column) (current-indentation)))
		(if c-syntactic-indentation
		    ;; Only a plain newline for now - it's indented
		    ;; after the cleanups when the line has its final
		    ;; appearance.
		    (newline)
		  (c-newline-and-indent)))
	    (forward-char)

	    ;; `syntax' is the syntactic context of the line which ends up
	    ;; with the brace on it.
	    (setq syntax (if (memq 'before newlines) br-syntax ln-syntax))

	    ;; Do all appropriate clean ups
	    (let ((here (point))
		  (pos (- (point-max) (point)))
		  mbeg mend
		  )

	      ;; `}': clean up empty defun braces
	      (when (c-save-buffer-state ()
		      (pp syntax)
		      (and (memq 'empty-defun-braces c-cleanup-list)
			   (eq last-command-event ?\})
			   (c-intersect-lists '(defun-close class-close inline-close)
					      syntax)
			   (progn
			     (forward-char -1)
			     (c-skip-ws-backward)
			     (eq (char-before) ?\{))
			   ;; make sure matching open brace isn't in a comment
			   (not (c-in-literal))))
		(delete-region (point) (1- here))
		(setq here (- (point-max) pos)))
	      (goto-char here)

	      ;; `}': compact to a one-liner defun?
	      (save-match-data
		(when
		    (and (eq last-command-event ?\})
			 (memq 'one-liner-defun c-cleanup-list)
			 (c-intersect-lists '(defun-close) syntax)
			 (c-try-one-liner))
		  (setq here (- (point-max) pos))))

	      ;; `{': clean up brace-else-brace and brace-elseif-brace
	      (when (eq last-command-event ?\{)
		(cond
		 ((and (memq 'brace-else-brace c-cleanup-list)
		       (re-search-backward
			(concat "}"
				"\\([ \t\n]\\|\\\\\n\\)*"
				"else"
				"\\([ \t\n]\\|\\\\\n\\)*"
				"{"
				"\\=")
			nil t))
		  (delete-region (match-beginning 0) (match-end 0))
		  (insert-and-inherit "} else {"))
		 ((and (memq 'brace-elseif-brace c-cleanup-list)
		       (progn
			 (goto-char (1- here))
			 (setq mend (point))
			 (c-skip-ws-backward)
			 (setq mbeg (point))
			 (eq (char-before) ?\)))
		       (zerop (c-save-buffer-state nil (c-backward-token-2 1 t)))
		       (eq (char-after) ?\()
		      ; (progn
			; (setq tmp (point))
			 (re-search-backward
			  (concat "}"
				  "\\([ \t\n]\\|\\\\\n\\)*"
				  "else"
				  "\\([ \t\n]\\|\\\\\n\\)+"
				  "if"
				  "\\([ \t\n]\\|\\\\\n\\)*"
				  "\\=")
			  nil t);)
		       ;(eq (match-end 0) tmp);
			 )
		  (delete-region mbeg mend)
		  (goto-char mbeg)
		  (insert ?\ ))))

	      (goto-char (- (point-max) pos))

	      ;; Indent the line after the cleanups since it might
	      ;; very well indent differently due to them, e.g. if
	      ;; c-indent-one-line-block is used together with the
	      ;; one-liner-defun cleanup.
	      (when c-syntactic-indentation
		(c-indent-line)))

	    ;; does a newline go after the brace?
	    (if (memq 'after newlines)
		(c-newline-and-indent))
	    ))))

    ;; blink the paren
    (and (eq last-command-event ?\})
	 (not executing-kbd-macro)
	 old-blink-paren
	 (save-excursion
	   (c-save-buffer-state nil
	     (c-backward-syntactic-ws safepos))
	   (funcall old-blink-paren)))))
