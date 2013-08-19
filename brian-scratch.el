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

;; improved version by OP author's comment feedback
(defun brian-ace-jump-to-char-within-N-lines (&optional n)
  (interactive "p")
  (let* ((N (or n 0))
     (query-char (read-char "Query Char:"))
     (start (save-excursion
	      (forward-line (- N))
	      (point)))
     (stop (save-excursion 
	     (forward-line (1+ N))
	     (point))))
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
		      ;(pp syntax)
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


;; Flymake + popup.el!
(defun flymake-display-err-popup-for-current-line ()
  "Display a menu with errors/warnings for current line if it has errors and/or warnings using popup from popup.el."
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
	 (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
	 (menu-data           (flymake-make-err-menu-data line-no line-err-info-list))
	 (popup-string))
    (if menu-data
	(progn
	  (popup-tip (concat (car menu-data) "\n\n"
			     (mapconcat 'car (car (cdr menu-data)) "\n"))))
      (flymake-log 1 "no errors for line %d" line-no))))

;; Compile Command that Pwns
;; (let ((compile-command (concat "CXXFLAGS=\""
;; 			       (mapconcat 
;; 				'identity 
;; 				(brian-clangcomplete-cflags-make "gcc")
;; 				" ")
;; 			       "\""
;; 			       " make -k "
;; 			       (file-name-nondirectory 
;; 				(file-name-sans-extension
;; 				 (buffer-file-name))))))
;;   (call-interactively (function compile)))


;; Stackoverflow helping cutdown on long, repetitive calls to defface
;; Making this macro was not fscking easy.
(defmacro brian-def-char-face (letter backgrnd foregrnd)
  `(defface ,(intern (concat "brian-char-face-"
			     letter))
     '((((type tty) (class color)) 
       	(:background 
	 ,backgrnd
	 :foreground
	 ,foregrnd))
       (((type tty) (class color)) (:inverse-video t))
       (((class color) (background dark))
	(:foreground
	 ,foregrnd
	 :background
	 ,backgrnd))
       (((class color) (background light))
	(:foreground
	 ,foregrnd
	 :background
	 ,backgrnd))
       (t (:background "gray")))
     ,(concat "Face for marking up " (upcase letter) "'s")))

(let ((letcol-alist '((s . (white black))
		      (t . (black yellow))
		      (u . (green pink)))))
  (assoc 'u letcol-alist)

  ;; (loop for elem in letcol-alist
  ;; 	for l = (format "%s" (car elem))
  ;; 	for back = (format "%s" (cadr elem))
  ;; 	for fore = (format "%s" (caddr elem))
  ;; 	do 
  ;; 	(eval (macroexpand `(brian-def-char-face ,l ,back ,fore))))

)
;; Stackoverflow answer to a question that I never posted because some
;; other guy replied fast with an existing solution :-(
(defun brian-magit-diff-file-at-point (&optional file)
  (interactive)
  (unless file
    (setq file (condition-case err 
		   (magit-diffstat-item-file (magit-current-section))
		 (error
		  (message "No file at point!")
		  nil))))
  (when file
    (magit-for-all-sections 
     #'(lambda (s)
	 (unless (eq magit-top-section s)
	   (magit-section-set-hidden s t))))
    (magit-for-all-sections
     #'(lambda (section)
	 (let ((type (magit-section-type section))
	       (curfile))
	   (if (and (eq type 'diff)
		    (setq curfile (magit-diffstat-item-file section))
		    (string= curfile file))
	       (progn (magit-section-expand-all section)
		      (goto-char (magit-section-beginning section)))))))))

;; Hacking on compilation stuff!
;; Now compilation buffer isn't shown at all unless it's invoked 
;; by grep, find, or gcc errors!
(setq compilation-finish-functions 'brian-compile-finish)
(defun brian-compile-finish (buffer outstr)
  (unless (string-match "finished" outstr)
    (switch-to-buffer-other-window buffer))
  t)

(defadvice compilation-start
  (around inhidbit-display
	  (command &optional mode name-function highlight-regexp)) 
  (if (not (string-match "^\\(find\\|grep\\)" command))
      (flet ((display-buffer)
	     (set-window-point)
	     (goto-char)) 
	(fset 'display-buffer 'ignore)
	(fset 'goto-char 'ignore)
	(fset 'set-window-point 'ignore)
	(save-window-excursion 
	  ad-do-it))
    ad-do-it))

(ad-activate 'compilation-start)

(ad-deactivate 'compilation-start)

(flet ((point-min)
       (goto-char))
  (fset 'point-min 'ignore)
  (fset 'goto-char 'ignore)
  (goto-char (point-min)))

;; json example for compile_commands from cmake
(require 'json)
(let ((ccs (json-read-file "~/code/clang-faces/build/compile_commands.json")))
  (loop for n from 0 to (1- (length ccs))
	for entry = (aref ccs n)
	for file = (cdr (assoc 'file entry))
	for cmd = (cdr (assoc 'command entry))
	collect (list file (cdr (split-string cmd)))))

;; unload symbols with prefix:
(mapc
 #'(lambda (f) (and (featurep f) (unload-feature f t)))
 (loop for file-syms in load-history
       for prov = (assoc 'provide file-syms)
       with features
       if (and prov (string-match "^clang-faces" (symbol-name (cdr prov)))) 
       collect (cdr prov) into features
       finally return features))


;; Hacking org mode to display inline images 
;; StackOverflow related: 
;; http://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it/
(defun brian-org-find-next-inline-image-data (maxpt)
  (let ((re-in-b (rx  "#<<<IMAGE>>>"))
	(re-in-e (rx  "#<<</IMAGE>>>"))
	beg end
	dbeg dend)
    (when (re-search-forward re-in-b maxpt t)
      (setq beg (match-beginning 0))
      (setq dbeg (1+ (match-end 0)))
      (when (re-search-forward re-in-e maxpt t)
	(setq end (match-end 0))
	(setq dend (1- (match-beginning 0)))
	(list beg end
	      (string-make-unibyte
	       (buffer-substring-no-properties dbeg dend)))))))

(defun brian-org-show-inline-image-data (inl-data-l)
"Show an inline image given INL-DATA-L which is a list of three elments: BEG END DATA , containing the beginning point of the inline image data region (including the marker/tag), "
  (let* ((imgdata (third inl-data-l))
	 (beg (first inl-data-l))
	 (end (second inl-data-l))
	 (img (create-image imgdata nil t :width 'imagemagick))
	 (ov (make-overlay beg end)))
    (overlay-put ov 'display img)
    (overlay-put ov 'face 'default)
    (overlay-put ov 'org-image-overlay t)
    (overlay-put ov 'modification-hooks
		 (list 'org-display-inline-remove-overlay))
    (push ov org-inline-image-overlays)))

(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (if (fboundp 'clear-image-cache) (clear-image-cache)))
    (save-excursion
      (save-restriction
	(widen)
	(setq beg (or beg (point-min)) end (or end (point-max)))
	(goto-char beg)

	(let (next-img-data)
	  (while (setq next-img-data (brian-org-find-next-inline-image-data end))
	    (brian-org-show-inline-image-data next-img-data)))

	(goto-char beg)
	(let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
			  (substring (org-image-file-name-regexp) 0 -2)
			  "\\)\\]" (if include-linked "" "\\]")))
	      (case-fold-search t)
	      
	      old file ov img type attrwidth width)
	  (while (and (re-search-forward re end t))
	    (setq old (get-char-property-and-overlay (match-beginning 1)
						     'org-image-overlay)
		  file (expand-file-name
			(concat (or (match-string 3) "") (match-string 4))))
	    (when (image-type-available-p 'imagemagick)
	      (setq attrwidth (if (or (listp org-image-actual-width)
				      (null org-image-actual-width))
				  (save-excursion
				    (save-match-data
				      (when (re-search-backward
					     "#\\+attr.*:width[ \t]+\\([^ ]+\\)"
					     (save-excursion
					       (re-search-backward "^[ \t]*$\\|\\`" nil t)) t)
					(string-to-number (match-string 1))))))
		    width (cond ((eq org-image-actual-width t) nil)
				((null org-image-actual-width) attrwidth)
				((numberp org-image-actual-width)
				 org-image-actual-width)
				((listp org-image-actual-width)
				 (or attrwidth (car org-image-actual-width))))
		    type (if width 'imagemagick)))
	    (when (file-exists-p file)
	      (if (and (car-safe old) refresh)
		  (image-refresh (overlay-get (cdr old) 'display))
		(setq img (save-match-data (create-image file type nil :width width)))
		(when img
		  (setq ov (make-overlay (match-beginning 0) (match-end 0)))
		  (overlay-put ov 'display img)
		  (overlay-put ov 'face 'default)
		  (overlay-put ov 'org-image-overlay t)
		  (overlay-put ov 'modification-hooks
			       (list 'org-display-inline-remove-overlay))
		  (push ov org-inline-image-overlays))))))))))
