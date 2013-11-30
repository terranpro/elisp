;; StackOverflow Answer for Question:
;; Nice Work Brian ! :)
;; http://tinyurl.com/7epdvlz
(defun brian-comment-offset (langelem)
  (save-excursion
    (back-to-indentation)
    (cond ((re-search-forward (regexp-quote "//+") (point-at-eol) t)
	   '+)
	  (t
	   nil))))


;; Use dtrt-indent to guess indentation!
(add-to-list 'load-path "~/elisp/foreign/dtrt-indent")
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(dtrt-indent-find-file-hook)
;; use smart tabs!
(add-to-list 'load-path "~/elisp/foreign/smart-tabs")
(autoload 'smart-tabs-mode "smart-tabs-mode"
  "Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")
(autoload 'smart-tabs-insinuate "smart-tabs-mode")

;; doxymacs (Doxygen helper)
(add-to-list 'load-path "~/elisp/foreign/doxymacs/lisp")
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(when (featurep 'doxymacs)
  (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook))

;; prevent align-regexp from using tabs
;; Align with spaces only
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

;;; Enabling smart-tabs-mode within language modes:
;; As of version 1.0 of this package, the easiest and preferred way to
;; enable smart-tabs-mode is with the smart-tabs-insinuate function;
;; for example,
;;
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python
		      'ruby 'nxml)

;; Check if the line with the < contains any other
;; definitions/types, if so, base lineup of subsequent lines
;; on the starting column position of the type; 

;; e.g. template<    class X
;; lineup here:      ^

;; otherwise, add a c-basic-offset to the previous indentation
;; this is useful for something like:
;; template<
;; ,,class T
;; ...
;; example where c-basic-offset=2
	  
(defvar brian-c-lineup-template-closebracket 'under 
  "Control the indentation of the closing template bracket, >.
Possible values and consequences:
'under : Align directly under (same column) the opening bracket.
t      : Align at the beginning of the line (or current indentation level.
nil    : Align at the same column of previous types (e.g. col of class T).")

(defun brian-c-lineup-template--closebracket-p ()
  "Return t if the line contains only a template close bracket, >."
  (save-excursion 
    (beginning-of-line)
    ;; Check if this line is empty except for the trailing bracket, >
    (looking-at (rx (zero-or-more blank)
		    ">"
		    (zero-or-more blank)))))

(defun brian-c-lineup-template--pos-to-col (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun brian-c-lineup-template--calc-open-bracket-pos (langelem)
  "Calculate the column position of a template declaration opening bracket."
  (save-excursion 
    (c-with-syntax-table c++-template-syntax-table
      (goto-char (c-langelem-pos langelem))
      (1- (re-search-forward "<" (point-max) 'move)))))

(defun brian-c-lineup-template--calc-indent-offset (ob-pos)
  "Calculate the indentation offset for lining up types given the
opening bracket position, OB-POS."
  (save-excursion
    (c-with-syntax-table c++-template-syntax-table
      (goto-char (1+ ob-pos))
      (cond ((re-search-forward (rx 
				 (or "class"
				     "typename"
				     (one-or-more (not blank))))
				(c-point 'eol)
				'move)
	     (goto-char (match-beginning 0))
	     (current-column))
	    (t
	     (back-to-indentation)
	     (+ c-basic-offset (current-column)))))))

(defun brian-c-lineup-template-args (langelem)
  "Align template arguments and the closing bracket in a semi-custom manner."
  (let* ((ob-pos (brian-c-lineup-template--calc-open-bracket-pos langelem))
	 (ob-col (brian-c-lineup-template--pos-to-col ob-pos))
	 (offset (brian-c-lineup-template--calc-indent-offset ob-pos)))

    ;; Optional check for a line consisting of only a closebracket and
    ;; line it up either at the start of indentation, or underneath the
    ;; column of the opening bracket
    ;;(message (format "%d %d %d" ob-pos ob-col offset))
    (cond ((and brian-c-lineup-template-closebracket
		  (brian-c-lineup-template--closebracket-p))
	     (cond ((eq brian-c-lineup-template-closebracket 'under)
		    (vector ob-col))
		   (t
		    0)))
	    (t
	     (vector offset)))))

;; Customs for C and C++ Programming styles I like
(c-add-style 
 "briancpp" '((c-basic-offset . 2)
	      (c-comment-only-line-offset . 0)
	      (c-cleanup-list . (brace-else-brace
				 brace-elseif-brace
				 brace-catch-brace
				 empty-defun-braces
				 defun-close-semi
				 space-after-funcall
				 ;one-liner-defun
				 compact-empty-funcall
				 list-close-comma
				 ;space-before-funcall
				 ))
 	      (c-hanging-braces-alist
	       . ((brace-list-open)
		  (brace-list-close after)
		  (brace-list-intro before)
		  (brace-entry-open)
		  (statement-cont)
		  (substatement-open after)
		  (block-close . c-snug-do-while)
		  (extern-lang-open after)
		  (namespace-open after)
		  (module-open after)
		  (composition-open after)
		  (inexpr-class-open after)
		  (inexpr-class-close before)))
	      (c-offsets-alist
	       (comment-intro . brian-comment-offset)
	       (defun-open . 0)
	       (defun-close . 0)
	       (inextern-lang . 0)
	       (arglist-close . c-lineup-arglist-close-under-paren)
	       (arglist-cont-nonempty . c-lineup-arglist)
	       (template-args-cont . brian-c-lineup-template-args)
	       (statement-block-intro . +)
	       (substatement-open . 0)
	       (substatement-label . 0)
	       (label . 0)
	       (statement-cont . +)
	       (inline-open . 0)
	       (inline-close . 0)
	       (innamespace . 0))))

;; c electric paren blink matching open paren 
;; delay need this if i use the space-after-funcall mod since the
;; blink severely slowed down response
(setq blink-matching-delay 0.1)

;; just open .h files in c++ mode since all these retard coders who
;; don't know wtf they are doing name them garbage.h - TILT
(setq auto-mode-alist
      (append '(("\\.h\\'" . c++-mode))
	      auto-mode-alist))

(defun brian-c-mode-common-hook ()
  (setq c-hungry-delete-key t)
  (c-toggle-hungry-state 1)
  (c-toggle-auto-newline 1)
  (setq tab-width c-basic-offset)
  (c-set-style "briancpp")
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  (subword-mode 1)
  ;(modify-syntax-entry ?_ "w")
  (set (make-local-variable 'time-stamp-format) 
       "%3a %3b %2d %02H:%02M:%02S %Z %:y")
  (set (make-local-variable 'time-stamp-pattern) 
       "50/Last modified: %%$")
  (add-hook 'write-file-hooks 'time-stamp))

(add-hook 'c-mode-common-hook 'brian-c-mode-common-hook)
(add-hook 'c++-mode-hook (lambda () 
			   (define-key 
			     c++-mode-map 
			     (kbd "RET") 
			     'newline-and-indent)))


;; hacking cc-mode a bit
;; TODO: experimental!
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
;; end EXPERIMENTAL

(provide 'brian-cc-mode)
