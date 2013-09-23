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
				 one-liner-defun
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
  (c-set-style "briancpp")
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

(provide 'brian-cc-mode)
