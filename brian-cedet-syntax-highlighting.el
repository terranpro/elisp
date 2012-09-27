(require 'brian-cedet)
(require 'cl)

;; elisp function face
(define-semantic-decoration-style semantic-decoration-on-elisp-functions
  "Highlight elisp functions."
  :enabled t)

(defface semantic-decoration-on-elisp-functions-face
  '((((class color) (background dark))
     (:background "#200000"))
    (((class color) (background light))
     (:background "#f0afff")))
  "*Face used to show privately scoped tags in.
Used by the decoration style: `semantic-decoration-on-elisp-functions'."
  :group 'semantic-faces)

(defun semantic-decoration-on-elisp-functions-highlight-default (tag)
  "Highlight TAG as designated to have PRIVATE access.
Use a primary decoration."
  (message "Highlighting elisp funcs!")
  (pp tag)
  (when (semantic-tag-with-position-p tag)
    (let* ((tstart (semantic-tag-start tag))
	     (tend (semantic-tag-end tag)))
	(semantic-decorate-tag tag tstart tend
			       'semantic-decoration-on-elisp-functions-face))))

(defun semantic-decoration-on-elisp-functions-p-default (tag)
  "Return non-nil if TAG has PRIVATE access."
  (and
   (or (eq major-mode 'emacs-lisp-mode)
       nil)
   (semantic-tag-of-class-p tag 'function)))


;; operator#( ) function face
(define-semantic-decoration-style semantic-decoration-on-operator-functions
  "Highlight operator= functions variables."
  :enabled t)

(defface semantic-decoration-on-operator-functions-face
  '((((class color) (background dark))
     (:background "#200000"))
    (((class color) (background light))
     (:background "#f0afff")))
  "*Face used to show privately scoped tags in.
Used by the decoration style: `semantic-decoration-on-operator-functions'."
  :group 'semantic-faces)

(defun semantic-decoration-on-operator-functions-highlight-default (tag)
  "Highlight TAG as designated to have PRIVATE access.
Use a primary decoration."
  (when (semantic-tag-with-position-p tag)
      (let* ((operator-start)
	  (operator-end)
	  (arguments (semantic-tag-function-arguments tag))
	  (args-with-pos 
	   (remove-if 'null (mapcar 'semantic-tag-with-position-p arguments)))
	  (arg-start (if args-with-pos
			 (apply 'min
				(remove-if-not 'integerp
					       (mapcar 'semantic-tag-end
						       (mapcar 'identity arguments))))
		       (semantic-tag-end tag))))
	(goto-char (semantic-tag-start tag))
	(setq operator-end 
	      (search-forward-regexp (regexp-quote 
				      (concat "operator"
					      (semantic-tag-name tag)))
				     (min (semantic-tag-end tag)
					  (point-max))
				     t))
	(setq operator-start (match-beginning 0))
	
	(semantic-decorate-tag tag operator-start operator-end
			       'semantic-decoration-on-operator-functions-face))))

(defun semantic-decoration-on-operator-functions-p-default (tag)
  "Return non-nil if TAG has PRIVATE access."
  (and
   (or (eq major-mode 'c++-mode)
       (eq major-mode 'c-mode))
   (semantic-tag-of-class-p tag 'function)
   (not (null (semantic-tag-get-attribute tag :operator-flag)))))


;; constant variables tooltip face (TODO)
(define-semantic-decoration-style semantic-decoration-on-constant-variables
  "Highlight operator= functions variables."
  :enabled t)

(defface semantic-decoration-on-constant-variables-face
  '((((class color) (background dark))
     (:background "#200000"))
    (((class color) (background light))
     (:background "#f0afff")))
  "*Face used to show privately scoped tags in.
Used by the decoration style: `semantic-decoration-on-constant-variables'."
  :group 'semantic-faces)

(defun semantic-decoration-on-constant-variables-highlight-default2 (tag)
  "Highlight TAG that's a constant variable with information
about its declaration as a tooltip.  Use a primary decoration."
  (interactive)
  (when (and (semantic-tag-with-position-p tag)
	     (string= (upcase (semantic-tag-name tag))
		      (semantic-tag-name tag)))
    (pp (semantic-tag-end tag))
    (message (semantic-format-tag-summarize tag nil nil))
    (let* ((left (or (car-safe (cdr-safe (frame-parameter (selected-frame) 'left)))
			(frame-parameter (selected-frame) 'left)))
	      (top (or (car-safe (cdr-safe (frame-parameter (selected-frame) 'top)))
		       (frame-parameter (selected-frame) 'top)))
	      (edges (window-inside-pixel-edges (selected-window)))
	      (pt )
	   
	      (tfp-left )
	      (tfp-top )
	      (overlay )
	      (tooltip-frame-parameters (append tooltip-frame-parameters nil)))
	 (save-excursion
	   (goto-char (point-min))
	   (pp (regexp-quote (semantic-tag-name tag)))
	   (while (re-search-forward (regexp-quote (semantic-tag-name tag))
				     (point-max)
				     'move)

	     ;;
	     ;; tooltip stuff
	     ;; (setq pt (posn-x-y (posn-at-point (point))))
	     ;; (setq tfp-left (+ (car pt) (car edges) left))
	     ;; (setq tfp-top (+ (cdr pt) (cadr edges) top))
	     ;; (setq tooltip-frame-parameters (append tooltip-frame-parameters nil))
	     ;; (message (format "TFP: %d,%d" tfp-left tfp-top))
	     ;; (push (cons 'left tfp-left) tooltip-frame-parameters)
	     ;; (push (cons 'top tfp-top) tooltip-frame-parameters)
	     ;; (tooltip-show (semantic-format-tag-summarize tag nil nil))

	     ;; (setq overlay (make-overlay (match-beginning 0) (+ 5 (match-end 0))))
	     ;; (overlay-put overlay 
	     ;; 	       'after-string 
	     ;; 	       (semantic-format-tag-summarize tag nil nil))
	     ;; (overlay-put overlay 'face 'isearch)
	     ;; (goto-char (+ 5 (match-end 0))))

	     )))))

(defun semantic-decoration-on-constant-variables-highlight-default (tag)
  "Highlight TAG that's a constant variable with information
about its declaration as a tooltip.  Use a primary decoration."
  (interactive)
  (when (semantic-tag-with-position-p tag)
    (cond ((semantic-tag-of-class-p tag 'function)
	   (progn
	     (semantic-beginning-of-context (semantic-tag-start tag))
	     (semantic-up-context)
	     (semantic-end-of-command)
	     (let* ((variable-tags (semantic-get-local-variables)))
	       (pp variable-tags)
	       (mapcar '(lambda (tag)
			  (message (format "s: %d e: %d"
					   (semantic-tag-start tag)
					   (semantic-tag-end tag)))
			  (add-text-properties (semantic-tag-start tag)
					       (semantic-tag-end tag)
					       '(face isearch-face))) 
		       variable-tags))))
	  ((semantic-tag-of-class-p tag 'variable)
	   (semantic-set-tag-face tag isearch-face)))
    (pp tag)
    (message (semantic-format-tag-summarize tag nil nil))))

(defun semantic-decoration-on-constant-variables-p-default (tag)
  "Return non-nil if TAG is a constant variable."
  (and
   (or (eq major-mode 'c++-mode)
       (eq major-mode 'c-mode))
   (or (semantic-tag-of-class-p tag 'variable)
       (semantic-tag-of-class-p tag 'function))))


;;
(defun my-c-mode-font-lock-if0 (limit)
  ;; needed `file-truename' here to get it working!
  (let* ((saved-buffer-name (file-truename (buffer-name)))
	 ;(semdb-table (semanticdb-file-table-object saved-buffer-name))
	 )
   (save-restriction 
     (widen) 
     (save-excursion 
       (goto-char (point-min)) 
       (let ((depth 0) str start start-depth) 
	 (while (re-search-forward "^\\s-*#\\s-*\\(\\(?:e\\(?:lse\\|ndif\\)\\|if\\(?:n?def\\)?\\)\\)" limit 'move)
	   (setq str (match-string 1))
	   ;;(pp str)
	   (if (or (string= str "ifdef") 
		   (string= str "if")
		   (string= str "ifndef")) 
	       (progn
		 (let* ((name-after-if-p 
			 (looking-at "\\s-+\\([^[:space:]\n\r]+\\)"))
			(name-after-if (if name-after-if-p
					   (match-string 1)
					 ""))
			;(saved-match-data (match-data))
			(def (string= str "ifdef"))
			(ndef (string= str "ifndef"))
			)
		   (setq depth (1+ depth))
		   ;;(pp name-after-if)
		   ;;(pp saved-buffer-name)
		   (when (or (and (null start)
				  (not def)
				  (not ndef)
				  (stringp name-after-if)
				  (string= name-after-if "0"))
			     (and (null start) 
				  (not (null name-after-if-p))
				  (stringp name-after-if)
				  (stringp saved-buffer-name)
				  (not (null def))
				  ;; (null (semantic-brute-find-first-tag-by-name 
				  ;; 	 name-after-if
				  ;; 	 saved-buffer-name
				  ;; 	 t
				  ;; 	 t))
				  (null (semantic-analyze-find-tag name-after-if))
				  
)
			     (and (null start) 
				  (not (null name-after-if-p))
				  (stringp name-after-if)
				  (stringp saved-buffer-name)
				  (not (null ndef))
				  ;; (semantic-brute-find-first-tag-by-name 
				  ;;  name-after-if
				  ;;  saved-buffer-name
				  ;;  t
				  ;;  t)
				  (semantic-analyze-find-tag name-after-if)))
		     (setq start (match-end 0) 
			   start-depth depth))))
	     (when (and start (= depth start-depth)) 
	       (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face) 
	       (setq start nil)) 
	     (when (string= str "endif") 
	       (setq depth (1- depth))))) 
	 (when (and start (> depth 0)) 
	   (c-put-font-lock-face start (point) 'font-lock-comment-face)))))) 
  nil)

(defun my-c-mode-common-hook () 
  (font-lock-add-keywords 
   nil 
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))
     ("\\([][|!.+=&/%*,<>(){}:^~-]+\\)" 1 'font-lock-builtin-face))
   'add-to-end))
 
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;;;;;


(defun my-emacs-lisp-mode-font-lock-defuns (limit)
  (save-excursion
    (widen)
    (save-excursion
      (goto-char (point-min))
      (forward-sexp)))
)

(defun my-emacs-lisp-mode-common-hook () 
  (font-lock-add-keywords 
   nil 
   '((my-emacs-lisp-mode-font-lock-defuns (0 font-lock-comment-face prepend))
     )
   'add-to-end))




(provide 'brian-cedet-syntax-highlighting)

;; experimental

(defun brian-replace-display-overlay (startpt 
				      endpt 
				      orig-text 
				      bonus-text 
				      &optional face rj pad)
  (save-excursion
    (goto-char startpt)
    ;;TODO: point-at-eol may be diff for startpt and endpt
    (let* ((overlay-orig (make-overlay startpt (point-at-eol 1)))
	   (overlay-bonus (make-overlay (1- endpt) (point-at-eol 1)))
	   (bonus-text-full (concat "[" 
				    (if (semantic-tag-p bonus-text)
					(semantic-tag-name bonus-text)
				      bonus-text)
				    "]"))
	   (btf-len (length bonus-text-full))
	   (eolpt (point-at-eol))
	   (bolpt (point-at-bol))
	   (ot-len (- eolpt bolpt))
	   (face (if face
		     face
		   'lazy-highlight))
	   (padding (if pad 
			pad
		      (make-string (- (window-body-width) 
				      ot-len
				      btf-len)
				   ? ))))
      (message (format "%d %d " (length orig-text) (length padding)))
      (overlay-put overlay-orig
		   'display
		   (concat orig-text
			   padding))
      (overlay-put overlay-bonus
		   'display
		   (concat (make-string (- (window-body-width)
					   (- eolpt
					      bolpt)
					   (length padding)
					   btf-len)
					? )
			   bonus-text-full))
      (overlay-put overlay-bonus
		   'face
		   face))))

(defun my-c-mode-font-lock-if0 (limit)
  ;; needed `file-truename' here to get it working!
  (let* ((saved-buffer-name (file-truename (buffer-name)))
	 (name-after-if-stack '())
	 ;(semdb-table (semanticdb-file-table-object saved-buffer-name))
	 )
   (save-restriction 
     (widen) 
     (save-excursion 
       (goto-char (point-min)) 
       (let ((depth 0) str start start-depth) 
	 (while (re-search-forward "^\\s-*#\\s-*\\(\\(?:e\\(?:lse\\|ndif\\)\\|if\\(?:n?def\\)?\\)\\)" limit 'move)
	   (setq str (match-string 1))
	   ;;(pp str)
	   (if (or (string= str "ifdef") 
		   (string= str "if")
		   (string= str "ifndef")) 
	       (progn
		 (let* ((name-after-if-p 
			 (looking-at "\\s-+\\([^[:space:]\n\r]+\\)"))
			(name-after-if (if name-after-if-p
					   (match-string 1)
					 ""))
			;(saved-match-data (match-data))
			(def (string= str "ifdef"))
			(ndef (string= str "ifndef"))
			)
		   (setq depth (1+ depth))
		   (pp name-after-if)
		   ;;(pp saved-buffer-name)
		   (push name-after-if name-after-if-stack)
		   (save-excursion
		     (forward-char 1)
		     (brian-replace-display-overlay (match-beginning 0)
						    (match-end 0)
						    (match-string 0)
						    (concat 					;(semantic-format-tag-summarize (semantic-analyze-find-tag name-after-if) nil t)
						     (semantic-format-tag-summarize (semantic-idle-summary-current-symbol-info) nil t))
						    'c-nonbreakable-space-face))
		   (when (or (and (null start)
				  (not def)
				  (not ndef)
				  (stringp name-after-if)
				  (string= name-after-if "0"))
			     (and (null start) 
				  (not (null name-after-if-p))
				  (stringp name-after-if)
				  (stringp saved-buffer-name)
				  (not (null def))
				  ;; (null (semantic-brute-find-first-tag-by-name 
				  ;; 	 name-after-if
				  ;; 	 saved-buffer-name
				  ;; 	 t
				  ;; 	 t))
				  (null (semantic-analyze-find-tag name-after-if))
				  
)
			     (and (null start) 
				  (not (null name-after-if-p))
				  (stringp name-after-if)
				  (stringp saved-buffer-name)
				  (not (null ndef))
				  ;; (semantic-brute-find-first-tag-by-name 
				  ;;  name-after-if
				  ;;  saved-buffer-name
				  ;;  t
				  ;;  t)
				  (semantic-analyze-find-tag name-after-if)))
		     
		     (setq start (match-end 0) 
			   start-depth depth))))
	     (when (and start (= depth start-depth)) 
	       (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face) 

	       (setq start nil)) 
	     (when (string= str "endif") 
	       (setq depth (1- depth))
	       (save-excursion
		 (forward-char 1)
		 (brian-replace-display-overlay (match-beginning 0)
						(match-end 0)
						(match-string 0)
						(concat 					;(semantic-format-tag-summarize (semantic-analyze-find-tag name-after-if) nil t)
						 (concat " /* " 
							 (pop name-after-if-stack)
							 " */ "))
						'c-nonbreakable-space-face))))) 
	 (when (and start (> depth 0))
	   
	   (c-put-font-lock-face start (point) 'font-lock-comment-face)))))) 
  nil)

