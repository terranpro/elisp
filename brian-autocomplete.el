(require 'brian-cedet-minimal)
(require 'brian-cedet-includes)

;; Auto Complete w/ Clang!
(setq ac-dictionary-directories "~/code/auto-complete/dict_brian")
(add-to-list 'load-path "~/code/auto-complete")
(add-to-list 'Info-default-directory-list
	     (expand-file-name "~/code/auto-complete/doc"))

(require 'auto-complete-config)
(ac-config-default)

;; Custom Configs
;; Fast Displays, Quick Helps, and Fuzzy
(setq ac-delay 0.25)
(setq ac-auto-start 2)
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 1.0)
(setq ac-auto-show-menu 0.25)
(setq ac-menu-height 20)
(setq ac-fuzzy-enable t)

;; Key Bindings
(define-key ac-mode-map  [(control tab)] 'auto-complete)
(define-key ac-mode-map [(control return)] 'ac-fuzzy-complete)
(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-mode-map (kbd "C-c H") 'ac-last-help)

(define-key ac-complete-mode-map (kbd "M-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "M-p") 'ac-previous)

(define-key ac-completing-map (kbd "M-n") 'ac-next)
(define-key ac-completing-map (kbd "M-p") 'ac-previous)

;; Custom Semantic Sources
(defun brian-ac-semantic-document-function (tag)
"Given a semantic function tag, TAG, return a formatted string
describing the function."
  (let* ((ret-type (semantic-format-tag-type tag nil))
	 (arglist (mapcar 'semantic-tag-type 
			  (semantic-tag-function-arguments tag)))
	 (argtype)
	 (argstring ""))
    
    (while arglist
      (if (semantic-tag-p (car arglist))
	  (setq argtype (semantic-tag-name (car arglist)))
	(setq argtype (car arglist)))

      (setq argstring (concat argstring
			      argtype " ,\n"))

      (setq arglist (cdr arglist)))

    (concat "Returns:\n" 
	    ret-type
	    "\n\n"
	    "Arguments:\n"
	    argstring)))

(defun brian-ac-semantic-document (prefix)
  "Given an ac-candidate prefix string, PREFIX, find the relevant
tag in the semantic database and return a formatted docstring."
  (ignore-errors
    (let* ((tags (semantic-analyze-possible-completions
		  (semantic-analyze-current-context)))
	  (tag (let ((tagfound))
		 (while (not (null tags))
		   (if (string= (semantic-tag-name (car tags))
				prefix)
		       (progn (setq tagfound (car tags))
			      (setq tags nil))
		     (setq tags (cdr tags))))
		 tagfound))
	  (doc (semantic-documentation-for-tag tag))
	  (prototype (cond ((eq (semantic-tag-class tag) 'function)
			    (brian-ac-semantic-document-function tag))
			   ((eq (semantic-tag-class tag) 'variable)
			    (semantic-format-tag-type tag t))
			   (t 
			    (semantic-format-tag-uml-prototype-c-mode tag)))))
     (concat prototype "\n\n" doc))))

(ac-define-source brian-semantic
  '((available . (or (require 'semantic-ia nil t)
                     (require 'semantic/ia nil t)))
    (candidates . (ac-semantic-candidates ac-prefix))
    (document . brian-ac-semantic-document)
    (prefix . c-dot-ref)
    (requires . 0)
    (symbol . "m")))

(ac-define-source brian-semantic-raw
  '((available . (or (require 'semantic-ia nil t)
                     (require 'semantic/ia nil t)))
    (candidates . (ac-semantic-candidates ac-prefix))
    (document . brian-ac-semantic-document)
    (symbol . "s")))

(defun my-ac-cc-mode-setup ()
  (setq ac-sources '(ac-source-brian-semantic
		     ac-source-brian-semantic-raw
		     ;ac-source-brian-semantic
		     ac-source-yasnippet))

  (semantic-mode t)
  (define-key ac-completing-map "\t" 'ac-complete)
  )

(defun my-ac-config ()
  (setq-default ac-sources 
		'(ac-source-brian-semantic 
		  ac-source-brian-semantic-raw
		  ac-source-abbrev 
		  ac-source-dictionary))
  ;;(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  ;;(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  ;;(add-hook 'css-mode-hook 'ac-css-mode-setup)
  ;;(add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

(provide 'brian-autocomplete)
