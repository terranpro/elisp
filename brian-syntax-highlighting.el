(defface brian-function-face
  '((t :background "green" :foreground "black"))
  "Brian Function Face C/C++"
  :group 'basic-faces)

(defface brian-function
  '((t :background "green" :foreground "black"))
  "Brian Function Face C/C++"
  :group 'basic-faces)

(add-hook 'c++-mode-hook
  (lambda ()
   (font-lock-add-keywords nil
    '(((regexp-opt '("FIXME")) 1 brian-function-face prepend)))))


(font-lock-add-keywords 'c++-mode
  '(("FIXME" . brian-function-face)))

(font-lock-add-keywords 'c++-mode
  '(("\\(FIXME\\)" . (1 font-lock-keyword-face prepend))))

(font-lock-add-keywords 'c++-mode
  '(("\\(\\w+\\)\\s-*\(" . (1 'brian-function-face prepend))))

;;;;
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction 
    (widen) 
    (save-excursion 
      (goto-char (point-min)) 
      (let ((depth 0) str start start-depth) 
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move) 
          (setq str (match-string 1)) 
          (if (string= str "if") 
              (progn
		(let* ((name-after-if-p 
			(looking-at "\\s-+\\([^[:space:]\n\r]+\\)"))
		       (name-after-if (if name-after-if-p
					  (match-string 1)
					""))
		       (saved-match-data (match-data)))
		  (setq depth (1+ depth)) 
		  (cond ((and (null start) (string= name-after-if "0"))
			 (setq start (match-end 0)
			       start-depth depth))
			((null 
			  (semantic-find-first-tag-by-name name-after-if
							   ))
			 (setq start (match-end 0) 
			       start-depth depth))))) 
            (when (and start (= depth start-depth)) 
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face) 
              (setq start nil)) 
            (when (string= str "endif") 
              (setq depth (1- depth))))) 
        (when (and start (> depth 0)) 
          (c-put-font-lock-face start (point) 'font-lock-comment-face))))) 
  nil)

(defun my-c-mode-common-hook () 
  (font-lock-add-keywords 
   nil 
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end)) 
 
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook) 
;;;;;
