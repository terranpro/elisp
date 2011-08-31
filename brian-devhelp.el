(defvar devhelp-function-completion-list "" "List of all glib functions")

(defun devhelp-find-function (func-name)
"Scans through the .devhelp files in search of a C function 
with an exact matching name.  It extracts the basic C 
function definition and echoes it on the modeline.

EX: M-x devhelp-find-function <RET> g_slist_append <RET>"

  (interactive "sFunction name: ")

  (with-temp-buffer 
    (let* ((glib-devhelp-file 
	   "/usr/share/doc/libglib2.0-doc/glib/glib.devhelp")

	  (glib-devhelp-file-gz (concat glib-devhelp-file ".gz"))

	  (find-func-name-regex "name=\"%s[ ]*(?)?\" link=\"\\([-_\\.0-9a-zA-Z]+\\)#\\([-_0-9a-zA-Z]+\\)"))
      
      (cond
       (
	(file-exists-p glib-devhelp-file) 
	(insert-file-contents-literally glib-devhelp-file))
       
       (
	(file-exists-p glib-devhelp-file-gz)
	(with-auto-compression-mode
	  (insert-file-contents glib-devhelp-file-gz))))

      (goto-char (point-min))
      (re-search-forward
       (format find-func-name-regex func-name))

      ;(message "%s // %s" (match-string 1) (match-string 2))
      (devhelp-find-function-info (match-string 1) (match-string 2)))))

(defun devhelp-find-function-at-point ()
  (interactive)

  (devhelp-find-function (devhelp-get-current-completion)))

(defun devhelp-find-function-info (devhelp-filename function-name)
  (with-temp-buffer
    (let ((func-return-type)
	  (func-parameters)
	  (func-short-description)
	  (my-temp-point)
	  (my-temp-point-end)
	  (another-temp-buffer))

      (insert-file-contents-literally 
       (format "/usr/share/doc/libglib2.0-doc/glib/%s" devhelp-filename))
      (goto-char (point-min))
      (re-search-forward 
       (format "<a name=\"%s\">" function-name))

      (setq my-temp-point (point))

      (re-search-forward "\"returnvalue\">\\([_-0-9A-Za-z]+\\)")
      (setq func-return-type (format "%s" (match-string 1)))

      ; search for the <p> which describes the function (short)
      (setq my-temp-point-end (re-search-forward "<p>"))

      (re-search-forward "[ \t
]*\\(.*\\)?[ \t
]*</p>")

      (setq func-short-description (format "%s" 
					   (devhelp-nuke-html-tags 
					    (match-string 1))))

      (goto-char my-temp-point)

      (re-search-forward "</h3>")

      (setq another-temp-buffer 
	    (devhelp-nuke-html-tags (buffer-substring 
				     (point) my-temp-point-end)))

      (setq func-parameters (format "%s" another-temp-buffer))
      (message (format "%s" func-parameters))
      ;func-parameters
      )))

(defun devhelp-nuke-html-tags (string)
  
  (let ((nuked-string))  
    (setq nuked-string
	  (replace-regexp-in-string "<[-\.#_= /()0-9a-zA-Z\"]+>" "" 
				    (replace-regexp-in-string 
				     "[ ]\\{2,\\}" " " string)))

    (replace-regexp-in-string "[
]+" "" nuked-string)))

(defun devhelp-build-completion-list ()
  (interactive)

  (with-temp-buffer 
    (devhelp-open-devhelp-file 
     "/usr/share/doc/libglib2.0-doc/glib/glib.devhelp.gz")
    (setq devhelp-function-completion-list (devhelp-find-all-functions))))

(defun devhelp-open-devhelp-file (file)

  (with-auto-compression-mode
    (insert-file-contents file))

  (goto-char (point-min)))

(defun devhelp-complete ()
  (interactive)
  (message (try-completion (devhelp-get-current-completion) 
			   devhelp-function-completion-list)))

(defun devhelp-complete-all ()
  (interactive)
  (message "%s" 
	   (all-completions (devhelp-get-current-completion) 
			    devhelp-function-completion-list)))

(defun devhelp-find-all-functions ()
  (interactive)
  (let ((func-name-regex "name=\"\\([-_0-9a-zA-Z]+\\)[ ]*(?)?\" link=\"\\([-_\\.0-9a-zA-Z]+\\)#\\([-_0-9a-zA-Z]+\\)")
	(func-name-list )
	(func-match-total 0))

    (while (re-search-forward func-name-regex nil 't)
      (1+ func-match-total)
      (message (format "%s %d" "MATCH" func-match-total))
      (add-to-list 'func-name-list (match-string 1)))

    func-name-list))

(defun devhelp-get-current-completion ()
  (interactive)
  (save-excursion
    
    (let ((start )
	  (end (point))
	  (completion-string))

      (search-backward-regexp "[ 
\t#\"\']+")
      (setq start (1+ (point)))
      (setq completion-string (buffer-substring start end))
      ;(message "%s" completion-string)
      completion-string)))

(global-set-key (kbd "C-c TAB") 'devhelp-complete)
(global-set-key (kbd "C-c A") 'devhelp-complete-all)
(global-set-key (kbd "C-c s") 'devhelp-find-function-at-point)
