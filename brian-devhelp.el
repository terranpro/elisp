(defun devhelp-find-function (func-name)
"Scans through the .devhelp files in search of a C function 
with an exact matching name.  It extracts the basic C 
function definition and echoes it on the modeline.

EX: M-x devhelp-find-function <RET> g_slist_append <RET>"

  (interactive "sFunction name: ")

  (with-temp-buffer 
    (let ((glib-devhelp-file 
	   "/usr/share/doc/libglib2.0-doc/glib/glib.devhelp")
	  (find-func-name-regex "name=\"%s[ ]*(?)?\" link=\"\\([-_\\.0-9a-zA-Z]+\\)#\\([-_0-9a-zA-Z]+\\)"))
      (insert-file-contents-literally glib-devhelp-file)
      (goto-char (point-min))
      (re-search-forward
       (format find-func-name-regex func-name))

      (message "%s // %s" (match-string 1) (match-string 2))
      (devhelp-find-function-info (match-string 1) (match-string 2)))))

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

      (message (format "Point %d and %d" my-temp-point my-temp-point-end))

      (goto-char my-temp-point)

      (re-search-forward "</h3>")
      (setq another-temp-buffer 
	    (devhelp-nuke-html-tags (buffer-substring 
				     (point) my-temp-point-end)))

      (setq func-parameters (format "%s" another-temp-buffer))
      (message (format "%s" func-parameters)))))

(defun devhelp-nuke-html-tags (string)
  
  (let ((nuked-string))  
    (setq nuked-string
	  (replace-regexp-in-string "<[-\.#_= /0-9a-zA-Z\"]+>" "" 
				    (replace-regexp-in-string 
				     "[ ]\\{2,\\}" " " string)))

    (replace-regexp-in-string "[
]+" "" nuked-string)))
