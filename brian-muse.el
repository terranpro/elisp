(setq load-path 
      (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/muse"))

(require 'htmlize)       ; pretty syntax highlighting w/muse
(require 'muse-mode)     ; load authoring mode

(require 'muse-html)     ; load publishing styles I use
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)
(require 'muse-colors)
(require 'muse-wiki)
(require 'muse-project)  ; publish files in projects

(setq terranpro-html-style-sheet
      (concat "<link rel=\"stylesheet\" type=\"text/css\""
              " charset=\"utf-8\" media=\"all\""
              " href=\"./css/style.css\" />"
      )
)

(setq terranpro-xhtml-style-sheet
      (concat "<link rel=\"stylesheet\" type=\"text/css\""
              " charset=\"utf-8\" media=\"all\""
              " href=\"./css/style.css\" />"
      )
)

(muse-derive-style
 "terranpro-html"
 "html"
 :author "assem <assem@terranpro.org>"
 :header "~/remote/header.html"
 :footer "~/remote/footer.html"
 :style-sheet terranpro-html-style-sheet
)

(muse-derive-style
 "terranpro-xhtml"
 "xhtml"
 :author "assem <assem@terranpro.org>"
 :header "~/remote/header.html"
 :footer "~/remote/footer.html"
 :style-sheet terranpro-xhtml-style-sheet
)

(setq muse-project-alist
      '(
	("TerranproOrg" ("~/remote/muse" :default "index.muse")
	 (:base "terranpro-xhtml" :path "~/remote/terranpro_org_web/")
;;	 (:base "pdf" :path "~/public_html/pdf")
	)
       )
)

(defun my-muse-update-directive-date()
  "
Update the #date directive that Muse recognizes, which I put at 
the top of my webpages.  This finds the first #date occurrence 
and kills the rest of the text at that line, replacing it with 
the current date, as reported by the shell command \"date\""
  (interactive)

  (if (equal major-mode 'muse-mode)
      (save-excursion

	  (goto-char (point-min))
	  (unless (equal (search-forward "#date" nil t) nil)

	    (kill-line)
	    (kill-line)
	    (insert " " (shell-command-to-string "date"))))))


(add-hook 'before-save-hook 'my-muse-update-directive-date())
