;;; org-mode dev version
(setq load-path (cons "~/elisp/foreign/org-mode/lisp" load-path))
(setq load-path (cons "~/elisp/foreign/org-mode/contrib/lisp" load-path))

;; org-mode Info directory
(add-to-list 'Info-default-directory-list
             (expand-file-name "~/elisp/foreign/org-mode/doc"))

;(require 'org-install)
(require 'org)
(require 'org-latex)
(require 'org-habit)
(require 'ob)
(require 'ob-ditaa)
(require 'ob-plantuml)
(require 'org-drill)
(require 'assoc)

;; indents and folding
(setq org-startup-folded 'showall)
(setq org-startup-indented 'indent)
(setq org-startup-with-inline-images t)


;; org-babel setup
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)))

(setq org-plantuml-jar-path "/home/terranpro/Downloads/plantuml.jar")

(global-set-key (kbd "C-c a") 'org-agenda)

;; thesis
(setq reftex-default-bibliography
      (quote
       ("~/code/research/papers/bib/db.bib")))

;; Use latexmk for PDF export
;; Because this pdflatex stuff is B0rked!
;; 7/24/2012
;; (setq org-latex-to-pdf-process (list "pdflatex -shell-escape %f"
;; 				     "bibtex %b"
;; 				     "pdflatex -shell-escape %f"
;; 				     "bibtex %b"
;; 				     "pdflatex -shell-escape %b"))

;; clean up files first, then make with --shell-escape for minted
(setq org-latex-to-pdf-process 
      (list "latexmk -c"
	    "latexmk -gg -pdf -pdflatex='pdflatex --shell-escape' %b"))

;; for my cygwin windows box, system default is broken
;; and i can't reach adobe or evince - so use garbage xpdf
(when (null (or (executable-find "adobe")
		(executable-find "evince")))
  (aput 'org-file-apps "\\.pdf\\'" "xpdf %s"))

(add-to-list 'org-export-latex-classes
	     '("brianthesis" "\\documentclass[11pt, b5paper, twoside]{article}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-export-latex-classes
	     '("brianletter" "\\documentclass[11pt, b5paper]{letter}"
	       ("\\begin{letter}{%s}" . "\\begin{letter}{%s}") 
	       ("\\opening{%s}" . "\\opening{%s}")
))

;;(add-to-list 'org-export-latex-packages-alist '("" "listings")
;; (add-to-list 'org-export-latex-packages-alist '("" "color"))
;; (setq org-export-latex-listings 'minted)
;; (add-to-list 'org-export-latex-packages-alist '("" "minted"))

;; TODO: Temporarily dont include minted until i fix install on
;; ubuntu12.04
(setq org-export-latex-packages-alist nil)
(setq org-src-fontify-natively t)

;; Org mode timestampage!
(setq org-time-stamp-rounding-minutes '(0 1))
(add-hook 'org-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c T u") 'org-timestamp-up)
	     (local-set-key (kbd "C-c T d") 'org-timestamp-down)))

;; Org mode templates
;; Fast Figure template with placement set to fixed position
(add-to-list 'org-structure-template-alist 
	     (list "f"
		   (concat "#+CAPTION: ?\n"
			   "#+LABEL: fig:?\n"
			   "#+ATTR_LATEX: placement=[H]\n"
			   "[[./images/?]]")))


;; org protocol and drill!
(require 'org-protocol)
(require 'org-drill)

(defvar brian-org-capture-dir "~/elisp/personal/org/capture/")
(setq org-capture-templates
      (quote
       (("w"
         "Default template"
         entry
         (file+headline (concat (file-name-directory brian-org-capture-dir)
				"inbox.org.gpg") 
			"Notes")
         "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
         :empty-lines 1)
        ;; ... more templates here ...
        )))


(setq org-capture-templates
       `(("u"
         "Task: Read this URL"
         entry
         (file+headline (concat (file-name-directory brian-org-capture-dir)
				"inc-reading.org.gpg")
			"Articles To Read")
         ,(concat "* TODO Read article: '%:description'\nURL: %c\n\n")
         :empty-lines 1
         :immediate-finish t)

	 ("k"
         "Korean Language Study"
         entry
         (file+headline  (concat (file-name-directory brian-org-capture-dir)
				 "korean.org")
			 "Inbox")
         ,(concat "* Fact: '%^{prompt|Question|%i}'        :"
                  (format "%s" org-drill-question-tag)
                  ":\n:PROPERTIES:\n:DATE_ADDED: %u\n:SOURCE_URL: %c\n"
		  ":END:\n\n%\\1\n%?\n"
		  "** Response\n%^{Response|%i}")
         :empty-lines 1
	 :immediate-finish t)

        ("b"
         "Capture emacs buffer snippet"
         entry
         (file+headline (concat (file-name-directory brian-org-capture-dir)
				"my-facts.org.gpg")
			"Inbox")
         ,(concat "* Fact: '%^{Title}'      " 
                  ":" (format "%s" org-drill-question-tag) ":"
                  "\n:PROPERTIES:\n:DATE_ADDED: %u\n:END:\n\n%i\n%?\n")
         :empty-lines 1
         :immediate-finish t)

	 ("w"
         "Capture web snippet"
         entry
         (file+headline (concat (file-name-directory brian-org-capture-dir)
				"my-facts.org.gpg")
			"Inbox")
         ,(concat "* Fact: '%:description'        :"
                  (format "%s" org-drill-question-tag)
                  ":\n:PROPERTIES:\n:DATE_ADDED: %u\n:SOURCE_URL: %c\n:END:\n\n%i\n%?\n")
         :empty-lines 1
         :immediate-finish nil)
        ;; ...other capture templates...
    ))

;; account for lumpiness: periods of nothing followed by adding lots
;; of facts at one time)
(setq org-drill-add-random-noise-to-intervals-p t)
(setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
;;(setq org-drill-hide-item-headings-p t)
;; org-drill customizations
(setq org-drill-scope 'directory)
(setq org-drill-maximum-items-per-session 40)
(setq org-drill-maximum-duration 30)   ; 30 minutes
(setq org-drill-save-buffers-after-drill-sessions-p nil)
(setq org-drill-spaced-repetition-algorithm 'simple8)

(provide 'brian-org)
