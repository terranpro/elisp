;;; org-mode dev version
(setq load-path (cons "~/code/org-mode/lisp" load-path))
(setq load-path (cons "~/code/org-mode/contrib/lisp" load-path))
(require 'org-install)
(require 'org-latex)
(require 'org-habit)
(require 'ob)
(require 'ob-ditaa)
(require 'ob-plantuml)
(require 'org-drill)

;; org-drill customizations
(setq org-drill-hide-item-headings-p t)

(setq org-drill-maximum-items-per-session 40)
(setq org-drill-maximum-duration 30)   ; 30 minutes

(setq org-drill-save-buffers-after-drill-sessions-p nil)

(setq org-drill-spaced-repetition-algorithm 'simple8)

(setq org-drill-add-random-noise-to-intervals-p t)

(setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)

;; org-babel setup
(setq org-confirm-babel-evaluate nil)
(setq org-plantuml-jar-path "/home/terranpro/Downloads/plantuml.jar")

(global-set-key (kbd "C-c a") 'org-agenda)

;; thesis
(setq reftex-default-bibliography
      (quote
       ("~/code/research/papers/bib/db.bib")))

;; Use latexmk for PDF export
(setq org-latex-to-pdf-process (list "pdflatex %f"
				     "pdflatex %f"
				     "bibtex %b"
				     "pdflatex %b"))

(add-to-list 'org-export-latex-classes
	     '("brianthesis" "\\documentclass[10pt, b5paper, twoside]{article}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
