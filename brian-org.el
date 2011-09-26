;;; org-mode dev version
(setq load-path (cons "~/code/org-mode/lisp" load-path))
(setq load-path (cons "~/code/org-mode/contrib/lisp" load-path))
(require 'org-install)
(require 'org-latex)
(require 'org-habit)
(require 'ob)
(require 'ob-ditaa)
(require 'ob-plantuml)
(setq org-confirm-babel-evaluate nil)
(setq org-plantuml-jar-path "/home/terranpro/Downloads/plantuml.jar")

(global-set-key (kbd "C-c a") 'org-agenda)
