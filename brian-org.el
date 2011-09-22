;;; org-mode dev version
(setq load-path (cons "~/code/org-mode/lisp" load-path))
(setq load-path (cons "~/code/org-mode/contrib/lisp" load-path))
(require 'org-install)
(require 'org-latex)
(require 'ob)
(require 'ob-ditaa)
(setq org-confirm-babel-evaluate nil)

(global-set-key (kbd "C-c a") 'org-agenda)
