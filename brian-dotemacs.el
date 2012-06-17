;; brian's git repo of elisp
;; Add this to your real .emacs and the rest is done.
;;(add-to-list 'load-path "~/elisp/")
;;(load-library "brian-dotemacs")

(load-library "brian-config")
(load-library "brian-cedet")
(load-library "brian-gnus")
(load-library "brian-paredit")
(load-library "brian-org")
(load-library "brian-yasnippet")
(load-library "brian-muse")
(load-library "brian-cmake")
(load-library "brian-ideone")

;;; tramp 
(setq tramp-default-method "scpc")
(setq tramp-chunksize 100)
(setq tramp-default-proxies-alist nil)
(add-to-list 'tramp-default-proxies-alist
	     '("pringles.terranpro.org"
	       "terranpro"
	       "/ssh:mirine.terranpro.org:"))


;; Reopen files/buffers from previous session on startup
(desktop-save-mode 1)

;;; org-mode dev version
(setq load-path (cons "~/code/org-mode/lisp" load-path))
(setq load-path (cons "~/code/org-mode/contrib/lisp" load-path))
;; org-mode Info directory
(add-to-list 'Info-default-directory-list
             (expand-file-name "~/code/org-mode/doc"))
;(add-to-list 'Info-directory-list
;             (expand-file-name "~/code/org-mode/doc"))
(require 'org-install)
(require 'org-latex)

(global-set-key (kbd "<f5>") 'compile)

;;workgroups.el
(add-to-list 'load-path "~/code/workgroups.el")
(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)
(setq wg-file "~/elisp/workgroups/Kenobi.wgrps")
;;(wg-load "~/elisp/workgroups/CAPP.wgrps")
