;; brian's git repo of elisp
;; Add this to your real .emacs and the rest is done.
;;(add-to-list 'load-path "~/elisp/")
;;(load-library "brian-dotemacs")
(add-to-list 'load-path "~/elisp/foreign")

(require 'brian-config)
(require 'brian-autocomplete)
;;(require 'brian-cedet)
;;(require 'brian-clang)
;;(require 'brian-gnus)
(require 'brian-paredit)
(require 'brian-org)
(require 'brian-yasnippet)
(require 'brian-muse)
(require 'brian-cmake)
(require 'brian-ideone)
(require 'brian-themes)

;;; tramp 
(setq tramp-default-method "scpc")
(setq tramp-chunksize 100)
(setq tramp-default-proxies-alist nil)
(add-to-list 'tramp-default-proxies-alist
	     '("pringles.terranpro.org"
	       "terranpro"
	       "/ssh:mirine.terranpro.org:"))


;; Reopen files/buffers from previous session on startup
(desktop-save-mode)
(desktop-read)

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
