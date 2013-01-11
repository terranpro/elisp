;; brian's git repo of elisp
;; Add this to your real .emacs and the rest is done.
;;(add-to-list 'load-path "~/elisp/")
;;(load-library "brian-dotemacs")
(add-to-list 'load-path "~/elisp/foreign")

(require 'brian-config)
(require 'brian-autocomplete)
(require 'brian-cedet)
;;(require 'brian-tizen)
;;(require 'brian-clang)
;;(require 'brian-gnus)
(require 'brian-paredit)
(require 'brian-org)
(require 'brian-yasnippet)
;;(require 'brian-muse)
(require 'brian-cmake)
;;(require 'brian-ideone)
(require 'brian-themes)
(require 'brian-magit)

;;; tramp 
(setq tramp-default-method "scpc")
(setq tramp-chunksize 100)
(setq tramp-default-proxies-alist nil)
(add-to-list 'tramp-default-proxies-alist
	     '("pringles.terranpro.org"
	       "terranpro"
	       "/ssh:mirine.terranpro.org:"))


;; Reopen files/buffers from previous session on startup
;;(desktop-read)
(desktop-save-mode t)



(global-set-key (kbd "<f5>") 'compile)

;;workgroups.el
;; (add-to-list 'load-path "~/code/workgroups.el")
;; (require 'workgroups)
;; (setq wg-prefix-key (kbd "C-c w"))
;; (workgroups-mode 1)
;; (setq wg-file "~/elisp/workgroups/Kenobi.wgrps")
;;(wg-load "~/elisp/workgroups/CAPP.wgrps")


(provide 'brian-dotemacs)
