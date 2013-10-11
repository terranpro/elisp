;; brian's git repo of elisp
;; Add this to your real .emacs and the rest is done.
;;(add-to-list 'load-path "~/elisp/")
;;(load-library "brian-dotemacs")
(add-to-list 'load-path "~/elisp/foreign")

(require 'brian-config)
(require 'brian-cc-mode)
(require 'brian-paredit)
(require 'brian-magit)
(require 'brian-autocomplete)
(require 'brian-ac-clang)
(require 'brian-rtags)
;(require 'brian-clang-faces)
;(require 'brian-cedet)
(require 'brian-srecode)
(require 'brian-tizen)
;;(require 'brian-clang)
;;(require 'brian-gnus)
(require 'brian-org)
(require 'brian-yasnippet)
;;(require 'brian-muse)
(require 'brian-cmake)
;;(require 'brian-ideone)
;;(require 'brian-themes)
(require 'brian-wgrep)

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
