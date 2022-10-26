;; brian's git repo of elisp
;; Add this to your real .emacs and the rest is done.
;;(add-to-list 'load-path "~/elisp/")
;;(load-library "brian-dotemacs")
(add-to-list 'load-path "~/elisp/foreign")

;;; package
(require 'package)
;; MELPA packages
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(setq package-selected-packages
      (append package-selected-packages
	      '(ccls
		with-editor
		transient
		magit
		f
		company
		pythonic
		indium
		anaconda
		company-anaconda
		org-jira
		org-drill)))
(package-install-selected-packages)

(require 'brian-config)
(require 'brian-cc-mode)
(require 'brian-paredit)
(require 'brian-magit)
;(require 'brian-autocomplete)
;(require 'brian-ac-clang)
(require 'brian-company-mode)
(require 'brian-nodejs)
(require 'brian-python)
;;(require 'brian-rtags)
;(require 'brian-clang-faces)
;(require 'brian-cedet)
(require 'brian-srecode)
(require 'brian-tizen)
;;(require 'brian-clang)
;;(require 'brian-gnus)
(require 'brian-org)
(require 'brian-lsp)
;;(require 'brian-muse)
(require 'brian-cmake)
;;(require 'brian-ideone)
(require 'brian-wgrep)
;;(require 'brian-slime)
(require 'brian-argo)

(require 'brian-yasnippet)

;; all faces/symbols should be defined before including the theme
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
