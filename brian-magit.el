;; (add-to-list 'load-path "~/elisp/foreign/git-modes/")
;; (add-to-list 'load-path "~/elisp/foreign/magit/")
;; (add-to-list 'load-path "~/elisp/foreign/magit/lisp")
(require 'magit)
;;(require 'magit-svn)

(add-to-list 'Info-default-directory-list
		 (expand-file-name "~/elisp/foreign/magit"))

;; (add-to-list 'load-path "~/elisp/external/mo-git-blame/")
;; (require 'mo-git-blame)

;; magit-gerrit plugin - by me!!
;; (add-to-list 'load-path "~/elisp/foreign/magit-gerrit/")
;; (require 'magit-gerrit)
;; (setq-default magit-gerrit-ssh-creds "br.fransioli@slp-info.sec.samsung.net")

(global-set-key (kbd "<f12>") 'magit-status)

(provide 'brian-magit)
