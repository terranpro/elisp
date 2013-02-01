;; replaced w/load-path and require to try to eliminate a double load
;; error, incase other files do (require 'brian-cedet)
;;(add-to-list 'load-path "~/elisp/foreign/cedet/")

(setq cedet-root-path (file-name-as-directory "~/code/cedet/"))
(add-to-list 'load-path (concat cedet-root-path "contrib"))
(load-file (concat cedet-root-path "cedet-devel-load.el"))
;;(require 'cedet-devel-load)
;;(require 'semantic)
(semantic-mode 1)

(add-to-list 'Info-default-directory-list
		 (expand-file-name (concat cedet-root-path "doc/info")))


;; test
(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-c+" 'semantic-tag-folding-show-block)
  (local-set-key "\C-c-" 'semantic-tag-folding-fold-block)
  (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all)
  (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all)
  (local-set-key [(meta return)] 'semantic-ia-complete-tip)

  (local-set-key "\C-cf" 'ede-find-file)
  (local-set-key "\C-c\C-f" 'ede-find-file))

(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)
(add-hook 'lisp-interaction-mode-hook 'my-cedet-hook)


(provide 'brian-cedet-minimal)
