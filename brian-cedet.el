;; brian-cedet.el
;; Contains my personal configuration for CEDET

;; Loads for CEDET 
(load-file "/home/terranpro/code/cedet/common/cedet.el")

(require 'ede)

(global-ede-mode t)

;;(semantic-load-enable-minimum-features)
;;(semantic-load-enable-code-helpers)
;;(semantic-load-enable-gaudy-code-helpers)
(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-semantic-debugging-helpers)

(require 'semantic-ia)

(require 'semantic-gcc)

(require 'semantic-sb)
;;DIDNT WORK... T.T
;;(semantic-add-system-include "/usr/local/include/opencv" 'c-mode)
;;(semantic-add-system-include "/usr/local/include/opencv2" 'c++-mode)

(require 'semanticdb)
(global-semanticdb-minor-mode 1)

(require 'semantic-c)

(setq semantic-load-turn-useful-things-on t)

(semantic-add-system-include "/usr/include/glib-2.0" 'c-mode)
(semantic-add-system-include "/usr/include/glib-2.0" 'c++-mode)

;; (defun my-cedet-hook ()
;;  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
;;  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
;;  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;;  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
;; (add-hook 'c-mode-common-hook 'my-cedet-hook)


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
  (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all))
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)
(add-hook 'lisp-interaction-mode-hook 'my-cedet-hook)

(global-semantic-tag-folding-mode 1)

;; speedbar customizations

(setq speedbar-frame-parameters
      '((minibuffer)
	(width . 42)
	(border-width . 0)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(unsplittable . t)
	(left-fringe . 0)))
