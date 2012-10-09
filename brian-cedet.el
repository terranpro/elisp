;; brian-cedet.el
;; Contains my personal configuration for CEDET

(require 'brian-cedet-minimal)
(require 'brian-cedet-includes)
(require 'ede)
(require 'semantic)

(add-to-list 'semantic-default-submodes 
	       'global-semantic-idle-completions-mode t)

(global-ede-mode 1)
 
;;(semantic-load-enable-minimum-features)
;;(semantic-load-enable-code-helpers)
(semantic-load-enable-gaudy-code-helpers)
;;(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-semantic-debugging-helpers)

;; CEDET-devel mailing list said these werent needed
;; and possibly hurtful
;;(require 'semantic-ia)

;;(require 'semantic-gcc)

;; (require 'semantic-sb)
;; (require 'semanticdb)
;; (global-semanticdb-minor-mode 1)

;;(require 'semanticdb-global)

;;(require 'semantic-c)

;;(setq semantic-load-turn-useful-things-on t)

;; Add C++ include search path given by gcc
(mapcar (lambda (includedir)
	  (semantic-add-system-include includedir 'c++-mode))
	(split-string 
	 (let*
	     ((out (shell-command-to-string "gcc -x c++ -v /dev/null"))
	      (st (string-match "> search starts here" out))
	      (se (match-end 0))
	      (eb (string-match "End of" out)))
	   (with-temp-buffer
	     (insert out)
	     (goto-char se)
	     (end-of-line)
	     (forward-char 1)
	     (let
		 ((buf (buffer-substring-no-properties (point) eb)))
	       buf)))))

(require 'brian-cedet-includes)
(require 'brian-srecode)

(setq semantic-complete-inline-analyzer-displayor-class
      'semantic-displayor-tooltip)

(setq semantic-complete-inline-analyzer-idle-displayor-class
      'semantic-displayor-tooltip)

(setq semantic-displayor-tooltip-initial-max-tags 25)
(setq semantic-displayor-tooltip-max-tags 25)

;; COGRE settings
;; Unicode characters make the lines/arrows pretty!
(cogre-uml-enable-unicode)

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

(setq global-semantic-tag-folding-mode 1)

;; speedbar customizations

(setq speedbar-frame-parameters
      '((minibuffer)
	(width . 42)
	(border-width . 0)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(unsplittable . t)
	(left-fringe . 0)))

;;EDE options

(setq ede-locate-setup-options '(ede-locate-base))
  
;; (when (cedet-cscope-version-check t)  ; Is it ok?
;;   ;; Configurations for CScope and CEDET.
;;   (setq ede-locate-setup-options
;; 	(append '(ede-locate-cscope) ede-locate-setup-options))
;;   (semanticdb-enable-cscope-databases))

;; (when (cedet-gnu-global-version-check t)
;;   (setq ede-locate-setup-options
;; 	(append '(ede-locate-global) ede-locate-setup-options))
;;   (semanticdb-enable-gnu-global-databases 'c-mode)
;;   (semanticdb-enable-gnu-global-databases 'c++-mode))

(setq semantic-idle-scheduler-idle-time 0.25)

;; face customizations

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(semantic-tag-boundary-face ((t nil))))

;; (ede-cpp-root-project "cpp-tests"
;; 		      :name "Research"
;; 		      :file "~/code/research/CMakeLists.txt"
;; 		      :include-path '("/"
;; 				      )
;; 		      :system-include-path '("/opt/local/include"
;; 					     "/opt/local/include/opencv"
;; 					     "/opt/local/include/opencv2"))

;; (ede-cpp-root-project "OpenCV/C++ test"
;;      :name "OpenCV/C++ test"
;;      :file "~/code/research/CMakeLists.txt"
;;      :include-path '("/"
;; 		     "/sift"
;; 		     "/sift_optflow"
;; 		     "/util"
;; 		     "/test"
;;                   )
;;      :system-include-path '("/usr/local/include"
;;                      "/usr/local/include/opencv"
;; 		     )
;;      :spp-table '(
;;                   ("CV_PROP_RW" . "")
;;                   ("CV_EXPORTS" . "")
;;                   ("CV_EXPORTS_W_SIMPLE" . "")
;;                ("CV_EXPORTS_W" . "")
;;                ("CV_EXPORTS_W_MAP" . "")
;;                ("CV_INLINE" . ""))
;;      :local-variables (list
;;                (cons 'semantic-lex-c-preprocessor-symbol-file
;;                  (cons "/usr/local/include/opencv2/core/types_c.h"
;;                    (cons "/usr/local/include/opencv2/imgproc/types_c.h"
;;                      semantic-lex-c-preprocessor-symbol-file)))))


(provide 'brian-cedet)
