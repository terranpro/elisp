;; brian-cedet.el
;; Contains my personal configuration for CEDET

;; Loads for CEDET 
;;(load-file "/home/terranpro/code/cedet/common/cedet.el")

;; Use the NEWTRUNK branch of CEDET? or TRUNK?
;; Currently TRUNK has a nasty bug when using ede-root-cpp-proj
;; feb16 2012
(setq brian-cedet-use-newtrunk 0)

(if (= brian-cedet-use-newtrunk 1)
  (setq brian-cedet-loadfile 
	"~/code/cedet-newtrunk/cedet-devel-load.el")
  (setq brian-cedet-loadfile "~/code/cedet/common/cedet.el"))

(load-file brian-cedet-loadfile)

(if (= brian-cedet-use-newtrunk 1)
    (add-to-list 'Info-default-directory-list
		 (expand-file-name "~/code/cedet-newtrunk/doc/info"))

  (add-to-list 'Info-default-directory-list
	       (expand-file-name "~/code/cedet/common"))
  (add-to-list 'Info-default-directory-list
	       (expand-file-name "~/code/cedet/common"))

  (add-to-list 'Info-default-directory-list
	       (expand-file-name "~/code/cedet/semantic/doc"))
  (add-to-list 'Info-default-directory-list
	       (expand-file-name "~/code/cedet/eieio"))
  (add-to-list 'Info-default-directory-list
	       (expand-file-name "~/code/cedet/speedbar"))
  (add-to-list 'Info-default-directory-list
	       (expand-file-name "~/code/cedet/cogre"))
  (add-to-list 'Info-default-directory-list
	       (expand-file-name "~/code/cedet/ede"))
  (add-to-list 'Info-default-directory-list
	       (expand-file-name "~/code/cedet/srecode")))


(require 'ede)
(require 'semantic)
(when (= brian-cedet-use-newtrunk 1)
  (require 'semantic/decorate)
  (require 'semantic/ia)
  (require 'ede/proj)
  (require 'ede/cpp-root)
  (require 'semantic)
  (require 'semantic/complete)
  (require 'semantic/db)
  (require 'semantic/db-cscope)
  (require 'semantic/db-global)
  (require 'semantic/bovine/c)
  (require 'ede/m3)
  (require 'srecode/m3)
  (require 'semantic/m3)
  (require 'cedet-m3)
  (require 'cedet-global)
  ;; Add minor-modes which should be loaded
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  ;; Activate semantic
  (semantic-mode 1))

(global-ede-mode 1)
(global-srecode-minor-mode 1)

;; (setq srecode-map-load-path 
;;       (list "~/.srecode"
;; 	    "~/code/cedet-newtrunk/etc/srecode"))

 
;;(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
;;(semantic-load-enable-gaudy-code-helpers)
(semantic-load-enable-excessive-code-helpers)
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
(setq semantic-load-turn-useful-things-on t)

(semantic-add-system-include "/usr/include/glib-2.0" 'c-mode)
(semantic-add-system-include "/usr/include/glib-2.0" 'c++-mode)
(semantic-add-system-include "/usr/include/gtk-3.0" 'c-mode)
(semantic-add-system-include "/usr/include/gtk-3.0" 'c++-mode)

(semantic-add-system-include "/usr/local/include/" 'c-mode)
(semantic-add-system-include "/usr/local/include/" 'c++-mode)

(semantic-add-system-include "/usr/local/include/opencv" 'c-mode)
(semantic-add-system-include "/usr/local/include/opencv" 'c++-mode)

;;(semantic-add-system-include "/usr/local/include/opencv2" 'c++-mode)

;;(semantic-add-system-include "/usr/local/include/opencv2" 'c++-mode)

;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file 
;; 	     '"/usr/local/include/opencv2/core/types_c.h")
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file 
;; 	     '"/usr/local/include/opencv2/imgproc/types_c.h")

(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_PROP_RW" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_EXPORTS" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_EXPORTS_W_SIMPLE" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_EXPORTS_W" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_EXPORTS_W_MAP" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_INLINE" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_IN_OUT" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_OUT" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_PROP" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_PROP_RW" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_WRAP" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_WRAP_AS" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("CV_WRAP_DEFAULT" . ""))

;;(require 'ede)
;;(global-ede-mode t)
;;(setq ede-locate-setup-options '(ede-locate-global ede-locate-base))

;; COGRE settings
;; Unicode characters make the lines/arrows pretty!
(cogre-uml-enable-unicode)

;;DIDNT WORK... T.T
;;(semantic-add-system-include "/usr/local/include/opencv" 'c-mode)
;;(semantic-add-system-include "/usr/local/include/opencv2" 'c++-mode)

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
  (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all)

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
  
(when (cedet-cscope-version-check t)  ; Is it ok?
  ;; Configurations for CScope and CEDET.
  (setq ede-locate-setup-options
	(append '(ede-locate-cscope) ede-locate-setup-options))
  (semanticdb-enable-cscope-databases))

(when (cedet-gnu-global-version-check t)
  (setq ede-locate-setup-options
	(append '(ede-locate-global) ede-locate-setup-options))
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

(setq semantic-idle-scheduler-idle-time 1)

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

(defun brian-srecode-projname-replace-and-comment (str)
  (let* ((dict srecode-inserter-variable-current-dictionary)
	 (projname (srecode-dictionary-lookup-name dict "PROJECTNAME"))
	 (cs (or (and dict
		      (srecode-dictionary-lookup-name dict "comment_prefix"))
		 (and comment-multi-line comment-continue)
		 (and (not comment-multi-line) comment-start)))
	 (strs (split-string str "\n"))
	 (newstr "")
	 )
    (while strs
      (cond ((and (not comment-multi-line) (string= (car strs) ""))
	     )
	    (t
	     (setq newstr (concat newstr cs " " (car strs)))))
      (setq strs (cdr strs))
      (when strs (setq newstr (concat newstr "\n"))))

    (setq newstr (replace-regexp-in-string "{{PROJECTNAME}}" 
					   projname 
					   newstr
					   t))
    newstr))

(defun brian-srecode-string-killer (str)
  "")


(define-key srecode-mode-map (kbd "C-c / U") 'srecode-map-update-map)
