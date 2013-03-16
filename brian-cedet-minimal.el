;; replaced w/load-path and require to try to eliminate a double load
;; error, incase other files do (require 'brian-cedet)
;;(add-to-list 'load-path "~/elisp/foreign/cedet/")
(setq cedet-root-path (file-name-as-directory "~/code/cedet/"))
(setq cedet-root-path (file-name-as-directory "~/elisp/foreign/cedet/"))
(add-to-list 'load-path (concat cedet-root-path "contrib"))
(load-file (concat cedet-root-path "cedet-devel-load.el"))
;;(require 'cedet-devel-load)
;;(require 'semantic)
(semantic-mode 1)

;; fixes semantic-analyze-tag-references
(require 'semantic/analyze/refs)

(add-to-list 'Info-default-directory-list
		 (expand-file-name (concat cedet-root-path "doc/info")))


;; amazing jump and return functions using semantic-ia-fast-jump
(defvar semantic-tags-location-ring (make-ring 20))

(defun semantic-goto-definition (point)
  "Goto definition using semantic-ia-fast-jump   
save the pointer marker if tag is found"
  (interactive "d")
  (condition-case err
      (progn                            
        (ring-insert semantic-tags-location-ring (point-marker))  
        (semantic-ia-fast-jump point))
    (error
     ;;if not found remove the tag saved in the ring  
     (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
     (signal (car err) (cdr err)))))

(defun semantic-pop-tag-mark ()             
  "popup the tag save by semantic-goto-definition"   
  (interactive)                                                    
  (if (ring-empty-p semantic-tags-location-ring)                   
      (message "%s" "No more tags available")                      
    (let* ((marker (ring-remove semantic-tags-location-ring 0))    
              (buff (marker-buffer marker))                        
                 (pos (marker-position marker)))                   
      (if (not buff)                                               
            (message "Buffer has been deleted")                    
        (switch-to-buffer buff)                                    
        (goto-char pos))                                           
      (set-marker marker nil nil))))

;; replace garbage tags- defaults
(global-set-key (kbd "M-.") 'semantic-goto-definition)
(global-set-key (kbd "M-*") 'semantic-pop-tag-mark)

;; test
(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  ;replaced w/better+upgraded semantic-goto-definition
  ;(local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cj" 'semantic-goto-definition)
  (local-set-key "\C-c\C-j" 'semantic-goto-definition)
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
;; just open .h files in c++ mode since all these retard who coders
;; don't know wtf they are doing name them garbage.h - TILT
(setq auto-mode-alist
      (append '(("\\.h\\'" . c++-mode))
	      auto-mode-alist))

(provide 'brian-cedet-minimal)
