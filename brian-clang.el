;; (load-library "clang-completion-mode")
;; (setq c++-mode-hook nil)
;; (add-hook 'c++-mode-hook (lambda () 
;; 			   (c-set-style "briancpp")
;; 			   (define-key 
;; 			     c++-mode-map 
;; 			     (kbd "RET") 
;; 			     'newline-and-indent)))

;; (setq clang-flags 
      
;;       (concat "-w " " -ferror-limit" " 1 "
;; 	      " -I/usr/include/c++/4.7.0"
;; 	      " -I/usr/include"
;; 	      " -I/usr/include/c++/4.7.0/i486-linux-gnu/"
;; 	      " -I/usr/include/c++/4.7.0/backward"
;; 	      " -I/usr/include/clang/3.1/include/"
;; 	      " -I/usr/local/include"))

;;(load-library "~")
(setq ac-dictionary-directories "~/code/auto-complete/dict_brian")
(add-to-list 'load-path "~/code/auto-complete")
(require 'auto-complete-config)
(ac-config-default)

(load-library "ac-clang")
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
"/usr/include/c++/4.7
 /usr/include/c++/4.7/i486-linux-gnu
 /usr/include/c++/4.7/backward
 /usr/lib/gcc/i486-linux-gnu/4.7/include
 /usr/local/include
 /usr/lib/gcc/i486-linux-gnu/4.7/include-fixed
 /usr/include/i386-linux-gnu
 /usr/include
")))

(setq ac-auto-start t)
(setq ac-quick-help-delay 0.5)
(define-key ac-mode-map  [(control tab)] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  ;;(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  ;;(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  ;;(add-hook 'css-mode-hook 'ac-css-mode-setup)
  ;;(add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(my-ac-config)
