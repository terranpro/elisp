(require 'brian-cedet-minimal)
(require 'brian-cedet-includes)

;; Auto Complete w/ Clang!
(setq ac-dictionary-directories "~/elisp/foreign/auto-complete/dict_brian")
(add-to-list 'load-path "~/elisp/foreign/auto-complete")
(add-to-list 'load-path "~/elisp/foreign/auto-complete/lib/popup")
(add-to-list 'load-path "~/elisp/foreign/auto-complete/lib/fuzzy")
(add-to-list 'Info-default-directory-list
	     (expand-file-name "~/elisp/foreign/auto-complete/doc"))

(require 'auto-complete-config)
(ac-config-default)

;; Custom Configs
;; Fast Displays, Quick Helps, and Fuzzy
(setq ac-delay 0.15)
(setq ac-auto-start 2)
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 0.5)
(setq ac-auto-show-menu 0.25)
(setq ac-menu-height 30)
(setq ac-fuzzy-enable t)

;; Key Bindings
(define-key ac-mode-map  [(control tab)] 'auto-complete)
(define-key ac-mode-map [(control return)] 'ac-fuzzy-complete)
(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-mode-map (kbd "C-c H") '(lambda ()
					 (interactive)
					 (ac-last-help t)))

(define-key ac-complete-mode-map (kbd "M-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "M-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "C-s") 'ac-isearch)
(define-key ac-complete-mode-map (kbd "M-s") 'ac-isearch-doc)

(define-key ac-completing-map (kbd "M-n") 'ac-next)
(define-key ac-completing-map (kbd "M-p") 'ac-previous)

;; AC Clang!!!
(defvar brian-clangcomplete-async-dir "~/elisp/foreign/clang-complete-async")
(defvar-local brian-clang-cflags-use-global t
  "Specify on buffer load/reload whether to append the global
  cflags to the `ac-clang-cflags' variable for use in code
  completion.")

(defun brian-clangcomplete-cflags-make (&optional compiler)
  (interactive)
  (unless compiler 
    (setq compiler "gcc"))
  (append 
   (list "-std=c++11" "-Wall" "-Wextra" "-pedantic")
   (mapcar '(lambda (inc) (concat "-I" inc))
	   (split-string 
	    (let*
		((out (shell-command-to-string (concat
						compiler
						" -x c++ -v /dev/null")))
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
		  buf)))))))

(defun brian-ac-clang-compile ()
  (interactive)
  (let ((compile-command (concat "CXXFLAGS=\""
				 (mapconcat 
				  'identity 
				  (brian-clangcomplete-cflags-make "gcc")
				  " ")
				 "\""
				 " make -k "
				 (file-name-nondirectory 
				  (file-name-sans-extension
				   (buffer-file-name))))))
    (call-interactively (function compile))))

(defvar brian-clangcomplete-cflags-global
  (append 
   (list "-std=c++11" "-Wall" "-Wextra" "-pedantic")
   (mapcar '(lambda (inc) (concat "-I" inc))
	   (split-string 
	    (let*
		((out (shell-command-to-string "clang -x c++ -v /dev/null"
					       ;"gcc -x c++ -v /dev/null"
					       ))
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
		  buf))))))
  "Global CFlags that should be added to `ac-clang-cflags' via
  the callback in `c-mode-common-hook' based on the buffer local
  variable `brian-clang-cflags-use-global'.

This is useful for excluding system global cflags for ecosystem
based subprojects (e.g. Tizen + GBS rootstrap image dir.")

(add-to-list 'load-path brian-clangcomplete-async-dir)
(require 'auto-complete-clang-async)

(when (featurep 'flymake)
  (defun flymake-display-err-popup-for-current-line ()
    "Display a menu with errors/warnings for current line if it has errors and/or warnings using popup from popup.el."
    (interactive)
    (let* ((line-no             (flymake-current-line-no))
	   (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
	   (menu-data           (flymake-make-err-menu-data line-no line-err-info-list)))
      (if menu-data
	  (progn
	    (popup-tip (concat (car menu-data) "\n\n"
			       (mapconcat 'car (car (cdr menu-data)) "\n"))))
	(flymake-log 1 "no errors for line %d" line-no)))))

(defun ac-cc-mode-setup ()
  (let ((exec-path (add-to-list 'exec-path brian-clangcomplete-async-dir)))
    (setq ac-clang-complete-executable (executable-find "clang-complete")))

  (setq ac-sources '(ac-source-clang-async
					;ac-source-semantic
					;ac-source-semantic-raw
		     ))

  (define-key ac-completing-map "\t" 'ac-complete)

  (local-set-key (kbd "<f5>") 'brian-ac-clang-compile)
  (local-set-key (kbd "<f6>") 'ac-clang-syntax-check)
  (local-set-key (kbd "S-<f6>") #'(lambda ()
				    (interactive)
				    (flymake-delete-own-overlays)))
  (local-set-key (kbd "<f7>")
		 #'(lambda () (interactive)
		     (flymake-goto-prev-error)
		     (flymake-display-err-popup-for-current-line)))
  (local-set-key (kbd "<f8>")
		 #'(lambda () (interactive)
		     (flymake-goto-next-error)
		     (flymake-display-err-popup-for-current-line)))

  (let ((process-environment 
	 (add-to-list 'process-environment
		      (concat "LD_LIBRARY_PATH=/usr/local/lib:"
			      (getenv "LD_LIBRARY_PATH")))))
    (ac-clang-launch-completion-process))

  (if brian-clang-cflags-use-global
      (setq ac-clang-cflags 
	    (append ac-clang-cflags brian-clangcomplete-cflags-global)))
  (ac-clang-update-cmdlineargs))


;; When we are using clang async, let's not use CEDET

(remove-hook 'c-mode-common-hook 'my-cedet-hook)

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup t)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup t)
  (global-auto-complete-mode t))

(my-ac-config)

(global-set-key (kbd "M-.") 
		'(lambda () (interactive)
		   (if (and auto-complete-mode
			    ac-clang-completion-process
			    (or (eq major-mode 'c++-mode)
				(eq major-mode 'c-mode)))
		       (ac-clang-send-location-request)
		     (semantic-goto-definition (point)))))

(global-set-key (kbd "M-*") 
		'(lambda () (interactive)
		   (if (and auto-complete-mode
			    ac-clang-completion-process
			    (or (eq major-mode 'c++-mode)
				(eq major-mode 'c-mode)))
		       (ac-clang-goto-last-location)
		     (semantic-pop-tag-mark))))

;; AC Clang Awesomeness END!


;;(require 'brian-clang-async)

;;
;; readline-complete
;;

;; (setq explicit-shell-file-name "bash")
;; (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
;; (setq comint-process-echoes t)
(setq comint-input-ignoredups t)
(setq comint-scroll-to-bottom-on-output nil)
(setq comint-scroll-to-bottom-on-input t)

;; ASIDE: if you call ssh from shell directly, add "-t" to
;; explicit-ssh-args to enable terminal.

;; (add-to-list 'load-path "~/elisp/foreign/readline-complete.el/")
;; (require 'readline-complete)

;; (add-to-list 'ac-modes 'shell-mode)
;; (setq shell-mode-hook nil)
;; (add-hook 'shell-mode-hook 
;; 	  '(lambda ()
;; 	     (setq comint-preinput-scroll-to-bottom t)
;; 	     (setq comint-move-point-for-output t)
;; 	     (setq comint-buffer-maximum-size 5000)
;; 	     (setq rlc-attempts 30)
;; 	     (setq rlc-timeout 0.03)
;; 	     (ac-rlc-setup-sources)))

(provide 'brian-autocomplete)
