;;; brian-ac-clang.el --- 
;;
;; Copyright (C) 2013 Brian Fransioli
;;
;; Author: Brian Fransioli <terranpro@triforce>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; 

;;; Code:

;; AC Clang!!!
(defvar brian-clangcomplete-async-dir "~/elisp/foreign/clang-complete-async")

(add-to-list 'load-path brian-clangcomplete-async-dir)
(require 'auto-complete-clang-async)

(setq max-specpdl-size 20000)
(setq max-lisp-eval-depth 3000)

(defvar-local brian-clang-cflags-use-global t
  "Specify on buffer load/reload whether to append the global
  cflags to the `ac-clang-cflags' variable for use in code
  completion.")

(defun brian-clangcomplete-cflags-make (&optional compiler)
  (interactive)
  (unless compiler 
    (setq compiler "gcc"))
  (append 
   ac-clang-cflags
   (list (if (eq major-mode 'c++-mode)
	     "-std=c++11"
	   "-std=c99")
	 "-g" "-lpthread" "-Wall" "-Wextra" "-pedantic")
   (mapcar #'(lambda (inc) (concat "-I" inc))
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

(defvar-local brian-ac-clang-ldflags
  nil
  "")

(defvar-local brian-ac-clang-ldlibs
  nil
  "")

(defun brian-ac-clang-compile-cmake ()
  (interactive)
  (let ((prjdir (locate-dominating-file default-directory
					"CMakeLists.txt")))
    (while (locate-dominating-file 
	    (file-name-directory (directory-file-name prjdir))
	    "CMakeLists.txt")
      
      (setq prjdir 
	    (locate-dominating-file
	     (file-name-directory (directory-file-name prjdir))
	     "CMakeLists.txt")))

    (unless (file-exists-p (concat prjdir "/build"))
      (make-directory prjdir (concat prjdir "/build") t))

    (let ((compile-command
	   (concat "make -C " prjdir "build/ "
		   " -j 4")))
      (call-interactively (function compile)))))

(defun brian-ac-clang-compile-default ()
  (let ((compile-command (concat (if (eq major-mode 'c-mode) 
				     "CFLAGS=\""
				   "CXXFLAGS=\"")
				 (mapconcat 
				  'identity 
				  (brian-clangcomplete-cflags-make "gcc")
				  " ")
				 "\""

				 " LDFLAGS=\""
				 (mapconcat 
				  'identity
				  brian-ac-clang-ldflags
				  " ")
				 "\""

				 " LDLIBS=\""
				 (mapconcat 
				  'identity
				  brian-ac-clang-ldlibs
				  " ")
				 "\""
				 " make -k "
				 (file-name-nondirectory 
				  (file-name-sans-extension
				   (buffer-file-name))))))
    (call-interactively (function compile))))

(defun brian-ac-clang-compile ()
  (interactive)
  (cond 
   ((locate-dominating-file default-directory "CMakeLists.txt")
    (brian-ac-clang-compile-cmake))
   (t 
    (brian-ac-clang-compile-default))))

(defvar brian-clangcomplete-cflags-global
  (append 
   (list "-std=c++11" "-Wall" "-Wextra" "-pedantic")
   (mapcar #'(lambda (inc) (concat "-I" inc))
	   (split-string 
	    (let*
		((out (shell-command-to-string ;"clang -x c++ -v /dev/null"
		       "gcc -x c++ -v /dev/null"
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

(defun brian-ac-clang-cflags-initialize ()
  (let* ((srcfile (or (buffer-file-name)
		      (buffer-name)))
	 (ccdir (locate-dominating-file
		  (or buffer-file-name
		      default-directory)
		  "compile_commands.json"))
	 (ccfile (concat ccdir "compile_commands.json"))
	 (cflags-def brian-clangcomplete-cflags-global)
	 (ccmds-cflags (or 
			(when (file-exists-p ccfile)
			  (tizen-project-ac-clang-cflags-from-ccmds
			   ccfile srcfile))
			cflags-def)))

    (setq ac-clang-cflags ccmds-cflags))
  (ac-clang-update-cmdlineargs))

(defun my-ac-cc-mode-setup ()
  (let ((exec-path (add-to-list 'exec-path brian-clangcomplete-async-dir)))
    (setq ac-clang-complete-executable (executable-find "clang-complete")))

  (setq ac-sources '(ac-source-clang-async))

  (define-key ac-completing-map "\t" 'ac-complete)

  (local-set-key (kbd "<f5>") 'brian-ac-clang-compile)
  (local-set-key (kbd "<f6>") 'ac-clang-syntax-check)
  
  (when (featurep 'flymake)
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
		       (flymake-display-err-popup-for-current-line))))

  (let* ((process-environment 
	  (append (list 
		   (concat "LD_LIBRARY_PATH="
			   (mapconcat
			    'identity 
			    (delete-dups 
			     (append
			      (list (expand-file-name "~/build/lib") 
				    "/usr/local/lib")
			      (split-string
			       (or (getenv "LD_LIBRARY_PATH") "") ":" t)))
			    ":")))
		  (remove-if #'(lambda (item)
				 (string-match "^LD_LIBRARY_PATH=" item))
			     process-environment))))
    (ac-clang-launch-completion-process))
  (brian-ac-clang-cflags-initialize))

(defun my-ac-clang-config ()
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup t))

(my-ac-clang-config)

(global-set-key (kbd "M-.") 
		#'(lambda () (interactive)
		    (if (and auto-complete-mode
			     ac-clang-completion-process
			     (or (eq major-mode 'c++-mode)
				 (eq major-mode 'c-mode)))
			(ac-clang-send-location-request)
		      ;(semantic-goto-definition (point))
		      )))

(global-set-key (kbd "M-*") 
		'(lambda () (interactive)
		   (if (and auto-complete-mode
			    ac-clang-completion-process
			    (or (eq major-mode 'c++-mode)
				(eq major-mode 'c-mode)))
		       (ac-clang-goto-last-location)
		     ;(semantic-pop-tag-mark)
		     )))

;; AC Clang Awesomeness END!

(provide 'brian-ac-clang)

;;; brian-ac-clang.el ends here
