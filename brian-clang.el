(require 'brian-autocomplete)

(load-library "auto-complete-clang")
(setq ac-clang-executable (executable-find "clang++"))
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
	      (split-string
	       " /home/Brian/build/gcc-4_7/lib/gcc/i686-pc-cygwin/4.7.1/../../../../include/c++/4.7.1
 /home/Brian/build/gcc-4_7/lib/gcc/i686-pc-cygwin/4.7.1/../../../../include/c++/4.7.1/i686-pc-cygwin
 /home/Brian/build/gcc-4_7/lib/gcc/i686-pc-cygwin/4.7.1/../../../../include/c++/4.7.1/backward
 /home/Brian/build/gcc-4_7/lib/gcc/i686-pc-cygwin/4.7.1/include
 /usr/local/include
 /home/Brian/build/gcc-4_7/include
 /home/Brian/build/gcc-4_7/lib/gcc/i686-pc-cygwin/4.7.1/include-fixed
 /usr/include
 /usr/lib/../include/w32api
 /home/Brian/code/research/sift_opflow
 /home/Brian/code/research/util
 /home/Brian/code/research/motion
"
	       ;; 	       "/usr/include/c++/4.7
	       ;; /usr/include/c++/4.7/i486-linux-gnu
	       ;; /usr/include/c++/4.7/backward
	       ;; /usr/lib/gcc/i486-linux-gnu/4.7/include
	       ;; /usr/local/include
	       ;; /usr/lib/gcc/i486-linux-gnu/4.7/include-fixed
	       ;; /usr/include/i386-linux-gnu
	       ;; /usr/include"
	       )))

(setq ac-clang-flags
      (mapcar (lambda (item)
		(concat "-I" item))
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
		     buf))))))
;;(setq ac-clang-flags (concat "-std=c++0x " (car ac-clang-flags)))
;;(add-to-list 'ac-clang-flags "-stdlib=libc++")
(add-to-list 'ac-clang-flags "-stdlib=c++0x")
;;(add-to-list 'ac-clang-flags "-U__GXX_EXPERIMENTAL_CXX0X__")

(define-key ac-mode-map (kbd "C-c c") 'ac-complete-clang)

(defvar brian-use-clang nil)

(unless (null brian-use-clang)
  (setq brian-cedet-loadfile "~/code/cedet/cedet-devel-load.el")
  (load-file brian-cedet-loadfile)

  (add-to-list 'Info-default-directory-list
	       (expand-file-name "~/code/cedet/doc/info"))


  (global-semantic-decoration-mode t)
  (global-semantic-highlight-func-mode t)
  (global-semantic-show-unmatched-syntax-mode t)

  

  (ac-define-source brian-semantic
    '((available . (or (require 'semantic-ia nil t)
		       (require 'semantic/ia nil t)))
      (candidates . (ac-semantic-candidates ac-prefix))
					;(prefix . c-dot-ref)
      (requires . 2)
      (symbol . "m"))))

(defun brian-ac-semantic-candidates (prefix)
    (with-no-warnings
      (delete ""            ; semantic sometimes returns an empty string
	      (mapcar 'semantic-tag-name
		      (ignore-errors
			(or (semantic-analyze-possible-completions
			     (semantic-analyze-current-context))
			    (senator-find-tag-for-completion prefix)))))))

(defun my-ac-cc-mode-setup ()
  (setq ac-sources '(ac-source-semantic
		     ;ac-source-semantic-raw
		     ;ac-source-brian-semantic
		     ac-source-yasnippet))
  (if (null brian-use-clang)
    (semantic-mode t))

  (define-key ac-completing-map "\t" 'ac-complete)

  (when brian-use-clang
    (setq ac-sources '(ac-source-clang ac-source-yasnippet))

    ;;(semantic-clang-activate)
    (setq semantic-clang-binary (executable-find "clang++"))
    ))




;; (setq ac-use-menu-map nil)
;; (define-key ac-menu-map "\M-n" 'ac-next)
;; (define-key ac-menu-map "\M-p" 'ac-previous)


(provide 'brian-clang)
