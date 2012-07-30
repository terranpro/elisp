(require 'brian-cedet-minimal)

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

(setq semantic-lex-c-preprocessor-symbol-map '())
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


;; glibc++
(add-to-list 'semantic-lex-c-preprocessor-symbol-map 
	     '("_GLIBCXX_VISIBILITY" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map 
	     '("_GLIBCXX_BEGIN_NAMESPACE_VERSION" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map 
	     '("_GLIBCXX_END_NAMESPACE_VERSION" . ""))

;; experimental
(add-to-list 'semantic-lex-c-preprocessor-symbol-map 
	     '("_GLIBCXX_DEBUG" . 1))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map 
	     '("_GLIBCXX_PROFILE" . ""))

(when (or (executable-find "gcc")
	  (executable-find "g++"))
      (mapcar '(lambda (include)
		 (semantic-add-system-include include 'c++-mode))
	      ;; create a list of system includes as determined by g++
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

(setq semantic-lex-c-preprocessor-symbol-file 
      '("/home/Brian/build/gcc-4_7/include/c++/4.7.1/i686-pc-cygwin/bits/c++config.h"))
 

(provide 'brian-cedet-includes)
