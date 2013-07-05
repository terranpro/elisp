(defface brian-function-face
  '((t :background "green" :foreground "black"))
  "Brian Function Face C/C++"
  :group 'basic-faces)

(defface brian-function
  '((t :background "green" :foreground "black"))
  "Brian Function Face C/C++"
  :group 'basic-faces)

(add-hook 'c++-mode-hook
  (lambda ()
   (font-lock-add-keywords nil
    '(((regexp-opt '("FIXME")) 1 brian-function-face prepend)))))

;;C++11 extras
;; TODO: kinda broken - investigate + refactor + elim garb
(add-hook
 'c++-mode-hook
 '(lambda()
    ;; We could place some regexes into `c-mode-common-hook', but note that their evaluation order
    ;; matters.
    (font-lock-add-keywords
     nil '(;; complete some fundamental keywords
           ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
           ;; namespace names and tags - these are rendered as constants by cc-mode
           ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
           ;;  new C++11 keywords
           ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
           ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
           ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)

           ;; integer/float/scientific numbers
           ;("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)

           ;; c++11 string literals
           ;;       L"wide string"
           ;;       L"wide string with UNICODE codepoint: \u2018"
           ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
           ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
           ;;       R"(user-defined literal)"
           ;;       R"( a "quot'd" string )"
           ;;       R"delimiter(The String Data" )delimiter"
           ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
           ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

           ;; user-defined types (rather project-specific)
           ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
           ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
           ))
    ) t)

;(pop c++-mode-hook)

(font-lock-add-keywords 'c++-mode
  '(("FIXME" . brian-function-face)))

(font-lock-add-keywords 'c++-mode
  '(("\\(FIXME\\)" . (1 font-lock-keyword-face prepend))))

(font-lock-add-keywords 'c++-mode
  '(("\\(\\w+\\)\\s-*\(" . (1 'brian-function-face prepend))))

;;;;
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction 
    (widen) 
    (save-excursion 
      (goto-char (point-min)) 
      (let ((depth 0) str start start-depth) 
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move) 
          (setq str (match-string 1)) 
          (if (string= str "if") 
              (progn
		(let* ((name-after-if-p 
			(looking-at "\\s-+\\([^[:space:]\n\r]+\\)"))
		       (name-after-if (if name-after-if-p
					  (match-string 1)
					""))
		       (saved-match-data (match-data)))
		  (setq depth (1+ depth)) 
		  (cond ((and (null start) (string= name-after-if "0"))
			 (setq start (match-end 0)
			       start-depth depth))
			((null 
			  (semantic-find-first-tag-by-name name-after-if
							   ))
			 (setq start (match-end 0) 
			       start-depth depth))))) 
            (when (and start (= depth start-depth)) 
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face) 
              (setq start nil)) 
            (when (string= str "endif") 
              (setq depth (1- depth))))) 
        (when (and start (> depth 0)) 
          (c-put-font-lock-face start (point) 'font-lock-comment-face))))) 
  nil)

(defun my-c-mode-common-hook () 
  (font-lock-add-keywords 
   nil 
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end)) 
 
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook) 
;;;;;
