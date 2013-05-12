;; Brian's LISP Code
(setq ring-bell-function 'ignore)
(setq enable-local-eval t)

(add-to-list 'default-frame-alist
	     `(width  . ,(+ 240
			    (/ (frame-parameter nil 'left-fringe) (frame-char-width))
			    (/ (frame-parameter nil 'right-fringe) (frame-char-width)))))

(add-to-list 'default-frame-alist
	     `(height  . ,(- (/ (display-pixel-height) (frame-char-height))
			     2)))

(defun enlarge-frame-height ()
  "Enlarge frame height by one"
  (interactive)
  (set-frame-height (selected-frame) (+ (frame-height) 1))
  (message "Frame height %s" (frame-height))
)

(defun enlarge-frame-width ()
  "Enlarge frame width by one"
  (interactive)
  (set-frame-width (selected-frame) (+ (frame-width) 1))
  (message "Frame width %s" (frame-width))
)

(defun shrink-frame-width ()
  "Shrink frame width by one"
  (interactive)
  (set-frame-width (selected-frame) (+ (frame-width) -1))
  (message "Frame width %s" (frame-width))
)

(defun maximize-frame-height ()
  "Cheap, Hacky way to Maximize Frame"
  (interactive)
  (set-frame-height (selected-frame) 100)
  (message "Maximized Frame height %s" (frame-height))
)

;; Two ways of doing key mapping translation
;; the latter two lines only worked when using kbd
;; presumably because of the { } keys?
(global-set-key "\C-\M-^" 'enlarge-frame-height)
(global-set-key (kbd "C-M-}") 'enlarge-frame-width)
(global-set-key (kbd "C-M-{") 'shrink-frame-width)
;;(global-set-key (kbd "C-c M") 'maximize-frame-height)
;;

;; windmove
;; Navigate windows via Super+[wasd]
(global-set-key (kbd "s-a") 'windmove-left)
(global-set-key (kbd "s-d") 'windmove-right)
(global-set-key (kbd "s-w") 'windmove-up)
(global-set-key (kbd "s-s") 'windmove-down)

(setq windmove-wrap-around t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; smoother scrolling - no jumpiness
(setq scroll-conservatively 1000)

;; ace jump
(add-to-list 'load-path "~/elisp/foreign/ace-jump-mode")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c c") 'ace-jump-char-mode)
(define-key global-map (kbd "C-c l") 'ace-jump-line-mode)
(define-key global-map (kbd "C-c w") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-l") 'ace-jump-line-mode)

;; after ace jump 1.0, they added a scope feature
;; it's still buggy in global mode
(setq ace-jump-mode-scope 'window)

;; emacs powerline
(add-to-list 'load-path "~/elisp/foreign/emacs-powerline")
;;(add-to-list 'load-path "~/elisp/foreign/powerline")
(require 'powerline)
;;(powerline-default-theme)
;;(powerline-center-theme)

;; (setq powerline-arrow-shape 'arrow)   ;; the default
;; (setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
;; (setq powerline-arrow-shape 'arrow14) ;; best for small fonts
(setq powerline-arrow-shape 'arrow)

;; (setq powerline-color1 "grey22")
(setq powerline-color1 "chocolate2")
;; (setq powerline-color2 "grey40")
(setq powerline-color2 "goldenrod")

;; expand region
(add-to-list 'load-path "~/elisp/foreign/expand-region")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Let X's clipboard play nice with emacs
(setq x-select-enable-clipboard t)

;; Remove eyecandy for mouse
;; we are one with the keyboard in emacs land
(scroll-bar-mode -1)        ;hide scroll-bar

(tool-bar-mode -1)          ;hide tool-bar
(menu-bar-mode -1)          ;hide menu-bar

;; ido mode is nuts
(ido-mode t)
(setq ido-enable-flex-matching t) ;fuzzy matching
(setq ido-default-buffer-method 'selected-window)
(setq ido-default-file-method 'selected-window)
(setq ido-auto-merge-work-directories-length -1)
;(setq ido-everywhere t)
(ido-everywhere t)

;; ido with imenu for fast symbol jumping
(require 'idomenu)
(global-set-key (kbd "C-c m") 'idomenu)

;; smex for smart M-x using ido
(add-to-list 'load-path "~/elisp/foreign/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(add-to-list 'load-path "~/elisp/foreign/multiple-cursors")
(require 'multiple-cursors)
(global-set-key (kbd "C-c M m") 'mc/edit-lines)
(global-set-key (kbd "C-c M n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c M p") 'mc/mark-previous-like-this)

; Emacs24 default font on one of my debian sids was 
; fucking stupid huge; so put this so the default font is 
; size 10pt.
;;(set-face-attribute 'default nil :height 105)
;(set-face-attribute 'default nil :font "")

;TODO: (temp) some default tweaking for window sizing
;;(add-to-list 'default-frame-alist '(height . 65))
;;(add-to-list 'default-frame-alist '(width  . 80))
;; TODO: experiment with using 3 windows in one big frame!

;; postip and tooltip configs
;; Use emacs tooltips instead of GTK+ so we can control the color scheme!
(setq x-gtk-use-system-tooltips nil)
(require 'pos-tip)

; Hotkey for compiling like in other IDEs
(global-set-key (kbd "<f5>") 'compile)

;; I Like Autofill mode for all files
;;(auto-fill-mode t)
(add-hook 'text-mode-hook '(lambda () 
			     (auto-fill-mode t)))

;; Never use backup files (garbage with tildes at end)
(setq make-backup-files nil)

;; Turn off that stupid startup screen
(setq inhibit-startup-screen t)

;; Destroy emacs server gracefully
(defun shutdown-emacs-server () 
  (interactive)
  (when (not (eq window-system 'x))
    (message "Initializing x windows system.")
    (x-initialize-window-system)
    (when (not x-display-name) 
      (setq x-display-name (getenv "DISPLAY")))
    (select-frame 
     (make-frame-on-display x-display-name '((window-system . x)))))
  (let ((last-nonmenu-event nil)
	(window-system "x"))
    (save-buffers-kill-emacs)))

;; dired find file in new frame
(require 'dired)
(require 'dired-x)
(defun brian-dired-find-file-other-frame ()
  (interactive)
  (find-file-other-frame (dired-get-file-for-visit)))
(define-key dired-mode-map (kbd "F") 'brian-dired-find-file-other-frame)

;; TODO create a brian-alias.el file later if this grows:
;; My aliases to make emacs even more amazing
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer

;; winner mode
(require 'winner)
(winner-mode t)

;; hide compile window on successful compile
(setq compilation-finish-functions 'brian-compile-finish)
(defun brian-compile-finish (buffer outstr)
  (cond ((string-match "grep" (buffer-name buffer))
	 nil)
	((string-match "finished" outstr)
	 (winner-undo)
	 t)
	(t
	 nil)))

;; StackOverflow Answer for Question:
;; Nice Work Brian ! :)
;; http://tinyurl.com/7epdvlz
(defun brian-comment-offset (langelem)
  (save-excursion
    (back-to-indentation)
    (cond ((re-search-forward (regexp-quote "//+") (point-at-eol) t)
	   '+)
	  (t
	   nil))))



;; Check if the line with the < contains any other
;; definitions/types, if so, base lineup of subsequent lines
;; on the starting column position of the type; 

;; e.g. template<    class X
;; lineup here:      ^

;; otherwise, add a c-basic-offset to the previous indentation
;; this is useful for something like:
;; template<
;; ,,class T
;; ...
;; example where c-basic-offset=2
	  
(defvar brian-c-lineup-template-closebracket 'under 
  "Control the indentation of the closing template bracket, >.
Possible values and consequences:
'under : Align directly under (same column) the opening bracket.
t      : Align at the beginning of the line (or current indentation level.
nil    : Align at the same column of previous types (e.g. col of class T).")

(defun brian-c-lineup-template--closebracket-p ()
  "Return t if the line contains only a template close bracket, >."
  (save-excursion 
    (beginning-of-line)
    ;; Check if this line is empty except for the trailing bracket, >
    (looking-at (rx (zero-or-more blank)
		    ">"
		    (zero-or-more blank)))))

(defun brian-c-lineup-template--pos-to-col (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun brian-c-lineup-template--calc-open-bracket-pos (langelem)
  "Calculate the column position of a template declaration opening bracket."
  (save-excursion 
    (c-with-syntax-table c++-template-syntax-table
      (goto-char (c-langelem-pos langelem))
      (1- (re-search-forward "<" (point-max) 'move)))))

(defun brian-c-lineup-template--calc-indent-offset (ob-pos)
  "Calculate the indentation offset for lining up types given the
opening bracket position, OB-POS."
  (save-excursion
    (c-with-syntax-table c++-template-syntax-table
      (goto-char (1+ ob-pos))
      (cond ((re-search-forward (rx 
				 (or "class"
				     "typename"
				     (one-or-more (not blank))))
				(c-point 'eol)
				'move)
	     (goto-char (match-beginning 0))
	     (current-column))
	    (t
	     (back-to-indentation)
	     (+ c-basic-offset (current-column)))))))

(defun brian-c-lineup-template-args (langelem)
  "Align template arguments and the closing bracket in a semi-custom manner."
  (let* ((ob-pos (brian-c-lineup-template--calc-open-bracket-pos langelem))
	 (ob-col (brian-c-lineup-template--pos-to-col ob-pos))
	 (offset (brian-c-lineup-template--calc-indent-offset ob-pos)))

    ;; Optional check for a line consisting of only a closebracket and
    ;; line it up either at the start of indentation, or underneath the
    ;; column of the opening bracket
    ;;(message (format "%d %d %d" ob-pos ob-col offset))
    (cond ((and brian-c-lineup-template-closebracket
		  (brian-c-lineup-template--closebracket-p))
	     (cond ((eq brian-c-lineup-template-closebracket 'under)
		    (vector ob-col))
		   (t
		    0)))
	    (t
	     (vector offset)))))

;; Customs for C and C++ Programming styles I like
(c-add-style 
 "briancpp" '((c-basic-offset . 2)
	      (c-comment-only-line-offset . 0)
	      (c-cleanup-list . (brace-else-brace
				 brace-elseif-brace
				 brace-catch-brace
				 empty-defun-braces
				 ;space-before-funcall
				 ))
	      (c-offsets-alist
	       (comment-intro . brian-comment-offset)
	       (defun-open . 0)
	       (defun-close . 0)
	       (arglist-close . c-lineup-arglist-close-under-paren)
	       (arglist-cont-nonempty . c-lineup-arglist)
	       (template-args-cont . brian-c-lineup-template-args)
	       (statement-block-intro . +)
	       (substatement-open . 0)
	       (substatement-label . 0)
	       (label . 0)
	       (statement-cont . +)
	       (inline-open . 0)
	       (inline-close . 0)
	       (innamespace . 0))))

(add-hook 'c++-mode-hook (lambda () 
			   (c-set-style "briancpp")
			   (define-key 
			     c++-mode-map 
			     (kbd "RET") 
			     'newline-and-indent)))


;; So I can use (require 'brian-config) elsewhere
(provide 'brian-config)
