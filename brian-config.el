;; Brian's LISP Code
(setq ring-bell-function 'ignore)

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
(global-set-key (kbd "C-c M") 'maximize-frame-height)
;;

;; smoother scrolling - no jumpiness
(setq scroll-conservatively 1000)

;; ace jump
(add-to-list 'load-path "~/elisp/foreign/ace-jump-mode")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c p") 'ace-jump-mode)
(define-key global-map (kbd "C-c l") 'ace-jump-line-mode)
(define-key global-map (kbd "C-c C-p") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-l") 'ace-jump-line-mode)

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
(add-to-list 'load-path "~/code/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;TODO: (temp) some default tweaking for window sizing
(add-to-list 'default-frame-alist '(height . 65))
(add-to-list 'default-frame-alist '(width  . 80))

; Emacs24 default font on one of my debian sids was 
; fucking stupid huge; so put this so the default font is 
; size 10pt.
(set-face-attribute 'default nil :height 90)
;(set-face-attribute 'default nil :font "")

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

;; dired find file in new frame
(require 'dired)
(defun brian-dired-find-file-other-frame ()
  (interactive)
  (find-file-other-frame (dired-get-file-for-visit)))
(define-key dired-mode-map (kbd "F") 'brian-dired-find-file-other-frame)

;; TODO create a brian-alias.el file later if this grows:
;; My aliases to make emacs even more amazing
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer

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

;; Customs for C and C++ Programming styles I like
(c-add-style 
 "briancpp" '((c-basic-offset . 2)
	    (c-comment-only-line-offset . 0)
	    (c-offsets-alist
	     (comment-intro . brian-comment-offset)
	     (defun-open . 0)
	     (defun-close . 0)
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
