;; Brian's LISP Code
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
  (set-frame-height (selected-frame) 131)
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

;; Reopen files/buffers from previous session on startup
(desktop-save-mode 1)

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

;TODO: (temp) some default tweaking for window sizing
(add-to-list 'default-frame-alist '(height . 100))

; Emacs24 default font on one of my debian sids was 
; fucking stupid huge; so put this so the default font is 
; size 10pt.
(set-face-attribute 'default nil :height 90)
;(set-face-attribute 'default nil :font "")

; Hotkey for compiling like in other IDEs
(global-set-key (kbd "<f5>") 'compile)

;; I Like Autofill mode for all files
(auto-fill-mode t)

;; Never use backup files (garbage with tildes at end)
(setq make-backup-files nil)

;; TODO create a brian-alias.el file later if this grows:
;; My aliases to make emacs even more amazing
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer

;; Customs for C and C++ Programming styles I like
(c-add-style 
 "briancpp" '((c-basic-offset . 2)
	    (c-comment-only-line-offset . 0)
	    (c-offsets-alist
	     (defun-open . 0)
	     (defun-close . 0)
	     (statement-block-intro . 0)
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

(message "Brian's ELISP Loaded.")

;; So I can use (require 'brian-config) elsewhere
(provide 'brian-config)
