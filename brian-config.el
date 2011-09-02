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
  (set-frame-height (selected-frame) 10000)
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

;TODO: (temp) some default tweaking for window sizing
(add-to-list 'default-frame-alist '(height . 100))

(message "Brian's ELISP Loaded.")

;; So I can use (require 'brian-config) elsewhere
(provide 'brian-config)
