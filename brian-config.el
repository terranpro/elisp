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

;; Two ways of doing key mapping translation
;; the latter two lines only worked when using kbd
;; presumably because of the { } keys?
(global-set-key "\C-\M-^" 'enlarge-frame-height)
(global-set-key (kbd "C-M-}") 'enlarge-frame-width)
(global-set-key (kbd "C-M-{") 'shrink-frame-width)
;;

(setq brian-elisp-active t)
(message "Brian's ELISP Loaded.")

