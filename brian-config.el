;; Brian's LISP Code
(setq ring-bell-function 'ignore)
(setq enable-local-eval t)


;; TODO: investigate
;; Had to add an extra two for one ubuntu linux machine that had three
;; split sizes of 80 : 79 : 79
(when (not (null (window-system)))

  (add-to-list
   'default-frame-alist
   `(width  . ,(+ 240 2
		  (/ (frame-parameter nil 'left-fringe) (frame-char-width))
		  (/ (frame-parameter nil 'right-fringe) (frame-char-width)))))

  (add-to-list
   'default-frame-alist
   `(height  . ,(- (/ (display-pixel-height) (frame-char-height))
		   2)))

  ;; Emacs24 default font on one of my debian sids was
  ;; fucking stupid huge; so put this so the default font is
  ;; size 10pt.
  (when (member "Ubuntu Mono 10" (font-family-list))
    (set-face-attribute 'default nil :font "Ubuntu Mono 10"))
  ;; (set-face-attribute 'default nil :font "")
  (set-face-attribute 'default nil :height 115))
(set-face-attribute 'default nil :height 145)

;TODO: (temp) some default tweaking for window sizing
;;(add-to-list 'default-frame-alist '(height . 65))
;;(add-to-list 'default-frame-alist '(width  . 80))
;; TODO: experiment with using 3 windows in one big frame!

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

;; buffer-move to 'assist' windmove
(require 'buffer-move)
(global-set-key (kbd "s-A")   'buf-move-left)
(global-set-key (kbd "s-D")  'buf-move-right)
(global-set-key (kbd "s-W")     'buf-move-up)
(global-set-key (kbd "s-S")   'buf-move-down)

;; pesky backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; smoother scrolling - no jumpiness
(setq scroll-conservatively 1000)
(setq next-screen-context-lines 1)
(setq scroll-preserve-screen-position t)

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
(set-face-attribute 'mode-line nil :foreground "purple4" :background "cyan2")
(set-face-attribute 'mode-line-inactive nil :foreground "seashell3")

;; expand region
(add-to-list 'load-path "~/elisp/foreign/expand-region")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Let X's clipboard play nice with emacs
(setq x-select-enable-clipboard t)

;; Remove eyecandy for mouse
;; we are one with the keyboard in emacs land
(scroll-bar-mode -1)        ;hide scroll-bar
(when (boundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

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

;; easily change my mind about open in new frame/window/split
(require 'ido-invoke)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
	      " [No match]" " [Matched]" " [Not readable]" " [Too big]"
	      " [Confirm]")))
;; Vertical completion display in ido!
(defun ido-disable-line-truncation ()
  (set
   (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-o") 'ido-invoke-in-other-window)
  (define-key ido-completion-map (kbd "C-2") 'ido-invoke-in-vertical-split)
  (define-key ido-completion-map (kbd "C-3") 'ido-invoke-in-horizontal-split)
  (define-key ido-completion-map (kbd "C-4") 'ido-invoke-in-other-window)
  (define-key ido-completion-map (kbd "C-5") 'ido-invoke-in-new-frame)

  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; TODO: want faster buffer switching than C-x b
;; but i'm only testing this key combo
(global-set-key (kbd "C-`") 'ido-switch-buffer)
(global-set-key (kbd "s-`") 'ido-switch-buffer)

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

;; tramp method to avoid stupid y/n question
(setq tramp-default-method "ssh")
(setq tramp-verbose 3)
;; (when (not (null 'tramp-remote-path))
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

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

;;; Follow-mode smarter!
;; Automagically use the window immediately to the right (if present)
;; to follow-mode scroll the current buffer; and if/when deactivated,
;; pop the window to the right to the previous buffer.
(defadvice follow-mode (before follow-buffer-to-right activate)
  (let ((rightwin (window-right (get-buffer-window)))
	(curr-buf (current-buffer)))
    (when (and follow-mode
	       rightwin)
      (with-selected-window rightwin
	(switch-to-prev-buffer)))

    (when (and (not follow-mode)
	       rightwin
	       (not (eq (window-buffer rightwin) curr-buf)))
      (message (format "Buffer: %s" curr-buf))
      (with-selected-window rightwin
	(switch-to-buffer curr-buf t t)))))

;; dired find file in new frame
(require 'dired)
(require 'dired-x)
(defun brian-dired-find-file-other-frame ()
  (interactive)
  (find-file-other-frame (dired-get-file-for-visit)))
(define-key dired-mode-map (kbd "F") 'brian-dired-find-file-other-frame)

;; human readable file sizes!
(setq dired-listing-switches "-alh")

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

;; google this
(add-to-list 'load-path "~/elisp/foreign/emacs-google-this")
(require 'google-this)
(global-set-key (kbd "C-x g") 'google-this-mode-submap)


;; dash
(add-to-list 'load-path "~/elisp/foreign/dash")

;;
;; Process Environment is so damn important...
;; Let's try just setting it here
;; (setq process-environment
;;  (append (list
;; 	  (concat "LD_LIBRARY_PATH="
;; 		  (mapconcat
;; 		   'identity
;; 		   (delete-dups
;; 		    (append
;; 		     (list (expand-file-name "~/build/lib")
;; 			   "/usr/local/lib")
;; 		     (split-string
;; 		      (or (getenv "LD_LIBRARY_PATH") "") ":" t)))
;; 		   ":")))
;; 	 (remove-if #'(lambda (item)
;; 			(string-match "^LD_LIBRARY_PATH=" item))
;; 		    process-environment)))

;; auto revert buffers (useful for magit branch changes/updates)
(global-auto-revert-mode)

;; ediff buffers in one frame - no new frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; echo keystrokes custom - seems better if faster than 1sec
(setq echo-keystrokes 0.1)

;; So I can use (require 'brian-config) elsewhere
(provide 'brian-config)
