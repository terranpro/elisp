;;; brian-autocomplete.el --- 
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

(setq ac-dictionary-directories "~/elisp/foreign/auto-complete/dict_brian")
(add-to-list 'load-path "~/elisp/foreign/auto-complete")
(add-to-list 'load-path "~/elisp/foreign/auto-complete/lib/popup")
(add-to-list 'load-path "~/elisp/foreign/auto-complete/lib/fuzzy")
(add-to-list 'Info-default-directory-list
	     (expand-file-name "~/elisp/foreign/auto-complete/doc"))

(require 'auto-complete-config)
(ac-config-default)

;; Custom Configs
;; Fast Displays, Quick Helps, and Fuzzy
(setq ac-delay 0.15)
(setq ac-auto-start 2)
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 0.5)
(setq ac-auto-show-menu 0.25)
(setq ac-menu-height 30)
(setq ac-fuzzy-enable t)

;; Key Bindings
(define-key ac-mode-map [(control tab)] 'auto-complete)
(define-key ac-mode-map [(control return)] 'ac-fuzzy-complete)
(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-mode-map (kbd "C-c H") #'(lambda ()
					  (interactive)
					  (ac-last-help t)))

(define-key ac-complete-mode-map (kbd "M-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "M-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "C-s") 'ac-isearch)
(define-key ac-complete-mode-map (kbd "M-s") 'ac-isearch-doc)

(define-key ac-completing-map (kbd "M-n") 'ac-next)
(define-key ac-completing-map (kbd "M-p") 'ac-previous)

;; When we are using clang async, let's not use CEDET
;(remove-hook 'c-mode-common-hook 'my-cedet-hook)

(defun my-ac-config ()
  ;(add-hook 'c-mode-common-hook 'ac-cc-mode-setup t)
  ;(add-hook 'auto-complete-mode-hook 'ac-common-setup t)
  (global-auto-complete-mode t))

(my-ac-config)

;;
;; readline-complete
;;
(require 'shell)

;; (setq explicit-shell-file-name "bash")
;; (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
;; (setq comint-process-echoes t)
(setq comint-input-ignoredups t)
(setq comint-scroll-to-bottom-on-output nil)
(setq comint-scroll-to-bottom-on-input t)
(add-to-list 'explicit-bash-args "--login")

;; Awesome fix for shell COLUMNS being too large
;; from: SO
(defun comint-fix-window-size ()
  "Change process window size."
  (when (and (derived-mode-p 'comint-mode)
	     (processp (get-buffer-process (current-buffer))))
    (set-process-window-size (get-buffer-process (current-buffer))
                         (window-height)
                         (window-width))))
(defun my-shell-mode-hook ()
  ;; add this hook as buffer local, so it runs once per window.
  (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;; ASIDE: if you call ssh from shell directly, add "-t" to
;; explicit-ssh-args to enable terminal.

;; (add-to-list 'load-path "~/elisp/foreign/readline-complete.el/")
;; (require 'readline-complete)

;; (add-to-list 'ac-modes 'shell-mode)
;; (setq shell-mode-hook nil)
;; (add-hook 'shell-mode-hook 
;; 	  #'(lambda ()
;; 	      (setq comint-preinput-scroll-to-bottom t)
;; 	      (setq comint-move-point-for-output t)
;; 	      (setq comint-buffer-maximum-size 5000)
;; 	      (setq rlc-attempts 30)
;; 	      (setq rlc-timeout 0.03)
;; 	      (ac-rlc-setup-sources)))

(provide 'brian-autocomplete)

;;; brian-autocomplete.el ends here
