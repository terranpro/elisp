(require 'paren)
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(let ((lispinit (lambda () 
		  (paredit-mode +1)
		  (make-variable-buffer-local 'show-paren-mode)
		  (set (make-variable-buffer-local 'show-paren-style) 'parenthesis)
		  (show-paren-mode 1))))
  (add-hook 'emacs-lisp-mode-hook       lispinit)
  (add-hook 'lisp-mode-hook             lispinit)
  (add-hook 'lisp-interaction-mode-hook lispinit))

(provide 'brian-paredit)
