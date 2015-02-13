(require 'auto-complete)

;; swank/slime
(load-file (expand-file-name "~/quicklisp/slime-helper.el"))
(executable-find "sbcl")
(setq inferior-lisp-program "sbcl")
;; TODO why couldn't i find this?! why did i put this here?!
;;(require 'swank)
(require 'slime)

(add-to-list 'load-path "~/elisp/foreign/ac-slime")
(require 'ac-slime)

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(provide 'brian-slime)
