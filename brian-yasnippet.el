(add-to-list 'load-path "~/code/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle

(setq yas/snippet-dirs (list "~/elisp/brian-snippets" 
			     "~/code/yasnippet/snippets"))
(mapcar 'yas/load-directory yas/snippet-dirs)
(yas/initialize)

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (make-variable-buffer-local 'yas/trigger-key)
	    (setq yas/trigger-key [tab])
	    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
	    (define-key yas/keymap [tab] 'yas/next-field)))


(provide 'brian-yasnippet)
