(add-to-list 'load-path "~/elisp/foreign/yasnippet")

(defun brian-yasnippet-indent-buffer ()
  (when (derived-mode-p 'prog-mode)
    (indent-region (point-min) (point-max))))

;; used for gobj header and source snippets
(defun brian-un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is an underscore \"_\".
    If third argument START is non-nil, convert words after that
    index in STRING."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (< (progn (subword-downcase 1) (point)) (point-max))
      (insert (or sep ?_)))
    (upcase (buffer-substring-no-properties (point-min) (point-max)))))

(require 'yasnippet) ;; not yasnippet-bundle

(setq yas-snippet-dirs (list "~/elisp/brian-snippets" 
			     "~/elisp/foreign/yasnippet/snippets"))
(mapcar 'yas-load-directory yas-snippet-dirs)
;;(yas/initialize)
(yas-global-mode t)

(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (make-variable-buffer-local 'yas-trigger-key)
	    ;(setq yas/trigger-key [tab])
	    ;(setq yas/trigger-key "<tab>")
	    (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
	    (define-key yas-keymap [tab] 'yas-next-field)))

;;;; TODO: this was bad; it broke shit when expanding snippets in code
;;;; where i used C-g
;; (add-hook 'yas-after-exit-snippet-hook 'brian-yasnippet-indent-buffer)

(provide 'brian-yasnippet)
