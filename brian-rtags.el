(defvar brian-rtags-root (expand-file-name "~/elisp/foreign/rtags/"))

(add-to-list 'load-path (concat brian-rtags-root "src/"))

(require 'brian-autocomplete)
(require 'auto-complete)

(require 'rtags)

(setq rtags-path (concat brian-rtags-root "build/"))

(rtags-enable-standard-keybindings c-mode-base-map)
(rtags-enable-standard-keybindings)

(setq rtags-rc-log-enabled t)

(setq rtags-completions-enabled t)

(defun rtags-ac-cc-mode-setup ()
  (setq ac-sources '(ac-source-rtags))
  ;;(add-to-list 'ac-sources 'ac-source-rtags)
  )

(defun my-rtags-start-process-maybe ()
  (let ((psout (shell-command-to-string "ps -ef | grep rdm | grep -v grep")))
    (when (and (stringp psout)
	       (= (length psout) 0))
      (rtags-start-process-maybe))))

;; (my-rtags-start-process-maybe)

(provide 'brian-rtags)

;; (require 'brian-company-mode)

(require 'rtags-ac)
(add-hook 'c-mode-common-hook 'rtags-ac-cc-mode-setup t)
