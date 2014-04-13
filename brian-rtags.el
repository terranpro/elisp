(defvar brian-rtags-root (expand-file-name "~/elisp/foreign/rtags/"))

(add-to-list 'load-path (concat brian-rtags-root "src/"))

(require 'rtags)
(require 'rtags-ac)

(setq rtags-path (concat brian-rtags-root "build/"))

(rtags-enable-standard-keybindings c-mode-base-map)
(rtags-enable-standard-keybindings)

(setq rtags-rc-log-enable t)

(setq rtags-completions-enabled t)

(defun rtags-ac-cc-mode-setup ()
  ;; (setq ac-sources '(ac-source-rtags))
  (add-to-list 'ac-sources 'ac-source-rtags)
  )

(add-hook 'c-mode-common-hook 'rtags-ac-cc-mode-setup t)

(provide 'brian-rtags)




