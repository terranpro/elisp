(defvar brian-rtags-root (expand-file-name "~/code/rtags/"))

(add-to-list 'load-path (concat brian-rtags-root "src/"))

(require 'rtags)

(setq rtags-path (concat brian-rtags-root "build/"))

(rtags-enable-standard-keybindings c-mode-base-map)
(rtags-enable-standard-keybindings)

(setq rtags-rc-log-enable t)

(provide 'brian-rtags)




