(require 'flymake)

(add-hook 'find-file-hook 'flymake-find-file-hook)

(global-set-key (kbd "C-c e d") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-c e n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c e p") 'flymake-goto-prev-error)
