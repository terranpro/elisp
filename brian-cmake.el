;; cmake-mode.el load configurations and hotkeys
;;

;;(setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path))
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

(define-prefix-command 'cmake-mode-map 'cmake-mode-map "CMake")
(global-set-key (kbd "C-C C") 'cmake-mode-map)
(define-key cmake-mode-map (kbd "h") 'cmake-help-command)

(provide 'brian-cmake)
