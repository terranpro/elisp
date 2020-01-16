(add-to-list 'load-path "~/code/dart-mode/")

(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
(autoload 'dart-mode "dart-mode")

(provide 'brian-dart)
