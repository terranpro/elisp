(add-to-list 'custom-theme-load-path "~/elisp/foreign/themes")
(add-to-list 'load-path "~/elisp/foreign/themes")


(if (null (member 'assem custom-enabled-themes))
    (load-theme 'assem))

(provide 'brian-themes)
