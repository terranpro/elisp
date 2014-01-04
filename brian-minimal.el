;; Minimal loads so I can use separate emacs for:
;; coding (primary)
;; gnus
;; irc
;; etc... more to come...
(add-to-list 'load-path "~/elisp/")
(add-to-list 'load-path "~/elisp/foreign")
;; TODO: temp hack to subvert problems with other libs using gnus
;; before we load the custom version
(add-to-list 'load-path "~/elisp/foreign/gnus/lisp")

(require 'brian-config)
(require 'brian-paredit)
;(require 'brian-themes)

;; done
(provide 'brian-minimal)
