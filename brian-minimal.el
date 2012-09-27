;; Minimal loads so I can use separate emacs for:
;; coding (primary)
;; gnus
;; irc
;; etc... more to come...
(add-to-list 'load-path "~/elisp/")
(add-to-list 'load-path "~/elisp/foreign")

(require 'brian-config)
(require 'brian-paredit)
(require 'brian-themes)

;; done
(provide 'brian-minimal)
