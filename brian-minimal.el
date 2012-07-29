;; Minimal loads so I can use separate emacs for:
;; coding (primary)
;; gnus
;; irc
;; etc... more to come...
(add-to-list 'load-path "~/elisp/")

(require 'brian-config)
(require 'brian-paredit)
;; done
(provide 'brian-minimal)
