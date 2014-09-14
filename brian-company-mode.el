(add-to-list 'load-path "~/code/company-mode/")

(require 'company)

(require 'brian-rtags)

(require 'company-rtags)

(add-to-list 'company-backends 'company-rtags)
(define-key company-mode-map [(control tab)] 'company-complete)
(define-key company-mode-map [(control return)] 'company-complete)

(global-company-mode)

(provide 'brian-company-mode)
