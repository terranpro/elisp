(add-to-list 'load-path "~/code/company-mode/")
(add-to-list 'load-path "~/code/pythonic/")
(add-to-list 'load-path "~/code/anaconda-mode/")
(add-to-list 'load-path "~/code/company-anaconda/")

(add-to-list 'load-path "~/code/emacs-jedi/")
(add-to-list 'load-path "~/code/emacs-company-jedi/")
(add-to-list 'load-path "~/code/emacs-deferred/")
(add-to-list 'load-path "~/code/emacs-ctable/")
(add-to-list 'load-path "~/code/emacs-epc/")
(add-to-list 'load-path "~/code/emacs-python-environment/")

(add-to-list 'load-path "~/code/f.el")
(add-to-list 'load-path "~/code/s.el")
(add-to-list 'load-path "~/code/dash.el")

(require 'f)
(require 's)
(require 'dash)

(require 'company)
;; (require 'company-jedi)

;;(require 'brian-rtags)

;;(require 'company-rtags)
(require 'pythonic)
(require 'anaconda-mode)

(require 'company-anaconda)

;;(add-to-list 'company-backends 'company-rtags)

(add-to-list 'company-backends 'company-anaconda)


;; (defun brian/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))

;; (add-hook 'python-mode-hook 'brian/python-mode-hook)

(add-hook 'python-mode-hook 'anaconda-mode)


(define-key company-mode-map [(control tab)] 'company-complete)
(define-key company-mode-map [(control return)] 'company-complete)

(global-company-mode)

(provide 'brian-company-mode)
