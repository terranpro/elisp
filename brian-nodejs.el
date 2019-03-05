(setq indium-chrome-executable "google-chrome")

;;(add-to-list 'load-path "~/code/company-tern")

(require 'company)
;;(require 'company-tern)
(require 'indium)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;;(add-to-list 'company-backends 'company-tern)

;; lsp requires npm package: javascript-typescript-langserver
(add-hook 'js2-mode-hook (lambda ()
                           ;;(tern-mode)
			   (lsp)
                           (company-mode)))

(provide 'brian-nodejs)
