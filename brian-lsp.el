(add-to-list 'load-path "~/code/f.el")
(add-to-list 'load-path "~/code/s.el")
(add-to-list 'load-path "~/code/dash.el")

(require 'f)
(require 's)
(load-library "dash")

(require 'company)

(add-to-list 'load-path "/home/terranpro/code/ht.el")
(add-to-list 'load-path "/home/terranpro/code/lsp-mode")

(add-to-list 'load-path "/home/terranpro/code/lsp-ui")


(add-to-list 'load-path "~/code/flycheck/")
(require 'flycheck)


(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'lsp-mode)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

(add-to-list 'load-path "~/code/company-lsp")
(require 'company-lsp)
(push 'company-lsp company-backends)

(setq lsp-clients-clangd-executable "/home/terranpro/minis/llvm/bin/clangd")

(provide 'brian-lsp)
