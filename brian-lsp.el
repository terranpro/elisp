(add-to-list 'load-path "~/code/f.el")
(add-to-list 'load-path "~/code/s.el")
(add-to-list 'load-path "~/code/dash.el")

(require 'f)
(require 's)
(load-library "dash")

(require 'company)

(add-to-list 'load-path "~/elisp/foreign/ht.el")
(add-to-list 'load-path "~/elisp/foreign/spinner.el")
(add-to-list 'load-path "~/elisp/foreign/markdown-mode")
(add-to-list 'load-path "~/elisp/foreign/lsp-mode")

(add-to-list 'load-path "~/elisp/foreign/lsp-ui")


(add-to-list 'load-path "~/elisp/foreign/flycheck/")
(require 'flycheck)


(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'lsp-mode)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

(add-to-list 'load-path "~/elisp/foreign/company-lsp")
(require 'company-lsp)
(push 'company-lsp company-backends)

(setq lsp-clients-clangd-executable "/home/terranpro/minis/llvm/bin/clangd")

(provide 'brian-lsp)
