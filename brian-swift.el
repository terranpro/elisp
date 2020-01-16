(add-to-list 'load-path "~/code/lsp-sourcekit")
(require 'lsp-sourcekit)
(setenv "SOURCEKIT_TOOLCHAIN_PATH" "/home/terranpro/minis/swift")
(setq lsp-sourcekit-executable "/home/terranpro/code/sourcekit-lsp/.build/x86_64-unknown-linux/debug/sourcekit-lsp")

(add-hook 'swift-mode-hook #'lsp)

(provide 'brian-swift)
