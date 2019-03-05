(add-to-list 'load-path "~/code/kotlin-mode")

(setq kotlin-command
      "/home/terranpro/.AndroidStudio3.2/config/plugins/Kotlin/kotlinc/bin/kotlinc")

(require 'kotlin-mode)


(add-to-list 'load-path "~/code/lsp-intellij")
;; this is broken with newest version of lsp RIP
(with-eval-after-load 'lsp-mode
  (require 'lsp-intellij)
  (add-hook 'java-mode-hook #'lsp-intellij-enable))

(provide 'brian-kotlin)
