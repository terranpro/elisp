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


(require 'flycheck)


(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'lsp-mode)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

(require 'tree-sitter)
(require 'tree-sitter-langs)

;; increase data emacs reads from process to 1Mb
;; default of 4k too low
;; some LSP responses may be 800k to 3Mb range
(setq read-process-output-max (* 1024 1024))

;; recommended by lsp-mode since client/server generates
;; lots of garbage  (100 Mb)
(setq gc-cons-threshold (* 100 1024 1024))

;; set default clangd options
;; limit the number of threads so it doesn't nuke my CPU
(setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "--pch-storage=memory"))

(setq lsp-clangd-binary-path "/home/bfransioli/av/argo/scripts/vscode-clangd.sh")

;; (add-to-list 'load-path "~/elisp/foreign/company-lsp")
;; (require 'company-lsp)
;; (push 'company-lsp company-backends)

;;(setq lsp-clients-clangd-executable "/home/terranpro/minis/llvm/bin/clangd")

;;(add-to-list 'load-path "~/code/emacs-ccls")
(require 'ccls)
;; (setq ccls-executable "~/git/ccls/Release/ccls")
;; (setq ccls-executable "docker exec bazel-av ~/git/ccls/Release/ccls")
(setq ccls-executable "~/git/ccls/Release/docker-ccls")

(setq ccls-args `((concat "--log-file=/tmp/ccls-" (int-to-string (random 32656)) ".log")))
;;(setq ccls-args nil)
(setq ccls-initialization-options '(
				    :index
				    (:threads 3)
				    :clang
				    (:resourceDir
				     "/usr/lib/llvm-11/lib/clang/11.1.0"
				     :pathMappings
				     ["/code>/home/bfransioli/av"])))

;; (setq lsp-file-watch-ignored)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(provide 'brian-lsp)
