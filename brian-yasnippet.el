(add-to-list 'load-path "~/code/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle

(setq yas/snippet-dirs "~/code/yasnippet/snippets")
(yas/load-directory yas/snippet-dirs)
(yas/initialize)


