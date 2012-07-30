;; replaced w/load-path and require to try to eliminate a double load
;; error, incase other files do (require 'brian-cedet)
(add-to-list 'load-path "~/code/cedet/")
(require 'cedet-devel-load)

(add-to-list 'Info-default-directory-list
		 (expand-file-name "~/code/cedet-newtrunk/doc/info"))

(provide 'brian-cedet-minimal)
