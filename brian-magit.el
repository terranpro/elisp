(add-to-list 'load-path "~/code/magit/")
(require 'magit)
(require 'magit-svn)

(add-to-list 'Info-default-directory-list
		 (expand-file-name "~/code/magit"))

(provide 'brian-magit)
