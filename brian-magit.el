(add-to-list 'load-path "~/elisp/foreign/magit/")
(require 'magit)
(require 'magit-svn)

(add-to-list 'Info-default-directory-list
		 (expand-file-name "~/elisp/foreign/magit"))

(add-to-list 'load-path "~/elisp/external/mo-git-blame/")
(require 'mo-git-blame)

(provide 'brian-magit)
