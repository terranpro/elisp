(setq python-shell-interpreter "python3")

(add-hook 'python-mode-hook
	  (lambda ()
	    ;;(setq-default indent-tabs-mode t)
	    ;;(setq-default tab-width 4)
	    ;;(setq-default py-indent-tabs-mode t)
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(provide 'brian-python)
