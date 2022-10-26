(define-derived-mode argo-asl-mode
  python-mode "ARGO Schema Language Mode"
  "Major mode for asl files."
  (progn
    (require 'flycheck)
    (flycheck-mode -1)
    )
  )

(setq auto-mode-alist
      (append
       '((".*\\.asl\\'" . argo-asl-mode))
       auto-mode-alist))

(provide 'brian-argo)
