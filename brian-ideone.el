(load-library "./ideone/ideone.el")

(setq ideone-user "terranpro")
(setq ideone-pass "capp1234")
;;(setq ideone-pass (read-passwd "IDEone API PW: "))
(ideone-enable-debug)
(ideone-init)

;; (ideone-submission-details "mxV38")
;; (ideone-submission-status "mxV38")
;; (ideone-show-output (ideone-invoke "getSubmissionStatus" "mxV38"))
;; (ideone-submission-details "v1DJh")
;; (ideone-submission-details "dstvf")
;; (use-region-p)
;; (while (not (= 1 0))
;;   (message "Compiling..."))
;; (setq ideone-recent-submissions)
;; (length ideone-recent-submissions)
;; (mapcar '(lambda (c) (car c)) ideone-recent-submissions)
;; http://ideone.com/R5SCg
;; http://ideone.com/X8YxL

;; Test snippets for various languages
;; http://ideone.com/2i50R
;; http://ideone.com/NGuRV
;; http://ideone.com/eNT8B
;; http://ideone.com/ctsjh
;; http://ideone.com/nSo68
;; http://ideone.com/t3Jfx

(provide 'brian-ideone)

