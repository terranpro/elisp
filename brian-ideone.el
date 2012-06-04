(load-library "soap-client")
(load-library "soap-inspect")

;;(setq ideone-wsdl-url "http://ideone.com/api/1/service.wsdl")
;;(setq ideone-wsdl (soap-load-wsdl-from-url ideone-wsdl-url))

(setq ideone-wsdl (soap-load-wsdl "~/elisp/ideone.wsdl"))

(setq ideone-user "terranpro")
(setq ideone-pass "capp1234")

(defun ideone-enable-debug ()
  (setq soap-debug t))
;;(ideone-enable-debug)

(setq ideone-supported-languages 
      (soap-invoke ideone-wsdl "Ideone_Service_v1Port" "getLanguages" 
		   "terranpro" "capp1234"))

(assoc-string "languages" ideone-supported-languages t)

(setq ideone-testme '(("language" . "C") ("language" . "C++")))
(rassoc "C" ideone-supported-languages)

(length ideone-supported-languages)
(car (car ideone-supported-languages))

;; ((item (key . foo) (value
;; 	  . bar)(item (key . foo) (value bar))) to an alist you could write
(defun convert-pair (pair) `(,(cdr (third pair)) . ,(cdr (second
							  pair))))

(setq ideone-testme (mapcar 'convert-pair  (cdr (car ideone-supported-languages))))

(setq ideone-languages (cdr (car (cdr (car ideone-supported-languages)))))
(rassoc "" ideone-languages)
(setq ideone-lang-rdy (car ideone-languages))
(pop ideone-lang-rdy)
(setq ideone-lang-alist (mapcar 'convert-pair ideone-lang-rdy))

(assoc "Bash (bash 4.0.35)" ideone-lang-alist)

(defun convert-soap (data key-field value-field) 
  (loop for element in data
	collect (cons (cdr (assoc key-field element))
		      (cdr (assoc value-field element)))))

(convert-soap (car ideone-supported-languages) 'key 'value)
;;(setq ideone-testme2 (apply 'vector ideone-supported-languages))


(defun ideone-submit (cmd &rest args)
  (car (apply 'soap-invoke 
	      ideone-wsdl 
	      "Ideone_Service_v1Port" 
	      cmd 
	      ideone-user 
	      ideone-pass
	      args)))

(defun ideone-create-submission ()
  (interactive)
  (let ((code (buffer-substring-no-properties (point-min) 
					      (point-max)))
	(submission-result))
    (setq submission-result (car (soap-invoke ideone-wsdl 
					      "Ideone_Service_v1Port" 
					      "createSubmission" 
					      ideone-user 
					      ideone-pass
					      code
					      44
					      ""
					      t
					      nil)))
(message "%s" submission-result)))

(ideone-create-submission)

(defun ideone-submission-status (id) 
(let ((status-result))
  (setq submission-result (ideone-submit "getSubmissionStatus" 
					 id))
  
))

(defun ideone-submission-details (id)
(let ((details-result))
  (setq submission-result (ideone-submit "getSubmissionDetails" 
					 id
					 nil
					 nil
					 t
					 t
					 t))
))

(ideone-submission-status "9zx3q")
(setq ideone-result (ideone-submission-details "aw77o"))

;;(setq max-specdl-size 50000)
;; (setq debug-on-error t)
