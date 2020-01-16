(add-to-list 'load-path "~/code/emacs-request")
(add-to-list 'load-path "~/elisp/foreign/smartthings")
(require 'request)

(defcustom st-access-token nil
  "Access Token which is required by Cloud API calls.")

(defcustom st-curl-options-default '("-L" "--trace-ascii" "/tmp/st-curl.log")
  "Default list of options to be added to curl API calls")

(defcustom st-curl-options nil
  "List of options to be added to curl API calls via `request-curl-options'")

(defcustom st-endpoint-servers '((prod . "https://api.smartthings.com")
				 (staging . "https://apis.smartthingsgdev.com")
				 (dev . "https://apid.smartthingsgdev.com"))
  "Alist of ST cloud endpoint servers")

(setq st-endpoint-servers '((prod . "https://api.smartthings.com")
				 (staging . "https://apis.smartthingsgdev.com")
				 (dev . "https://apid.smartthingsgdev.com")))
(cl-defun st-request (api &rest other
			  &key
			  (type "GET")
			  (version "/v1")
			  (endpoint 'prod)
			  (token st-access-token)
			  (data nil)
			  (sync t)
			  (parser 'json-read)
			  (output-key 'items)
			  )
  (let ((request-curl-options (concatenate 'list st-curl-options-default st-curl-options))
	(request-log-level 'debug)
	(request-message-level 'debug)
	(hdrs `(("Authorization" . ,(concat "Bearer " token))))
	out)
    (pp (format "endpoint: %s token: %s st-access-token: %s" endpoint token st-access-token))
    (pp data)
    (pp (json-encode data))
    (if data (add-to-list 'hdrs '("Content-Type" . "application/json")))
    (request
     (concat (cdr-safe (assoc endpoint st-endpoint-servers)) version api)
     :headers hdrs
     :type type
     :data (json-encode data)
     :parser parser
     :sync sync
     :success (cl-function
	       (lambda (&key data &allow-other-keys)
		 (pp data)
		 (if output-key
		     (setq out (or (cdr-safe (assoc output-key data))
				   data))
		   (setq out data)))))
    out))

(provide 'smartthings)
