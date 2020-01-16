(defun st-apps-list (&rest other)
  (apply #'st-request "/apps" other))

(defun st-apps-get (appnameorid &rest other)
  (apply #'st-request (concat "/apps/" appnameorid) other))

(defun st-apps-create (appname
		       dispname
		       desc
		       &optional singleinstance iconurl
		       &rest other)
  (let ((payload `((appName . ,appname)
		   (displayName . ,dispname)
		   (description . ,desc)
		   (appType . "LAMBDA_SMART_APP")
		   (lambdaSmartApp
		    (functions . ["arn:aws:lambda:us-east-1:247376929319:function:serviceplugin_example_na"]))
		   (oauth
		    (clientName . "My SmartThings Integration")
		    (scope . [
			      "r:installedapps"
			      "w:installedapps"
			      ])))))
    ;; (apply #'st-request (concat "/apps")
    ;; 	   (list :type "POST"
    ;; 		 :data payload))
    (st-request (concat "/apps")
		:type "POST"
		:data payload)

    ))

(defun st-apps-delete (nameorid)
  (st-request (concat "/apps/" nameorid)
	      :type "DELETE"))

(defun st-apps-update (nameorid payload)
  (st-request (concat "/apps/" nameorid)
	      :type "PUT"
	      :data payload))

(provide 'st-apps)
