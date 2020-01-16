(add-to-list 'load-path "~/code/elisp/foreign/smartthings")
(require 'smartthings)
(require 'st-apps)

(setq st-access-token "b3kGjPdbGGfjKRHQCOiorx547")
(setq st-curl-options '("-x" ""))

(st-apps-list :endpoint 'staging)


(setq st-scrap
      (cl-loop for applist across (st-apps-list :endpoint 'prod)
	       vconcat (list (st-apps-get (cdr (assoc 'appId applist))))))

(st-apps-get "financeserviceplugin" :endpoint 'prod)

(st-apps-update "financeserviceplugin"
		'((appName . "financeserviceplugin")
		  (appId . "4555fe50-989f-4d4c-87d4-22ab5ebcc65a")
		  (appType . "LAMBDA_SMART_APP")
		  (classifications .
				   [])
		  (displayName . "financeserviceplugin")
		  (description . "Plugin For Finance Service!")
		  (singleInstance . t)
		  (installMetadata)
		  (owner
		   (ownerType . "USER")
		   (ownerId . "79c1d6e2-d1b9-4adf-b9cf-188f98dced82"))
		  (createdDate . "2018-05-03T08:10:11Z")
		  (lastUpdatedDate . "2018-07-18T08:50:39Z")
		  (lambdaSmartApp
		   (functions .
			      ["arn:aws:lambda:us-east-1:247376929319:function:finance_service_endpoint"]))
		  (ui
		   (pluginId . "com.example.service.plugin.finance")
		   (dashboardCardsEnabled . t)
		   (preInstallDashboardCardsEnabled . :json-false))))
(st-apps-create "apiunittest999"
		"Automated Unit Testing"
		"API Unit Testing")

(st-apps-list)

(st-apps-delete "1b074c82-b5fa-459c-87be-a0e8f6ff33e8")

(st-apps-list)
