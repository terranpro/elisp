(add-to-list 'load-path "~/elisp/foreign/smartthings")

(require 'smartthings)
(require 'st-devices)

(setq st-access-token "CDmRgEAvYnwnJ0DTV2FNnnUz7")
(setq st-access-token "a1a9da6e-7bb5-4636-a0c5-e65b5b24159c")
(setq st-curl-options '("-x" ""))

(st-devices-get "1ff8012d-21c3-43a7-83cc-601e4fe4f315")

(st-devices-status "13174461-90a8-435d-94d4-0124be3d2cd7")
;; http://account.samsung.com/mobile/account/check.do?actionID=StartOAuth2&serviceID=166135d296&countryCode=us&languageCode=en&accessToken=Y&svcIptLgnID=yourMailId&svcIptLgnPD=password

(st-devices-status "b3169915-4d34-4dfe-b34b-ea3493aa8900")
(st-devices-status "90e49224-763d-439a-ab1d-664fec6ecf1b")
(st-devices-status "1b391034-3fb3-4f25-894b-165921e577cf")
(st-devices-status "598d3291-9383-47d1-99e5-c53a7286b093")
(st-devices-status "66d99c96-5081-492d-84d9-be416b63700a")
(st-devices-status "73cf1576-faa4-49cf-8a64-4af5a9c30b40")
(st-devices-status "7e52e002-eb73-41ae-ba53-3b88347718fa")
(st-devices-status "91232ff3-311a-4f6b-a37a-5716b0256ff8" :endpoint 'staging)


(st-devices-get "73cf1576-faa4-49cf-8a64-4af5a9c30b40")
(st-devices-get "7e52e002-eb73-41ae-ba53-3b88347718fa")

(st-devices-get "01ec7ec9-dd84-4c43-9641-f172a0a190cc")
(st-devices-status "01ec7ec9-dd84-4c43-9641-f172a0a190cc")
(st-devices-component-status "01ec7ec9-dd84-4c43-9641-f172a0a190cc" "main")
(st-devices-capability-status "01ec7ec9-dd84-4c43-9641-f172a0a190cc" "main" "relativeHumidityMeasurement")


(st-devices-get "17e41eb3-7d1e-45af-b161-0b231a997a35")
(st-devices-status "17e41eb3-7d1e-45af-b161-0b231a997a35")

;; akash multicomponent dev
(st-devices-get "aa8b5223-207a-403a-adaf-a353a54e110e" :endpoint 'staging)
(st-devices-status "8495b269-9bea-4a98-bc25-7809ecbe866b" :endpoint 'staging)

;;akashmemeadmiral multicomponent dev
(st-devices-get "304ca881-4703-4270-93db-6ad7d0e7a1ff" :endpoint 'staging)
(st-devices-status "304ca881-4703-4270-93db-6ad7d0e7a1ff" :endpoint 'staging)

(st-deviceprofiles-list :endpoint 'staging)
(st-deviceprofiles-get "e7995f29-b9a9-48c6-bf70-5202d14de105" :endpoint 'staging)
(st-deviceprofiles-get "075c24c3-1d27-45b6-a412-9177dd591200" :endpoint 'staging)

;; akashmemeprivate profile
(st-deviceprofiles-get "c5b09082-4cc7-4b2a-aca6-bd706395253e" :endpoint 'staging)

;; akashtemperature profile
(st-deviceprofiles-get "32b2ef7d-b2ed-4ec6-885b-0472a1dbdc40" :endpoint 'staging)

(st-devices-get "06a2022f-95be-42d1-a02e-d83900f9642c" :endpoint 'staging)


(st-devices-status "b9012a09-3808-4735-9614-e1c3b9d9e115" :endpoint 'staging)

;; delete test
(st-deviceprofiles-delete "0c309f37-1e33-452f-8454-5a51bd3ba7b2" :endpoint 'staging)


(st-deviceprofiles-create
 '((name . "akashtemperature")
   (components .
	       [((id . "main")
		 (capabilities .
			       [
				((id . "temperatureMeasurement")
				 (version . 1))
				]))])
   (metadata
    (vid . "vidforakashtemperature")
    (deviceType . "Switch")
    (ocfDeviceType . "oic.d.switch")
    (mnmn . "Smartthings")))
 :endpoint 'staging)









(st-deviceprofiles-update
 "e7995f29-b9a9-48c6-bf70-5202d14de105"
 '((components .
	       [((id . "main")
		 (capabilities .
			       [((id . "switch")
				 (version . 1))
				((id . "BrianCompany.myCapability")
				 (version . 1))]))])
   (metadata
    (vid . "vidformulticomponentdevice")
    (deviceType . "Switch")
    (ocfDeviceType . "oic.d.switch")
    (mnmn . "Smartthings")))
 :endpoint 'staging)


(let* ((data t)
       (hdrs `(("Authorization" . ,(concat "Bearer " "hahayeshaha"))
	       ))
      )
  (if data (add-to-list 'hdrs '("Content-Type" . "application/json")))
  (pp hdrs))
