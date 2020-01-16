(require 'smartthings)

(setq st-access-token "cKkGaVLO2mm6nIo93rfWYqEmW")
(setq st-curl-options '("-x" ""))

(st-locations-list :endpoint 'staging :output-key 'items)

(st-locations-get "26828566-f0cd-47e5-9532-86af11f33d02" :endpoint 'staging)
