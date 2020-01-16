(require 'smartthings)
(require 'st-locations)
(require 'st-installedapps)

(setq st-access-token "a1a9da6e-7bb5-4636-a0c5-e65b5b24159c")
(setq st-curl-options '("-x" ""))


(st-installedapps-list "307e5473-72db-4472-84a7-d3f3470f830d")

(setq st-scrap
      (cl-loop for ((locationId . locId) . (name . locName)) across (st-locations-list)
	       for appinfo = (st-installedapps-list locId :endpoint 'prod)
	       vconcat appinfo))



(setq st-scrap
      (cl-loop for ((locationId . locId) . (name . locName)) across (st-locations-list)
	       vconcat (st-installedapps-list locId :endpoint 'prod)))
