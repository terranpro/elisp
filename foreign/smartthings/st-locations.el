(defun st-locations-list (&rest other)
  (apply #'st-request "/locations" other))


(defun st-locations-get (locid &rest other)
  "Get info on a location by `locid'"
  (apply #'st-request (concat "/locations/" locid) other))

(provide 'st-locations)
