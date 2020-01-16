(defun st-installedapps-list (locId &rest other)
  (apply #'st-request (concat "/installedapps?locationId=" locId) other))

(defun st-installedapps-get (instappid)
  (st-request (concat "/installedapps/" instappid)))

(defun st-installedapps-configs-list (instappid)
  (st-request (concat "/installedapps/" instappid "/configs")))

(provide 'st-installedapps)
