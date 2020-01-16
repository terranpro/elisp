(require 'smartthings)

(defun st-devices-get (devid &rest other)
  (apply #'st-request (concat "/devices/" devid) other))

(defun st-devices-status (devid &rest other)
  (apply #'st-request (concat "/devices/" devid "/status") other))

(defun st-devices-component-status (devid comp &rest other)
  (apply #'st-request (concat "/devices/" devid "/components/" comp "/status") other))

(defun st-devices-capability-status (devid comp cap &rest other)
  (apply #'st-request (concat "/devices/" devid "/components/" comp "/capabilities/" cap "/status") other))

(provide 'st-devices)
