(require 'smartthings)

(defun st-deviceprofiles-list (&rest other)
  (apply #'st-request (concat "/deviceprofiles/") other))

(defun st-deviceprofiles-get (profid &rest other)
  (apply #'st-request (concat "/deviceprofiles/" profid) other))

(defun st-deviceprofiles-create (body &rest other)
  (apply #'st-request (concat "/deviceprofiles/") :type "POST" :data body other)
  ;; (st-request (concat "/deviceprofiles/")
  ;; 	      :type "POST"
  ;; 	      :data body
  ;; 	      :endpoint 'staging)
  )

(defun st-deviceprofiles-update (profid body &rest other)
  ;;(apply #'st-request (concat "/deviceprofiles/" profid) :type "PUT" :data body other)
  (st-request (concat "/deviceprofiles/" profid)
	      :type "PUT"
	      :data body
	      :endpoint 'staging))

(defun st-deviceprofiles-delete (profid &rest other)
  (apply #'st-request (concat "/deviceprofiles/" profid) :type "DELETE" other))
