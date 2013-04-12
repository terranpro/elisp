(require 'cl)
(defun save-frame-configuration ()
  (let 
      ((fs)(f))
    (setq fs (loop for c in (cdr (current-frame-configuration)) 
		   collect (progn 
			     (setq f (cadr c))
			     (reduce (lambda (acc a) (if (find (symbol-name (car a)) '("top" "left" "width" "height") :test 'equal) (cons a acc) acc)) f :initial-value 'nil)
			     )))
    ;; fs contains a list of attribs for each frame
    (save-window-excursion
      (find-file "~/.e_last_frame_config.el")
      (erase-buffer)
      (print (cons 'version 1) (current-buffer))
      (print fs (current-buffer))
      (save-buffer)
      )
    ))

(defun load-frame-configuration ()
  "load the last saved frame configuration, if it exists"
  (let
      ((v) (fs))
    (if (file-exists-p "~/.e_last_frame_config.el")
        (save-window-excursion
          (find-file "~/.e_last_frame_config.el")
          (beginning-of-buffer)
          (setq v (read (current-buffer)))
          (if (not (and (equal 'version (car v)) (= 1 (cdr v))))
              (error "version %i not understood" (cdr v)))
          (setq fs (read (current-buffer)))
          (loop for f in fs do
                (make-frame f)))
      (message "~/.e_last_frame_config.el not found. not loaded"))))

