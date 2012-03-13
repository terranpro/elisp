;; GOing to FucKinG MonsTeR SomE EMacS LiSp nOw~
;; eXtrEmE h4x0r1ng 2 m4k3 s1k m0n3y.
;; assem

(setq tinypaste-username "terranpro")
(setq tinypaste-password "28558d0eb5bb988b0df76eae36a4446d")

(define-button-type 'tinypaste-button
  'action 'browse-url-at-point
  'follow-link nil)

(setq tinypaste-create-template "
<?php
$user = \"terranpro\";
$password = \"28558d0eb5bb988b0df76eae36a4446d\";

require(\"/home/terranpro/elisp/tinypaste.api.php\");
$api = new TinyPaste('create');
$api->authenticate($user, $password);
$contents = file_get_contents(\"/tmp/tinypaste.contents\");
$api->isCode(true);
$api->paste(\"$contents\");
$array = $api->execute();
print_r($array);
")

;; BROKEN! FVCK!
;; long cpp files apparently with quotes in them break this shit!
(defun tinypaste-create ()
  (interactive)
  (let* ((thepaste (buffer-substring-no-properties (point-min) (point-max)))
	 (output-buffer "Brian*TinyPaste*")
	 (input-file "/tmp/tinypaste-create.php")
	 (contents-file "/tmp/tinypaste.contents")
	 (command-string (concat "php " input-file))
	 (output-url))
    (with-temp-buffer
      (insert thepaste)
      ;; (goto-char (point-min))
      ;; (while (search-forward "\"" nil t)
      ;; 	(replace-match "O&#39;" t t))
      ;; (setq thepaste (buffer-substring-no-properties (point-min) (point-max))))
      (write-file contents-file nil)
    (with-temp-buffer
      (insert tinypaste-create-template)
      ;; (goto-char (point-min))
      ;; (search-forward "MYNEWPASTE" nil t)
      ;; (replace-match thepaste t t)
      (write-file input-file nil)
      (save-excursion
	(shell-command command-string 
		       output-buffer
		       )))

    (setq output-url (tinypaste-find-result-url))
    (message "TinyPaste Created @ %s" output-url) 
    ;; (insert-text-button output-url
    ;; 			:action 'browse-url)
)))

;;(shell-command "php /tmp/tinypaste-create.php" "Brian*TinyPaste*")

(defun tinypaste-find-result-url ()
"Scans the output of a successful tinypaste POST and returns the
full URL, ex: http://tinypaste.com/abcde13456"
  (interactive)
  (let ((output-buffer "Brian*TinyPaste*")
	(result-regexp "=>[[:space:]]*[[:alnum:]]*")
	(flush-regexp "=>[[:space:]]*Array")
	(output-url "http://tinypaste.com/"))

    (save-excursion
      (switch-to-buffer output-buffer)
      (goto-char (point-min))
      (flush-lines flush-regexp)
      (search-forward-regexp result-regexp)
      (setq output-url (concat output-url (thing-at-point 'word)))
      (switch-to-prev-buffer))
    output-url))
