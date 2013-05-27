;;; Modified version due to bug when invoking mo-git-blame-file in
;;; Emacs 24.2.50.2
;; file-name was "DaliPTEWidget.cpp" but it looks like it expected it
;; to be /full/path/to/DaliPTEWidget.cpp aka root + DaliPTEWidget.cpp
 
(defun dir-locals-collect-variables (class-variables root variables)
  "Collect entries from CLASS-VARIABLES into VARIABLES.
ROOT is the root directory of the project.
Return the new variables list."
  (let* ((file-name (file-truename (buffer-file-name)))
	 (sub-file-name (if file-name
                            ;; FIXME: Why not use file-relative-name?
			    (substring file-name (length root)))))
    (condition-case err
        (dolist (entry class-variables variables)
          (let ((key (car entry)))
            (cond
             ((stringp key)
              ;; Don't include this in the previous condition, because we
              ;; want to filter all strings before the next condition.
              (when (and sub-file-name
                         (>= (length sub-file-name) (length key))
                         (string-prefix-p key sub-file-name))
                (setq variables (dir-locals-collect-variables
                                 (cdr entry) root variables))))
             ((or (not key)
                  (derived-mode-p key))
              (let* ((alist (cdr entry))
                     (subdirs (assq 'subdirs alist)))
                (if (or (not subdirs)
                        (progn
                          (setq alist (delq subdirs alist))
                          (cdr-safe subdirs))
                        ;; TODO someone might want to extend this to allow
                        ;; integer values for subdir, where N means
                        ;; variables apply to this directory and N levels
                        ;; below it (0 == nil).
                        (equal root default-directory))
                    (setq variables (dir-locals-collect-mode-variables
                                     alist variables))))))))
      (error
       ;; The file's content might be invalid (e.g. have a merge conflict), but
       ;; that shouldn't prevent the user from opening the file.
       (message ".dir-locals error: %s" (error-message-string err))
       nil))))
