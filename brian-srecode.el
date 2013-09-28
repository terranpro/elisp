;;; brian-srecode.el --- 
;;
;; Copyright (C) 2012 Brian Fransioli
;;
;; Author: Brian Fransioli <assem@terranpro.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Setup essential directory structure, minor mode, and extra
;; functions used for srecode templates.  Mostly (currently) used with
;; C/C++.

;;; Code:

(require 'srecode)
(require 'srecode/map)

(setq srecode-map-save-file "~/elisp/srecode/srecode-map")
(if (boundp 'srecode-map-load-path)
    (add-to-list 'srecode-map-load-path "~/elisp/srecode/")
  (setq srecode-map-load-path "~/elisp/srecode/"))

(global-srecode-minor-mode 1)

(defun brian-srecode-projname-replace-and-comment (str)
  (let* ((dict srecode-inserter-variable-current-dictionary)
	 (projname (srecode-dictionary-lookup-name dict "PROJECTNAME"))
	 (cs (or (and dict
		      (srecode-dictionary-lookup-name dict "comment_prefix"))
		 (and comment-multi-line comment-continue)
		 (and (not comment-multi-line) comment-start)))
	 (strs (split-string str "\n"))
	 (newstr "")
	 )
    (while strs
      (cond ((and (not comment-multi-line) (string= (car strs) ""))
	     )
	    (t
	     (setq newstr (concat newstr cs " " (car strs)))))
      (setq strs (cdr strs))
      (when strs (setq newstr (concat newstr "\n"))))

    (setq newstr (replace-regexp-in-string "{{PROJECTNAME}}" 
					   projname 
					   newstr
					   t))
    newstr))

(defun brian-srecode-string-killer (str)
  "")

;;(define-key srecode-mode-map (kbd "C-c / U") 'srecode-map-update-map)

(defun brian-srecode-dash-2-underscore (str)
  (while (string-match (regexp-quote "-")
		       str)
    (setq str (replace-match "_" nil nil str)))
  str)

(defun brian-srecode-filename-symbol (str)
  (upcase (brian-srecode-dash-2-underscore str)))

(provide 'brian-srecode)

;;; brian-srecode.el ends here
