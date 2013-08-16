;;; options-mode.el --- 
;;
;; Copyright (C) 2013 Brian Fransioli
;;
;; Author: Brian Fransioli <terranpro@triforce>
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
;; A simple library to assist with mini-windows used for options for
;; various commands (post invocation but pre evaluation); aiming for
;; simple but elegant (pretty!).
;;
;;; Example Usage:
;;
;; Kick Ass!

;;; Code:

(require 'eieio)

(defun options-mode-build-window ())

(defclass Option () 
  ((hidden :initarg :hidden
	   :initform nil)
   (display-name :initarg :display-name
		 :initform nil)
   (key :initarg :key
	:initform nil)
   (desc :initarg :desc
	 :initform "")
   (face :initarg :face
	 :initform 'ac-candidate-face)
   (auto :initarg :auto 
	 :initform t)
   (onactivate :initarg :onactivate
	       :initform nil)
   (userdata :initarg :userdata
	     :initform nil))

  "A basic option!
HIDDEN will effectively hide the option from being displayed in the window.
KEY should be a bindable key that `kbd' recognizes.
DESC is a description that will be rendered in the option window for guidance.
FACE is the default face to use when displaying in the option window (TODO).
ONACTIVATE is the user callback that is called when the option activated.
The one argument passed to the callback is the Option obj. ")

(defvar NewLineOption (Option "newline"
			      :display-name "\n"
			      :auto nil))

(defvar NullOption (Option "null"
			   :display-name ""
			   :auto nil))

(defmethod Redraw ((obj Option))
  "Redraw an individual Option"
  (with-slots (hidden display-name) obj 
    (format "%s" (if hidden
		     "" 
		   (or display-name (object-name-string obj))))))

(defclass Options ()
  ((elems :initarg :elems)
   (separator :initarg :separator
	      :initform "\n"))

  "A Group of Options")

(defmethod Redraw ((obj Options))
  "Redraw an Options group"
  (with-slots (elems separator) obj
    (mapc #'(lambda (option) 
	      (insert (propertize 
		       (format "%s%s" (Redraw option) separator)
		       'option
		       option)))
	  elems)))

(defmethod SearchName ((obj Options) name)
  (with-slots (elems) obj
   (cdr (assoc name (object-assoc-name elems)))))

(defclass Switch (Option)
  ((active :initform nil
	   :initarg :active)
   (active-face :initform 'ac-selection-face
		:initarg :active-face))
  "Options to represent command-line switches (e.g. --clean)")

(defmethod Redraw ((obj Switch))
  (with-slots (key desc active face active-face display-name) obj
    (let ((key-face (if active active-face face))
	  (name-face (if active active-face 'default))
	  (desc-face 'ac-candidate-face))
      (format "%s : %s %s" 
	      (if key 
		  (propertize key 'face key-face)
		" ")
	      (propertize 
	       (or display-name (object-name-string obj))
	       'face name-face)
	      (if (and desc (not (string= "" desc)))
		  (concat " [ " desc " ] ")
		"")))))

(defmethod IsActive ((obj Switch))
  "Returns if a Switch object is active (toggleable status by user)"
  (oref obj active))

(defclass SwitchArg (Option)
  ((arg :initform ""
	:initarg :arg))

  "A Switch that also has a trailing arg (e.g. --profile SLP")

(defmethod Redraw ((obj SwitchArg))
  (with-slots (key face arg) obj
    (let ((objstr (call-next-method)))
      (format "%s : %s %s" 
	      (propertize key 'face face)
	      (propertize (object-name-string obj) 'face face)
	      arg))))

(defclass Command ()
  ((options :initform nil
	    :initarg :options)
   (command :initform nil
	    :initarg :command)
   (help-string :initarg :help-string
		:initform ""))
  "A wrapper around a command that requires extra options!")

(defmethod Redraw ((obj Command))
  (with-slots (options help-string) obj
    (insert (format "%s\n\n%s\n\n"
		    (object-name-string obj)
		    help-string))
    (Redraw options)))

(defmethod CreateKeymap ((obj Options))
  (with-slots (elems) obj 
    (let ((keymap (make-sparse-keymap)))
      (mapc #'(lambda (option)
		(with-slots (key onactivate) option
		  (lexical-let ((option-lex option))
		    (when key
		      (define-key keymap (kbd key) 
			(lambda () 
			  (interactive)
			  (Activate option-lex)
			  (options-redisplay)))))))
	    elems)
      keymap)))

(defmethod CreateKeymap ((obj Command) cmd-invoker)
  (with-slots (options command) obj
    (let ((keymap (CreateKeymap options)))
      (define-key keymap (kbd "SPC")
	'(lambda () (interactive)
	   (when (get-text-property (point) 'option)
	     (Activate (get-text-property (point) 'option))
	     (options-redisplay))))
      (define-key keymap (kbd "RET") 
	(symbol-function cmd-invoker))
      (define-key keymap (kbd "M-n") 'options-forward)
      (define-key keymap (kbd "M-p") 'options-backward)
      (define-key keymap (kbd "n") 'options-forward)
      (define-key keymap (kbd "p") 'options-backward)
      (define-key keymap (kbd "q")
	'(lambda () (interactive)
	   (switch-to-prev-buffer)))
      (define-key keymap (kbd "Q")
	'(lambda () (interactive) 
	   (switch-to-prev-buffer)))
      keymap)))

(defmethod Invoke ((obj Command))
  "Invoke the command by dumping the current state of the options"
  (with-slots (command options) obj 
    (funcall command (Invoke options))))

(defmethod Invoke ((obj Options))
  "Systematically dump each option into an list for invocation by a command."
  (mapcar 'Invoke (oref obj elems)))

(defmethod Invoke ((obj Option))
  "Return the option if active or an empty option"
  (with-slots (key) obj 
    `(,(make-symbol (concat ":"key)) . ,key)))

(defmethod Activate ((obj Option))
  (with-slots (onactivate) obj
    (when (functionp onactivate) 
      (funcall onactivate obj))))

(defmethod Activate ((obj Switch))
  (with-slots (onactivate) obj
    (oset obj active (not (oref obj active)))
    (call-next-method)))

(defmethod Activate ((obj SwitchArg))
  (with-slots (onactivate arg) obj
    (message (format "Entered SwitchArg Activate! %s" (oref obj arg)))
    (oset obj arg (call-next-method))))

(defmethod BuildOption ((obj Option))
  ""
  "")

(defmethod BuildOption ((obj Switch))
  (format "%s" 
	  (if (and (oref obj active)
		   (oref obj auto))
	      (object-name-string obj)
	    "")))

(defmethod BuildOption ((obj SwitchArg))
  (if (oref obj auto)
      (format "%s %s" (object-name-string obj) (oref obj arg))
    ""))

(defmethod BuildOptions ((obj Options))
  (with-slots (elems) obj
    (mapcar 'BuildOption elems)))

(object-assoc-list 'elems (list (Options "a" :elems "123") (Options "b" :elems "678")))

(defun object-assoc-name (list)
  "Return an association list with the object name as the key element.
LIST must be a list of objects with names.
This is useful when you need to do completing read on an object group."
  (eieio--check-type listp list)
  (let ((assoclist nil))
    (while list
      (setq assoclist (cons (cons (object-name-string (car list))
				  (car list))
			    assoclist))
      (setq list (cdr list)))
    (nreverse assoclist)))

(defun options-redisplay ()
  (let ((inhibit-read-only t)
	(old-line (line-number-at-pos (point))))
    (erase-buffer)
    (goto-char (point-min))
    ;(Redraw options-mode-options)
    (Redraw options-mode-command)
    (goto-char (point-min))
    (forward-line (1- old-line))))

(defun options-get-option-pos (optn)
  (let ((opt-table (get-text-property (point-min) 'options-table)))
    (aref optn opt-table)))

(defun options-build-table ()
)

(defun options-goto-prev-option ()
  (let ((p (previous-single-property-change (point) 'option)))
    (if p (goto-char p))))

(defun options-skip-p (option)
  (or (eq option NewLineOption)
      (eq option NullOption)))

(defun options-goto-next-option ()
  (let ((p (next-single-property-change (point) 'option)))
    (if p (goto-char p))))

(defun options-forward ()
  (interactive)
  (while (and (options-goto-next-option)
	      (options-skip-p (get-text-property (point) 'option)))))

(defun options-backward ()
  (interactive)
  (while (and (options-goto-prev-option)
	      (options-skip-p (get-text-property (point) 'option)))))

(define-derived-mode options-mode special-mode "Options" 
  "")

(defvar-local options-mode-command nil "")
(defvar-local options-mode-command-callback nil "")
(defvar-local options-mode-options nil "")

(defun options-mode-invoke-command ()
  (interactive)
  (funcall options-mode-command-callback
	   options-mode-options
	   (BuildOptions options-mode-options))
  ;(kill-buffer (current-buffer))
  )

(defun options-mode-new (name cmd)
  (with-slots (command options) cmd
    (when (Options-p options)
      (switch-to-buffer-other-window 
       (generate-new-buffer-name (concat "Options: " name)))
      (erase-buffer)
      (options-mode)
      (setq options-mode-map (CreateKeymap cmd 'options-mode-invoke-command))
      (setq options-mode-command cmd)
      (setq options-mode-command-callback command)
      (setq options-mode-options options)
      (use-local-map (CreateKeymap cmd 'options-mode-invoke-command))
      (options-redisplay))
    t))

;; (options-mode-new 
;;  "gbs-build"
;;  (Command "gbs-build"
;; 	  :command
;; 	  'tizen-gbs-build-worker
;; 	  :options 
;; 	  (Options 
;; 	   "options"
;; 	   :elems
;; 	   (list (Switch "--clean" 
;; 			 :key "C"
;; 			 :desc "Clean the GBS buildroot & cached pkgs"
;; 			 :onactivate '(lambda (opt)
;; 					(pp (oref opt active))))
;; 		 (Switch "--noinit"
;; 			 :key "N"
;; 			 :desc "Do not check the state of GBS buildroot; fast"
;; 			 :onactivate '(lambda (opt)
;; 					(message "")))

;; 		 (Switch "--keep-packs"
;; 			 :key "K"
;; 			 :desc "Keep unused packages in build root"
;; 			 :onactivate '(lambda (opt)
;; 					(message "Toggled Keep Packs")))

;; 		 (Switch "--include-all"
;; 			 :key "I"
;; 			 :desc "Include uncommited changes and untracked files"
;; 			 :onactivate '(lambda (opt)
;; 					(message "Toggled Include All")))
		 
;; 		 (SwitchArg "--profile"
;; 			    :key "P"
;; 			    :desc "Specify the GBS profile to be used"
;; 			    :arg "slp"
;; 			    :onactivate '(lambda (opt)
;; 					   (ido-completing-read 
;; 					    "Profile: "
;; 					    (list "slp" "surc" "latest")
;; 					    "slp")))
		 
;; 		 (SwitchArg "--arch"
;; 			    :key "A"
;; 			    :desc "Specify the GBS profile to be used"
;; 			    :arg "armv7l"
;; 			    :onactivate '(lambda (opt)
;; 					   (ido-completing-read 
;; 					    "Profile: "
;; 					    (list "armv7l" "i586")
;; 					    "armv7l")))))))

(provide 'options-mode)

;;; options-mode.el ends here
