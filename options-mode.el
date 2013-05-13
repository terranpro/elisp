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

(defun options-mode-build-window ())

(defclass Option () 
  ((key :initarg :key
	:initform nil)
   (desc :initarg :desc)
   (onactivate :initarg :onactivate)) 

  "A basic option!")

(defclass Options ()
  ((elems :initarg :elems))

  "A Group of Options")

(defmethod CreateKeymap ((obj Options))
  (with-slots (elems) obj 
    (let ((keymap (make-sparse-keymap)))
	(mapc #'(lambda (option)
		  (with-slots (key onactivate) option
		    (define-key keymap (kbd key) onactivate)))
	      elems)
	keymap)))

(CreateKeymap (Options "opts"
	       :elems
	       (list (Option "a" :key "q" :onactivate 'kill-buffer-and-window)
		     (Option "e" :key "RET" :onactivate 'switch-to-prev-buffer))))

(defmethod Redraw ((obj Option))
  "Redraw an individual Option"
  (format "%s!" (oref obj key)))

(defmethod Redraw ((obj Options))
  "Redraw an Options group"
  (with-slots (elems) obj
      (mapcar 'Redraw elems)))

(defmethod Redraw ((obj Switch))
  (with-slots (key desc active) obj
    (let ((key-face (if active 'ac-candidate-face 'ac-selection-face))
	  (desc-face 'ac-selection-face))
      (format "%s : %s" 
	      (propertize key 'face key-face)
	      desc))))

(defclass Switch (Option)
  ((active :initform nil
	   :initarg :active))
  "Options to represent command-line switches (e.g. --clean)")


(Redraw (Options "arf!" 
		 :elems
		 (list (Option "opt")
		       (Switch "switch" :key "--switch" :desc "amazing"))))

(oref (Switch "opt" :key "Magic" :desc "Forever!" :active t) active)

(Redraw (Option "opt"))


(defclass Command ()
  ((options :initform nil
	    :initarg :options)
   (command :initform nil
	    :initarg :command))
  "A wrapper around a command that requires extra options!")

(defmethod Invoke ((obj Command))
  "Invoke the command by dumping the current state of the options"
  (with-slots (command options) obj 
    (funcall command (Invoke options))))

(defmethod Invoke ((obj Options))
  "Systematically dump each option into an list for invocation by a command."
  (mapcar 'Invoke (oref obj elems)))

(defmethod Invoke ((obj Option))
  (with-slots (key) obj 
    `(,(make-symbol (concat ":"key)) . ,key)))

(symbol-value :key)
(make-symbol (concat ":" "key"))

(Invoke (Options "options"
		 :elems
		 (list (Option "opt1" :key "--clean")
		       (Option "opt2" :key "--noinit"))))



(Invoke (Command "gbs-build"
		 :command
		 'pp
		 :options 
		 (Options "options"
		 :elems
		 (list (Option "opt1" :key "--clean")
		       (Option "opt2" :key "--noinit")))))

(eieio-build-class-alist)

(object-assoc-list 'elems (list (Options "a" :elems "123") (Options "b" :elems "678")))

(defun options-redisplay (elems))

(define-derived-mode options-mode nil "Options" 
  "")

(defun options-mode-new (name cmd)
  (with-slots (command options) cmd
   (Options-p options)
   (switch-to-buffer-other-window (concat "Options: " name))
   (erase-buffer)
   (options-mode)
   (setq options-mode-map (CreateKeymap options))
   (use-local-map options-mode-map)
   (mapc '(lambda (str) (insert (format "%s\n" str)))
	 (Redraw options))))

(options-mode-new "gbs-build"
		  (Command "gbs-build"
			   :command
			   'pp
			   :options 
			   (Options 
			    "options"
			    :elems
			    (list (Option "opt1" 
					  :key "C"
					  :desc "--clean"
					  :onactivate 'kill-buffer-and-window)
				  (Switch "opt2"
					  :key "I"
					  :desc "--noinit"
					  :onactivate 'switch-to-prev-buffer)))))

(provide 'options-mode)

;;; options-mode.el ends here
