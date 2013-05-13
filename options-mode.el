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
  ((key :initarg :key)
   (desc :initarg :desc)
   (onactivate :initarg :onactivate)) 

  "A basic option!")

(make-instance 'option-element :name nil)

(option-element "noinit" :key "I" :desc "--noinit")


(format "%s" (kbd "C-c"))

(defclass Options ()
  ((arf)
   (elems :initarg :elems))

  "A Group of Options")
(defmethod Redraw ((obj Options))
  "Redraw an Options group"
  (insert "Arf!!"))

(Redraw (Options "arf!"))

(defun options-redisplay (elems))

(defun options-mode-new (name opts))

(defun options-mode ())

(provide 'options-mode)

;;; options-mode.el ends here
