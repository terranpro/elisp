;;; brian-clang-faces.el --- 
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
;; 

;;; Code:

(add-to-list 'load-path "~/code/clang-faces")
(require 'clang-faces)

(defun brian-clang-faces-init ()
  (hack-local-variables)
  (clang-faces-mode t))

;; TODO: clang-faces depends on `ac-clang-cflags' to be set
;; So this hook needs to be appended and not prepended (after ac-clang setup)
;; (add-hook 'c-mode-common-hook 'brian-clang-faces-init t)

(provide 'brian-clang-faces)

;;; brian-clang-faces.el ends here
