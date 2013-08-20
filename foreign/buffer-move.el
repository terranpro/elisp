;;; buffer-move.el --- Swap buffers without typing C-x b on each window

;; Copyright (C) 2004  Lucas Bonnet <lukhas@free.fr>

;; Author: Lucas Bonnet <lucas@rincevent.net>
;; Keywords: lisp,convenience
;; Version: 0.4
;; URL : http://lukhas.free. fr/emacs/elisp/buffer-move.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; This file is for lazy people wanting to swap buffers without
;; typing C-x b on each window. This is useful when you have :

;; +--------------+-------------+
;; |              |             |
;; |    #emacs    |    #gnus    |
;; |              |             |
;; +--------------+-------------+
;; |                            |
;; |           .emacs           |
;; |                            |
;; +----------------------------+

;; and you want to have :

;; +--------------+-------------+
;; |              |             |
;; |    #gnus     |   .emacs    |
;; |              |             |
;; +--------------+-------------+
;; |                            |
;; |           #emacs           |
;; |                            |
;; +----------------------------+

;; With buffer-move, just go in #gnus, do buf-move-left, go to #emacs
;; (which now should be on top right) and do buf-move-down.

;; To use it, simply put a (require 'buffer-move) in your ~/.emacs and
;; define some keybindings. For example, i use :

;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;; (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;; (global-set-key (kbd "<C-S-right>")  'buf-move-right)


;;; Code:

(require 'windmove)

;;;###autoload
(defun buf-move--dir (dir other-buf-p)
"Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled.  Use OTHER-BUF-P to determine whether
the current window will receive the other window's buffer, or the
last accessed buffer as determined by `other-buffer'."
  (let* ((other-win (windmove-find-other-window dir))
	 (other-win-buf (window-buffer (selected-window)))
	 (this-win-buf (if other-buf-p
			   (other-buffer)
			 (window-buffer other-win)) ))
    (if (null other-win)
        (error "No split in that direction")
      ;; swap top with this one
      (set-window-buffer (selected-window) this-win-buf)
      ;; move this one to top
      (set-window-buffer other-win other-win-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-up (&optional arg)
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled.  With optional prefix, ARG, move the current
buffer UP and bring `other-buffer' to the current window."
;;  "Switches between the current buffer, and the buffer above the
;;  split, if possible."
  (interactive "P")
  (buf-move--dir 'up arg))

;;;###autoload
(defun buf-move-down (&optional arg)
"Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled.  With optional prefix, ARG, move the current
buffer DOWN and bring `other-buffer' to the current window."
  (interactive "P")
  (buf-move--dir 'down arg))

;;;###autoload
(defun buf-move-left (&optional arg)
"Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled.  With optional prefix, ARG, move the
current buffer LEFT and bring `other-buffer' to the current
window."
  (interactive "P")
  (buf-move--dir 'left arg))

;;;###autoload
(defun buf-move-right (&optional arg)
"Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled.  With optional prefix, ARG, move the
current buffer RIGHT and bring `other-buffer' to the current
window."
  (interactive "P")
  (buf-move--dir 'right arg))


(provide 'buffer-move)
;;; buffer-move.el ends here
