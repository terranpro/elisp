(add-to-list 'load-path "/home/terranpro/code/color-theme")
(add-to-list 'load-path "/home/terranpro/code/emacs-color-theme-solarized")
(require 'color-theme)
(require 'color-theme-solarized)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
;     (color-theme-hober)
     (color-theme-euphoria)))
