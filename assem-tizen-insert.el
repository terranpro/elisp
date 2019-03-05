(defmacro map-bind (args sexp seq)
  "Bind ARGS to successive elements of SEQ and eval SEXP.

A hybrid of `destructuring-bind' and `mapcar'
ARGS shall be of the form used with `destructuring-bind'

Unlike most other mapping forms this is a macro intended to be
used for structural transformations, so the expected usage will
be that ARGS describes the structure of the items in SEQ, and
SEXP will describe the structure desired."
  (declare (indent 2))
  (let ((entry (gensym)))
    `(mapcar (lambda (,entry)
               (destructuring-bind ,args ,entry ,sexp))
             ,seq)))

(defun split-string-1 (string seperators)
  (let ((match (string-match (eval `(rx (any ,seperators)))
                             string)))
    (when match
      (list (substring string 0 match)
            (substring string (1+ match))))))

(defun assem-group-by-key (alist)
  (let ((keys (mapcar 'list
                      (remove-duplicates (mapcar 'car alist)
                                         :test 'equal))))
    (map-bind (key value)
        (setcdr (last (assoc key keys)) (list value))
      alist)
    keys))

(defun assem-group-menus (dirty-alist)
  (let ((alist (remove-if-not 'consp dirty-alist))
        (dirt (remove-if 'consp dirty-alist)))
    (sort (append dirt (assem-group-by-key alist))
          (lambda (a b)
            (string< (if (consp a) (car a) a)
                     (if (consp b) (car b) b))))))

(defun assem-transform (string-list)
  (mapcar (lambda (item)
            (if (consp item)
                (cons (car item)
                      (assem-transform (cdr item)))
              item))
          (assem-group-menus (mapcar (lambda (str)
                                       (or (split-string-1 str "/") str))
                                     string-list))))


(assem-group-menus (mapcar (lambda (str)
			     (or (split-string-1 str "/")))
			   '("mag/apps/b"
			     "mag/apps/c"
			     "mag/framework/y"
			     "mag/framework/z"
			     "kern/net/data/wifi"
			     "kern/ip/v4"
			     "kern/ip/v6"
			     "kern/net/tcp"
			     "kern/net/udp")))
;;testing
(popup-cascade-menu )
(pp(assem-transform
  (split-string "adaptation/mtdev
adaptation/opengl-es-virtual-drv
adaptation/system-plugin-ia-generic
adaptation/system-plugin-slp
adaptation/wlandrv-plugin-tizen-bcm43xx
adaptation/ap_samsung/system-plugin-slp
adaptation/devices/alsa-scenario-scn-data-0-base
adaptation/devices/alsa-scenario-scn-data-0-mc1n2
adaptation/devices/alsa-ucm-conf-mc1n2
adaptation/devices/bluetooth-firmware-bcm
adaptation/devices/bluetooth-tools
adaptation/intel_mfld/psb-headers
adaptation/xorg/driver/xserver-xorg-input-evdev
adaptation/xorg/driver/xserver-xorg-input-evdev-multitouch
adaptation/xorg/driver/xserver-xorg-input-gesture
adaptation/xorg/driver/xserver-xorg-misc
adaptation/xorg/driver/xserver-xorg-video-emulfb
adaptation/xserver-xorg-input-evdev
adaptation/xserver-xorg-input-gesture
adaptation/xserver-xorg-misc
adaptation/xserver-xorg-video-emulfb
")))
;; (("a" ("b" "c" "d" ("f" "g" "h")) "e") ("i" "j") "k")

;;testing
(assem-transform
 '("a/b/c" "a/b/d" "a/e" "a/b/f/g" "a/b/f/h" "i/j" "k"))
;; (("a" ("b" "c" "d" ("f" "g" "h")) "e") ("i" "j") "k")
