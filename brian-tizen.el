;;; brian-tizen.el --- 
;;
;; Copyright (C) 2012 Brian Fransioli
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
(require 'ido)

(add-to-list 'load-path "/home/terranpro/elisp/foreign/auto-complete/lib/popup")
(require 'popup)

;; TODO: very useful function needed for my dir locals
;; move it elsewhere later
;; Modified it to not ignore "." so it retrns passed root directory too
(defun folder-dirs (folder)
  (delete-if-not 'file-directory-p
    (mapcar (lambda(arg) (file-name-as-directory (concat (file-name-as-directory folder) arg)))
      (delete-if (lambda (arg) (or (string= ".." arg) (string= "." arg)))
        (directory-files folder)))))

(defun folder-dirs-recursive-impl (func folder)
  (when (and (not (null folder))
	     (file-directory-p folder))
    (funcall func folder)
    (mapcar '(lambda (folder)
	       (folder-dirs-recursive-impl func folder))
	    (folder-dirs folder))
    t))

(defun folder-dirs-recursive (folder)
  (let ((output (list "")))
    (folder-dirs-recursive-impl 
     '(lambda (arg)
	(add-to-list 'output
		     arg
		     t))
     folder)
    output))

;;(folder-dirs-recursive "/home/terranpro/tizen/git/ebook/include/")

(defvar tizen-gbs-chroot
 "/home/terranpro/tizen/SURC/build/local/scratch.armv7l.0")
(defvar tizen-gbs-conf (expand-file-name  "~/.gbs.conf"))
(defvar tizen-gerrit-server-address "165.213.149.219")
(defvar tizen-gerrit-server-port "29418")
(defvar tizen-gerrit-server-userid "br.fransioli"
  "")
(defvar tizen-packages-root-directory "/home/terranpro/tizen/gbs-git")

(defvar tizen-lthor-executable (let ((exec-path (append exec-path "/home/terranpro/tizen/")))
				 (executable-find "lthor"))
  "")

(defvar tizen-binaries-directory "/home/terranpro/tizen/binaries"
  "")

(defvar tizen-binary-download-use-proxy t
  "When nil, block the environment variables http_proxy and
  https_proxy by clearing them before running a wget command.")

(defvar tizen-gbs-built-rpm-directory "/home/terranpro/tizen/SURC/rpms"
  "")

(defvar-local tizen-project-directory nil 
  "Toplevel directory for a single Tizen package.
Example:  /home/terranpro/tizen/git/ebookviewer/")

(defvar tizen-mode-map 
  (let ((map (make-keymap)))
    (define-key map (kbd "F") 'tizen-flash-binary)
    (define-key map (kbd "D") 'tizen-download-binary)
    map)
  "")

(defvar tizen-minor-mode-map 
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c T F") 'tizen-flash-binary)
    (define-key map (kbd "C-c T D") 'tizen-download-binary)
    map)
  "")

(define-minor-mode tizen-minor-mode
  "Tizen!"
  :lighter " tizen"
  :keymap tizen-minor-mode-map
  :global t
  (progn
    (message "Tizen Burning!")))

(defun tizen-mode-init ()
  ""
  )

(defun tizen-mode ()
  ""
  )

(defun tizen-translate-file-to-prjpath (gbsroot-file prjdir)
  "Takes a full path to source file building inside a GBS Root
and translates it to a project directory based on PRJDIR. "
  (interactive)
  (let* ((file-info (split-string gbsroot-file ":" t))
	 (before (first file-info))
	 (reg (rx "/home/abuild/rpmbuild/BUILD/"
		  (zero-or-more (not (any "/")))
		  "/"
		  (group (one-or-more any) "/")
		  (group (one-or-more (not (any ":"))))))
	 (dir-part)
	 (file-part)
	 (line))
    (list
     (if (string-match reg before)
	 (progn
	   (setq dir-part (match-string 1 before))
	   (setq file-part (match-string 2 before))
	   (expand-file-name
	    (concat prjdir
		    dir-part
		    file-part))))
     (second file-info)
     (third file-info))))

(defun tizen-jump-to-prj-file (&optional file)
  (interactive)
  (unless file
    (setq file (thing-at-point 'filename)))
  (let* ((file-info  (tizen-translate-file-to-prjpath 
		      file
		      (or tizen-project-directory
			  default-directory)))
	 (file (first file-info))
	 (line-str  (second file-info))
	 (line (unless (null line-str)
		 (string-to-number line-str)))
	 (col-str (third file-info))
	 (col (unless (null col-str)
		(string-to-number col-str))))
    (find-file-other-window file)
    (unless (null (second file-info))
      (goto-char (point-min))
      (forward-line (1- line))
      (unless (null (third file-info))
	(forward-char (1- (string-to-number (third file-info))))))))

(defun tizen-gerrit-address (&optional proto user port)
  ""
  (let ((address ""))
    (cond 
     ((not (null proto))
      (setq address "ssh://")))

    (cond
     ((not (null user))
      (setq address (concat address
			    tizen-gerrit-server-userid
			    "@"))))

    (setq address (concat address
			  tizen-gerrit-server-address))
    (if port
	(setq address (concat address
			      ":"
			      tizen-gerrit-server-port)))
    address))
(tizen-gerrit-address nil t nil)

(defun tizen-git-clone-package--work (repo-addr pkg-full-path)
  ""
  (save-window-excursion
    (let ((cmd (concat "git clone "
			   repo-addr
			   "/"
			   pkg-full-path)))
      (cd tizen-packages-root-directory)
      (message cmd)
      (shell-command 
       cmd
       nil
       "Tizen Git Error"))))

(defun tizen-git-update-package (pkg-path)
  (save-window-excursion
    (cd pkg-path)
    (shell-command (concat "git pull")
		   nil
		   "Tizen Git Error")))

(defun tizen-git-clone-package ()
  ""
  (interactive)
  (let* ((repo-addr (tizen-gerrit-address t t t))
	 (pkg (ido-completing-read "Package: "
				   (tizen-gerrit-ls-projects)))
	 (pkg-full-path pkg)
	 (pkg-basename (file-name-base pkg-full-path))
	 (pkg-localdir (concat 
			(file-name-directory tizen-packages-root-directory)
			pkg-basename))
	 (cmd))
    ;(tizen-git-clone-package--work repo-addr pkg-full-path)
    (message (format "repo: %s" repo-addr))
    (cond 
     ((file-directory-p pkg-localdir)
      (message "Arf!")
      (tizen-git-update-package pkg-full-path))
     (t
      (message "woof!")
      (tizen-git-clone-package--work repo-addr pkg-full-path)))
    pkg))

;; (let* ((alist '(("mag" 
;; 		 ("app" "a")
;; 		 ("frame" "b" "c"))))
;;        (aval (aget alist "mag"))
;;        (addme '("lib" "z")))
;;   (aput 'alist "mag" (append aval (list addme))))

;; (let ((alist))
;;   (aput 'alist "mag")
;;   (aput 'alist "mag" "arf")
;;   (aput 'alist "mag" "woof"))

;; Original, semi-working version.
;; (defun tizen-insert (x T)
;;   (if (null T)
;;       (if (consp (cdr x))
;; 	  (list (car x) (tizen-insert (cdr x) nil))
;; 	(car x))
;;     (let ((key (assoc (car x) T)))
;;       (cond
;;        ((= (length x) 1)
;; 	(append T x))

;;        ((null key)
;; 	(aput 'T (car x) (list (tizen-insert (cdr x) nil))))
       
;;        (t
;; 	(aput 'T (car x) (tizen-insert (cdr x) (cdr key))))))))

(defun tizen-insert (x T)
  (if (null T)
      (if (consp (cdr x))
	  (list (car x) (tizen-insert (cdr x) nil))
	(car x))
    (let ((key (assoc (car x) T)))
      (cond
       ((= (length x) 1)
	(sort (append T x)
	      (lambda (a b)
		(string< (if (consp a) (car a) a)
			 (if (consp b) (car b) b))) ))

       ((null key)
	(sort 
	 (push 
	  (append (list (car x)) (list (tizen-insert (cdr x) nil))) T)
	 (lambda (a b)
		(string< (if (consp a) (car a) a)
			 (if (consp b) (car b) b)))))
       
       (t
	(setcdr key 
		(tizen-insert (cdr x) (cdr key)))
	T)))))

;; (popup-cascade-menu
;;  (tizen-insert
;;    '("mag" "apps" "x" "y" "1" "2" "3" "alleviate" "pain" "inside!")
;;    (tizen-insert 
;;     '("kern" "apps" "z")
;;     (tizen-insert 
;;      '("kern" "apps" "b") 
;;      (list (tizen-insert '("mag" "apps" "a") nil))))))

;; (let ((x)) 
;;   (push '("apps" "b") x)
;;   (if (assoc "apps" x) 
;;       (setcdr (assoc "apps" x) (append (cdr (assoc "apps" x)) (list "c"))))
;;   x)

;; (tizen-insert 
;;  '("kern" "good")
;;  (tizen-insert 
;;   '("kern" "net" "tcp")
;;   (tizen-insert 
;;    '("kern" "apps" "c")
;;    (tizen-insert '("kern" "apps" "b") 
;; 		 (list (tizen-insert '("mag" "apps" "a")
;; 				     nil))))))



;; ((lambda ()
;;    (let* ((l '(("mag" 
;; 		("apps" "b" "c")
;; 		("framework" "y" "z"))
;; 	       ("kern"
;; 		("net"
;; 		 ("data" "eth" "wifi")
;; 		 ("ip" "v4" "v6")
;; 		 "tcp" 
;; 		 "udp"))))
;; 	  ;; after parsing, should resemble something similar to `l'
;; 	  ;; (prjstrs '("mag/apps/b"
;; 	  ;; 	     "mag/apps/c"
;; 	  ;; 	     "mag/garb"
;; 	  ;; 	     "mag/apps/d/e"
;; 	  ;; 	     "mag/framework/y"
;; 	  ;; 	     "mag/framework/z"
;; 	  ;; 	     "kern/net/data/wifi"
;; 	  ;; 	     "kern/ip/v4"
;; 	  ;; 	     "kern/ip/v6"
;; 	  ;; 	     "kern/net/tcp"
;; 	  ;; 	     "kern/net/udp"))





;; 	  (prjstrs (split-string "adaptation/mtdev
;; adaptation/opengl-es-virtual-drv
;; adaptation/system-plugin-ia-generic
;; adaptation/system-plugin-slp
;; adaptation/wlandrv-plugin-tizen-bcm43xx
;; adaptation/ap_samsung/system-plugin-slp
;; adaptation/devices/alsa-scenario-scn-data-0-base
;; adaptation/devices/alsa-scenario-scn-data-0-mc1n2
;; adaptation/devices/alsa-ucm-conf-mc1n2
;; adaptation/devices/bluetooth-firmware-bcm
;; adaptation/devices/bluetooth-tools
;; adaptation/intel_mfld/psb-headers
;; adaptation/xorg/driver/xserver-xorg-input-evdev
;; adaptation/xorg/driver/xserver-xorg-input-evdev-multitouch
;; adaptation/xorg/driver/xserver-xorg-input-gesture
;; adaptation/xorg/driver/xserver-xorg-misc
;; adaptation/xorg/driver/xserver-xorg-video-emulfb
;; adaptation/xserver-xorg-input-evdev
;; adaptation/xserver-xorg-input-gesture
;; adaptation/xserver-xorg-misc
;; adaptation/xserver-xorg-video-emulfb
;; "))


     ;; 	  ;(prjstrs '("a/b/c" "a/b/d" "a/e" "a/b/f/g" "a/b/f/h" "i/j" "k"))
     ;; 	  ;; s: (("mag" "apps" "b") ... etc.)
     ;; 	  ;; r: finished tree/menu structure
     ;; 	  (s (mapcar '(lambda (p)  (split-string p "/")) prjstrs))
     ;; 	  (r '(())))

     ;; ;; (while s
     ;; ;;   (setq r (tizen-insert (first s) r))
     ;; ;;   (setq s (rest s)))

     ;; (mapcar '(lambda (x) (setq r (tizen-insert x r))) s)

     ;; ;(popup-cascade-menu (delq nil r))
     ;; ;(eq (delq nil r) (assem-transform prjstrs))
     ;; (pp (delq nil r))
     ;; (pp (assem-transform prjstrs))
     ;; ;(pprint r)
     ;; )))

;; (popup-cascade-menu '(("mag" ("apps" "b" "c")("framework" "y" "z"))("kern" ("net" ("data" "eth" "wifi")("ip" "v4" "v6") "tcp" "udp"))))

(defun tizen-gerrit-parse-ls-projects-popup (projs)
  ""
  (let ((split-projs (mapcar '(lambda (p) (split-string p "/"))
			     projs))
	(r '(())))
    (while split-projs
      (setq r (tizen-insert (first split-projs) r))
      (setq split-projs (rest split-projs)))))

(defun tizen-gerrit-ls-projects ()
  ""
  (interactive)
  (let* ((cmd (concat "ssh -p "
		      tizen-gerrit-server-port
		      " "
		      (tizen-gerrit-address nil t nil)
		      " "
		      "gerrit ls-projects"))
	 ;(projs (split-string (shell-command-to-string cmd)))
	 (projs (shell-command-to-string cmd))
	 )
    projs))
;(with-output-to-temp-buffer "Tizen" (insert (tizen-gerrit-ls-projects)))

(defun tizen-download-binary-catch-tarballs (path)
  (let ((cmd (concat "curl "
		     path 
		     " 2>/dev/null | w3m -dump -T text/html | awk '/tar.gz/ { print $3; } '")))
    (message cmd)
    (split-string (shell-command-to-string cmd))))

(defvar tizen-chosen-binary-files nil "")
(defun tizen-download-binary (snapshot)
  ""
  (interactive "sSnapshot URL: ")
  (let* ((img-dir (concat snapshot "/images/"))
	   (boot-dir (concat img-dir "/BOOT-REDWOOD453G-ENG/"))
	   (fs-dir (concat img-dir "/REDWOOD453G-EUR-OPEN/"))
	   (csc-dir (concat img-dir "/CSC-I8800-OXA/"))
	   (file-rx (shell-quote-wildcard-pattern "*tar*"))
	   (subdirs (split-string
		     (shell-command-to-string
		      (concat "curl "
			      img-dir
			      " 2>/dev/null | w3m -dump -T text/html | awk '/DIR/ { if ($2 != \"Parent\") print $2; }' "))))
	   (tmp-buf (generate-new-buffer "Tizen Binary Download")))
  
    (with-current-buffer tmp-buf
      
      (setq tizen-chosen-binary-files nil)
      (pp subdirs)
      (tizen-create-binary-form-pre)
      (mapcar '(lambda (subdir) (tizen-create-subdir-checkboxes
				 subdir 
				 (tizen-download-binary-catch-tarballs 
				  (concat img-dir subdir))))
	      subdirs)
      (tizen-create-binary-form-post img-dir)
      (goto-char (point-min)))
    (switch-to-buffer tmp-buf)))

(defun tizen-download-binary-worker (img-dir files)
  (message "Entering Binary Download Worker")
  (let ((temp-directory (make-temp-name
			 (concat tizen-binaries-directory
				 "/"
				 (format-time-string "%Y-%m-%d")
				 "-")))
	(old-directory default-directory))
    (if (file-exists-p temp-directory)
     (delete-directory temp-directory t))
    (make-directory temp-directory)
    (cd temp-directory)

    (mapcar '(lambda (file)
	       (shell-command (concat
			       (if tizen-binary-download-use-proxy
				   ""
				 "http_proxy=\"\" https_proxy=\"\" ")
			       "wget "
				      img-dir
				      file
				      " &")
			      (generate-new-buffer "Tizen: wget")))
	    files)

    (cd old-directory))
  (message "Done!"))

(defun tizen-create-binary-form-pre ()
  (interactive)
  (kill-all-local-variables)
  (erase-buffer)
)

(defun tizen-create-binary-form-post (img-dir)
  (interactive)

  (widget-insert "\n\n")
  (widget-create 'checkbox 
		 :notify (lambda (widget &rest ignore)
			   (setq tizen-binary-download-use-proxy (widget-value widget)))
		 t)
  (widget-insert "\tUse HTTP(S) Proxy for Downloading\n\n")

  (widget-create 'push-button 
		 :notify (lambda (widget &rest ignore)
			   (tizen-download-binary-worker 
			    (car (widget-get widget :sibling-args))
			    tizen-chosen-binary-files))
		 :sibling-args (list img-dir)
		 "Download")
  (use-local-map widget-keymap)
  (widget-setup)
)

(defun tizen-create-subdir-checkboxes (dir files)
  (widget-insert (concat dir "\n"))

  (mapcar '(lambda (file)
	     (widget-create 
	      'checkbox 
	      :notify (lambda (widget &rest ignore)
			(if (widget-value widget)
			    (setq tizen-chosen-binary-files 
				  (cons (car (widget-get widget :sibling-args)) 
					tizen-chosen-binary-files))
			  (setq tizen-chosen-binary-files
				(delete (car (widget-get widget :sibling-args)) 
					tizen-chosen-binary-files)))
			(pp tizen-chosen-binary-files))
	      :sibling-args (list (concat dir file))
	      nil)
	     (widget-insert (concat "\t" file "\n")))
	  files)
  (widget-insert "\n"))

(defun tizen-flash-binary ()
  ""
  (interactive)
  (let* ((bin-dir (expand-file-name
		   (ido-completing-read "Binary Directory: "
					(directory-files 
					 tizen-binaries-directory))
		   tizen-binaries-directory))
	 (tar-files-regexp (rx (group (one-or-more any)
				      ".tar"
				      (zero-or-more ".gz"))))
	 (pit-file-regexp (rx (one-or-more any)
			      ".pit"))
	 (tar-files (directory-files bin-dir t tar-files-regexp))
	 (pit-file (directory-files bin-dir t pit-file-regexp))
	 (cmd (concat "sudo "
		      tizen-lthor-executable
		      " "
		      (if pit-file
			  (concat "-p " (car pit-file) " "))
		      (mapconcat 'identity tar-files " ")
		      " &")))
    (cd bin-dir)
    (shell-command cmd (get-buffer-create "LThor") nil)))
;(tizen-flash-binary)

(defvar tizen-gbs-build-mode-map (make-sparse-keymap))
(defvar tizen-gbs-build-profile nil)
(defvar tizen-gbs-build-option-noinit nil)
(defvar tizen-gbs-build-option-clean nil)

(defvar tizen-gbs-build-options-list
  '(switches
    (noinit "I" "--noinit" tizen-gbs-build-mode-cb-noinit)
    (clean "C" "--clean" tizen-gbs-build-mode-cb-clean)))

(defun tizen-gbs-build-mode-redisplay-window ()
  (erase-buffer)
  ;(insert (propertize))
  (insert "I :  --no-init\n")
  (insert "C :  --clean\n")
  (insert "P :  Choose Profile\n")
  (insert "B :  Build!!!"))

(defun tizen-gbs-build-options-window ()
  (save-excursion
    (split-window-below)
    (other-window 1)
    (switch-to-buffer "*tizen: GBS Build*")
    (tizen-gbs-build-mode)
    (tizen-gbs-build-mode-redisplay-window)))

(defun tizen-gbs-build-mode-cb ())

(defun tizen-gbs-build-mode-cb-noinit ()
  (interactive)
  (setq tizen-gbs-build-option-noinit (not tizen-gbs-build-option-noinit))
  (tizen-gbs-build-mode-redisplay-window))

(defun tizen-gbs-build-mode-cb-build ()
  )

(defun tizen-gbs-build-mode-cb-quit ()
  (interactive)
  (kill-buffer-and-window))

(defun tizen-gbs-build-mode-build-keymap ()
  (define-key tizen-gbs-build-mode-map (kbd "I")
    'tizen-gbs-build-mode-cb-noinit)
  (define-key tizen-gbs-build-mode-map (kbd "C")
    'tizen-gbs-build-mode-cb-clean)
  (define-key tizen-gbs-build-mode-map (kbd "P")
    'tizen-gbs-build-mode-cb-profile)
  (define-key tizen-gbs-build-mode-map (kbd "B")
    'tizen-gbs-build-mode-cb-build)
  (define-key tizen-gbs-build-mode-map (kbd "RET")
    'tizen-gbs-build-mode-cb-build)
  (define-key tizen-gbs-build-mode-map (kbd "C-g")
    'tizen-gbs-build-mode-cb-quit)
  (define-key tizen-gbs-build-mode-map (kbd "q")
    'tizen-gbs-build-mode-cb-quit))

(define-minor-mode tizen-gbs-build-mode ""
  :lighter "GBS" :init-value nil
  :keymap 'tizen-gbs-build-mode-map
  (tizen-gbs-build-mode-build-keymap)
  (tizen-gbs-build-mode-cb))

(defun tizen-gbs-build-worker (args)
  (interactive)
  (let ((cmd (concat "gbs build "
		     (mapconcat 'identity
				(remove-if 
				 '(lambda (str)
				    (or (string= str "")
					(string= str " ")))
				 args)
				" ")
		     " &")))
    
    (let* ((process-environment
	    ;; TODO: What's better than this double cons?! this can't be ideal
	    (cons "http_proxy=" 
		  (cons "https_proxy=" 
			process-environment))))
      (message cmd)
      (shell-command cmd "Tizen GBS Build")
      (save-window-excursion 
	(switch-to-buffer "Tizen GBS Build")
	(setq tizen-project-directory
	      (locate-dominating-file
	       (or buffer-file-name
		   default-directory)
	       ".dir-locals.el"))
	(local-set-key (kbd "C-c j") 'tizen-jump-to-prj-file)
	(local-set-key (kbd "C-c J") 'tizen-jump-to-prj-file)))))


;(tizen-gbs-build-worker '("--include-all " "" " " "--no-init "))

(require 'options-mode)
(defun tizen-gbs-build ()
  (interactive)

  (options-mode-new 
   "gbs-build"
   (Command "gbs-build"
	    :command
	    'tizen-gbs-build-worker
	    :options 
	    (Options 
	     "options"
	     :elems
	     (list (Switch "--clean" 
			   :key "C"
			   :desc "Clean the GBS buildroot & cached pkgs"
			   :onactivate '(lambda (opt)
					  (pp (oref opt active))))
		   (Switch "--noinit"
			   :key "N"
			   :active t
			   :desc "Do not check the state of GBS buildroot; fast"
			   :onactivate '(lambda (opt)
					  (message "")))

		   (Switch "--keep-packs"
			   :key "K"
			   :active t
			   :desc "Keep unused packages in build root"
			   :onactivate '(lambda (opt)
					  (message "Toggled Keep Packs")))

		   (Switch "--include-all"
			   :key "I"
			   :active t
			   :desc "Include uncommited changes and untracked files"
			   :onactivate '(lambda (opt)
					  (message "Toggled Include All")))
		   
		   (Switch "--incremental"
			   :key "i"
			   :desc "Incremental build - continue failed builds"
			   :onactivate '(lambda (opt)
					  (message "Toggled Incremental")))

		   (SwitchArg "--profile"
			      :key "P"
			      :desc "Specify the GBS profile to be used"
			      :arg "surc"
			      :onactivate '(lambda (opt)
					     (ido-completing-read 
					      "Profile: "
					      (list "slp" "surc" "latest")
					      "surc")))
		   
		   (SwitchArg "--arch"
			      :key "A"
			      :desc "Specify the GBS profile to be used"
			      :arg "armv7l"
			      :onactivate '(lambda (opt)
					     (ido-completing-read 
					      "Profile: "
					      (list "armv7l" "i586")
					      "armv7l"))))))))


;;(tizen-gbs-build)
(defun tizen-build-package-gbs (&optional profile)
  ""
  (interactive)

  (unless (stringp profile)
    (setq profile
	  (ido-completing-read 
	   "Profile: "
	   (split-string
	    (shell-command-to-string 
	     (concat
	      "awk '/^\\[profile/ { print $0; }' "
	      tizen-gbs-conf
	      " | sed -e 's/\\[//' -e 's/\\]//' -e 's/profile.//'"))))))
  
  (tizen-gbs-build-options-window)
  (let* ((process-environment
	  ;; TODO: What's better than this double cons?! this can't be ideal
	  (cons "http_proxy=" 
		(cons "https_proxy=" 
		      process-environment)))
	 (arch "armv7l")
	 (misc-args "--include-all")
	 (cmd (concat "gbs build "
		      "-A " arch " "
		      "--profile " profile " "
		      misc-args
		      " &")))
    (message cmd)
    (shell-command cmd
		   "Tizen GBS Build")))

(defun tizen-sdb-push-file (file)
  ""
  (let* ((local-file (and (file-exists-p file)
			  file))
	 (remote-dir "/tmp/")
	 (cmd (concat "sdb push "
		      local-file
		      " "
		      remote-dir)))
    (shell-command cmd
		   "Tizen SDB Push File")))

(defun tizen-sdb-shell-cmd (cmd)
  (let* ((sdbcmd (concat "sdb shell "
			 cmd
			 " &")))
    (shell-command sdbcmd
		   "Tizen SDB Shell")))

(defun tizen-sdb-install-image-i  ()
  (interactive)
  (let* ((image (ido-read-file-name
		 "RPM: "
		 (if (file-directory-p tizen-gbs-built-rpm-directory)
		      tizen-gbs-built-rpm-directory
		    (expand-file-name "~")))))
    (setq tizen-gbs-built-rpm-directory (file-name-directory image))
    (tizen-sdb-push-file image)
;; pkgcmd -i -t rpm -p PATH_TO_RPM
    (tizen-sdb-shell-cmd (concat
			  ;;"rpm -ivh --force "
			  "pkgcmd -i -q -t rpm -p "
			  "/tmp/"
			  (file-name-nondirectory image)))))

(defun tizen-ssh-push-file (files &optional targetdir)
  (let ((cmd (concat "scp "
		     (if (listp files)
			 (mapconcat 'identity files " ")
		       files)
		     " "
		     "root@192.168.129.3:"
		     targetdir 
		     " &")))
    (shell-command cmd "Tizen SCP File")))

;; (tizen-ssh-push-file "/home/terranpro/tizen/SURC/build/local/repos/RelRedwoodCISOPEN/armv7l/RPMS/com.samsung.ebookviewer-0.1.8-7.armv7l.rpm"
;; 		     "/tmp/")

;; (tizen-ssh-push-file 
;;  (list  "/home/terranpro/tizen/SURC/build/local/repos/RelRedwoodCISOPEN/armv7l/RPMS/com.samsung.ebookviewer-debugsource-0.1.8-7.armv7l.rpm"
;; 	"/home/terranpro/tizen/SURC/build/local/repos/RelRedwoodCISOPEN/armv7l/RPMS/com.samsung.ebookviewer-debuginfo-0.1.8-7.armv7l.rpm")
;; 		     "/tmp/")

(defun tizen-ssh-shell-cmd (cmd) 
  (let ((sshcmd (concat "ssh " 
			"root@192.168.129.3"
			" "
			" << 'EOF' \n"
			cmd
			"\nEOF")))
    (shell-command sshcmd)))

;; (tizen-ssh-shell-cmd
;;  "ls -lrtha /tmp | awk ' {print $NF; }' | sort && echo $HOME")
;; (tizen-ssh-shell-cmd
;;  "ls -lrtha /tmp ")

(defun tizen-ssh-install-rpm (&optional package)
  (interactive)
  
)

(defun tizen-ldd-executable ()
  ""
  (interactive)
  (let* ((exec-path )
	 ()
	 (cmd))
    ))

;; Almost finished?!
;; Improve later
(defun tizen-create-barebone-project (&optional parent-dir project-name)
  ""
  (interactive "D")
  (save-window-excursion
    (let* ((prj-dir (file-name-directory
		     (or parent-dir 
			 (ido-read-directory-name 
			  "Project Directory: "
			  parentdir))))
	   (prj-name (file-name-nondirectory (directory-file-name prj-dir)))
	   (spec-file (concat
		       prj-dir
		       "packaging/"
		       prj-name
		       ".spec"))
	   (cmake-file (concat 
			prj-dir 
			"CMakeLists.txt")))

      (message (format "1 %s 2 %s 3 %s 4 %s 5 %s" parent-dir prj-dir prj-name spec-file cmake-file))
      (save-excursion
	(find-file spec-file)
	(let* ((tmp-name "tizen:spec")
	       (tmp-tbl (srecode-template-get-table
			 (srecode-get-mode-table 'sh-mode) tmp-name))
	       (dict (srecode-create-dictionary)))
	  (srecode-dictionary-set-value dict "NAME" prj-name)
	  (srecode-dictionary-set-value dict "DESCRIPTION"
					(concat "TODO: "
						"Description for "
						prj-name "."))
	  (srecode-resolve-arguments tmp-tbl dict)
	  (srecode-insert-fcn tmp-tbl dict)))

      (save-excursion 
       (find-file cmake-file)
       (let* ((tmp-name "tizen:empty")
	      (tmp-tbl (srecode-template-get-table  
			(srecode-get-mode-table 'cmake-mode) tmp-name))
	      (dict (srecode-create-dictionary)))
	 (srecode-dictionary-set-value dict "PNAME" prj-name)
	 (srecode-resolve-arguments tmp-tbl dict)
	 (srecode-insert-fcn tmp-tbl dict))))))


(defun tizen-get-cur-directory-name (&optional dir)
  ""
  (car (last (split-string (directory-file-name (or dir 
						    default-directory)) "/"))))

(defun tizen-trim-pkgconfig-includes (cflags-str)
  ""
  (replace-regexp-in-string
   (rx (or "  " "   "))
   ""
   (replace-regexp-in-string 
    (rx (one-or-more (or "\r" "\n")))
    " "
    (replace-regexp-in-string
     (rx "-I")
     ""
     cflags-str))))

(defun tizen-prefix-pkgconfig-includes (dir)
  (concat
   tizen-gbs-chroot
   dir))

(defun tizen-system-include-check-pkgconfig (pkgname pkgconfigpath)
  ""
  (let ((process-environment 
	 (cons pkgconfigpath
	       process-environment)))
    (if (= 
	 (call-process 
	  "pkg-config" 
	  nil 
	  nil 
	  nil 
	  "--exists" 
	  pkgname) 
	 0)
	(shell-command-to-string 
	 (concat "pkg-config --cflags-only-I " pkgname))
      " ")))

(defun tizen-system-include-paths (file)
  ""
  (interactive)
  (let ((incs "/usr/include"))
    (save-window-excursion
      (find-file file)
      (goto-char (point-min))
      (while (progn
	       (search-forward-regexp 
		(rx "pkg_check_modules") 
		(point-max)
		'move)
	       (search-forward-regexp
		"("
		(point-max)
		t)
	       (search-forward-regexp
		(rx
		 (minimal-match
		  (and (group (one-or-more (not (any ")"))))
		       ")")))
		(point-max)
		t))

	
	
	(let ((libs (split-string (match-string 1) nil t))
	      (pkgconfigpath 
	       (concat "PKG_CONFIG_PATH="
		       tizen-gbs-chroot
		       "/usr/lib/pkgconfig")))
	  ;(message (format "libs: %s" libs))
	  (setq incs 
		(concat
		 incs 
		 " "
		 (tizen-trim-pkgconfig-includes
		  (mapconcat
		   '(lambda (pkg)
		      (tizen-system-include-check-pkgconfig 
		       pkg	
		       pkgconfigpath))
		   libs
		   " ")))))))

    (mapconcat 
     'tizen-prefix-pkgconfig-includes
     (split-string incs)
     " ")))

(defun tizen-project-cmakelist-parse-include-directories (file)
  (save-window-excursion
    (let ((incs ""))
      (find-file file)
      (goto-char (point-min))
      (while (search-forward-regexp 
	      "include_directories[ \n]+?(\\([ \n]*\\(.*?[ \n]*\\)+?\\))"
	      (point-max)
	      t)
	(setq incs (split-string (match-string 1) nil t))
	(setq incs 
	      (mapconcat 
	       '(lambda (dir) 
		  (if (file-exists-p dir)
		      (progn
			(message (format "Directory %s exists" dir))
			(concat "/"
				(directory-file-name dir)))
		    " "))
	       incs
	       " ")))
      incs)))

(defun tizen-ede-cpp-root-project (&optional file)
  ""
  (interactive)
  (let ((prj-root-dir (file-name-directory file)))
    (ede-cpp-root-project
     (tizen-get-cur-directory-name prj-root-dir)
     :name (tizen-get-cur-directory-name prj-root-dir)
     :file (concat prj-root-dir "/CMakeLists.txt")
     ;;     :include-path '( "/include" )
     :include-path (split-string
		    (tizen-project-cmakelist-parse-include-directories
		     file)
		    nil
		    t)
     :system-include-path
     (split-string 
      (tizen-system-include-paths file)
      nil
      t)
     :spp-table '( ("CONST" . "const"))
     :local-variables
     '((compile-command . "gbs build -A armv7el --include-all")))))

(require 'easymenu)
(easy-menu-define tizen-mode-menu tizen-mode-map
  "Tizen"
  '("Tizen"
    ["Flash Binary" tizen-flash-binary t]
    ["Download Binary" tizen-download-binary t]
    "---"
    ["Clone GBS Repo" nil t]))
(easy-menu-add 'tizen-mode-menu)
(provide 'brian-tizen)


;; (require 'brian-tizen)

;; (tizen-ede-cpp-root-project 
;;   "/home/terranpro/tizen/testgbs/elm-demo-tizen/CMakeLists.txt")

;; (tizen-ede-cpp-root-project 
;;  "/home/terranpro/tizen/gbs-git/ebook/CMakeLists.txt")

;; (tizen-ede-cpp-root-project 
;;  "/home/terranpro/eb-git/ebookviewer/CMakeLists.txt")
(tizen-ede-cpp-root-project 
 "/home/terranpro/tizen/git/ebook/CMakeLists.txt")
(tizen-ede-cpp-root-project 
 "/home/terranpro/tizen/git/ebookviewer/CMakeLists.txt")

(when (file-exists-p "/home/terranpro/tizen/git/pte-standalone/CMakeLists.txt")
 (tizen-ede-cpp-root-project 
  "/home/terranpro/tizen/git/pte-standalone/CMakeLists.txt"))

;; (tizen-ede-cpp-root-project 
;;  "/home/terranpro/tizen/git/tizen15/CMakeLists.txt")

;(tizen-system-include-paths "/home/terranpro/tizen/git/tizen15/CMakeLists.txt")
(tizen-system-include-paths
 "/home/terranpro/tizen/git/ebookviewer/CMakeLists.txt")
;;; brian-tizen.el ends here
