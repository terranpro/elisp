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

(add-to-list 'load-path
	     "/home/terranpro/elisp/foreign/auto-complete/lib/popup")
(require 'popup)
(require 'options-mode)

;; TODO: very useful function needed for my dir locals
;; move it elsewhere later
;; Modified it to not ignore "." so it returns passed root directory too
(defun folder-dirs (folder)
  (delete-if-not 'file-directory-p
    (mapcar (lambda(arg) (file-name-as-directory (concat (file-name-as-directory folder) arg)))
      (delete-if (lambda (arg) (or (string= ".." arg) (string= "." arg)))
        (directory-files folder)))))

(defun folder-dirs-recursive-impl (func folder)
  (when (and (not (null folder))
	     (file-directory-p folder))
    (funcall func folder)
    (mapcar #'(lambda (folder)
		(folder-dirs-recursive-impl func folder))
	    (folder-dirs folder))
    t))

(defun folder-dirs-recursive (folder)
  (let ((output))
    (folder-dirs-recursive-impl 
     #'(lambda (arg)
	 (setq output (append output (list arg))))
     folder)
    output))

;;(folder-dirs-recursive "/home/terranpro/tizen/git/libwakeup/src")

(defvar tizen-gbs-chroot
 "/home/terranpro/tizen/SURC/build/local/scratch.armv7l.0")
(defvar tizen-gbs-conf (expand-file-name  "~/.gbs.conf"))
(defvar tizen-gerrit-server-address "165.213.149.219")
(defvar tizen-gerrit-server-port "29418")
(defvar tizen-gerrit-server-userid "br.fransioli"
  "")
(defvar tizen-packages-root-directory "/home/terranpro/tizen/gbs-git")

(defvar tizen-lthor-executable
  (let ((exec-path (append exec-path (list "/home/terranpro/tizen/"))))
    (executable-find "lthor"))
  "")

(defvar tizen-sdb-executable
  (let ((exec-path (append exec-path (list "/home/terranpro/tizen-sdk/tools"))))
    (executable-find "sdb")))

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

(defvar-local tizen-gbs-current-profile "latest"
  "Default GBS profile to be used for builds and buildroot
  determination (e.g. for parsing system headers
  w/clang-async).")

(defvar-local tizen-gbs-build-options-parent-buffer nil
  "TODO: Hack to let a gbs build buffer know the options buffer
  that created it, so that it can scan some of the options
  again (like if Auto Push+Install to device is enabled).")

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
  (let ((split-projs (mapcar #'(lambda (p) (split-string p "/"))
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
;(with-output-to-temp-buffer "Tizen" (princ (tizen-gerrit-ls-projects)))

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
      (mapcar #'(lambda (subdir) (tizen-create-subdir-checkboxes
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

    (mapcar #'(lambda (file)
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
  (local-set-key (kbd "SPC") 'widget-button-press)
  (local-set-key (kbd "n") 'widget-forward)
  (local-set-key (kbd "C-c C-n") 'widget-forward)
  (local-set-key (kbd "p") 'widget-backward)
  (local-set-key (kbd "C-c C-p") 'widget-backward)

  (widget-setup))

(defun tizen-create-subdir-checkboxes (dir files)
  (widget-insert (concat dir "\n"))

  (mapcar #'(lambda (file)
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

(defun tizen-gbs-build-worker (opts args)
  (interactive)
  (let ((auto-install (IsActive (SearchName opts "PushDevice+Install")))
	(parent-buffer (current-buffer))
	(cmd (concat "gbs build "
		     (mapconcat 'identity
				(remove-if 
				 #'(lambda (str)
				     (or (string= str "")
					 (string= str " ")))
				 args)
				" "))))
    
    (let* ((process-environment
	    ;; TODO: What's better than this double cons?! this can't be ideal
	    (cons "http_proxy=" 
		  (cons "https_proxy=" 
			process-environment)))
	   (proc-buf-name (generate-new-buffer-name 
			   "Tizen GBS Build"))
	   (proc (apply 'start-process
			"gbs-build" 
			proc-buf-name
			"gbs"
			(cdr (split-string cmd " " t)))))
      
      (message cmd)

      ;;(set-process-filter proc 'ansi-color-for-comint-mode-on)      
      (set-process-filter proc
      			  #'(lambda (proc output)
			      (with-current-buffer (process-buffer proc)
				(ignore-errors
				  (let ((lastpt (point))
					(gbswin (get-buffer-window 
						 (process-buffer proc)
						 t)))
				     (goto-char (point-max))
				     (insert output)
				     (ansi-color-apply-on-region
				      lastpt (point-max))
				     (when gbswin
				       (with-selected-window gbswin
					 (goto-char (point-max)))))))))

      (set-process-sentinel proc (function tizen-gbs-build-proc-sentinel))

      (save-window-excursion 
	(switch-to-buffer proc-buf-name)
	(setq tizen-project-directory
	      (locate-dominating-file
	       (or buffer-file-name
		   default-directory)
	       ".dir-locals.el"))
	(setq tizen-gbs-build-options-parent-buffer parent-buffer)
	(local-set-key (kbd "C-c j") 'tizen-jump-to-prj-file)
	(local-set-key (kbd "C-c J") 'tizen-jump-to-prj-file))
      (display-buffer proc-buf-name))))

(defun tizen-gbs-build-proc-sentinel (proc event)
  (let ((status (process-status proc)))
    (if (eq 'exit status)
	(progn
	  (message "Colorizing!")
	  (with-current-buffer (process-buffer proc)
	    
	    (ansi-color-apply-on-region 
	     (point-min)
	     (point-max))
	    (let ((case-fold-search nil))
	      (highlight-regexp
	       (rx "warning:" (zero-or-more any) eol) 
	       compilation-warning-face)
	      (highlight-regexp
	       (rx "error:" (zero-or-more any) eol) 
	       compilation-error-face))
	    (setq tizen-gbs-built-rpm-directory 
		  (tizen-gbs-build-get-rpm-directory))
	    (when (and (= (process-exit-status proc) 0)
		       (with-current-buffer tizen-gbs-build-options-parent-buffer
			 (IsActive (first (options-find-by-name "PushDevice")))))
		(let ((rpm-list (mapcar
				 #'(lambda (gbsroot-rpm)
				     (file-name-nondirectory
				      (expand-file-name
				       (concat tizen-gbs-built-rpm-directory
					       gbsroot-rpm))))
				 (tizen-gbs-build-get-built-rpms)))
		      (rpmpush-buf (tizen-rpm-push-mode
				    tizen-gbs-built-rpm-directory)))
		  (with-current-buffer rpmpush-buf
		    (loop for file in rpm-list 
			  do (tizen-rpm-push-mode-mark-regexp file))
		    ;; it's like manually striking RET in the buffer
		    (call-interactively (key-binding (kbd "RET")))))))))))

(defun tizen-gbs-build-get-rpm-directory (&optional gbsbuild-buf)
  (with-current-buffer (or gbsbuild-buf (current-buffer))
   (let ((dir-rx (rx 
		  "generated RPM packages can be found from local repo:"
		  (zero-or-more (or whitespace ?\n))
		  (group (zero-or-more not-newline))))
	 (string (buffer-substring-no-properties 
		  (point-min) 
		  (point-max))))
     (if (string-match dir-rx string)
	 (match-string 1 string)))))

(defun tizen-gbs-build-get-built-rpms ()
  (save-excursion
    (goto-char (point-min))
    (let ((bin-rpm-rx (rx "Wrote: " (group "/home/abuild/rpmbuild/RPMS/"
					   (one-or-more any) eol))))
      (loop while (search-forward-regexp bin-rpm-rx (point-max) t)
	    collect (match-string 1)))))

;(tizen-gbs-build-worker '("--include-all " "" " " "--no-init "))

(defvar tizen-gbs-build-help-string 
  "Select options with their key prefix or SPC on the associated line.

When all options are selected, press ENTER to launch gbs build.")

(defun tizen-gbs-build ()
  (interactive)

  (options-mode-new 
   "gbs-build"
   (Command "gbs-build"
	    :command
	    'tizen-gbs-build-worker
	    :help-string
	    (concat tizen-gbs-build-help-string
		    "\n\nDetected Project Directory:\n"
		    (or tizen-project-directory default-directory))
	    :options 
	    (Options 
	     "options"
	     :elems
	     (list (Switch "--clean" 
			   :key "C"
			   :desc "Clean the GBS buildroot & cached pkgs"
			   :onactivate #'(lambda (opt)
					   (pp (oref opt active))))
		   (Switch "--noinit"
			   :key "N"
			   :active t
			   :desc "Do not check the state of GBS buildroot; fast"
			   :onactivate #'(lambda (opt)
					   (message "")))

		   (Switch "--keep-packs"
			   :key "K"
			   :active t
			   :desc "Keep unused packages in build root"
			   :onactivate #'(lambda (opt)
					   (message "Toggled Keep Packs")))

		   (Switch "--include-all"
			   :key "I"
			   :active t
			   :desc "Include uncommited changes and untracked files"
			   :onactivate #'(lambda (opt)
					   (message "Toggled Include All")))
		   
		   (Switch "--incremental"
			   :key "i"
			   :desc "Incremental build - continue failed builds"
			   :onactivate #'(lambda (opt)
					   (message "Toggled Incremental")))

		   (SwitchArg "--profile"
			      :key "P"
			      :desc "Specify the GBS profile to be used"
			      :arg "latest"
			      :onactivate #'(lambda (opt)
					      (ido-completing-read 
					       "Profile: "
					       (tizen-gbs-conf-get-profiles)
					       "latest")))
		   
		   (SwitchArg "--arch"
			      :key "A"
			      :desc "Specify the GBS profile to be used"
			      :arg "armv7l"
			      :onactivate #'(lambda (opt)
					      (ido-completing-read 
					       "Profile: "
					       (list "armv7l" "i586")
					       "armv7l")))

		   NewLineOption

		   (Switch "PushDevice+Install"
			   :key "!"
			   :active t
			   :auto nil
			   :desc "Use RPMPushMode to Push+Install to Device"
			   :onactivate #'(lambda (opt)
					   (message "Toggled Push+Install"))))))))


;;(tizen-gbs-build)
(defun tizen-gbs-conf-get-profiles (&optional gbsconf)
  (split-string
   (shell-command-to-string 
    (concat
     "awk '/^\\[profile/ { print $0; }' "
     (or gbsconf tizen-gbs-conf)
     " | sed -e 's/\\[//' -e 's/\\]//' -e 's/profile.//'"))))

(defun tizen-gbs-get-buildroot-for-profile (profile &optional gbsconf)
  "Grabs the buildroot line for a specific profile in a gbs conf
file; if multiple are found, just return the first in the list or
the default chroot if none are found."
  (expand-file-name
   (or 
    (car 
     (split-string
      (shell-command-to-string
       (concat
	"awk '/^\\[profile\." 
	profile
	"/ { notdone = 1; while( notdone ) { getline; "
	"if ( $1 == \"buildroot\") { notdone = 0; print $NF; } } }' "
	(or gbsconf tizen-gbs-conf)))))
    tizen-gbs-chroot)))
;(tizen-gbs-get-buildroot-for-profile "svoice")

(defun tizen-build-package-gbs (&optional profile)
  ""
  (interactive)

  (unless (stringp profile)
    (setq profile
	  (ido-completing-read 
	   "Profile: "
	   (tizen-gbs-conf-get-profiles))))
  
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

(defun tizen-key-from-count (count)
  (cond 
   ((> count (+ 9 26)) nil)
   ((> count 9) (char-to-string (- (+ ?A (- count 9)) 1)))
   (t (number-to-string count))))

(defvar tizen-rpm-push-mode-help-string 
"Use SPC or the prefix key at the beginning of the line to toggle selection/installation of RPM files.

Press ENTER when all files are selected, and they will be uploaded to the device.

Directory:
")

(defun tizen-rpm-push-mode-install-after-onactivate (opt)
  (let* ((userdata (oref opt userdata))
	 (choices (append (cdr userdata) (list (car userdata))))
	 (choice (car choices))
	 (fmt (format "%s%s"
		      (object-name-string opt) 
		      (if choice
			  (concat ": " choice)
			""))))
    (oset opt userdata choices)
    (oset opt active (not (null choice)))
    (oset opt display-name fmt)))

(defun tizen-rpm-push-mode (&optional dir)
  (interactive)
  (let* ((rpm-dir (directory-file-name 
		   (cond ((and dir (file-exists-p dir)) dir)
			 ((file-exists-p tizen-gbs-built-rpm-directory)
			  tizen-gbs-built-rpm-directory)
			 ((file-exists-p default-directory)
			  default-directory))))
	 (rpm-files (remove-if 
		     'file-directory-p
		     (directory-files rpm-dir t)))
	 (counter 0)
	 (options))
    
    (setq options 
	  (loop for file in rpm-files
		for count from 1 to (length rpm-files)
		collect (Switch file
				:display-name (file-name-nondirectory file)
				:key (tizen-key-from-count count)
				:active nil
				:onactivate #'(lambda (opt)
						(message "Toggled!")))))

    (setq options
	  (append (list 
		   (Switch "Install After Uploading To Device"
			   :key "i"
			   :active t
			   :auto nil
			   :userdata '("rpm" "pkgcmd" nil)
			   :display-name "Install After Uploading To Device: rpm"
			   :onactivate 
			   'tizen-rpm-push-mode-install-after-onactivate)
		   (Switch "Open Remote Install Mode Window After Uploading"
			   :key "r"
			   :active nil
			   :auto nil)
		   NullOption
		   NullOption)
		  options))

    (options-mode-new 
     "RPMPush"
     (Command "Tizen RPM Push"
	      :help-string
	      (concat tizen-rpm-push-mode-help-string
		      rpm-dir)
	      :command 
	      'tizen-rpm-push-mode-worker
	      :options
	      (Options "options"
		       :elems 
		       options)))))

(defun tizen-change-booting-mode ()
  (shell-command (concat tizen-sdb-executable 
			 " shell "
			 "change-booting-mode.sh --update")))

(defun tizen-fix-broken-strings ()
  (tizen-shell-cmd
   "\"find /usr/share/locale -exec chsmack -a '_' {} \\; \"" nil))

(defvar tizen-rpm-push-pre-hook nil)
(add-hook 'tizen-rpm-push-pre-hook 
	  #'(lambda ()
	      (when (tizen-sdb-is-active)
		(tizen-sdb-root)
		(tizen-change-booting-mode))))

(defvar tizen-rpm-push-post-hook nil)

(defun tizen-rpm-push-mode-worker (Opts files)
  "TODO: this shit needs serious work. searching for opts by name :("
  (interactive)
  (with-slots ((opts elems)) Opts
    (let* ((remote-dir "/opt/usr")
	   (method-table
	    '(("rpm" . tizen-remote-install-rpm)
	      ("pkgcmd" . tizen-remote-install-pkgcmd)))
	   (filt-files (remove-if #'(lambda (f) (or (string= " " f)
						    (string= "" f)))
				  files))
	   (remote (IsActive
		    (SearchName 
		     Opts
		     "Open Remote Install Mode Window After Uploading")))
	   (install-opt (cdr (assoc "Install After Uploading To Device"
				    (object-assoc-name opts))))
	   (install (IsActive install-opt)))
      
      (run-hooks 'tizen-rpm-push-pre-hook)

      (tizen-push-files filt-files remote-dir t)
      (cond (remote 
	     (tizen-remote-install-mode remote-dir))
	    (install
	     (message "Will Now Install")
	     (funcall
	      (cdr (assoc (car (oref install-opt userdata)) method-table))
	      (mapcar #'(lambda (file)
			  (expand-file-name (concat remote-dir "/" file)))
		      (mapcar #'file-name-nondirectory filt-files)) 
	      t))
	    (t
	     (message "Enjoy!"))))))

(defun tizen-rpm-push-mode-mark-regexp-pred (opt)
  (with-slots (display-name) opt
    (string-match (or mark-rx "") (or display-name ""))))

(defun tizen-rpm-push-mode-mark-regexp (&optional regx)
  (interactive "MRegexp: ")
  (let ((mark-rx regx))
   (options-mark-unmark-options #'tizen-rpm-push-mode-mark-regexp-pred)))

(defun tizen-sdb-is-active ()
  (> (string-to-number (shell-command-to-string (concat tizen-sdb-executable " devices" " | wc -l"))) 1))

(defun tizen-sdb-root (&optional off)
  (interactive)
  (shell-command (concat tizen-sdb-executable
			 " root " (if off 
				      "off"
				    "on"))))

;; (tizen-rpm-push-mode
;;  "/home/terranpro/tizen/HQ/build/local/repos/RelRedwoodCISOPEN/armv7l/RPMS/")

(defun tizen-remote-install-mode (&optional remotedir)
  (interactive)
  (let* ((options)
	 (remote-files
	  (tizen-shell-cmd-to-string (concat
				      "'find "
				      "/opt/usr" ;remotedir
				      " -name \"*.rpm\" 2>/dev/null '"
				      "| awk '{ print $NF; }'")))
	 (remote-rpms (split-string remote-files)))

    (setq options 
	  (loop for file in remote-rpms
		for count from 1 to (length remote-rpms)
		collect (Switch file 
				:display-name (file-name-nondirectory file)
				:key (tizen-key-from-count count)
				:active nil
				:onactivate #'(lambda (opt)
						(message "Toggled!")))))
    (options-mode-new "Remote PKG Install"
		      (Command "Tizen Remote PKG Install"
			       :help-string ""
			       :command 
			       'tizen-remote-install-mode-worker
			       :options 
			       (Options "options"
					:elems
					(append
					 options
					 (list
					  (SwitchArg "InstallMethod"
						     :key "m"
						     :desc "Blergh"
						     :arg "rpm"
						     :auto nil
						     :onactivate
						     #'(lambda (opt)
							 (ido-completing-read
							  "Method: "
							  (list "pkgcmd" "rpm")
							  "rpm"))))))))))

(defvar tizen-remote-install-post-hook nil)
(add-hook 'tizen-remote-install-post-hook
	  #'(lambda ()
	      (when (tizen-sdb-is-active)
		(tizen-sdb-root))
	      (tizen-fix-broken-strings)))

(defun tizen-remote-install-mode-worker (Opts builtopts)
  (with-slots ((opts elems)) Opts
   (let* ((method
	   (oref
	    (cdr (assoc "InstallMethod" (object-assoc-name opts)))
	    arg))
	  (method-table
	   '(("rpm" . tizen-remote-install-rpm)
	     ("pkgcmd" . tizen-remote-install-pkgcmd)))
	  (filt-files (remove-if #'(lambda (f) (or (string= " " f)
						   (string= "" f)))
				 builtopts)))

     (funcall (cdr (assoc method method-table)) filt-files)
     (run-hooks 'tizen-remote-install-post-hook))))

(defun tizen-remote-install-rpm (files &optional foreground)
  (let ((cmd (concat "rpm -ivh --force --nodeps "
		     (mapconcat 'identity files " "))))
    (tizen-shell-cmd cmd foreground)))

(defun tizen-remote-install-pkgcmd (files &optional foreground)
  (loop for file in files
	with cmd = (concat "pkgcmd -i -t rpm -q -p ")
	do
	(tizen-shell-cmd (concat cmd file) foreground)))

;(tizen-remote-install-mode "/opt/usr")

(defun tizen-push-files (files &optional dstdir foreground)
  "Generic push file function to a Tizen device.  Tries to determine the method (SSH vs SDB)."
  (cond ((tizen-sdb-is-active)
	 (tizen-sdb-push-file files dstdir foreground))
	(t 
	 (tizen-ssh-push-file files dstdir foreground))))

(defun tizen-sdb-push-file (file &optional dstdir foreground)
  ""
  (let* ((remote-dir (or dstdir "/tmp/"))
	 (cmd (concat " push "
		      (if (listp files)
			  (mapconcat 'identity file " ")
			file)
		      " "
		      remote-dir
		      (if foreground "" 
			" &"))))
    (tizen-sdb-cmd cmd)))

(defun tizen-shell-cmd-to-string (cmd &optional foreground)
  (tizen-shell-cmd-internal cmd foreground t))

(defun tizen-shell-cmd (cmd &optional foreground)
  (tizen-shell-cmd-internal cmd foreground nil))

(defun tizen-shell-cmd-internal (cmd &optional foreground tostring)
  (cond ((tizen-sdb-is-active)
	 (tizen-sdb-shell-cmd cmd foreground tostring))
	(t
	 (tizen-ssh-shell-cmd cmd foreground tostring))))

(defun tizen-sdb-shell-cmd (cmd &optional foreground tostring)
  (let* ((sdbcmd (concat 
		  tizen-sdb-executable
		  " shell "
		  cmd
		  (if foreground "" " &"))))
    
    (if tostring 
	(shell-command-to-string sdbcmd)
      (shell-command sdbcmd (generate-new-buffer-name "Tizen SDB Shell")))))

(defun tizen-sdb-cmd (cmd)
  (let* ((sdbcmd (concat tizen-sdb-executable 
			 " "
			 cmd)))
    (message sdbcmd)
    (shell-command sdbcmd)))

;(tizen-sdb-shell-cmd "ls -l /opt/usr" t t)

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

(defun tizen-ssh-push-file (files &optional targetdir foreground)
  (let ((cmd (concat "scp "
		     (if (listp files)
			 (mapconcat 'identity files " ")
		       files)
		     " "
		     "root@192.168.129.3:"
		     targetdir 
		     (if foreground "" " &"))))
    (shell-command cmd (generate-new-buffer-name "Tizen SCP File"))))

;; (tizen-ssh-push-file "/home/terranpro/tizen/SURC/build/local/repos/RelRedwoodCISOPEN/armv7l/RPMS/com.samsung.ebookviewer-0.1.8-7.armv7l.rpm"
;; 		     "/tmp/")

;; (tizen-ssh-push-file 
;;  (list  "/home/terranpro/tizen/SURC/build/local/repos/RelRedwoodCISOPEN/armv7l/RPMS/com.samsung.ebookviewer-debugsource-0.1.8-7.armv7l.rpm"
;; 	"/home/terranpro/tizen/SURC/build/local/repos/RelRedwoodCISOPEN/armv7l/RPMS/com.samsung.ebookviewer-debuginfo-0.1.8-7.armv7l.rpm")
;; 		     "/tmp/")

(defun tizen-ssh-shell-cmd (cmd &optional foreground tostring) 
  (let ((sshcmd (concat "ssh " 
			"root@192.168.129.3"
			" "
			" << 'EOF' "
			(if foreground "" " &")
			"\n"
			cmd
			"\nEOF")))
    (if tostring
	(shell-command-to-string sshcmd)
      (shell-command sshcmd (generate-new-buffer-name "Tizen SSH Cmd")))))

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
	       ;; TODO: srecode mode table isn't always populated!
	       ;; seems like we have to prime it first with a `srecode-insert'
	       ;; inside of each major mode before it populates it
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

(defun tizen-prefix-pkgconfig-includes (prefix dir)
  (concat
   prefix
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

(defun tizen-system-include-paths (file brdir)
  ""
  (interactive)
  (let ((incs "/usr/include"))
    (with-temp-buffer 
      (insert-file-contents-literally file)
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
		   #'(lambda (pkg)
		       (tizen-system-include-check-pkgconfig 
			pkg	
			pkgconfigpath))
		   libs
		   " ")))))))

    (mapconcat 
     #'(lambda (inc)
	 (tizen-prefix-pkgconfig-includes brdir inc))
     (split-string incs)
     " ")))

(defun tizen-project-cmakelist-parse-include-directories (file)
  (with-temp-buffer
    (let ((incs ""))
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (while (search-forward-regexp 
	      "include_directories[ \n]+?(\\([ \n]*\\(.*?[ \n]*\\)+?\\))"
	      (point-max)
	      t)
	(setq incs (split-string (match-string 1) nil t))
	(setq incs 
	      (mapconcat 
	       #'(lambda (dir) 
		   (if (file-exists-p dir)
		       (progn
			 (message (format "Directory %s exists" dir))
			 (concat "/"
				 (directory-file-name dir)))
		     " "))
	       incs
	       " ")))
      incs)))

(defun tizen-ede-cpp-root-project (&optional profile file)
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
      (tizen-system-include-paths 
       file
       (tizen-gbs-get-buildroot-for-profile profile))
      nil
      t)
     :spp-table '( ("CONST" . "const"))
     :local-variables
     '((compile-command . "gbs build -A armv7el --include-all")))))

;; NEW CODE BEGIN

(defun tizen-gbs-conf-new-profile (gbsfile user passx obs repos buildroot)
  (save-excursion
    (switch-to-buffer (find-file-existing gbsfile) t)
    (save-excursion 
      (goto-char (point-min))
      (let ((regex (rx bol "[profile." 
		       (minimal-match 
			(one-or-more (zero-or-more not-newline) eol))
		       bol (zero-or-more (any blank)) eol)))
	(when (search-forward-regexp regex (point-max) t)
	  (message (match-string 0))))))
)

(defun tizen-gbs-get-key (id)
  (let* ((id-regex  (eval `(rx bol ,id 
			       (zero-or-more (any blank))
			       "="
			       (zero-or-more (any blank))
			       (group word-start (one-or-more any) word-end)))))
    (when (search-forward-regexp id-regex (point-max) t)
      (message (match-string 1)))))

(defun tizen-gbs-insert-new-profile (gbsfile newprofile)
 (save-window-excursion 
   (let ((regex (rx
		 bol "[profile." )))
     (find-file-existing gbsfile)
     (goto-char (point-min))
     (when (search-forward-regexp regex (point-max) t)
       (beginning-of-line)
       (insert (format "%s\n" newprofile))))))

(defun tizen-gbs-format-new-profile (name user passwdx repos buildroot)
  (format (concat "[profile.%s]\n" 
		  "user = %s\n"
		  "passwdx = %s\nobs = %s\nrepos = %s\nbuildroot = %s\n")
	  name
	  user
	  passwdx
	  "obs.slp"  ;; TODO: hardcoded OBS for now
	  repos
	  buildroot))

(defun tizen-gbs-insert-new-repo (gbsfile newrepo)
 (save-window-excursion 
   (let ((regex (rx
		 bol "[repo." )))
     (find-file-existing gbsfile)
     (goto-char (point-min))
     (when (search-forward-regexp regex (point-max) t)
       (beginning-of-line)
       (insert (format "%s\n" newrepo))))))

(defun tizen-gbs-format-new-repo (name url)
  (format (concat "[repo.%s]\nurl = %s\n") name url))

(defun tizen-gbs-conf-repo-url-list (gbsfile)
  (save-window-excursion
    (let ((repo-regex (rx bol
			  "url"
			  (zero-or-more (any blank))
			  "="
			  (zero-or-more (any blank))
			  (group word-start (one-or-more any) word-end))))
      (switch-to-buffer (find-file-existing gbsfile))
      (goto-char (point-min))
      (loop while (search-forward-regexp repo-regex (point-max) t)
	    collect (match-string 1)))))

;(tizen-gbs-conf-repo-url-list tizen-gbs-conf)

(defvar tizen-gbs-profiles-dir (file-name-directory 
				(expand-file-name "~/tizen/gbs/profiles")))


(defun tizen-gbs-build-new-profile (prjdir &optional user passwdx)
  (let* ((gbsfile tizen-gbs-conf)
	 (repos (ido-completing-read "Repo/Snapshot URL: "
					 (tizen-gbs-conf-repo-url-list 
					  gbsfile)))
	     (profilename (file-name-nondirectory prjdir))
	     (reponame (concat "repo." profilename))
	     (profiledir (file-name-as-directory 
			  (concat 
			   (file-name-as-directory tizen-gbs-profiles-dir)
			   profilename)))
	     (br profiledir))

    (save-window-excursion
      (find-file-existing tizen-gbs-conf)
      (goto-char (point-min))
      (let ((u (or user (tizen-gbs-get-key "user")))
	  (p (or passwdx (tizen-gbs-get-key "passwdx"))))
	

	(when (file-exists-p profiledir)
	  (error (format "Profile directory %s already exists!" profiledir)))

	(mkdir profiledir t)

	(tizen-gbs-insert-new-repo 
	 gbsfile
	 (tizen-gbs-format-new-repo profilename repos))

	(tizen-gbs-insert-new-profile 
	 gbsfile
	 (tizen-gbs-format-new-profile profilename u p reponame br))))))

;(tizen-gbs-build-new-profile "~/tizen/git/clownviewer")



(defun tizen-project-create-dir-locals-cflags (prjdir brdir)
  (setq ac-clang-cflags 
	(mapcar 
	 #'(lambda (arg) (concat "-I" arg))
	 (append 
	  (mapcar #'(lambda (suffix) 
		      (concat (file-name-as-directory brdir) suffix))
		  (split-string
		   "/local/scratch.armv7l.0/usr/include/c++/4.5.3
 /local/scratch.armv7l.0/usr/include/c++/4.5.3/armv7l-tizen-linux-gnueabi
 /local/scratch.armv7l.0/usr/include/c++/4.5.3/backward
 /local/scratch.armv7l.0/usr/lib/gcc/armv7l-tizen-linux-gnueabi/4.5.3/include
 /local/scratch.armv7l.0/usr/lib/gcc/armv7l-tizen-linux-gnueabi/4.5.3/include-fixed
 /local/scratch.armv7l.0/usr/include"))
	  (split-string
	   (tizen-system-include-paths
	    (concat (file-name-as-directory prjdir)
		    "CMakeLists.txt")))
	  (folder-dirs-recursive (concat 
				  (file-name-as-directory prjdir)
				  "/include/")))))



  )

(defun tizen-project-create-dir-locals (prjdir)
  
)

(require 'json)

(defun brian-cflags-hack-out-arm (args)
  (let ((oldrx (regexp-opt '("-mthumb"
			     "-mfpu"
			     "-mlittle-endian"
			     "-mfpu"
			     "-mfloat-abi"
			     "-D__SOFTFP__"
			     "-it=thumb"
			     "-march="
			     "-mtune="))))
    (loop for arg in args
	 if (string-match oldrx arg)
	 do (setq arg "")
	 collect arg)))

(defun brian-include-directives-substitute (args oldpath newpath)
  (let ((oldrx (eval `(rx "-I" (group ,oldpath)))))
    (loop for arg in args
	 if (string-match oldrx arg)
	 do (setq arg (replace-regexp-in-string oldrx newpath arg))
	 collect arg)))

(defun tizen-project-read-compile-commands (ccfile)
  "Read a compile_commands.json file, CCFILE, and return a list
  with format

filename (arg1 arg2 ... argN) 

where the final argument (the filename) has been cut from the arg
list."
  (let ((ccs (json-read-file ccfile)))
    (loop for n from 0 to (1- (length ccs))
	  for entry = (aref ccs n)
	  for file = (cdr (assoc 'file entry))
	  for cmd = (cdr (assoc 'command entry))
	  for cmdlist = (subseq (split-string cmd) 0 -2)
	  ;for modcmdlist = 
	  collect (list file (cdr cmdlist)))))

(defun tizen-project-ac-clang-cflags-from-ccmds (ccfile srcfile)
  "Given a compile_commands.json file, returns the relevant
cflags for it in a format ready for `ac-clang-cflags'."
  (car-safe 
   (cdr-safe (assoc-if 
	      #'(lambda (file) (string-match 
				(file-name-sans-extension srcfile)
				(file-name-sans-extension file)))
	      (tizen-project-read-compile-commands ccfile)))))

;; (assoc-if #'(lambda (file) (message file))
;; 	  (tizen-project-read-compile-commands "~/tizen/git/libwakeup/compile_commands.json"))

(when (file-exists-p tizen-gbs-conf)
  (brian-include-directives-substitute
   (tizen-project-ac-clang-cflags-from-ccmds 
    "~/tizen/git/libwakeup/compile_commands.json"
    "wu_independent_verify.c")
   "/home/abuild/rpmbuild/BUILD/libwakeup-0.0.9"
   "/home/terranpro/tizen/git/libwakeup")

  (brian-include-directives-substitute
   (tizen-project-ac-clang-cflags-from-ccmds 
    "~/tizen/git/libwakeup/compile_commands.json"
    "wu_independent_verify.c")
   "/usr/"
   "/home/terranpro/tizen/SURC/build/local/scratch.armv7l.0/usr/"))

;; (tizen-project-ac-clang-cflags-from-ccmds 
;;  "~/tizen/git/voice-talk2/compile_commands.json"
;;  "auto_test.cpp")

(defun tizen-project-ac-clang-cflags-generic (srcfile)
  (append
   (list "-std=c++0x")
   (mapcar 
    #'(lambda (arg) (concat "-I" arg))
    (mapcar
     'expand-file-name
     (append
      (mapcar
       #'(lambda (dir)
	   (concat tizen-gbs-current-profile-br dir))
       
       '("/local/scratch.armv7l.0/usr/include/c++/4.5.3"
	 "/local/scratch.armv7l.0/usr/include/c++/4.5.3/armv7l-tizen-linux-gnueabi"
	 "/local/scratch.armv7l.0/usr/include/c++/4.5.3/backward"
	 "/local/scratch.armv7l.0/usr/lib/gcc/armv7l-tizen-linux-gnueabi/4.5.3/include" 
	 "/local/scratch.armv7l.0/usr/lib/gcc/armv7l-tizen-linux-gnueabi/4.5.3/include-fixed" 
	 "/local/scratch.armv7l.0/usr/include"))
      (split-string
       (tizen-system-include-paths
	(concat ac-clang-project-directory
		"/CMakeLists.txt")
	
	tizen-gbs-current-profile-br))
      (mapcar
       'expand-file-name
       (folder-dirs-recursive
	(concat ac-clang-project-directory
		"/include/"))))))))

(defun tizen-project-new (prjdir &optional enable-clang enable-ede)
  (interactive)
  (let ((prjname (file-name-nondirectory prjdir)))
    (tizen-gbs-build-new-profile prjdir)

   (when enable-clang 
     )

   (when enable-ede
     ))
  
)
(defun tizen-project-dir-locals-set-class-variables (prjdir profile)
  (dir-locals-set-class-variables
   'tizen
   `((nil . ((ac-clang-project-directory . ,prjdir)
	     (tizen-project-directory . ,prjdir)))
     (c++-mode . ((c-basic-offset . 2)
		  (eval . (setq 
			   tizen-gbs-current-profile ,profile
			   tizen-gbs-current-profile-br 
			   (tizen-gbs-get-buildroot-for-profile ,profile)
			   ac-clang-cflags
			   (append
			    (list "-std=c++0x")
			    (mapcar 
			     #'(lambda (arg) (concat "-I" arg))
			     (mapcar
			      'expand-file-name
			      (append
			       (mapcar
				#'(lambda (dir)
				    (concat tizen-gbs-current-profile-br dir))
				
				'("/local/scratch.armv7l.0/usr/include/c++/4.5.3"
				  "/local/scratch.armv7l.0/usr/include/c++/4.5.3/armv7l-tizen-linux-gnueabi"
				  "/local/scratch.armv7l.0/usr/include/c++/4.5.3/backward"
				  "/local/scratch.armv7l.0/usr/lib/gcc/armv7l-tizen-linux-gnueabi/4.5.3/include" 
				  "/local/scratch.armv7l.0/usr/lib/gcc/armv7l-tizen-linux-gnueabi/4.5.3/include-fixed" 
				  "/local/scratch.armv7l.0/usr/include"))
			       (split-string
				(tizen-system-include-paths
				 (concat ac-clang-project-directory
					 "/CMakeLists.txt")
				 
				 tizen-gbs-current-profile-br))
			       (mapcar
				'expand-file-name
				(folder-dirs-recursive
				 (concat ac-clang-project-directory
					 "/include/")))))))
			   )))))))
;; (tizen-project-dir-locals-set-class-variables 
;;  "/home/terranpro/tizen/git/voice-talk2"
;;  "latest")

(defun tizen-project-dir-locals-set-directory-class (prjdir class)
  (dir-locals-set-directory-class prjdir class))

;; TODO: FUCKY BUGGY
;; (tizen-project-dir-locals-set-directory-class "/home/terranpro/tizen/git/voice-talk2" 'tizen)

;; NEW CODE END

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


(defun compile-commands-remove-keywords (rmkeywords)
  (save-excursion
    (let ((keyword-rx (regexp-opt rmkeywords)))
     (while (search-forward-regexp keyword-rx (point-max) t)
       (replace-match "")))))

(defun compile-commands-replace-includes-local (incdir)
  (save-excursion
    (let* ((srcinc-rx (rx "-I/home/abuild/rpmbuild/BUILD/"
			  (one-or-more (not (any "/" " ")))))
	   (incdir-repl (concat "-I" 
				(expand-file-name
				 (directory-file-name incdir)))))
      (while (search-forward-regexp srcinc-rx (point-max) t)
	(replace-match incdir-repl)))))

(defun compile-commands-replace-includes-global (brdir)
  (save-excursion
    (let* ((globinc-rx (rx "-I" (group "/usr/" (or "lib" "include"))))
	   (brinc-dir (expand-file-name 
		       (directory-file-name (concat brdir))))
	   (brinc-rpl (concat "-I" brinc-dir "\\1")))
      (while (search-forward-regexp globinc-rx (point-max) t)
	(replace-match brinc-rpl)))))

(defun compile-commands-add-include-global (brdir)
  (save-excursion
    (let* ((brinc-dir (expand-file-name 
		       (directory-file-name (concat brdir)))))
	(while (search-forward-regexp "command\":" (point-max) t)
	  (if (search-forward-regexp "-I" (point-max) t)
	      (replace-match (concat "-I" brinc-dir "/usr/include"
				     " " "-I")))))))

(defun compile-commands-replace-includes (brdir incdir)
  (compile-commands-replace-includes-global brdir)
  (compile-commands-add-include-global brdir)
  (compile-commands-replace-includes-local incdir))

(defun compile-commands-replace-srcs (srcdir)
  (save-excursion
    (let* ((src-rx (rx "/home/abuild/rpmbuild/BUILD/"
		       (one-or-more (not (any "/" "\"")))))
	   (srcdir-repl (concat (expand-file-name
				 (directory-file-name srcdir)))))
      (while (search-forward-regexp src-rx (point-max) t)
	(replace-match srcdir-repl)))))

(defun compile-commands-deroot (ccfile brdir prjdir rmkeywords)
  (with-current-buffer (get-buffer-create (generate-new-buffer "deroot"))
    (insert-file ccfile)
    (goto-char (point-min))
    (compile-commands-remove-keywords rmkeywords)
    (compile-commands-replace-includes brdir prjdir)
    (compile-commands-replace-srcs prjdir)
    (current-buffer)))

(defvar compile-commands-nuke-keywords-arm
  '("-march=armv7-a" 
    "-fmessage-length=0"
    "-mtune=cortex-a8"
    "-mfpu=vfpv3"
    "-mfloat-abi=softfp"
    "-D__SOFTFP__"
    "-mthumb"
    "-mlittle-endian"
    "-Wa,-mimplicit-it=thumb")
  "List of strings to search for and remove from the
  compile_commands JSON file's compiler flags.")

(defun tizen-gbs-build-update-compile-commands (&optional 
						buildroot-dir
						gbsroot-cc-file 
						gitprj-dir
						nuke-keywords)
  (interactive)
  (unless buildroot-dir
    (if tizen-gbs-built-rpm-directory
	(setq buildroot-dir (progn 
			      (string-match "\\(.*local/\\)" 
					    tizen-gbs-built-rpm-directory)
			      (concat (match-string 1 tizen-gbs-built-rpm-directory) 
				      "scratch.armv7l.0")))
      (setq buildroot-dir (ido-read-directory-name 
			   "GBS Root (ex: .../local/armv7l.0/): "
			   "~/tizen/builds/"))))
  (unless gbsroot-cc-file
    (setq gbsroot-cc-file
	  (ido-read-file-name "Compile Commands JSON File: " 
			      (expand-file-name 
			       (concat buildroot-dir
				       "/home/abuild/rpmbuild/BUILD")))))

  (unless gitprj-dir
    (if tizen-project-directory
	(setq gitprj-dir tizen-project-directory)
      (setq gitprj-dir (ido-read-directory-name "GBS GIT Project Directory: "
						"~/tizen/git/"))))

  (unless nuke-keywords
    (setq nuke-keywords compile-commands-nuke-keywords-arm))
  (pop-to-buffer
   (compile-commands-deroot gbsroot-cc-file
			    buildroot-dir
			    gitprj-dir
			    nuke-keywords))
  (write-file (expand-file-name (concat gitprj-dir "/compile_commands.json")) t))

;;(tizen-gbs-build-update-compile-commands)


;; ;; libwakeup
;; (compile-commands-deroot "~/tizen/git/libwakeup/compile_commands.json.GBSROOT"
;; 			 "~/tizen/builds/eur-open-mk1/local/scratch.armv7l.0"
;; 			 "~/tizen/git/libwakeup/"
;; 			 compile-commands-nuke-keywords-arm)

;; ;; svoice
;; (compile-commands-deroot "~/tizen/git/voice-talk2/compile_commands.json.GBSROOT"
;; 			 "~/tizen/builds/eur-open-mk1/local/scratch.armv7l.0"
;; 			 "~/tizen/git/voice-talk2/"
;; 			 compile-commands-nuke-keywords-arm)

;; ;; libsttnuance
;; (compile-commands-deroot "~/tizen/builds/eur-open-mk1/local/scratch.armv7l.0/home/abuild/rpmbuild/BUILD/libsttnuance-0.0.11/compile_commands.json"
;; 			 "~/tizen/builds/eur-open-mk1/local/scratch.armv7l.0"
;; 			 "~/tizen/git/libsttnuance/"
;; 			 compile-commands-nuke-keywords-arm)

;;(expand-file-name (directory-file-name "~/tizen"))

;; (require 'brian-tizen)

;; (tizen-ede-cpp-root-project 
;;   "/home/terranpro/tizen/testgbs/elm-demo-tizen/CMakeLists.txt")

;; (tizen-ede-cpp-root-project 
;;  "/home/terranpro/tizen/gbs-git/ebook/CMakeLists.txt")

;; (tizen-ede-cpp-root-project 
;;  "/home/terranpro/eb-git/ebookviewer/CMakeLists.txt")

;; (let ((tizen-projects 
;;        '(
;; 	 ("slp" "/home/terranpro/tizen/git/ebook/CMakeLists.txt")
;; 	 ("surc" "/home/terranpro/tizen/git/ebookviewer/CMakeLists.txt")
;; 	 ("slp" "/home/terranpro/tizen/git/pte-standalone/CMakeLists.txt")
;; 	 ("svoice" "/home/terranpro/tizen/git/voice-talk2/CMakeLists.txt"))))
;;   (mapc '(lambda (prj)
;; 	   (when (file-exists-p (cadr prj))
;; 	     (tizen-ede-cpp-root-project (car prj) (cadr prj))))
;; 	tizen-projects))

;; (tizen-ede-cpp-root-project 
;;  "/home/terranpro/tizen/git/tizen15/CMakeLists.txt")

;(tizen-system-include-paths "/home/terranpro/tizen/git/tizen15/CMakeLists.txt")

;;; brian-tizen.el ends here
