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
(defvar tizen-gerrit-server-address "165.213.149.219")
(defvar tizen-gerrit-server-port "29418")
(defvar tizen-gerrit-server-userid "br.fransioli"
  "")
(defvar tizen-packages-root-directory "/home/terranpro/tizen/gbs-git")

(defvar tizen-lthor-executable "/home/terranpro/tizen/lthor"
  "")
(defvar tizen-binaries-directory "/home/terranpro/tizen/binaries"
  "")
(defvar tizen-gbs-built-rpm-directory "/home/terranpro/tizen/SURC/rpms"
  "")

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

(defun tizen-download-binary ()
  ""
  (interactive)
  )
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

(defun tizen-build-package-gbs ()
  ""
  (interactive)
  (let* ((arch "armv7l")
	 (misc-args "--include-all")
	 (cmd (concat "gbs build "
		      "-A " arch " "
		      misc-args
		      " &")))
    ;(cd )
    (shell-command cmd
		   "Tizen GBS Build")))

(defun tizen-sdb-push-file (file)
  ""
  (let* ((local-file (and (file-exists-p file)
			  file))
	 (remote-dir "/root/")
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
  (let* ((image (ido-completing-read 
		 "RPM: "
		 (directory-files 
		  tizen-gbs-built-rpm-directory
		  t 
		  (rx (one-or-more any ".rpm"))
		  nil))))
    (tizen-sdb-push-file image)
;; pkgcmd -i -t rpm -p PATH_TO_RPM
    (tizen-sdb-shell-cmd (concat "rpm -ivh --force "
			  ;;"pkgcmd -i -t rpm -p "
				 "/root/"
				 (file-name-nondirectory image)))))

(defun tizen-ldd-executable ()
  ""
  (interactive)
  (let* ((exec-path )
	 ()
	 (cmd))
    ))

;; TODO: finish
(defun tizen-create-barebone-project (&optional parent-dir project-name)
  ""
  (interactive "D")
  (save-window-excursion
    (let* ((parentdir (expand-file-name 
		       (or parent-dir 
			   (ido-read-directory-name 
			    "Project Parent Directory: "
			    default-directory))))
	   (prj-dir (expand-file-name 
		     (or parent-dir 
			 (ido-read-directory-name 
			  "Project Name (Directory): "
			  parentdir))))
	   (prj-name (file-name-directory prj-dir))
	   (spec-file (concat prj-dir
			      "/"
			      prj-name
			      ".spec")))

      )))

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
(tizen-ede-cpp-root-project 
 "/home/terranpro/tizen/git/tizen15/CMakeLists.txt")
;(tizen-system-include-paths "/home/terranpro/tizen/git/tizen15/CMakeLists.txt")
(tizen-system-include-paths
 "/home/terranpro/tizen/git/ebookviewer/CMakeLists.txt")
;;; brian-tizen.el ends here
