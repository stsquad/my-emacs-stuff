;;
;; Customisation for eproject (http://github.com/jrockway/eproject)
;;

(require 'eproject)
(require 'eproject-extras)
(require 'eproject-compile)

;; Work around the compiler, (look-for) is actually
;; flet inside eproject's run-selector code.
(declare-function look-for "eproject" t t)

;; Shrink mode line for mode display
(setcdr (assq 'eproject-mode minor-mode-alist) '(" eprj"))

;; Config
(setq eproject-completing-read-function 'eproject--ido-completing-read)

;; Key hooks
; Hook in eproject-compile to normal key-binding
(define-key eproject-mode-map (kbd "C-c c") 'eproject-compile)
(define-key eproject-mode-map (kbd "C-x C-b") 'eproject-ibuffer)

(when (require 'helm-eproject 'nil 'noerror)
  (define-key eproject-mode-map (kbd "C-c h") 'helm-eproject))

;;
;; Individual project definitions
;;

; Functions to build command lines (need my patch)
(defun build-bldv-command-line(root)
  "Build a bldv command line for a given root"
  (let ((remote (file-remote-p root)))
    (when remote
      (let* ((path (substring root (length remote)))
	     (components (split-string path "/"))
	     (bldv "bldv --noems --noapc"))
	(setq bldv (concat
		    bldv
		    (format " --repo_name %s" (nth 3 components))
		    " --version 5.1.25-"
		    (next-rc-build)))
	bldv))))

(defvar vectastar-build-id
  (format-time-string "%-m%d1")
  "VectaStar Build ID")

(defun build-incrementing-rc-release(root)
  "Build a release command line with a version for the day"
  (when (not (file-remote-p root))
    (format
     "cd %s && make release PLATFORM=Linux_OE_RC VECTASTARBUILD=%s"
     root vectastar-build-id)))

(defun next-rc-build()
  "Increment the build id"
  (interactive)
  (if (boundp 'vectastar-build-id)
      (setq vectastar-build-id (format "%s" (+ 1 (string-to-number vectastar-build-id))))
    (setq vectastar-build-id (format-time-string "%-m%d1"))))

(define-project-type cbnl-tree
  (generic)
  (and
   (look-for "Makefile.cleanenv")
   (look-for "build-system"))
  :common-compiles (build-bldv-command-line
		    "make build-nms PLATFORM=Linux_Desktop"
		    "make build-nms PLATFORM=Linux_Desktop EMSDEBUG=1"
		    "make build-nms PLATFORM=Linux_Desktop EMSDEBUG=1 VNMS=0"
		    "make pkg-nms PLATFORM=Linux_Desktop"
		    build-incrementing-rc-release
		    "make -C new_rcd PLATFORM=Linux_Desktop"
		    "make -C packaging PLATFORM=Linux_OE_RC"))


(defun cbnl-compile-buffer-func (mode)
  "Return a compile buffer for a CBNL project"
  (let ((root (ignore-errors (eproject-root))))
    (if root
	(let ((proj-dir (nth 1 (reverse (split-string root "/")))))
	  (concat "* compilation of " proj-dir " *"))
      (concat "*" (downcase mode) "*"))))

(defun cbnl-hook-function ()
  "My hook function for all CBNL projects"
  (require 'cbnl)
  (set
   (make-local-variable 'compilation-buffer-name-function)
   'cbnl-compile-buffer-func))

(add-hook 'cbnl-tree-project-file-visit-hook 'cbnl-hook-function)

;; QEMU
(define-project-type qemu
  (generic-git)
  (look-for "qapi-types.c"))

(add-hook 'qemu-visit-hook '(lambda ()
                              (require 'my-c-mode)
                              (c-set-style "qemu")))

(define-project-type debian-package
  (generic)
  (look-for "debian")
  :common-compiles ("dpkg-buildpackage -rfakeroot"
		    "dpkg-buildpackage -rfakeroot -k834BE2B6"))

(define-project-type android-package
  (generic)
  (look-for "AndroidManifest.xml")
  :common-compiles ("ant compile"))

(define-project-type rockbox
  (generic-git)
  (look-for "../rockbox.git/rbutil")
  :common-compiles ("make" "make install" "make fullzip"))

(define-project-type opennms
  (generic-git)
  (and 
   (look-for "opennms-daemon")
   (look-for "build.sh"))
  :common-compiles ("./compile.pl && ./assemble.pl -Dbuild.profile=dir"))

(add-hook 'opennms-project-file-visit-hook '(lambda ()
					     (load-library "my-onms")))

(define-project-type chrome-extension
  (generic)
  (look-for "manifest.json"))

(add-hook 'chrome-extension-visit-hook '(lambda ()
					  (maybe-load-library "js2-mode")
					  (setq tab-width 2
						c-basic-offset 2
						indent-tabs-mode 't)))

(define-project-type kernel
  (generic-git)
  (look-for "Documentation/CodingStyle")
  :common-compiles ("ARCH=x86 make" "make" "ARCH=x86 make TAGS"))

(add-hook 'kernel-visit-hook '(lambda ()
				(require 'my-c-mode)
				(c-set-style "linux")))

(define-project-type easytag
  (generic-git)
  (look-for "src/et_core.c"))

(add-hook 'easytag-visit-hook '(lambda ()
				(require 'my-c-mode)
				(c-set-style "easytag")))
				 
  
(define-project-type wireshark
  (generic-git)
  (look-for "rawshark.c")
  :common-compiles ("make" "make install"))

(add-hook 'wireshark-visit-hook '(require 'my-c-mode))

;; Turn on eproject on various dev modes
;
; In theory eproject should already be catching this when major modes
; are switched. But this does make sure.
(add-hook 'c-mode-common-hook 'eproject-maybe-turn-on)
(add-hook 'makefile-mode-hook 'eproject-maybe-turn-on)
(add-hook 'java-mode-hook 'eproject-maybe-turn-on)

;;
;; Find methods
;
; I want two find methods, one from the project root (f5) and the
; other potentially from where I am.
;

(when (require 'ack-and-a-half nil 'noerror)
  (defun my-ack-and-a-half-current-directory ()
    "Return the current buffers directory if it exists, else nil"
    (cond
     (dired-directory dired-directory)
     ((and buffer-file-name
	   (file-exists-p buffer-file-name))
      (file-name-directory (file-truename buffer-file-name)))
     (t nil)))
  
  (add-to-list 'ack-and-a-half-root-directory-functions
	       'my-ack-and-a-half-current-directory)
  (setq ack-and-a-half-prompt-for-directory 'unless-guessed)
  (define-key eproject-mode-map (kbd "<f6>") 'ack-and-a-half))

(defun my-eproject-find ()
  "Do a find across the project"
  (interactive)
  (if (file-exists-p (concat eproject-root ".git"))
      (if (functionp 'helm-git-grep)
	  (helm-git-grep)
	(let* ((search (my-find-search-string))
               (buffer (concat "*git grep for " search "*" ))
               (command (format "git grep -n %s -- %s" search eproject-root)))
	  (setq grep-command "git grep -n ")
	  (message "Using git grep for searches")
	  (shell-command command buffer)
	  (pop-to-buffer buffer)
	  (grep-mode)))
    (funcall my-project-find-fallback-func eproject-root (my-find-search-string))))

(define-key eproject-mode-map (kbd "<f5>") 'my-eproject-find)

