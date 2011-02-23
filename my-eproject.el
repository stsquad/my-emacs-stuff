;;
;; Customisations for eproject (http://github.com/jrockway/eproject)
;;

(require 'eproject)
(require 'eproject-extras)

(define-project-type cbnl-tree
  (generic-git)
  (and
   (look-for "Makefile.cleanenv")
   (look-for "build-system"))
  :common-compiles ("make build-nms PLATFORM=Linux_Desktop"
		    "make build-nms PLATFORM=Linux_Desktop EMSDEBUG=1"
		    "make build-nms PLATFORM=Linux_Desktop EMSDEBUG=1 VNMS=0"
		    "make pkg-nms PLATFORM=Linux_Desktop"
		    "make release PLATFORM=Linux_OE_RC"
		    "make -C packaging PLATFORM=Linux_OE_RC"))

(add-hook 'cbnl-tree-project-file-visit-hook '(lambda ()
					       (require 'cbnl)))

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
  :common-compile ("ARCH=x86 make" "make" "ARCH=x86 make TAGS"))

(add-hook 'kernel-visit-hook '(lambda ()
				(require 'my-c-mode)
				(c-set-style "linux")))

; Hook in compile
(global-set-key (kbd "C-c c") 'compile)
(define-key eproject-mode-map (kbd "C-c c") 'eproject-compile) 
(global-set-key (kbd "C-c r") 'recompile)

;; Turn on eproject on various dev modes
;
; In theory eproject should already be catching this when major modes
; are switched. But this does make sure.
(add-hook 'c-mode-common-hook 'eproject-maybe-turn-on)
(add-hook 'makefile-mode-hook 'eproject-maybe-turn-on)
(add-hook 'java-mode-hook 'eproject-maybe-turn-on)

(defun my-eproject-find (&optional search)
  "Do a find across the project"
  (interactive "sSearch string:")

  (if (file-exists-p (concat eproject-root "/.git"))
      (let ((buffer (concat "*git grep for " search "*" ))
	    (command (concat "git grep -n " search)))
	(setq grep-command "git grep -n ")
	(message "Using git grep for searches")
	(shell-command command buffer)
	(pop-to-buffer buffer)
	(grep-mode)))
  (grep-find (concat
	      "find "
	      eproject-root " -print0 |  xargs -0 -e grep -n -e " search)))
  

(unless (global-key-binding (kbd "<f5>"))
  (global-set-key (kbd "<f5>") 'my-eproject-find))

