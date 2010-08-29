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
		    "make pkg-nms PLATFORM=Linux_Desktop"
		    "make release PLATFORM=Linux_OE_RC"
		    "make -C packaging PLATFORM=Linux_OE_RC"))

(add-hook 'cbnl-tree-project-file-visit-hook '(lambda ()
					       (require 'cbnl)))

(define-project-type debian-package
  (generic)
  (look-for "debian")
  :common-compiles ("dpkg-buildpackage -rfakeroot"))

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
  :common-compiles ("./build.sh install assembly:directory-inline"))

(add-hook 'opennms-project-file-visit-hook '(lambda ()
					     (load-library "my-onms")))


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

