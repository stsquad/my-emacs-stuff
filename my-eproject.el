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
		    "make release PLATFORM=Linux_OE_RC"))

					;      (add-hook cbnl-tree-project-file-visit-hook '(lambda ()
					;						    (require 'cbnl)))

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

(global-set-key (kbd "C-c c") 'eproject-compile)
(global-set-key (kbd "C-c r") 'recompile)
  
