;;; my-eproject.el --- Customisation for eproject
;;
;;; Commentary:
;;
;; eproject (http://github.com/jrockway/eproject) is a project
;; management framework for Emacs.
;;
;;; Code:

(require 'use-package)
(require 'my-find)
(require 'my-c-mode)

;; Helper variables
(defvar my-core-count
  (string-to-number
   (shell-command-to-string
    "cat /proc/cpuinfo | grep processor | wc -l"))
  "Count of the CPU cores on this system.")

(use-package eproject
  :commands define-project
  :diminish "eprj"
  :config
  (progn
    ;; Key bindings
    (define-key eproject-mode-map (kbd "C-c c") 'eproject-compile) ; override 'compile
    (define-key eproject-mode-map (kbd "C-x C-b") 'eproject-ibuffer)
    (define-key eproject-mode-map (kbd "C-c h") 'helm-eproject)))

;; Config

(use-package eproject-compile
  :commands eproject-compile)

;; HACK - something is breaking autoloads
(require 'eproject-contrib-autoloads nil t)

(use-package eproject-ibuffer
  :commands eproject-ibuffer
  :config
  (setq eproject-completing-read-function 'eproject--ido-completing-read))

(use-package helm-eproject
  :commands helm-eproject)

(defun my-eproj-is-c ()
  "Require my-c-mode for project."
  (require 'my-c-mode))

(defun my-eproject-c-hook ()
  "Hook for cc-mode to run on eproject based projects."
  (message "in my-eproject-c-hook")
  (eproject-maybe-turn-on)
  (when (and eproject-root (eproject-attribute :c-style))
    (message "setting C style based on eproject")
    (c-set-style (eproject-attribute :c-style))))

(defun my-eproject-is-type-p (type)
  "Return t when current project of `TYPE'."
  (eq (eproject-attribute :type) type))

(defun my-eproject-asm-hook ()
  "Hook for assembly mode."
  (message "Running my-eproject-asm-hook")
  (when buffer-file-name
    (eproject-maybe-turn-on)
    (when (my-eproject-is-type-p 'kernel)
      (message "Setting indent")
      (setq indent-tabs-mode t))))

(add-hook 'c-mode-hook 'my-eproject-c-hook)
(add-hook 'asm-mode-hook 'my-eproject-asm-hook)

;;
;; Individual project definitions
;;

;; ELPA Packages
(define-project-type elpa-pkg
  (generic)
  (look-for "../../elpa"))

(defun my-docker-compile-strings (root make list-of-tags)
  "Return a list of compile strings using docker at `ROOT'."
  (let ((builds '()))
    (--each list-of-tags
      (add-to-list
       'builds
       (format
        "docker run --rm -v %s:/src --user alex:alex -w /src %s %s"
        root it make)))
    builds))

;; QEMU
(define-project-type qemu
  (generic-git)
  (look-for "qemu-log.c")
  :c-style "qemu-c-style")

(add-hook 'qemu-project-file-visit-hook 'my-eproj-is-c)
(add-hook 'qemu-project-file-visit-hook 'whitespace-mode)
(add-hook 'qemu-project-file-visit-hook 'ws-butler-mode)

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

(define-project-type chrome-extension
  (generic)
  (look-for "manifest.json"))

(add-hook 'chrome-extension-visit-hook '(lambda ()
					  (require "js2-mode" nil t)))


(defun my-kernel-build-dirs (root)
  "Return a list of build directories for the kernel at `ROOT'.

Return in order of most recently updated."
  (mapcar #'car
          (sort
           (directory-files-and-attributes
            (file-name-directory (directory-file-name root)) t "build")
           #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))

; (my-kernel-build-dirs "/home/alex/lsrc/kvm/linux.git/")

(defun my-kernel-compile-strings (root)
  "Return a list of compile strings for the kernel at `ROOT'."
  (let ((j-field (format "-j%d" (1+ my-core-count)))
        (build-dirs (my-kernel-build-dirs root))
        (builds '()))
    (--each build-dirs
      (let ((arch
             (cond
              ((string-match "v7" it) "arm")
              ((string-match "v8" it) "arm64")
              (nil))))
        (setq builds
              (if arch
                  (cons
                   (format
                    "cd %s && ARCH=%s make %s" it arch j-field)
                   (cons
                    (format "cd %s && ARCH=%s make gtags" root arch) builds))
                  (cons (format "cd %s && make %s" it j-field) builds)))))
    builds))

; (my-kernel-compile-strings "/home/alex/lsrc/kvm/linux.git/")
  

(define-project-type kernel
  (generic-git)
  (look-for "Documentation/CodingStyle")
  :c-style "linux-tabs-style"
  :common-compiles (my-kernel-compile-strings "make" "make gtags" "make TAGS"))

(add-hook 'kernel-project-file-visit-hook 'my-eproj-is-c)
(add-hook 'kernel-project-file-visit-hook 'ws-butler-mode)

(define-project-type kvm-unit-tests
  (generic-git)
  (look-for "arm/selftest.c")
  :c-style "linux-tabs-style")

(add-hook 'kvm-unit-tests-project-file-visit-hook 'my-eproj-is-c)
(add-hook 'kvm-unit-tests-project-file-visit-hook 'ws-butler-mode)

(define-project-type easytag
  (generic-git)
  (look-for "src/et_core.c")
  :c-style "easytag")

(add-hook 'easytag-project-file-visit-hook 'my-eproj-is-c)
				 
  
(define-project-type wireshark
  (generic-git)
  (look-for "rawshark.c")
  :common-compiles ("make" "make install"))

(add-hook 'wireshark-project-file-visit-hook 'my-eproj-is-c)

(defun my-risu-compile-strings (root)
  (my-docker-compile-strings
   root
   "/bin/sh -c \"./configure -s && make clean && make\""
   '("debian:arm64" "debian:armhf" "debian:ppc64el")))

(define-project-type risu
  (generic-git)
  (look-for "risu.c")
  :c-style "risu-c-style"
  :common-compiles (my-risu-compile-strings))

(define-project-type gtkg
  (generic-git)
  (look-for "gtk-gnutella.spec")
  :c-style "gtkg-style")

;; Turn on eproject on various dev modes
;
; In theory eproject should already be catching this when major modes
; are switched. But this does make sure.
;; (add-hook 'c-mode-common-hook 'eproject-maybe-turn-on)
;; (add-hook 'makefile-mode-hook 'eproject-maybe-turn-on)
;; (add-hook 'java-mode-hook 'eproject-maybe-turn-on)
;; ensure eproject is at the start, before any mode hooks
(setq find-file-hook
      (append '(eproject-maybe-turn-on) (remq 'eproject-maybe-turn-on
                                              find-file-hook)))

;; Find methods
;
; This has gotten overly complex to the point I need to cleanse with
; fire and fixup my-find.el

(defun my-eproject-find ()
  "Do a find across the project."
  (interactive)
  (my-project-find eproject-root))

(define-key eproject-mode-map (kbd "<f5>") 'my-eproject-find)
(define-key eproject-mode-map (kbd "<S-s-XF86TouchpadToggle>") 'my-eproject-find)

(provide 'my-eproject)
;;; my-eproject.el ends here
