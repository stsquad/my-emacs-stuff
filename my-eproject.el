;;; my-eproject.el --- Customisation for eproject
;;
;;; Commentary:
;;
;; eproject (http://github.com/jrockway/eproject) is a project
;; management framework for Emacs.
;;
;;; Code:

(require 'eproject)
(require 'eproject-extras)
(require 'eproject-compile)
(require 'my-find)
(require 'ack-and-a-half nil t)

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

;; QEMU
(define-project-type qemu
  (generic-git)
  (look-for "qapi-types.c"))

(defun qemu-setup ()
  "Setup for QEMU."
  (interactive)
  (message "running qemu-setup hook")
  (require 'my-c-mode)
  (c-set-style "qemu-c-style"))

(add-hook 'qemu-visit-hook 'qemu-setup)

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

(provide 'my-eproject)
;;; my-eproject.el ends here
