; -*- emacs-lisp -*-
;
; Cambridge Broadband Emacs Setup
;
; A bunch of handy customisations for hacking around with CBNL
; code.
;
; This assumes certain functions exist from my dotemacs

(message "Doing local CNBL Customisations")

;; Sanity

(if (or (not (functionp 'chomp))
	(not (functionp 'extract-string)))
    (error "Need some string munging functions defined"))

;; Project Root variables
;
; These are defined in my .emacs and are re-implemented here for
; portability reasons.
;
; The current project root, used to build stuff later
(if (not (bound-and-true-p current-project-root))
    (progn

      (defvar current-project-root
	(concat (chomp (shell-command-to-string "pwd")))
	"Describes the current project root dir (top of src tree)")

      (defvar project-root-history 'current-project-root
	"A history of projects I have been to")

      ; this is redefined later to someting smarter
      (defun set-project-root ()
	"Set a primary project root"
	(interactive)
	(setq current-project-root
	      (read-file-name "Project Root:")))))

;; Project Variables
;
; These two variables are used to suply the make target invocation
; for something in vectastar. The full string includes 

(defvar cbnl-build-targets
  '("-C common/libasync -f Makefile"
    "-C common/libtimer -f Makefile"
    "-C common/libdbg -f Makefile"
    "-C net-snmp -f Makefile.net-snmp"
    "-C third-party/libxml -f ../local-config/Makefile.libxml"
    "-C nms-manager-apps -f Makefile")
  "A List of CBNL build targets")

;; current-build-target
;
; *CHANGE* You can set your default build target here
(defvar current-build-target
  (concat "-C nms-manager-apps -f Makefile")
  "Describes the current build target")

;; current-build-flags
;
; *CHANGE* You can set your default build flags here.

(defvar cbnl-build-flags
  '("VECTASTARBUILD=1 CROSS_COMPILE=ppc8xx"
    "GTK=2")
  "Describes the current build flags")


(defvar current-build-flags
  (concat "GTK=2")
  "Describes the current default build flags")

(message "Defined CBNL project variables")

;; Sanity
;
; I only expect to start in a working directory of checked out src
; tree. So there should be a `pwd`/build-system

(let ((bs (concat current-project-root "/build-system")))
  (if (not (file-exists-p bs))
      (warn "current-project-root is not a CBNL project directory")
    (message (concat "Project directory is " current-project-root))))

;; set-cbnl-compile-command
;
; Construct a compile command for building something in the CBNL
; environment. The compile command is of the form:
;
; cd "current-project-root" && make "current-build-target" "current-build-flags"
;
; e.g.
;
; cd /eng/ajb/reference.cvs && make -C nms-manager-apps -f Makefile GTK=2

(defun set-cbnl-compile-command ()
  "Set the compile command for a cbnl project"
  (interactive)
  (setq compile-command
	(format "cd %s && make %s %s"
		current-project-root
		current-build-target
		current-build-flags)))

; And actually do it
(set-cbnl-compile-command)

;; Switch Targets/Flags
;
; Allow easy swithcing based on history

; set-current-build-target
(defun set-current-build-target ()
  "Switch build target"
  (interactive)
  (setq current-build-target
	(read-string "New target:"
		     nil
		     'cbnl-build-targets
		     0))
  (set-cbnl-compile-command))

; set-current-build-flags
(defun set-current-build-flags ()
  "Switch build flags"
  (interactive)
  (setq current-build-flags
	(read-string "New flags:"
		     nil
		     'cbnl-build-flags
		     0))
  (set-cbnl-compile-command))


(message "Done with cbnl customisations")

