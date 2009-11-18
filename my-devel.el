;; Development Hooks
;;
;; All other development modes should be triggered from here.
;;

;; Define a current-project-root
;
; In most cases I open emacs at the top of a src tree. Searches and compiles
; can then use this as a starting point
;
; change with (setq current-project-root (chomp (shell-command-to-string "pwd")))
(defvar current-project-root
  (concat (chomp (shell-command-to-string "pwd")))
  "Describes the current project root dir (top of src tree)")

(defun set-project-root ()
  "Set a primary project root"
  (interactive)
  (setq current-project-root
	(read-file-name "Project Root:")))

;; secondary-project-root
;
; Sometimes your working with two source trees. The secondary tree is
; tracked and useful for bringing up ediff's and the like
;

(defvar secondary-project-root
  ()
  "Describes a secondary project root (say for quick ediff)")

(defun set-secondary-root ()
  "Set a secondary project root"
  (interactive)
  (setq secondary-project-root
	(read-file-name "Secondary Project Root:")))

(defun set-secondary-root-if-not ()
  "Check is secondary-project-root is defined, if not set it"
  (if (not secondary-project-root)
      (set-secondary-root)))

;; my-first-compile
; Try and guess what we are doing and then call compile, we assume
; that any of the other modes will rebind the compile keys once done
(defun my-first-compile ()
  "First compile standard hook, decide what to do"
  (interactive)
  (message "my-first-compile: start")
  (cond
   ; Work
   ((file-exists-p (concat current-project-root "/build-system"))
    (load-library "cbnl"))
   ; Android
   ((file-exists-p (concat current-project-root
			   "/AndroidManifest.xml"))
    (load-library "my-android"))
   ; Rockbox
   ((file-exists-p (concat current-project-root "/rbutil"))
    (load-library "my-c-mode")
    (setq compile-command (format
			   "cd %s/../ipodsim.build && make -k"
			   current-project-root)))
   ; OpenNMS
   ((and (file-exists-p (concat current-project-root "/build.sh"))
	 (file-exists-p (concat current-project-root "/pom.xml")))
    (load-library "my-onms"))
   ; Generic Makefile driven C
   ((file-exists-p (concat current-project-root "/Makefile"))
    (load-library "my-c-mode"))
   (t
    (setq compile-command (format
			   "cd %s && make -k" current-project-root))))
  (compile (eval compile-command)))
		   
(global-set-key (kbd "C-c c") 'my-first-compile)
(global-set-key (kbd "<f3>")  'my-first-compile)

;; Compilation mode tweaks
(if I-am-emacs-23+
    (setq compilation-auto-jump-to-first-error t))
(setq compilation-scroll-output t
      compilation-window-height 10)

(add-hook 'compilation-mode-hook '(lambda()
				    (define-key compilation-mode-map (kbd "n") 'compilation-next-error)
				    (define-key compilation-mode-map (kbd "p") 'compilation-previous-error)))

;; Makefiles
;
; The auto-mode-alist isn't quite set-up to handle Makefile.something
(setq auto-mode-alist
      (append (list (cons "Makefile\.*" 'makefile-gmake-mode))
	      auto-mode-alist))
